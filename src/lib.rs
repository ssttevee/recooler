#![deny(clippy::all)]
mod banned_imports;
mod es_ast_helpers;
mod head;
mod htmx;
mod ids;
mod imports;
mod layouts;
mod nested_compile;
mod routes;

use std::{
  cell::RefCell,
  collections::{HashMap, HashSet},
  fs,
  path::PathBuf,
  sync::{Arc, Mutex},
};

use farmfe_compiler::Compiler;
use farmfe_core::{
  config::{bool_or_obj::BoolOrObj, Config, SourcemapConfig},
  context::CompilationContext,
  error::{CompilationError, Result},
  module::{ModuleId, ModuleMetaData, ModuleType},
  plugin::*,
  regex::Regex,
  relative_path::{PathExt, RelativePathBuf},
  serde_json,
};
use farmfe_macro_plugin::farm_plugin;
use farmfe_toolkit::swc_atoms::AtomStore;

use htmx::FormActionMethod;
use ids::GlobalIds;
use imports::UniqueImportIdentifiers;
use routes::file_to_route_path;

#[derive(Debug, serde::Deserialize)]
#[serde(untagged)]
pub enum RouteHandlerFactoryInput {
  String(String),
  Tuple1((String,)),
  Tuple2((String, Option<String>)),
}

#[derive(Default, serde::Deserialize)]
pub struct RecoolerOptions {
  pub src_dir: Option<String>,
  pub routes_dir: Option<String>,
  pub route_handler_factory: Option<RouteHandlerFactoryInput>,
  pub action_secret: Option<String>,
  pub client_banned_imports: Option<Vec<String>>,
}

struct ModuleSymbol {
  module: String,
  /// the imported export, the default export when `None`
  export: Option<String>,
  /// the local identifier of the import in the generated entrypoint
  name: String,
}

impl ModuleSymbol {
  fn route_handler_factory(input: Option<RouteHandlerFactoryInput>) -> Self {
    let (module, export) = match input {
      None => (
        "farm-plugin-recooler/helpers".to_string(),
        Some(DEFAULT_ROUTE_HANDLER_IDENT.to_string()),
      ),
      Some(RouteHandlerFactoryInput::String(module))
      | Some(RouteHandlerFactoryInput::Tuple1((module,))) => (module, None),
      // a null name imports the default export, like when it is omitted
      Some(RouteHandlerFactoryInput::Tuple2((module, export))) => (module, export),
    };

    Self {
      module,
      export,
      // the export is aliased so that its name can't collide with the other identifiers of the
      // generated entrypoint, which are either fixed (`app`, `Hono`, `buildHeadFn`, ...) or
      // contain an underscore (see `UniqueImportIdentifiers`)
      name: ROUTE_HANDLER_FACTORY_IDENT.to_string(),
    }
  }

  fn import_statement(&self) -> String {
    format!(
      "import {} from {};",
      match &self.export {
        Some(export) => format!("{{ {} as {} }}", export, self.name),
        None => self.name.clone(),
      },
      serde_json::to_string(&self.module).unwrap()
    )
  }
}

#[farm_plugin]
pub struct FarmPluginRecooler {
  src_dir: String,
  routes_dir: String,
  ids: Mutex<RefCell<GlobalIds>>,
  atom_store: Mutex<RefCell<AtomStore>>,
  scanned_dir: farmfe_core::parking_lot::RwLock<Option<routes::ScannedDir>>,
  scanned_routes: farmfe_core::parking_lot::RwLock<Option<routes::ScanResult>>,
  route_handler_factory: ModuleSymbol,
  action_secret: Option<String>,
  /// patterns for modules that must not be included in client scripts
  client_banned_imports: Vec<Regex>,
  /// errors in the plugin options, reported by the `config` hook since the plugin can't fail to
  /// be created: a panic while it is being created aborts the host process
  option_errors: Vec<String>,
  /// compiled client scripts keyed by their generated entry source, so that
  /// routes with identical client entries are only compiled once per build
  client_script_cache: Mutex<HashMap<String, CachedClientScript>>,
}

/// A client script compiled by [`FarmPluginRecooler::build_client_script`].
struct CachedClientScript {
  /// the script, or the banned modules that it includes, see
  /// [`banned_imports::banned_client_modules`]
  script: std::result::Result<String, Vec<String>>,
  /// the routes that use the script
  routes: Vec<String>,
}

static DEFAULT_ROUTE_HANDLER_IDENT: &str = "jsxRouteHandler";
static ROUTE_HANDLER_FACTORY_IDENT: &str = "routeHandlerFactory";

impl FarmPluginRecooler {
  fn new(_config: &Config, options_str: String) -> Self {
    let mut option_errors = vec![];

    let options = serde_json::from_str::<RecoolerOptions>(&options_str).unwrap_or_else(|e| {
      option_errors.push(e.to_string());
      Default::default()
    });

    let mut client_banned_imports = vec![];
    for pattern in options.client_banned_imports.unwrap_or_default() {
      match Regex::new(&pattern) {
        Ok(regex) => client_banned_imports.push(regex),
        Err(e) => option_errors.push(format!(
          "invalid client_banned_imports pattern {:?}: {}",
          pattern, e
        )),
      }
    }

    let cwd = std::env::current_dir().unwrap();
    // falls back to the configured path, which is only used until the error is reported
    let mut relative_dir = |option: &str, dir: String| {
      fs::canonicalize(&dir)
        .map_err(|e| e.to_string())
        .and_then(|path| path.relative_to(&cwd).map_err(|e| e.to_string()))
        .unwrap_or_else(|e| {
          option_errors.push(format!("invalid {} {:?}: {}", option, dir, e));
          RelativePathBuf::from(dir)
        })
    };

    let src_dir = relative_dir("src_dir", options.src_dir.unwrap_or("src".to_string()));

    let routes_dir = if let Some(routes_dir) = options.routes_dir {
      relative_dir("routes_dir", routes_dir)
    } else {
      src_dir.join("routes")
    };

    Self {
      src_dir: src_dir.to_string(),
      routes_dir: routes_dir.to_string(),
      ids: Mutex::new(RefCell::new(GlobalIds::new())),
      atom_store: Default::default(),
      scanned_routes: Default::default(),
      scanned_dir: Default::default(),
      action_secret: options.action_secret.filter(|secret| !secret.is_empty()),
      client_banned_imports,
      option_errors,
      client_script_cache: Default::default(),
      route_handler_factory: ModuleSymbol::route_handler_factory(options.route_handler_factory),
    }
  }
}

const CLIENT_MODULE_PREFIX: &str = "recooler:@";
/// The name of the entry of a client script compile.
const CLIENT_ENTRY_NAME: &str = "client";
const ENTRYPOINT_MODULE: &str = "recooler::app";
/// The module that [`ENTRYPOINT_MODULE`] re-exports the generated app from, see
/// `generate_entrypoint_module`.
const ENTRYPOINT_IMPL_MODULE: &str = "recooler::app-impl";
const METADATA_MODULE: &str = "recooler::metadata";
/// Query for the variant of a component module that only contains its event handlers, the route
/// `onLoad` export and what they reference, see [`htmx::transform::slice_handler_module`].
const HANDLERS_MODULE_QUERY: &str = "?recooler-handlers";

impl Plugin for FarmPluginRecooler {
  fn name(&self) -> &str {
    "FarmPluginRecooler"
  }

  fn config(&self, config: &mut Config) -> Result<Option<()>> {
    if !self.option_errors.is_empty() {
      return Err(CompilationError::GenericError(format!(
        "invalid farm-plugin-recooler options:\n{}",
        self.option_errors.join("\n")
      )));
    }

    if config.input.len() == 0 {
      config
        .input
        .insert("main".to_string(), ENTRYPOINT_MODULE.to_string());
    }

    for (k, v) in config.input.iter() {
      if v.eq(format!("./{}", ENTRYPOINT_MODULE).as_str()) {
        config
          .input
          .insert(k.clone(), ENTRYPOINT_MODULE.to_string());
        return Ok(Some(()));
      }

      if v.eq(ENTRYPOINT_MODULE) {
        return Ok(Some(()));
      }
    }

    Ok(Some(()))

    // let mut more: String = String::new();
    // for (k, v) in config.input.iter() {
    //   more += format!(", input.{}: {}", k, v).as_str();
    // }

    // Err(CompilationError::GenericError(
    //   format!(
    //     "bad input config: missing \"{}\" (len: {}{})",
    //     ENTRYPOINT_MODULE,
    //     config.input.len(),
    //     more
    //   )
    //   .to_string(),
    // ))
  }

  fn resolve(
    &self,
    param: &PluginResolveHookParam,
    _context: &Arc<CompilationContext>,
    _hook_context: &PluginHookContext,
  ) -> Result<Option<PluginResolveHookResult>> {
    if param.source == ENTRYPOINT_MODULE
      || param.source == ENTRYPOINT_IMPL_MODULE
      || param.source == METADATA_MODULE
      || param.source.starts_with(CLIENT_MODULE_PREFIX)
    {
      return Ok(Some(PluginResolveHookResult {
        resolved_path: param.source.clone(),
        external: false,
        side_effects: false,
        query: Default::default(),
        meta: Default::default(),
      }));
    }

    Ok(None)
  }

  fn load(
    &self,
    param: &PluginLoadHookParam,
    context: &Arc<CompilationContext>,
    hook_context: &PluginHookContext,
  ) -> Result<Option<PluginLoadHookResult>> {
    if param.module_id == ENTRYPOINT_MODULE || param.module_id == ENTRYPOINT_IMPL_MODULE {
      let content = if param.module_id == ENTRYPOINT_MODULE {
        self.generate_entrypoint_module()
      } else {
        self.generate_entrypoint_impl_module(context, hook_context)
      };
      match content {
        Ok(content) => {
          return Ok(Some(PluginLoadHookResult {
            content,
            module_type: ModuleType::Js,
            source_map: None,
          }))
        }
        Err(e) => return Err(report_load_error(context, &param.module_id, e)),
      }
    }

    if param.module_id == METADATA_MODULE {
      match self.generate_metadata_module() {
        Ok(content) => {
          return Ok(Some(PluginLoadHookResult {
            content,
            module_type: ModuleType::Js,
            source_map: None,
          }))
        }
        Err(e) => return Err(report_load_error(context, &param.module_id, e)),
      }
    }

    if param.module_id.starts_with(CLIENT_MODULE_PREFIX) {
      match self.generate_client_entrypoint(
        context,
        &param.module_id[CLIENT_MODULE_PREFIX.len()..].to_string(),
      ) {
        Ok(content) => {
          return Ok(Some(PluginLoadHookResult {
            content,
            module_type: ModuleType::Js,
            source_map: None,
          }))
        }
        Err(e) => return Err(report_load_error(context, &param.module_id, e)),
      }
    }

    Ok(None)
  }

  fn process_module(
    &self,
    param: &mut PluginProcessModuleHookParam,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    let script = if let ModuleMetaData::Script(script) = param.meta {
      if param.module_id.relative_path().starts_with(&self.src_dir) {
        script
      } else {
        return Ok(None);
      }
    } else {
      return Ok(None);
    };

    // the handlers variant is transformed as the original module so that the handler names and
    // ids are identical, then sliced
    let handlers_slice = param.module_id.query_string() == HANDLERS_MODULE_QUERY;
    let module_id = if handlers_slice {
      ModuleId::new(param.module_id.relative_path(), "", "")
    } else {
      param.module_id.clone()
    };

    let module_path = PathBuf::from(param.module_id.relative_path());
    let is_route_module = routes::is_route_file(&self.routes_dir, &module_path);

    // form actions are only handled on the paths of the pages that render them, so only the
    // actions of a route module, which renders on its own path, get an absolute url; those of any
    // other module, e.g. a layout that renders on every page under its directory, get a relative
    // url that posts to the current page
    let route_pathname = if is_route_module {
      strip_path_route_groups(&routes::file_to_route_path(&self.routes_dir, &module_path))
    } else {
      String::new()
    };

    let mut ids_lock = self.ids.lock().unwrap();
    let mut atoms_lock = self.atom_store.lock().unwrap();

    htmx::transform::transform_module(
      &module_id,
      route_pathname,
      ids_lock.get_mut(),
      atoms_lock.get_mut(),
      &mut script.ast,
      &mut script.comments,
    )
    .map_err(CompilationError::GenericError)?;

    if handlers_slice {
      if let Err(e) = htmx::transform::slice_handler_module(
        &module_id,
        ids_lock.get_mut(),
        &mut script.ast,
        // only the `onLoad` of route modules runs on the client
        is_route_module,
        &self.client_banned_imports,
      ) {
        // reported as is rather than as the cause of a `process_module` error (see
        // `report_load_error`), which fails the client script compile once it finishes; the module
        // is emptied in the meantime so that its server side imports aren't built
        report_errors(context, [e]);
        script.ast.body.clear();
      }
    }

    Ok(Some(()))
  }
}

/// removes the route group components from a route path, using the same rule as the route
/// registration so that the paths always agree with the registered routes
fn strip_path_route_groups(path: &str) -> String {
  routes::route_path_without_groups(path)
}

/// Returns the messages of an error. A failed nested compile reports all of its errors at once as
/// a JSON array, e.g. one per banned client import, which is split into its messages.
fn error_messages(e: &CompilationError) -> Vec<String> {
  if let CompilationError::GenericError(msg) = e {
    if let Ok(messages) = serde_json::from_str::<Vec<String>>(msg) {
      return messages
        .into_iter()
        .flat_map(|msg| error_messages(&CompilationError::GenericError(msg)))
        .collect();
    }
  }
  vec![e.to_string()]
}

/// Reports errors as they are when the compile finishes, see [`report_load_error`].
fn report_errors(context: &CompilationContext, messages: impl IntoIterator<Item = String>) {
  let mut log_store = context.log_store.lock();
  for message in messages {
    // e.g. the app and its impl module both fail to scan the routes
    if !log_store.errors().contains(&message) {
      log_store.add_error(message);
    }
  }
}

/// Reports an error from generating one of the plugin's modules and returns the error for the
/// `load` hook.
///
/// The host wraps a load error in its own, which shows the original error in its `Debug` format,
/// i.e. as `Some(GenericError("..."))` with the newlines of multi-line messages escaped. So the
/// messages are added to the compile's log store instead, whose errors the host reports as they
/// are along with the returned one once the build or update finishes.
fn report_load_error(
  context: &CompilationContext,
  module_id: &str,
  e: CompilationError,
) -> CompilationError {
  report_errors(context, error_messages(&e));
  CompilationError::GenericError(format!(
    "failed to generate {}, see the errors below",
    module_id
  ))
}

impl FarmPluginRecooler {
  fn route_has_client_script(
    &self,
    context: &Arc<CompilationContext>,
    route_module_id: &ModuleId,
  ) -> Result<bool> {
    let scan_result = self.scan_routes(context)?;
    let route_info = if let Some(info) = scan_result.routes.get(&routes::file_to_route_path(
      &self.routes_dir,
      &PathBuf::from(route_module_id.relative_path()),
    )) {
      info
    } else {
      return Ok(false);
    };

    let page_info = if let Some(info) = &route_info.page_component {
      info
    } else {
      return Ok(route_info.on_load);
    };

    Ok(page_info.handlers.event_handlers.len() > 0 || route_info.on_load)
  }

  fn build_client_script(
    &self,
    context: &Arc<CompilationContext>,
    module_id: &ModuleId,
  ) -> Result<Option<String>> {
    if !self.route_has_client_script(context, module_id)? {
      // println!(
      //   "route has no event handlers, not generating client script for {}",
      //   module_id.relative_path()
      // );
      return Ok(None);
    }

    let entry_source =
      self.generate_client_entrypoint(context, &module_id.relative_path().to_string())?;

    // the lock must not be held across the nested compile, which calls back
    // into this plugin instance
    if let Some(cached) = self
      .client_script_cache
      .lock()
      .unwrap()
      .get_mut(&entry_source)
    {
      cached.routes.push(module_id.relative_path().to_string());
      return Ok(cached.script.as_ref().ok().cloned());
    }

    let script = self.compile_client_script(context, module_id)?;
    let client_script = script.as_ref().ok().cloned();
    self.client_script_cache.lock().unwrap().insert(
      entry_source,
      CachedClientScript {
        script,
        routes: vec![module_id.relative_path().to_string()],
      },
    );

    Ok(client_script)
  }

  /// Returns an error for the client scripts built by [`Self::build_client_script`] that include
  /// banned modules, listing every route that uses each of them. Like the error of a failed nested
  /// compile, it is a JSON array of messages (see [`error_messages`]), one per script.
  fn banned_client_scripts_error(&self) -> Result<()> {
    let mut errors = self
      .client_script_cache
      .lock()
      .unwrap()
      .values()
      .filter_map(|cached| {
        let banned = cached.script.as_ref().err()?;
        Some(format!(
          "client script for {} includes banned modules:\n  {}",
          cached.routes.join(", "),
          banned.join("\n  ")
        ))
      })
      .collect::<Vec<_>>();
    if errors.is_empty() {
      return Ok(());
    }
    errors.sort();
    Err(CompilationError::GenericError(
      serde_json::to_string(&errors).unwrap(),
    ))
  }

  /// Compiles the client script of a route, or returns the banned modules that it includes, see
  /// [`banned_imports::banned_client_modules`].
  fn compile_client_script(
    &self,
    context: &Arc<CompilationContext>,
    module_id: &ModuleId,
  ) -> Result<std::result::Result<String, Vec<String>>> {
    let mut config = context.config.as_ref().clone();
    config.progress = false;
    // see `nested_compile::client_script_plugins`
    config.lazy_compilation = false;
    *config.tree_shaking = BoolOrObj::Bool(true);
    *config.minify = BoolOrObj::Bool(true);
    *config.sourcemap = SourcemapConfig::Bool(false);
    nested_compile::force_client_script_output(&mut config);
    config.input = HashMap::from([(
      String::from(CLIENT_ENTRY_NAME),
      format!("{}{}", CLIENT_MODULE_PREFIX, module_id.relative_path()),
    )]);

    let plugins =
      nested_compile::client_script_plugins(nested_compile::shared_host_plugins(context), &config);

    let compiler = Compiler::new_without_internal_plugins(config, plugins)?;

    compiler.compile()?;

    if !self.client_banned_imports.is_empty() {
      // catches banned modules imported indirectly through other source files
      let module_graph = compiler.context().module_graph.read();
      if let Some(entry) = module_graph
        .entries
        .iter()
        .find_map(|(id, name)| (name == CLIENT_ENTRY_NAME).then_some(id))
      {
        let banned = banned_imports::banned_client_modules(&module_graph, entry, |id| {
          banned_imports::is_banned_module(&self.client_banned_imports, id)
        });
        if !banned.is_empty() {
          return Ok(Err(banned));
        }
      }
    }

    let context = compiler.context();
    let entries = context.module_graph.read().entries.clone();
    let resource_pot_map = context.resource_pot_map.read();
    let resources = context.resources_map.lock();
    let script = nested_compile::entry_script(
      &entries,
      &resource_pot_map.resource_pots(),
      &resources,
      CLIENT_ENTRY_NAME,
    )
    .ok_or_else(|| {
      CompilationError::GenericError(format!(
        "client script compile for {} generated no script",
        module_id.relative_path()
      ))
    })?;
    Ok(Ok(String::from_utf8_lossy(&script.bytes).into_owned()))
  }

  fn generate_metadata_module(&self) -> Result<String> {
    let dir_contents = self.scan_dir()?;

    let mut imports = UniqueImportIdentifiers::new();
    let pages = dir_contents
      .route_files
      .iter()
      .map(|f| {
        format!(
          "{{ path: {}, metadata: typeof {1} === 'undefined' ? undefined : {} }}",
          serde_json::to_string(&strip_path_route_groups(&file_to_route_path(
            &self.routes_dir,
            f
          )))
          .unwrap(),
          imports.identifier(&ModuleId::from(f.to_str().unwrap()), "metadata")
        )
      })
      .collect::<Vec<String>>()
      .join(", ");

    return Ok(format!(
      "{}\nexport const pages = [{}];\n",
      imports.imports_str(),
      pages
    ));
  }

  fn generate_client_entrypoint(
    &self,
    context: &Arc<CompilationContext>,
    file_path: &String,
  ) -> Result<String> {
    let scan_result = self.scan_routes(context)?;

    let route_info = if let Some(info) = scan_result.routes.get(&routes::file_to_route_path(
      &self.routes_dir,
      &PathBuf::from(file_path),
    )) {
      info
    } else {
      return Ok(String::new());
    };

    let page_info = if let Some(info) = &route_info.page_component {
      info
    } else {
      return Ok(String::new());
    };

    let mut import_idents = UniqueImportIdentifiers::new();

    // sorted so that the generated source (and therefore the output) is deterministic
    let mut handler_ids = page_info.handlers.event_handlers.iter().collect::<Vec<_>>();
    handler_ids.sort();

    let mut handlers = String::new();
    for handler_id in handler_ids {
      let lock = self.ids.lock().unwrap();
      let ids = lock.borrow();
      if let Some(export) = ids.event_handler_ids.id_export(handler_id) {
        // import from the handlers variant to leave out the server side code of the module
        let handlers_module_id =
          ModuleId::new(export.module_id.relative_path(), HANDLERS_MODULE_QUERY, "");
        handlers += format!(
          "{}: {}, ",
          serde_json::to_string(handler_id).unwrap(),
          import_idents.identifier(&handlers_module_id, export.export_name),
        )
        .as_str();
      }
    }

    let mut out = String::new();
    if route_info.on_load {
      let handlers_module_id = ModuleId::new(
        route_info.module_id.relative_path(),
        HANDLERS_MODULE_QUERY,
        "",
      );
      out += format!(
        "{}();\n",
        import_idents.identifier(&handlers_module_id, "onLoad")
      )
      .as_str();
    }

    if handlers.len() > 0 {
      out = format!("window.$handlers = {{{}}};\n", handlers) + out.as_str();
    }

    out = import_idents.imports_str() + out.as_str();

    out += "export {}\n";

    // println!("generate_client_module: {} {:?}", file_path, out);

    Ok(out)
  }

  /// Generates the [`ENTRYPOINT_MODULE`], which re-exports the app from the
  /// [`ENTRYPOINT_IMPL_MODULE`], see [`Self::generate_entrypoint_impl_module`].
  ///
  /// Generating the app takes a while, since the routes must be scanned and their client scripts
  /// compiled first, and until then the host compile can't discover the modules of the routes. So
  /// this module is generated right away instead and also imports the route files that the app is
  /// certain to import (see [`Self::entrypoint_prefetch_files`]), which lets the host compile
  /// start on them in the meantime. The re-export comes first so that the app and its imports are
  /// still evaluated in the same order.
  fn generate_entrypoint_module(&self) -> Result<String> {
    let mut out = format!(
      "export {{ default, onRequest }} from {};\n",
      serde_json::to_string(ENTRYPOINT_IMPL_MODULE).unwrap()
    );

    for file in self.entrypoint_prefetch_files()? {
      out += format!(
        "import {};\n",
        imports::import_source(&ModuleId::from(file.to_str().unwrap()))
      )
      .as_str();
    }

    Ok(out)
  }

  fn generate_entrypoint_impl_module(
    &self,
    context: &Arc<CompilationContext>,
    _hook_context: &PluginHookContext,
  ) -> Result<String> {
    let scan_result = self.scan_routes(context)?;

    // scripts from a previous build in this process (e.g. watch mode) may be stale
    self.client_script_cache.lock().unwrap().clear();

    let mut import_idents = UniqueImportIdentifiers::new();
    let mut after_imports = String::new();

    if let Some(secret) = &self.action_secret {
      // only inlined into the server entrypoint, never the client scripts
      after_imports += format!(
        "configureActions({{ secret: {} }});\n",
        serde_json::to_string(secret).unwrap()
      )
      .as_str();
    }

    let mut handlers = String::new();
    let (root_hoc_ident, root_action_methods): (String, HashSet<FormActionMethod>) = if scan_result
      .routes
      .iter()
      .any(|(.., info)| info.page_component.is_some())
    {
      if let Some(root) = &scan_result.root_component {
        let mut methods = root.handlers.action_handlers.keys().collect::<Vec<_>>();
        methods.sort();

        for method in &methods {
          let mut action_ids = root
            .handlers
            .action_handlers
            .get(method)
            .unwrap()
            .iter()
            .collect::<Vec<_>>();
          action_ids.sort();

          let mut actions = String::new();
          for action_id in action_ids {
            if let Some(export) = {
              let lock = self.ids.lock().unwrap();
              let ids = lock.borrow();
              ids.form_action_ids.id_export(action_id)
            } {
              actions += format!(
                "{}: {}, ",
                serde_json::to_string(action_id).unwrap(),
                import_idents.identifier(&export.module_id, export.export_name),
              )
              .as_str();
            }
          }

          handlers = format!(
            "const root{:?}ActionsMiddleware = actionsMiddleware({{{}}});\n",
            method, actions,
          ) + handlers.as_str();
        }

        (
          import_idents.identifier(&root.module_id, "default"),
          methods.into_iter().copied().collect(),
        )
      } else {
        // renders the page in a basic html document with the head elements and the client script
        after_imports +=
          "import { DefaultRootHOC as RootHOC } from \"farm-plugin-recooler/helpers\";\n";

        ("RootHOC".to_string(), Default::default())
      }
    } else {
      Default::default()
    };

    handlers += self
      .generate_route_statements(
        context,
        &mut import_idents,
        "app".to_string(),
        &root_action_methods,
        &root_hoc_ident,
        &scan_result.root_component,
        &scan_result.route_plan(),
      )?
      .as_str();

    self.banned_client_scripts_error()?;

    return Ok(
      format!(
        r#"
        import {{ Hono }} from "hono";
        import {{ actionsMiddleware, buildHeadFn, configureActions, makeCloudflarePagesHandler }} from "farm-plugin-recooler/helpers";
        {}
        {}

        {}

        const app = /* #__PURE__ */ (() => {{
            const app = new Hono({{ strict: true }});
            {}
            return app;
        }})();

        export default app;
        export const onRequest = /* #__PURE__ */ makeCloudflarePagesHandler(app);
        "#,
        self.route_handler_factory.import_statement(),
        import_idents.imports_str(),
        after_imports,
        handlers,
      )
      .to_string(),
    );
  }
}

#[cfg(test)]
mod tests {
  use farmfe_core::{
    config::Config, context::CompilationContext, error::CompilationError, serde_json,
  };

  use super::{
    error_messages, report_load_error, strip_path_route_groups, ModuleSymbol,
    RouteHandlerFactoryInput,
  };

  fn route_handler_factory_import(input: Option<RouteHandlerFactoryInput>) -> String {
    let symbol = ModuleSymbol::route_handler_factory(input);
    assert_eq!(symbol.name, "routeHandlerFactory");
    symbol.import_statement()
  }

  #[test]
  fn route_handler_factory_is_imported_under_an_alias() {
    assert_eq!(
      route_handler_factory_import(None),
      r#"import { jsxRouteHandler as routeHandlerFactory } from "farm-plugin-recooler/helpers";"#
    );
    // names of the generated entrypoint's own identifiers
    for export in ["app", "Hono", "buildHeadFn", "actionsMiddleware", "RootHOC"] {
      assert_eq!(
        route_handler_factory_import(Some(RouteHandlerFactoryInput::Tuple2((
          "./x".to_string(),
          Some(export.to_string())
        )))),
        format!(
          r#"import {{ {} as routeHandlerFactory }} from "./x";"#,
          export
        )
      );
    }
    for input in [
      RouteHandlerFactoryInput::String("./x".to_string()),
      RouteHandlerFactoryInput::Tuple1(("./x".to_string(),)),
      RouteHandlerFactoryInput::Tuple2(("./x".to_string(), None)),
    ] {
      assert_eq!(
        route_handler_factory_import(Some(input)),
        r#"import routeHandlerFactory from "./x";"#
      );
    }
  }

  #[test]
  fn strips_every_group_that_route_registration_treats_as_a_group() {
    assert_eq!(strip_path_route_groups("/(a)/x"), "/x");
    assert_eq!(strip_path_route_groups("/(a.b)/x"), "/x");
    assert_eq!(strip_path_route_groups("/(v2.0)/(c)/y"), "/y");
    assert_eq!(strip_path_route_groups("/a/(b c)/(d+e)/:id"), "/a/:id");
    assert_eq!(strip_path_route_groups("/x/(a.b)"), "/x");
    assert_eq!(strip_path_route_groups("/(a)"), "/");
    assert_eq!(strip_path_route_groups("/(a)/(b)"), "/");
    assert_eq!(strip_path_route_groups("/"), "/");
  }

  #[test]
  fn keeps_parentheses_that_are_not_a_whole_path_component() {
    assert_eq!(strip_path_route_groups("/(a)b/x"), "/(a)b/x");
    assert_eq!(strip_path_route_groups("/a(b)/x"), "/a(b)/x");
  }

  #[test]
  fn splits_nested_compile_errors() {
    // as reported by a failed nested compile
    let nested = CompilationError::GenericError(
      serde_json::json!(["first\n  line two", "second"]).to_string(),
    );
    assert_eq!(error_messages(&nested), vec!["first\n  line two", "second"]);

    let single = CompilationError::GenericError("not json\n  line two".to_string());
    assert_eq!(error_messages(&single), vec!["not json\n  line two"]);
  }

  #[test]
  fn reports_load_errors_as_they_are() {
    let context = CompilationContext::new(Config::default(), vec![]).unwrap();
    let nested = || {
      CompilationError::GenericError(
        serde_json::json!([
          "client script for src/routes/a/index.tsx includes banned modules:\n  fakedb/index.js",
          "second",
        ])
        .to_string(),
      )
    };

    let e = report_load_error(&context, "recooler::app-impl", nested());
    assert_eq!(
      e.to_string(),
      "failed to generate recooler::app-impl, see the errors below"
    );
    // reported once, even if another module fails with the same errors
    report_load_error(&context, "recooler::app", nested());
    assert_eq!(
      context.log_store.lock().errors(),
      &vec![
        "client script for src/routes/a/index.tsx includes banned modules:\n  fakedb/index.js"
          .to_string(),
        "second".to_string(),
      ]
    );
  }
}
