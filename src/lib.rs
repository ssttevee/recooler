#![deny(clippy::all)]
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
use farmfe_toolkit::{lazy_static::lazy_static, swc_atoms::AtomStore};

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
  default: bool,
  name: String,
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
  client_script_cache: Mutex<HashMap<String, String>>,
}

static DEFAULT_ROUTE_HANDLER_IDENT: &str = "jsxRouteHandler";

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
      route_handler_factory: options.route_handler_factory.map_or_else(
        || ModuleSymbol {
          module: "farm-plugin-recooler/helpers".to_string(),
          default: false,
          name: DEFAULT_ROUTE_HANDLER_IDENT.to_string(),
        },
        |input| match input {
          RouteHandlerFactoryInput::String(module) => ModuleSymbol {
            module: module,
            default: true,
            name: DEFAULT_ROUTE_HANDLER_IDENT.to_string(),
          },
          RouteHandlerFactoryInput::Tuple1((module,)) => ModuleSymbol {
            module: module,
            default: true,
            name: DEFAULT_ROUTE_HANDLER_IDENT.to_string(),
          },
          RouteHandlerFactoryInput::Tuple2((module, name)) => ModuleSymbol {
            module: module,
            default: name.is_none(),
            name: name.unwrap(),
          },
        },
      ),
    }
  }
}

const CLIENT_MODULE_PREFIX: &str = "recooler:@";
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
        self.generate_entrypoint_module(context)
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
        Err(e) => {
          match &e {
            CompilationError::GenericError(msg) => match serde_json::from_str::<Vec<String>>(msg) {
              // nested compiles report every error, e.g. one per banned client import
              Ok(msg) => {
                println!("{}", msg.join("\n"));
              }
              Err(_) => match serde_json::from_str::<String>(msg) {
                Ok(msg) => {
                  println!("{}", msg);
                }
                Err(_) => {
                  println!("{}", msg);
                }
              },
            },
            _ => println!("qwer {}", e),
          }
          return Err(CompilationError::GenericError(
            "failed to generate entrypoint module".to_string(),
          ));
        }
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
        Err(e) => {
          println!("{}", e);
          return Err(CompilationError::GenericError(
            "failed to generate metadata module".to_string(),
          ));
        }
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
        Err(e) => {
          println!("{}", e);
          return Err(CompilationError::GenericError(
            "failed to generate client module".to_string(),
          ));
        }
      }
    }

    Ok(None)
  }

  fn process_module(
    &self,
    param: &mut PluginProcessModuleHookParam,
    _context: &Arc<CompilationContext>,
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

    let mut ids_lock = self.ids.lock().unwrap();
    let mut atoms_lock = self.atom_store.lock().unwrap();

    htmx::transform::transform_module(
      &module_id,
      strip_path_route_groups(&routes::file_to_route_path(
        &self.routes_dir,
        &PathBuf::from(param.module_id.relative_path()),
      )),
      ids_lock.get_mut(),
      atoms_lock.get_mut(),
      &mut script.ast,
      &mut script.comments,
    )
    .map_err(CompilationError::GenericError)?;

    if handlers_slice {
      htmx::transform::slice_handler_module(
        &module_id,
        ids_lock.get_mut(),
        &mut script.ast,
        // only the `onLoad` of route modules runs on the client
        routes::is_route_file(
          &self.routes_dir,
          &PathBuf::from(param.module_id.relative_path()),
        ),
        &self.client_banned_imports,
      )
      .map_err(CompilationError::GenericError)?;
    }

    Ok(Some(()))
  }
}

lazy_static! {
  static ref ROUTE_PATH_GROUP_SEGMENT_PATTERN: Regex = Regex::new(r"/\([\d\w_-]+\)").unwrap();
}

fn strip_path_route_groups(path: &String) -> String {
  ROUTE_PATH_GROUP_SEGMENT_PATTERN
    .replace_all(path.as_str(), "")
    .to_string()
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
    let cached = self
      .client_script_cache
      .lock()
      .unwrap()
      .get(&entry_source)
      .cloned();
    if let Some(client_script) = cached {
      return Ok(Some(client_script));
    }

    let client_script = self.compile_client_script(context, module_id)?;
    self
      .client_script_cache
      .lock()
      .unwrap()
      .insert(entry_source, client_script.clone());

    Ok(Some(client_script))
  }

  fn compile_client_script(
    &self,
    context: &Arc<CompilationContext>,
    module_id: &ModuleId,
  ) -> Result<String> {
    let mut config = context.config.as_ref().clone();
    config.progress = false;
    *config.tree_shaking = BoolOrObj::Bool(true);
    *config.minify = BoolOrObj::Bool(true);
    *config.sourcemap = SourcemapConfig::Bool(false);
    config.input = nested_compile::nested_compile_input(
      &context.config,
      [(
        String::from("client"),
        format!("{}{}", CLIENT_MODULE_PREFIX, module_id.relative_path()),
      )],
    );

    let mut plugins = nested_compile::shared_host_plugins(context);
    plugins.push(Arc::new(farmfe_plugin_minify::FarmPluginMinify::new(&config)) as _);

    let compiler = Compiler::new_without_internal_plugins(config, plugins)?;

    compiler.compile()?;

    if !self.client_banned_imports.is_empty() {
      // catches banned modules imported indirectly through other source files
      let mut banned = compiler
        .context()
        .module_graph
        .read()
        .modules()
        .into_iter()
        .map(|module| module.id.to_string())
        .filter(|id| {
          !id.starts_with(CLIENT_MODULE_PREFIX)
            && self
              .client_banned_imports
              .iter()
              .any(|pattern| pattern.is_match(id))
        })
        .collect::<Vec<_>>();
      if !banned.is_empty() {
        banned.sort();
        return Err(CompilationError::GenericError(format!(
          "client script for {} includes banned modules:\n  {}",
          module_id.relative_path(),
          banned.join("\n  ")
        )));
      }
    }

    let resource_map = compiler.context().resources_map.lock();
    Ok(String::from_utf8(resource_map.get("client.js").unwrap().bytes.clone()).unwrap())
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
  fn generate_entrypoint_module(&self, context: &Arc<CompilationContext>) -> Result<String> {
    let mut out = format!(
      "export {{ default, onRequest }} from {};\n",
      serde_json::to_string(ENTRYPOINT_IMPL_MODULE).unwrap()
    );

    for file in self.entrypoint_prefetch_files(&context.config)? {
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
        // generate a identity component; this is a workaround for the case where there is no root component
        after_imports += r#"
        function RootHOC(Component) {
          return Component
        }
        "#;

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
        &scan_result.tree(),
        None,
      )?
      .as_str();

    return Ok(
      format!(
        r#"
        import {{ Hono }} from "hono";
        import {{ actionsMiddleware, buildHeadFn, configureActions, makeCloudflarePagesHandler }} from "farm-plugin-recooler/helpers";
        import {} from {};
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
        if self.route_handler_factory.default {
            self.route_handler_factory.name.clone()
        } else {
            format!("{{ {} }}", self.route_handler_factory.name)
        },
        serde_json::to_string(&self.route_handler_factory.module).unwrap(),
        import_idents.imports_str(),
        after_imports,
        handlers,
      )
      .to_string(),
    );
  }
}
