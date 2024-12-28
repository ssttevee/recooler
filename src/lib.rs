#![deny(clippy::all)]
mod es_ast_helpers;
mod head;
mod htmx;
mod ids;
mod imports;
mod layouts;
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
  relative_path::PathExt,
  serde_json,
};
use farmfe_macro_plugin::farm_plugin;
use farmfe_toolkit::{lazy_static::lazy_static, swc_atoms::AtomStore};

use htmx::FormActionMethod;
use ids::GlobalIds;
use imports::UniqueImportIdentifiers;
use routes::file_to_route_path;

#[derive(serde::Deserialize)]
pub struct RecoolerOptions {
  pub src_dir: Option<String>,
  pub routes_dir: Option<String>,
  pub reuse_modules: Option<bool>,
}

#[farm_plugin]
pub struct FarmPluginRecooler {
  src_dir: String,
  routes_dir: String,
  ids: Mutex<RefCell<GlobalIds>>,
  atom_store: Mutex<RefCell<AtomStore>>,
  scanned_dir: farmfe_core::parking_lot::RwLock<Option<routes::ScannedDir>>,
  scanned_routes: farmfe_core::parking_lot::RwLock<Option<routes::ScanResult>>,
  reuse_modules: bool,
}

impl FarmPluginRecooler {
  fn new(_config: &Config, options_str: String) -> Self {
    let options: RecoolerOptions = serde_json::from_str(&options_str).unwrap();

    let cwd = std::env::current_dir().unwrap();
    let src_dir = fs::canonicalize(options.src_dir.unwrap_or("src".to_string()))
      .unwrap()
      .relative_to(&cwd)
      .unwrap();

    let routes_dir = if let Some(routes_dir) = options.routes_dir {
      fs::canonicalize(routes_dir)
        .unwrap()
        .relative_to(&cwd)
        .unwrap()
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
      reuse_modules: options.reuse_modules.unwrap_or(false),
    }
  }
}

const CLIENT_MODULE_PREFIX: &str = "recooler:@";
const ENTRYPOINT_MODULE: &str = "recooler::app";
const METADATA_MODULE: &str = "recooler::metadata";

impl Plugin for FarmPluginRecooler {
  fn name(&self) -> &str {
    "FarmPluginRecooler"
  }

  fn config(&self, config: &mut Config) -> Result<Option<()>> {
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
    if param.module_id == ENTRYPOINT_MODULE {
      match self.generate_entrypoint_module(context, hook_context) {
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
              Ok(msg) => {
                print!("{}", msg[0]);
              }
              Err(_) => match serde_json::from_str::<String>(msg) {
                Ok(msg) => {
                  print!("{}", msg);
                }
                Err(_) => {
                  print!("{}", msg);
                }
              },
            },
            _ => print!("qwer {}", e),
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

    let mut ids_lock = self.ids.lock().unwrap();
    let mut atoms_lock = self.atom_store.lock().unwrap();

    htmx::transform::transform_module(
      param.module_id,
      strip_path_route_groups(&routes::file_to_route_path(
        &self.routes_dir,
        &PathBuf::from(param.module_id.relative_path()),
      )),
      ids_lock.get_mut(),
      atoms_lock.get_mut(),
      &mut script.ast,
    );

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

    let mut config = context.config.as_ref().clone();
    config.progress = false;
    *config.tree_shaking = BoolOrObj::Bool(true);
    *config.minify = BoolOrObj::Bool(true);
    *config.sourcemap = SourcemapConfig::Bool(false);
    config.input = HashMap::new();
    config.input.insert(
      String::from("client"),
      format!("{}{}", CLIENT_MODULE_PREFIX, module_id.relative_path()),
    );

    let mut plugins = context.plugin_driver.plugins.clone();
    plugins.push(Arc::new(farmfe_plugin_minify::FarmPluginMinify::new(&config)) as _);

    let compiler = Compiler::new_without_internal_plugins(config, plugins)?;

    compiler.compile()?;

    let resource_map = compiler.context().resources_map.lock();
    Ok(Some(
      String::from_utf8(resource_map.get("client.js").unwrap().bytes.clone()).unwrap(),
    ))
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

    let mut handlers = String::new();
    for handler_id in &page_info.handlers.event_handlers {
      let lock = self.ids.lock().unwrap();
      let ids = lock.borrow();
      if let Some(export) = ids.event_handler_ids.id_export(handler_id) {
        handlers += format!(
          "{}: {}, ",
          serde_json::to_string(handler_id).unwrap(),
          import_idents.identifier(&export.module_id, export.export_name),
        )
        .as_str();
      }
    }

    let mut out = String::new();
    if route_info.on_load {
      out += format!(
        "{}();\n",
        import_idents.identifier(&route_info.module_id, "onLoad")
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

  fn generate_entrypoint_module(
    &self,
    context: &Arc<CompilationContext>,
    _hook_context: &PluginHookContext,
  ) -> Result<String> {
    let scan_result = self.scan_routes(context)?;

    let mut import_idents = UniqueImportIdentifiers::new();
    let mut after_imports = String::new();

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
          let mut actions = String::new();

          for action_id in root.handlers.action_handlers.get(method).unwrap() {
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
        import {{ jsxRouteHandler, actionsMiddleware, buildHeadFn, makeCloudflarePagesHandler }} from "farm-plugin-recooler/helpers";
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
        import_idents.imports_str(),
        after_imports,
        handlers,
      )
      .to_string(),
    );
  }
}
