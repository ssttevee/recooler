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
  error::Result,
  module::{ModuleId, ModuleMetaData, ModuleType},
  plugin::*,
  relative_path::PathExt,
};
use farmfe_macro_plugin::farm_plugin;

use head::build_head_expr;
use htmx::FormActionMethod;
use ids::GlobalIds;
use imports::UniqueImportIdentifiers;
use routes::file_to_route_path;
use swc_atoms::AtomStore;

#[derive(serde::Deserialize)]
pub struct RecoolerOptions {
  pub src_dir: Option<String>,
  pub routes_dir: Option<String>,
}

#[farm_plugin]
pub struct FarmPluginRecooler {
  src_dir: String,
  routes_dir: String,
  ids: Mutex<RefCell<GlobalIds>>,
  atom_store: Mutex<RefCell<AtomStore>>,
  scanned_dir: farmfe_core::parking_lot::RwLock<Option<routes::ScannedDir>>,
  scanned_routes: farmfe_core::parking_lot::RwLock<Option<routes::ScanResult>>,
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
      return Ok(Some(PluginLoadHookResult {
        content: self.generate_entrypoint_module(context, hook_context)?,
        module_type: ModuleType::Js,
        source_map: None,
      }));
    }

    if param.module_id == METADATA_MODULE {
      return Ok(Some(PluginLoadHookResult {
        content: self.generate_metadata_module()?,
        module_type: ModuleType::Js,
        source_map: None,
      }));
    }

    if param.module_id.starts_with(CLIENT_MODULE_PREFIX) {
      return Ok(Some(PluginLoadHookResult {
        content: self.generate_client_entrypoint(
          context,
          &param.module_id[CLIENT_MODULE_PREFIX.len()..].to_string(),
        )?,
        module_type: ModuleType::Js,
        source_map: None,
      }));
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

    htmx::transform_module(
      param.module_id,
      ids_lock.get_mut(),
      atoms_lock.get_mut(),
      &mut script.ast,
    );

    Ok(Some(()))
  }
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
          serde_json::to_string(&file_to_route_path(&self.routes_dir, f)).unwrap(),
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

    let sorted_paths = {
      let mut paths = HashSet::<String>::new();
      paths.extend(scan_result.routes.keys().cloned());
      paths.extend(scan_result.middlewares.keys().cloned());
      let mut paths = paths.into_iter().collect::<Vec<String>>();
      paths.sort();
      paths
    };

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
            "const root{:?}ActionsMiddleware actionsMiddleware({{{}}});\n",
            method, actions,
          ) + handlers.as_str();
        }

        (
          import_idents.identifier(&root.module_id, "default"),
          methods.into_iter().copied().collect(),
        )
      } else {
        // generate a identity component; this is a workaround for the case where there is no root component
        handlers += r#"
        function RootHOC(Component) {
          return Component
        }
        "#;

        ("RootHOC".to_string(), Default::default())
      }
    } else {
      Default::default()
    };

    for path in &sorted_paths {
      if let Some(middlewares) = scan_result.middlewares.get(path) {
        for module_id in middlewares {
          handlers += format!(
            "app.use({}, {});\n",
            serde_json::to_string(&format!("{}/*", if path == "/" { "" } else { path })).unwrap(),
            import_idents.identifier(&module_id, "default"),
          )
          .as_str();
        }
      }

      if let Some(route) = scan_result.routes.get(path) {
        let path_js_str = serde_json::to_string(&path.to_string()).unwrap();

        let mut request_handlers = String::new();
        for method in &route.request_handlers {
          request_handlers += format!(
            "app.{}({}, {});\n",
            format!("{:?}", method).to_lowercase(),
            path_js_str,
            import_idents.identifier(&route.module_id, format!("on{:?}", method)),
          )
          .as_str();
        }

        let mut action_handlers = String::new();
        let mut page_handler = String::new();
        if let Some(page) = &route.page_component {
          let mut methods = page.handlers.action_handlers.keys().collect::<Vec<_>>();
          methods.sort();

          let mut root_methods_copy = root_action_methods.clone();
          for method in methods {
            let mut actions = String::new();

            for action_id in page.handlers.action_handlers.get(method).unwrap() {
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

            let mut route_params = path_js_str.clone();
            if root_methods_copy.contains(method) {
              route_params += format!(", root{:?}ActionsMiddleware", method).as_str();
              root_methods_copy.remove(method);
            }

            action_handlers += format!(
              "app.{}({}, actionsMiddleware({{{}}}));\n",
              format!("{:?}", method).to_lowercase().as_str(),
              route_params,
              actions,
            )
            .as_str();
          }

          let mut remaining_root_methods = root_methods_copy.into_iter().collect::<Vec<_>>();
          remaining_root_methods.sort();
          for method in remaining_root_methods {
            action_handlers += format!(
              "app.{}({}, root{:?}ActionsMiddleware);\n",
              format!("{:?}", method).to_lowercase().as_str(),
              path_js_str,
              method
            )
            .as_str();
          }

          let mut component_expr = import_idents.identifier(&route.module_id, "default");
          for layout_module in &page.layouts {
            component_expr = format!(
              "{}({})",
              import_idents.identifier(&layout_module.module_id, "default"),
              component_expr
            );
          }

          let mut params = format!(
            "{}, {}, {}",
            root_hoc_ident,
            component_expr,
            build_head_expr(
              &mut import_idents,
              &scan_result.root_component,
              &page.layouts,
              &route
            )
          );

          if let Some(client_script) = self.build_client_script(context, &route.module_id)? {
            params += ", ";
            params += serde_json::to_string(&client_script).unwrap().as_str();
          }

          page_handler +=
            format!("app.get({}, jsxRouteHandler({}));\n", path_js_str, params).as_str();
        }

        handlers += action_handlers.as_str();
        handlers += request_handlers.as_str();
        handlers += page_handler.as_str();
      }
    }

    return Ok(
      format!(
        r#"
        import {{ Hono }} from "hono";
        import {{ jsxRouteHandler, actionsMiddleware, buildHeadFn, makeCloudflarePagesHandler }} from "farm-plugin-recooler/helpers";
        {}

        const app = /* #__PURE__ */ (() => {{
            const app = new Hono({{ strict: false }});
            {}
            return app;
        }})();

        export default app;
        export const onRequest = /* #__PURE__ */ makeCloudflarePagesHandler(app);
        "#,
        import_idents.imports_str(),
        handlers,
      )
      .to_string(),
    );
  }
}
