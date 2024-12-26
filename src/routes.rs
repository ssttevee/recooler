use std::{
  cell::RefCell,
  collections::{HashMap, HashSet, VecDeque},
  path::{Path, PathBuf},
  sync::{Arc, Mutex},
};

use farmfe_compiler::{trace_module_graph::TracedModuleGraph, Compiler};
use farmfe_core::{
  config::persistent_cache::PersistentCacheConfig,
  context::CompilationContext,
  error::{CompilationError, Result},
  module::{
    module_graph::{ModuleGraph, ModuleGraphEdge},
    Module, ModuleId, ModuleMetaData,
  },
  parking_lot::{MappedRwLockReadGuard, RwLockReadGuard},
  regex::Regex,
  relative_path::PathExt,
  serde_json,
};
use farmfe_toolkit::lazy_static::lazy_static;
use walkdir::WalkDir;

use crate::{
  es_ast_helpers::{find_exports, find_head_export_type},
  head::{build_head_expr, HeadType},
  htmx::{FormActionMethod, RequestHandlerMethod},
  ids::ComponentHandlerIds,
  imports::UniqueImportIdentifiers,
  layouts::LayoutTree,
  FarmPluginRecooler, GlobalIds,
};

#[derive(Clone)]
pub(crate) struct ModuleExport {
  pub(crate) module_id: ModuleId,
  pub(crate) export_name: String,
}

#[derive(Clone)]
pub(crate) struct PageComponentLayoutItem {
  pub(crate) module_id: ModuleId,
  pub(crate) head: Option<HeadType>,
}

#[derive(Clone)]
pub(crate) struct PageComponentInfo {
  pub(crate) handlers: ComponentHandlerIds,
  pub(crate) layouts: Vec<PageComponentLayoutItem>,
}

/// Critical route information
#[derive(Clone)]
pub(crate) struct RouteInfo {
  pub(crate) module_id: ModuleId,
  pub(crate) page_component: Option<PageComponentInfo>,
  pub(crate) request_handlers: HashSet<RequestHandlerMethod>,
  pub(crate) on_request: bool,
  pub(crate) on_load: bool,
  pub(crate) head: Option<HeadType>,
}

pub(crate) struct RootComponentInfo {
  pub(crate) module_id: ModuleId,
  pub(crate) handlers: ComponentHandlerIds,
  pub(crate) head: Option<HeadType>,
}

#[derive(Default)]
pub(crate) struct RouteAndMiddleware {
  route: Option<RouteInfo>,
  middleware: Option<HashSet<ModuleId>>,
}

/// routes are split at group boundaries (i.e. parenthesized dir names), so each
/// tree node contains multiple routes
#[derive(Default)]
pub(crate) struct RouteGroupTree {
  routes_and_middlewares: HashMap<String, RouteAndMiddleware>,
  children: HashMap<String, Box<RouteGroupTree>>,
}

impl FarmPluginRecooler {
  pub(crate) fn generate_route_statements(
    &self,
    context: &Arc<CompilationContext>,
    import_idents: &mut UniqueImportIdentifiers,
    hono_ident: String,
    root_action_methods: &HashSet<FormActionMethod>,
    root_hoc_ident: &String,
    root_component: &Option<RootComponentInfo>,
    tree: &RouteGroupTree,
    inline_middlewares: Option<Vec<ModuleId>>,
  ) -> Result<String> {
    let mut handlers = String::new();

    let should_inline_middleware = inline_middlewares.is_some();
    let mut base_middleware = inline_middlewares.unwrap_or_default();

    if should_inline_middleware {
      if let Some(RouteAndMiddleware {
        middleware: Some(root_middleware),
        ..
      }) = tree.routes_and_middlewares.get("/")
      {
        let mut sorted = root_middleware.iter().cloned().collect::<Vec<_>>();
        sorted.sort();

        base_middleware.extend(sorted.clone());
      }
    }

    for (group_path, group_tree) in &tree.children {
      let group_hono = hono_ident.clone() + "$";
      let fixed_group_path = &group_path[0..group_path.rfind("/").unwrap() + 1];

      handlers += format!(
        r#"{}.route({}, (() => {{
            const {} = new Hono();
            {}
            return {2};
        }})());"#,
        hono_ident,
        serde_json::to_string(fixed_group_path).unwrap(),
        group_hono,
        self.generate_route_statements(
          context,
          import_idents,
          group_hono.clone(),
          root_action_methods,
          root_hoc_ident,
          root_component,
          group_tree,
          if fixed_group_path == "/" {
            Some(base_middleware.clone())
          } else {
            None
          },
        )?,
      )
      .as_str();
    }

    let mut sorted_routes_and_middlewares = tree.routes_and_middlewares.iter().collect::<Vec<_>>();
    sorted_routes_and_middlewares.sort_by(|a, b| {
      // put parameterized routes after other routes
      if a.0.ends_with("{.+}") && !b.0.ends_with("{.+}") {
        std::cmp::Ordering::Greater
      } else if !a.0.ends_with("{.+}") && b.0.ends_with("{.+}") {
        std::cmp::Ordering::Less
      } else {
        a.0.cmp(&b.0)
      }
    });

    for (path, RouteAndMiddleware { route, middleware }) in sorted_routes_and_middlewares {
      let mut combined_middleware = base_middleware.clone();

      if let Some(module_ids) = middleware {
        let mut sorted_module_ids = module_ids.iter().cloned().collect::<Vec<_>>();
        sorted_module_ids.sort();
        combined_middleware.extend(sorted_module_ids);
      }

      let pathjs = serde_json::to_string(path).unwrap();
      // if path.ends_with("{.+}") {
      //   pathjs = format!(
      //     "[{}, {}]",
      //     serde_json::to_string(&String::from(&path[0..path.rfind("/:").unwrap() + 1])).unwrap(),
      //     pathjs
      //   );
      // }

      let mut route_params_prefix = pathjs.clone() + ", ";

      if should_inline_middleware {
        for module_id in combined_middleware {
          route_params_prefix +=
            format!("{}, ", import_idents.identifier(&module_id, "default")).as_str()
        }
      } else {
        for module_id in combined_middleware {
          let use_params_prefix = if path == "/" {
            String::from("\"*\"")
          } else {
            serde_json::to_string(&(path.clone() + "/*")).unwrap()
          };

          handlers += format!(
            "{}.use({}{});\n",
            hono_ident,
            use_params_prefix + ", ",
            import_idents.identifier(&module_id, "default"),
          )
          .as_str();
        }
      }

      if let Some(route) = route {
        let mut hono_app_obj = hono_ident.clone();

        let mut request_handlers = String::new();
        if route.on_request {
          request_handlers += format!(
            "{}.all({}{})",
            hono_app_obj,
            route_params_prefix,
            import_idents.identifier(&route.module_id, "onRequest")
          )
          .as_str();

          hono_app_obj = String::new();
          route_params_prefix = String::new();
        }

        for method in &route.request_handlers {
          request_handlers += format!(
            "{}.{}({}{})",
            hono_app_obj,
            format!("{:?}", method).to_lowercase(),
            route_params_prefix,
            import_idents.identifier(&route.module_id, format!("on{:?}", method)),
          )
          .as_str();

          hono_app_obj = String::new();
          route_params_prefix = String::new();
        }

        if hono_app_obj.is_empty() {
          request_handlers += ";\n";
        }

        let mut action_handlers = String::new();
        let mut page_handler = String::new();
        if let Some(page) = &route.page_component {
          let mut methods = page.handlers.action_handlers.keys().collect::<Vec<_>>();
          methods.sort();

          let mut root_methods_copy = root_action_methods.clone();
          for method in methods {
            let actions = page
              .handlers
              .action_handlers
              .get(method)
              .unwrap()
              .iter()
              .flat_map(|action_id| {
                if let Some(export) = {
                  let mut lock = self.ids.lock().unwrap();
                  let ids = lock.get_mut();
                  ids.form_action_ids.id_export(action_id)
                } {
                  vec![format!(
                    "{}: {}",
                    serde_json::to_string(action_id).unwrap(),
                    import_idents.identifier(&export.module_id, export.export_name),
                  )]
                } else {
                  vec![]
                }
              })
              .collect::<HashSet<_>>()
              .into_iter()
              .collect::<Vec<_>>()
              .join(", ");

            let mut route_params = pathjs.clone();
            if root_methods_copy.contains(method) {
              route_params += format!(", root{:?}ActionsMiddleware", method).as_str();
              root_methods_copy.remove(method);
            }

            action_handlers += format!(
              "{}.{}({}, actionsMiddleware({{{}}}));\n",
              hono_ident,
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
              "{}.{}({}, root{:?}ActionsMiddleware);\n",
              hono_ident,
              format!("{:?}", method).to_lowercase().as_str(),
              pathjs,
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

          let mut page_handler_params = format!(
            "{}, {}, {}",
            root_hoc_ident,
            component_expr,
            build_head_expr(import_idents, root_component, &page.layouts, &route)
          );

          if let Some(client_script) = self.build_client_script(context, &route.module_id)? {
            page_handler_params += ", ";
            page_handler_params += serde_json::to_string(&client_script).unwrap().as_str();
          }

          page_handler += format!(
            "{}.get({}, jsxRouteHandler({}));\n",
            hono_ident, pathjs, page_handler_params
          )
          .as_str();
        }

        handlers += action_handlers.as_str();
        handlers += request_handlers.as_str();
        handlers += page_handler.as_str();
      }
    }

    Ok(handlers)
  }
}

impl RouteGroupTree {
  fn from_routes_and_middlewares(
    routes_and_middlewares: HashMap<String, RouteAndMiddleware>,
  ) -> Self {
    let mut groups = HashMap::<String, HashMap<String, RouteAndMiddleware>>::new();
    let mut current = HashMap::<String, RouteAndMiddleware>::new();
    for (path, entry) in routes_and_middlewares {
      if let Some(group) = PATH_COMPONENT_ROUTE_GROUP_PATTERN
        .captures(&path)
        .and_then(|c| c.get(1))
      {
        // found a group boundary, let save it for later
        let (head, mut tail) = path.split_at(group.end() + 1);
        if tail == "" {
          tail = "/";
        }

        groups
          .entry(head.to_string())
          .or_default()
          .insert(tail.to_string(), entry);
      } else {
        current.insert(path.clone(), entry);
      }
    }

    Self {
      routes_and_middlewares: current,
      children: HashMap::from_iter(
        groups
          .into_iter()
          .map(|(k, v)| (k, Box::new(Self::from_routes_and_middlewares(v)))),
      ),
    }
  }
}

impl ScanResult {
  /// returns a route tree node for the routes in this scan result
  pub(crate) fn tree(&self) -> RouteGroupTree {
    RouteGroupTree::from_routes_and_middlewares(HashMap::from_iter(
      self
        .routes
        .keys()
        .chain(self.middlewares.keys())
        .cloned()
        .collect::<HashSet<_>>()
        .into_iter()
        .map(|path| {
          let entry = RouteAndMiddleware {
            route: self.routes.get(&path).cloned(),
            middleware: self.middlewares.get(&path).cloned(),
          };
          (path, entry)
        }),
    ))
  }
}

/// maps are index by route pathnames with leading slash before the route groups
/// (i.e. parenthesized dir names) are stripped.
pub(crate) struct ScanResult {
  pub(crate) routes: HashMap<String, RouteInfo>,
  pub(crate) layouts: LayoutTree,
  pub(crate) middlewares: HashMap<String, HashSet<ModuleId>>,
  pub(crate) root_component: Option<RootComponentInfo>,
}

impl ScanResult {
  fn new() -> Self {
    Self {
      routes: Default::default(),
      layouts: LayoutTree::new(),
      middlewares: Default::default(),
      root_component: None,
    }
  }

  fn add_route(
    &mut self,
    path: String,
    ids_mutex: &Mutex<RefCell<GlobalIds>>,
    modules: &Vec<Module>,
  ) -> Result<()> {
    let main_module = &modules[0];

    if let ModuleMetaData::Script(script) = main_module.meta.as_ref() {
      let exports = find_exports(&main_module.id, &script);

      let mut request_handlers = HashSet::<RequestHandlerMethod>::new();
      for export in &exports {
        let lower = export.to_lowercase();
        if lower.starts_with("on") {
          macro_rules! request_handler {
            ($name:ident) => {
              if &lower[2..]
                == const_format::map_ascii_case!(const_format::Case::Lower, stringify!($name))
              {
                if export == concat!("on", stringify!($name)) {
                  request_handlers.insert(RequestHandlerMethod::$name);
                } else {
                  println!(
                    "WARNING: found export {} but expected on{} in {}",
                    export,
                    stringify!($name),
                    main_module.id.relative_path()
                  )
                }

                continue;
              }
            };
          }

          request_handler!(Get);
          request_handler!(Post);
          request_handler!(Put);
          request_handler!(Delete);
          request_handler!(Patch);
          request_handler!(Options);
        }
      }

      let page_component = if exports.contains("default") {
        let mut handlers = ids_mutex
          .lock()
          .unwrap()
          .get_mut()
          .handler_ids_for_modules(modules.iter().map(|m| &m.id));

        let layouts = self.layouts.get_layouts_for_path(&path);
        for layout in &layouts {
          if let Some(layout_handlers) = layout.handlers {
            // println!(
            //   "merging {} handlers with layout {}:",
            //   main_module.id.relative_path(),
            //   layout.module_id.relative_path()
            // );
            handlers = handlers.merged(layout_handlers);
          }
        }

        Some(PageComponentInfo {
          handlers,
          layouts: layouts
            .into_iter()
            .map(|l| PageComponentLayoutItem {
              module_id: l.module_id,
              head: l.head,
            })
            .collect(),
        })
      } else {
        None
      };

      // println!("inserting route: {}", path);
      self.routes.insert(
        path,
        RouteInfo {
          module_id: main_module.id.clone(),
          page_component,
          request_handlers,
          on_request: exports.contains("onRequest"),
          on_load: exports.contains("onLoad"),
          head: find_head_export_type(script),
        },
      );
    } else {
      println!("WARNING: js module not found - skipping route: {}", path);
    }

    Ok(())
  }

  fn add_layout(
    &mut self,
    path: String,
    ids_mutex: &Mutex<RefCell<GlobalIds>>,
    modules: &Vec<Module>,
  ) -> Result<()> {
    self.layouts.add_layout(path, ids_mutex, modules)
  }

  fn add_middleware(&mut self, path: String, module_id: &ModuleId) {
    // println!("adding middleware {}", module_id.relative_path());
    self
      .middlewares
      .entry(path)
      .or_default()
      .insert(module_id.clone());
  }
}

fn copy_dependency(
  from: &ModuleGraph,
  to: &mut ModuleGraph,
  module: &Module,
  overwrite: bool,
) -> Result<bool> {
  if overwrite || !to.has_module(&module.id) {
    let mut deps = Vec::<(ModuleId, &ModuleGraphEdge)>::new();
    for (dep_id, edge) in from.dependencies(&module.id) {
      if !to.has_module(&dep_id) {
        if let Some(dep) = from.module(&dep_id) {
          if !copy_dependency(from, to, dep, overwrite)? {
            // println!("skipped dep {:?}", dep.id);
            return Ok(false);
          }
        } else {
          // just ignore this
          return Ok(false);
        }

        deps.push((dep_id, edge));
      }
    }

    to.add_module(module.clone());
    for (dep, edge) in deps {
      to.add_edge(&module.id, &dep, edge.clone())?;
    }
  }

  Ok(true)
}

fn copy_completed_modules(from: &ModuleGraph, to: &mut ModuleGraph, overwrite: bool) -> Result<()> {
  for module in from.modules() {
    if module.size == 0 {
      continue;
    }

    copy_dependency(from, to, module, overwrite)?;
  }

  Ok(())
}

impl FarmPluginRecooler {
  fn list_dependencies_for_module(
    &self,
    traced_graph: &TracedModuleGraph,
    module_graph: &ModuleGraph,
    source: &String,
  ) -> Vec<Module> {
    let mut modules_queue = VecDeque::<String>::new();
    modules_queue.push_back(
      PathBuf::from(source)
        .relative_to(&traced_graph.root)
        .unwrap()
        .to_string(),
    );

    let mut deps = Vec::<Module>::new();
    let mut visited = HashSet::<String>::new();

    while let Some(module_id) = modules_queue.pop_front() {
      if !module_id.starts_with(&self.src_dir) {
        continue;
      }

      if visited.contains(&module_id) {
        continue;
      }

      visited.insert(module_id.clone());

      if let Some(deps) = traced_graph.edges.get(&module_id) {
        modules_queue.extend(deps.iter().cloned());
      }

      let module_id = ModuleId::from(module_id);

      // TODO: ensure module form actions and event handlers are properly registered
      if let Some(module) = module_graph.module(&module_id) {
        // println!("module decls {}", module.meta.as_script().ast.body.len());
        if let ModuleMetaData::Script(script) = module.meta.as_ref() {
          let mut ids = self.ids.lock().unwrap();
          crate::htmx::validate::ensure_registered_handlers(&module_id, ids.get_mut(), &script.ast);

          deps.push(module.clone());
        }
      } else {
        println!(
          "WARNING: module not found in module graph: {}",
          module_id.relative_path()
        );
      }
    }

    deps
  }

  fn get_modules_dependencies(
    &self,
    source_paths: Vec<PathBuf>,
    context: &Arc<CompilationContext>,
  ) -> Result<HashMap<PathBuf, Vec<Module>>> {
    let mut config = context.config.as_ref().clone();
    config.progress = false;
    config.input = HashMap::from_iter(
      source_paths
        .iter()
        .map(|sp| {
          let s = sp.to_str().unwrap().to_string();
          if let Ok(canonical) = sp.canonicalize() {
            Ok(vec![(s, canonical.to_str().unwrap().to_string())])
          } else {
            Err(CompilationError::GenericError(format!(
              "failed to find source: {}",
              s
            )))
          }
        })
        .reduce(|a, b| Ok(a?.into_iter().chain(b?.into_iter()).collect()))
        .unwrap()?
        .into_iter(),
    );
    config.persistent_cache = Box::new(PersistentCacheConfig::Bool(false));
    let canonical_path_map = config.input.clone();

    let compiler =
      Compiler::new_without_internal_plugins(config, context.plugin_driver.plugins.clone())?;

    // copy module graph to new compiler
    // if self.reuse_modules {
    //   let mut module_graph = compiler.context().module_graph.write();
    //   copy_completed_modules(&context.module_graph.read(), &mut *module_graph, true)?;

    //   module_graph.update_execution_order_for_modules();
    // }

    let traced_module = compiler.trace_module_graph()?;

    if self.reuse_modules {
      let mut module_graph = context.module_graph.write();
      copy_completed_modules(
        &compiler.context().module_graph.read(),
        &mut *module_graph,
        false,
      )?;

      module_graph.update_execution_order_for_modules();
    }

    let module_graph_rg = compiler.context().module_graph.read();
    let module_graph = &*module_graph_rg;

    Ok(HashMap::from_iter(source_paths.into_iter().map(|sp| {
      let s = sp.to_str().unwrap().to_string();
      (
        sp.clone(),
        self.list_dependencies_for_module(
          &traced_module,
          module_graph,
          canonical_path_map.get(&s).unwrap(),
        ),
      )
    })))
  }
}

lazy_static! {
  static ref PATH_COMPONENT_ROUTE_GROUP_PATTERN: Regex =
    Regex::new(r"/\(([^\)/]+)\)(/|$)").unwrap();
  static ref PATH_COMPONENT_ROUTE_PARAM_PATTERN: Regex =
    Regex::new(r"^\[([^\[\]][^\]]*)\]$").unwrap();
  static ref PATH_COMPONENT_ROUTE_WILDCARD_PATTERN: Regex =
    Regex::new(r"^\[\[([^\]]+)\]\]$").unwrap();
}

pub(crate) fn file_to_route_path<P: AsRef<Path>>(route_dir: P, file_path: &Path) -> String {
  // println!(
  //   "file_to_route_path: {:?} {:?}",
  //   file_path,
  //   route_dir.as_ref()
  // );
  let mut relpath = file_path.relative_to(route_dir).unwrap();
  relpath.pop();

  format!(
    "/{}",
    relpath
      .iter()
      .map(|p| {
        PATH_COMPONENT_ROUTE_PARAM_PATTERN
          .replace(p, ":$1")
          .to_string()
      })
      .map(|p| {
        PATH_COMPONENT_ROUTE_WILDCARD_PATTERN
          .replace(p.as_str(), ":$1{.+}")
          .to_string()
      })
      .collect::<Vec<String>>()
      .join("/")
  )
}

lazy_static! {
  static ref ROUTE_FILE_PATTERN: Regex = Regex::new("^index\\.[jt]sx?$").unwrap();
  static ref LAYOUT_FILE_PATTERN: Regex = Regex::new("^layout\\.[jt]sx?$").unwrap();
  static ref MIDDLEWARE_FILE_PATTERN: Regex =
    Regex::new("^middleware(?:@[\\w\\d-]+)?\\.[jt]sx?$").unwrap();
}

pub(crate) struct ScannedDir {
  pub(crate) layout_files: Vec<PathBuf>,
  pub(crate) route_files: Vec<PathBuf>,
  pub(crate) middleware_files: Vec<PathBuf>,
  pub(crate) root_file: Option<PathBuf>,
}

impl FarmPluginRecooler {
  pub(crate) fn scan_dir(&self) -> Result<MappedRwLockReadGuard<ScannedDir>> {
    return Ok(RwLockReadGuard::map(
      {
        let lock = self.scanned_dir.read();
        if lock.is_none() {
          drop(lock);

          *self.scanned_dir.write() = Some(self.scan_dir_()?);

          self.scanned_dir.read()
        } else {
          lock
        }
      },
      |r| r.as_ref().unwrap(),
    ));
  }

  fn scan_dir_(&self) -> Result<ScannedDir> {
    // println!("scanning dir {} for routes", self.routes_dir);

    let mut layout_files = Vec::<PathBuf>::new();
    let mut route_files = Vec::<PathBuf>::new();
    let mut middleware_files = Vec::<PathBuf>::new();
    for e in WalkDir::new(&self.routes_dir) {
      match e {
        Ok(entry) => {
          if !entry.file_type().is_file() {
            continue;
          }

          let file_name = entry.file_name().to_str().unwrap();
          if ROUTE_FILE_PATTERN.is_match(file_name) {
            route_files.push(entry.path().to_path_buf());
          } else if LAYOUT_FILE_PATTERN.is_match(file_name) {
            layout_files.push(entry.path().to_path_buf());
          } else if MIDDLEWARE_FILE_PATTERN.is_match(file_name) {
            middleware_files.push(entry.path().to_path_buf());
          }
        }
        Err(err) => {
          if let Some(path) = err.path() {
            println!("warning: failed to access entry {}", path.display());
          } else {
            return Err(CompilationError::GenericError(format!(
              "error while scanning routes dir: {}",
              err.to_string()
            )));
          }
        }
      }
    }

    let root_file = PathBuf::from(&self.src_dir).join("root.tsx");
    Ok(ScannedDir {
      layout_files,
      route_files,
      middleware_files,
      root_file: if root_file.exists() {
        Some(root_file)
      } else {
        None
      },
    })
  }

  pub(crate) fn scan_routes(
    &self,
    context: &Arc<CompilationContext>,
  ) -> Result<MappedRwLockReadGuard<ScanResult>> {
    return Ok(RwLockReadGuard::map(
      {
        let lock = self.scanned_routes.read();
        if lock.is_none() {
          drop(lock);

          *self.scanned_routes.write() = Some(self.scan_routes_(context)?);

          self.scanned_routes.read()
        } else {
          lock
        }
      },
      |r| r.as_ref().unwrap(),
    ));
  }

  fn scan_routes_(&self, context: &Arc<CompilationContext>) -> Result<ScanResult> {
    let dir_contents = self.scan_dir()?;

    let mut result = ScanResult::new();
    let deps_map = self.get_modules_dependencies(
      dir_contents
        .layout_files
        .iter()
        .chain(dir_contents.route_files.iter())
        .chain(dir_contents.root_file.iter())
        .cloned()
        .collect(),
      context,
    )?;

    for layout_file in &dir_contents.layout_files {
      result.add_layout(
        file_to_route_path(&self.routes_dir, layout_file),
        &self.ids,
        // &self.get_module_dependencies(layout_file, context)?,
        deps_map.get(layout_file).unwrap(),
      )?;
    }

    for middleware_file in &dir_contents.middleware_files {
      result.add_middleware(
        file_to_route_path(&self.routes_dir, middleware_file),
        &ModuleId::from(middleware_file.to_str().unwrap()),
      );
    }

    for route_file in &dir_contents.route_files {
      result.add_route(
        file_to_route_path(&self.routes_dir, route_file),
        &self.ids,
        // &self.get_module_dependencies(route_file, context)?,
        deps_map.get(route_file).unwrap(),
      )?;
    }

    let root_file = PathBuf::from(&self.src_dir).join("root.tsx");
    if let Some(root_file) = &dir_contents.root_file {
      // let root_deps = self.get_module_dependencies(&root_file, context)?;
      let root_deps = deps_map.get(root_file).unwrap();
      let root_module_id = root_deps[0].id.clone();

      result.root_component = Some(RootComponentInfo {
        head: find_head_export_type(root_deps[0].meta.as_script()),
        module_id: root_module_id,
        handlers: {
          let mut ids_lock = self.ids.lock().unwrap();
          let handlers = ids_lock
            .get_mut()
            .handler_ids_for_modules(root_deps.iter().map(|m| &m.id));
          handlers
        },
      });
    } else {
      println!(
        "WARNING: could not find page root module, using default root component (expected at {})",
        serde_json::to_string(&root_file.to_str().unwrap()).unwrap()
      );
    };

    // println!("scan found {} routes", result.routes.len());

    Ok(result)
  }
}
