use std::{
  cell::RefCell,
  collections::{HashMap, HashSet, VecDeque},
  path::{Path, PathBuf},
  sync::{Arc, Mutex},
};

use farmfe_compiler::Compiler;
use farmfe_core::{
  context::CompilationContext,
  error::{CompilationError, Result},
  module::{Module, ModuleId, ModuleMetaData},
  parking_lot::{MappedRwLockReadGuard, RwLockReadGuard},
  regex::Regex,
  relative_path::PathExt,
};
use lazy_static::lazy_static;

use walkdir::WalkDir;

use crate::{
  es_ast_helpers::{find_exports, find_head_export_type},
  head::HeadType,
  htmx::RequestHandlerMethod,
  ids::ComponentHandlerIds,
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

pub(crate) struct PageComponentInfo {
  pub(crate) handlers: ComponentHandlerIds,
  pub(crate) layouts: Vec<PageComponentLayoutItem>,
}

/// Critical route information
pub(crate) struct RouteInfo {
  pub(crate) module_id: ModuleId,
  pub(crate) page_component: Option<PageComponentInfo>,
  pub(crate) request_handlers: HashSet<RequestHandlerMethod>,
  pub(crate) on_load: bool,
  pub(crate) head: Option<HeadType>,
}

pub(crate) struct RootComponentInfo {
  pub(crate) module_id: ModuleId,
  pub(crate) handlers: ComponentHandlerIds,
  pub(crate) head: Option<HeadType>,
}

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
    modules: Vec<Module>,
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
          .borrow()
          .handler_ids_for_modules(modules.iter().map(|m| &m.id));

        let layouts = self.layouts.get_layouts_for_path(&path);
        for layout in &layouts {
          if let Some(layout_handlers) = layout.handlers {
            // println!(
            //   "merging {} handlers with layout {}:",
            //   main_module.id.relative_path(),
            //   layout.module_id.relative_path()
            // );
            for (method, action_ids) in &layout_handlers.action_handlers {
              for action_id in action_ids {
                println!("  {:?}: {}", method, action_id);
              }
            }
            for event_id in &layout_handlers.event_handlers {
              println!("  event: {}", event_id);
            }
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
    modules: Vec<Module>,
  ) -> Result<()> {
    self.layouts.add_layout(path, ids_mutex, modules)
  }

  fn add_middleware(&mut self, path: String, module_id: &ModuleId) {
    println!("adding middleware {}", module_id.relative_path());
    self
      .middlewares
      .entry(path)
      .or_default()
      .insert(module_id.clone());
  }
}

impl FarmPluginRecooler {
  fn get_module_dependencies<P: AsRef<Path>>(
    &self,
    source_path: P,
    context: &Arc<CompilationContext>,
  ) -> Result<Vec<Module>> {
    let source = if let Ok(canonical) = source_path.as_ref().canonicalize() {
      canonical.to_str().unwrap().to_string()
    } else {
      return Err(CompilationError::GenericError(format!(
        "failed to find source: {}",
        source_path.as_ref().to_str().unwrap().to_string()
      )));
    };

    let mut config = context.config.as_ref().clone();
    config.progress = false;
    config.input = HashMap::new();
    config.input.insert(String::from("child"), source.clone());

    let compiler =
      Compiler::new_without_internal_plugins(config, context.plugin_driver.plugins.clone())?;

    // copy module graph to new compiler
    // context
    //   .module_graph
    //   .read()
    //   .copy_to(&mut *compiler.context().module_graph.write(), true)?;

    let module_graph = compiler.trace_module_graph()?;

    // compiler
    //   .context()
    //   .module_graph
    //   .read()
    //   .copy_to(&mut *context.module_graph.write(), false)?;

    // {
    //   let mg_in = compiler.context().module_graph.read();
    //   let mut mg_out = context.module_graph.write();

    //   let mut new_modules = Vec::<ModuleId>::new();
    //   for module in mg_in.modules() {
    //     if !mg_out.has_module(&module.id) && module.id.relative_path().starts_with(&self.src_dir) {
    //       mg_out.add_module(module.clone());
    //       new_modules.push(module.id.clone());
    //     }
    //   }

    //   for module_id in &new_modules {
    //     for (dep, edge) in mg_in.dependencies(&module_id) {
    //       mg_out.add_edge(&module_id, &dep, edge.clone())?;
    //     }
    //   }
    // }

    let mut modules_queue = VecDeque::<String>::new();
    modules_queue.push_back(
      PathBuf::from(&source)
        .relative_to(module_graph.root)
        .unwrap()
        .to_string(),
    );

    let mut deps = Vec::<Module>::new();

    while let Some(module_id) = modules_queue.pop_front() {
      if !module_id.starts_with(&self.src_dir) {
        continue;
      }

      if let Some(deps) = module_graph.edges.get(&module_id) {
        modules_queue.extend(deps.iter().cloned());
      }

      let module_id = ModuleId::from(module_id);

      // TODO: ensure module form actions and event handlers are properly registered
      let graph = compiler.context().module_graph.read();
      let module = graph.module(&module_id).unwrap();
      // println!("module decls {}", module.meta.as_script().ast.body.len());
      if let ModuleMetaData::Script(script) = module.meta.as_ref() {
        let mut ids = self.ids.lock().unwrap();
        crate::htmx::validate::ensure_registered_handlers(&module_id, ids.get_mut(), &script.ast);

        deps.push(module.clone());
      }
    }

    Ok(deps)
  }
}

lazy_static! {
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
    for layout_file in &dir_contents.layout_files {
      result.add_layout(
        file_to_route_path(&self.routes_dir, layout_file),
        &self.ids,
        self.get_module_dependencies(layout_file, context)?,
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
        self.get_module_dependencies(route_file, context)?,
      )?;
    }

    let root_file = PathBuf::from(&self.src_dir).join("root.tsx");
    if let Some(root_file) = &dir_contents.root_file {
      let root_deps = self.get_module_dependencies(&root_file, context)?;
      let root_module_id = root_deps[0].id.clone();

      result.root_component = Some(RootComponentInfo {
        head: find_head_export_type(root_deps[0].meta.as_script()),
        module_id: root_module_id,
        handlers: {
          let ids_lock = self.ids.lock().unwrap();
          let handlers = ids_lock
            .borrow()
            .handler_ids_for_modules(root_deps.iter().map(|m| &m.id));
          handlers
        },
      });
    } else {
      println!(
        "WARNING: could not find page root module (expected at {})",
        serde_json::to_string(&root_file.to_str().unwrap()).unwrap()
      );
    };

    // println!("scan found {} routes", result.routes.len());

    Ok(result)
  }
}
