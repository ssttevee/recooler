use std::{
  cell::RefCell,
  cmp::Ordering,
  collections::{HashMap, HashSet, VecDeque},
  fs,
  path::{Path, PathBuf},
  sync::{Arc, Mutex},
};

use farmfe_compiler::{trace_module_graph::TracedModuleGraph, Compiler};
use farmfe_core::{
  config::{persistent_cache::PersistentCacheConfig, Config},
  context::CompilationContext,
  error::{CompilationError, Result},
  module::{module_graph::ModuleGraph, Module, ModuleId, ModuleMetaData, ModuleType},
  parking_lot::{MappedRwLockReadGuard, RwLockReadGuard},
  plugin::{Plugin, PluginHookContext, PluginLoadHookParam, PluginLoadHookResult},
  rayon::prelude::*,
  regex::Regex,
  relative_path::PathExt,
  serde_json,
  swc_ecma_ast::EsVersion,
  swc_ecma_parser::{EsSyntax, Syntax, TsSyntax},
};
use farmfe_toolkit::{
  lazy_static::lazy_static,
  script::{module_type_from_id, parse_module},
};
use walkdir::WalkDir;

use crate::{
  es_ast_helpers::{find_exports, find_head_export_type, find_source_value_exports},
  head::{build_head_expr, HeadType},
  htmx::{FormActionMethod, RequestHandlerMethod},
  ids::ComponentHandlerIds,
  imports::UniqueImportIdentifiers,
  layouts::LayoutTree,
  nested_compile::{nested_compile_input, shared_host_plugins},
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
  /// Whether the layout module has a default export, i.e. the layout HOC that wraps the page.
  pub(crate) has_hoc: bool,
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

/// A route of a [`RoutePlan`].
pub(crate) struct PlannedRoute<'a> {
  /// the hono path of the route, i.e. its route path without the groups
  pub(crate) path: String,
  /// the middleware placed directly in the directories of the groups enclosing the route,
  /// outermost group first
  pub(crate) group_middleware: Vec<ModuleId>,
  pub(crate) route: &'a RouteInfo,
}

/// The routes and middleware of a [`ScanResult`] in the order they are registered on the hono app.
pub(crate) struct RoutePlan<'a> {
  /// `(use path, module id)` of the middleware that isn't placed directly in a group directory,
  /// which handles every request under its directory, so it is registered before every route
  pub(crate) middlewares: Vec<(String, ModuleId)>,
  /// sorted with [`compare_route_paths`] so that hono matches a route before any route that would
  /// otherwise shadow it
  pub(crate) routes: Vec<PlannedRoute<'a>>,
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
    plan: &RoutePlan,
  ) -> Result<String> {
    let mut middlewares = String::new();
    let mut handlers = String::new();

    for (use_path, module_id) in &plan.middlewares {
      middlewares += format!(
        "{}.use({}, {});\n",
        hono_ident,
        serde_json::to_string(use_path).unwrap(),
        import_idents.identifier(module_id, "default"),
      )
      .as_str();
    }

    for PlannedRoute {
      path,
      group_middleware,
      route,
    } in &plan.routes
    {
      let pathjs = serde_json::to_string(path).unwrap();

      // group middleware only applies to the routes of the group, so rather than for every
      // request under a prefix, it is used for the exact path of each of the group's routes, right
      // before the route's own handlers so that it doesn't run for the routes registered earlier
      for module_id in group_middleware {
        handlers += format!(
          "{}.use({}, {});\n",
          hono_ident,
          pathjs,
          import_idents.identifier(module_id, "default"),
        )
        .as_str();
      }

      let mut route_params_prefix = pathjs.clone() + ", ";
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

      let mut request_methods = route.request_handlers.iter().collect::<Vec<_>>();
      request_methods.sort();

      for method in request_methods {
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
          let mut action_ids = page
            .handlers
            .action_handlers
            .get(method)
            .unwrap()
            .iter()
            .collect::<Vec<_>>();
          action_ids.sort();

          let actions = action_ids
            .into_iter()
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
        // a layout without a HOC, e.g. one that only exports `head`, doesn't wrap the page
        for layout_module in page.layouts.iter().filter(|l| l.has_hoc) {
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
          build_head_expr(import_idents, root_component, &page.layouts, route)
        );

        if let Some(client_script) = self.build_client_script(context, &route.module_id)? {
          page_handler_params += ", ";
          page_handler_params += serde_json::to_string(&client_script).unwrap().as_str();
        }

        page_handler += format!(
          "{}.get({}, {}({}));\n",
          hono_ident, pathjs, self.route_handler_factory.name, page_handler_params
        )
        .as_str();
      }

      handlers += action_handlers.as_str();
      handlers += request_handlers.as_str();
      handlers += page_handler.as_str();
    }

    Ok(middlewares + handlers.as_str())
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RouteSegmentKind {
  Static,
  Param,
  CatchAll,
}

fn route_segment_kind(segment: &str) -> RouteSegmentKind {
  if !segment.starts_with(':') {
    RouteSegmentKind::Static
  } else if segment.ends_with("{.+}") {
    RouteSegmentKind::CatchAll
  } else {
    RouteSegmentKind::Param
  }
}

/// Orders hono paths so that a path goes before the paths that could match the same requests less
/// specifically. The paths are compared segment by segment: a static segment goes before a
/// `:param`, which goes before a `:catchAll{.+}`, and static segments are compared lexically. When
/// one path is a prefix of the other, the shorter one goes first unless the prefix ends at a
/// catch-all, which also matches the longer one's requests. Paths that are otherwise equal, such as
/// those differing only in parameter names, are compared lexically.
pub(crate) fn compare_route_paths(a: &str, b: &str) -> Ordering {
  let mut a_segments = a.split('/').filter(|s| !s.is_empty());
  let mut b_segments = b.split('/').filter(|s| !s.is_empty());
  let mut after_catch_all = false;

  loop {
    let (a_segment, b_segment) = match (a_segments.next(), b_segments.next()) {
      (None, None) => return a.cmp(b),
      (None, Some(_)) if after_catch_all => return Ordering::Greater,
      (None, Some(_)) => return Ordering::Less,
      (Some(_), None) if after_catch_all => return Ordering::Less,
      (Some(_), None) => return Ordering::Greater,
      (Some(a_segment), Some(b_segment)) => (a_segment, b_segment),
    };

    let kind = route_segment_kind(a_segment);
    let order = kind.cmp(&route_segment_kind(b_segment)).then_with(|| {
      if kind == RouteSegmentKind::Static {
        a_segment.cmp(b_segment)
      } else {
        Ordering::Equal
      }
    });
    if order != Ordering::Equal {
      return order;
    }

    after_catch_all = kind == RouteSegmentKind::CatchAll;
  }
}

/// Returns whether a path segment is a group, i.e. a parenthesized directory name.
fn is_group_segment(segment: &str) -> bool {
  segment.len() > 2
    && segment.starts_with('(')
    && segment.ends_with(')')
    && !segment[1..segment.len() - 1].contains(')')
}

/// Returns the hono path of a route path, i.e. the route path without the groups.
pub(crate) fn route_path_without_groups(path: &str) -> String {
  let segments = path
    .split('/')
    .filter(|s| !s.is_empty() && !is_group_segment(s))
    .collect::<Vec<_>>();

  format!("/{}", segments.join("/"))
}

impl ScanResult {
  /// Returns the routes and middleware in hono registration order. Every route is registered on the
  /// same hono app, so that routes of different groups are ordered among each other too.
  pub(crate) fn route_plan(&self) -> RoutePlan<'_> {
    let mut middlewares = self
      .middlewares
      .iter()
      // middleware placed directly in a group directory is in `PlannedRoute::group_middleware`
      .filter(|(path, _)| !path.rsplit('/').next().is_some_and(is_group_segment))
      .map(|(path, module_ids)| (route_path_without_groups(path), path, module_ids))
      .collect::<Vec<_>>();
    middlewares.sort_by(|a, b| compare_route_paths(&a.0, &b.0).then_with(|| a.1.cmp(b.1)));

    let mut routes = self
      .routes
      .iter()
      .map(|(path, route)| (route_path_without_groups(path), path, route))
      .collect::<Vec<_>>();
    // routes with the same hono path, from different groups, are ordered by their group names
    routes.sort_by(|a, b| compare_route_paths(&a.0, &b.0).then_with(|| a.1.cmp(b.1)));

    RoutePlan {
      middlewares: middlewares
        .into_iter()
        .flat_map(|(hono_path, _, module_ids)| {
          let use_path = if hono_path == "/" {
            String::from("*")
          } else {
            hono_path + "/*"
          };

          let mut module_ids = module_ids.iter().cloned().collect::<Vec<_>>();
          module_ids.sort();
          module_ids
            .into_iter()
            .map(move |module_id| (use_path.clone(), module_id))
        })
        .collect(),
      routes: routes
        .into_iter()
        .map(|(hono_path, path, route)| PlannedRoute {
          path: hono_path,
          group_middleware: self.group_middleware(path),
          route,
        })
        .collect(),
    }
  }

  /// Returns the middleware placed directly in the directories of the groups enclosing the route
  /// at `path`, outermost group first.
  fn group_middleware(&self, path: &str) -> Vec<ModuleId> {
    let mut group_middleware = vec![];
    let mut prefix = String::new();

    for segment in path.split('/').filter(|s| !s.is_empty()) {
      prefix += "/";
      prefix += segment;

      if is_group_segment(segment) {
        if let Some(module_ids) = self.middlewares.get(&prefix) {
          let mut module_ids = module_ids.iter().cloned().collect::<Vec<_>>();
          module_ids.sort();
          group_middleware.extend(module_ids);
        }
      }
    }

    group_middleware
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

    // e.g. the routes of the `[id]` and `:id` directories
    if let Some(existing) = self.routes.get(&path) {
      return Err(CompilationError::GenericError(format!(
        "only one route may be used per path, found {} and {} for {}",
        existing.module_id.relative_path(),
        main_module.id.relative_path(),
        path
      )));
    }

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

          // must match `REQUEST_HANDLER_EXPORTS`
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
              has_hoc: l.handlers.is_some(),
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
          head: find_head_export_type(&script.ast),
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

/// Extensions of the files that the route scan's trace compile loads with their actual content, see
/// [`is_trace_stubbed`].
const TRACE_SCRIPT_EXTENSIONS: [&str; 8] = ["js", "jsx", "ts", "tsx", "mjs", "cjs", "mts", "cts"];

/// Version of the rules of [`is_trace_stubbed`], which is part of the trace compile's persistent
/// cache namespace, see [`FarmPluginRecooler::trace_persistent_cache_config`]. It must be bumped
/// whenever the rules change, or modules cached under the previous rules would be reused.
const TRACE_STUB_RULES_VERSION: u32 = 2;

/// Returns whether the route scan's trace compile loads the module as an empty script instead of
/// its actual content. That is:
///
/// - every file outside of `src_dir` and `routes_dir`: the scan never walks through them (see
///   [`FarmPluginRecooler::list_dependencies_for_module`]) and they are not transformed by
///   [`FarmPluginRecooler::process_module`], so loading and parsing them and their dependencies is
///   wasted work. The checks are the same string prefix checks as there, on the full id including
///   the query.
/// - every file that is not a script, by the extension of its resolved path, e.g. stylesheets: the
///   scan only reads the handlers and exports of script modules, and running e.g. postcss on them
///   is by far the most expensive part of the trace. Ids with a query, e.g. the
///   [`crate::HANDLERS_MODULE_QUERY`] variant of a component, are decided by the resolved path too.
///
/// Virtual modules don't have an absolute resolved path and are loaded as usual.
fn is_trace_stubbed(module_id: &str, resolved_path: &str, src_dir: &str, routes_dir: &str) -> bool {
  let resolved_path = Path::new(resolved_path);
  if !resolved_path.is_absolute() {
    return false;
  }

  let is_script = resolved_path
    .extension()
    .and_then(|ext| ext.to_str())
    .is_some_and(|ext| TRACE_SCRIPT_EXTENSIONS.contains(&ext));

  !is_script || (!module_id.starts_with(src_dir) && !module_id.starts_with(routes_dir))
}

/// Stubs out the modules of the route scan's trace compile that can't affect the scan result, see
/// [`is_trace_stubbed`].
struct TraceStubPlugin {
  src_dir: String,
  routes_dir: String,
}

impl Plugin for TraceStubPlugin {
  fn name(&self) -> &str {
    "FarmPluginRecoolerTraceStub"
  }

  // the load hook must run before any other plugin loads the module
  fn priority(&self) -> i32 {
    i32::MAX
  }

  fn load(
    &self,
    param: &PluginLoadHookParam,
    _context: &Arc<CompilationContext>,
    _hook_context: &PluginHookContext,
  ) -> Result<Option<PluginLoadHookResult>> {
    if !is_trace_stubbed(
      &param.module_id,
      param.resolved_path,
      &self.src_dir,
      &self.routes_dir,
    ) {
      return Ok(None);
    }

    Ok(Some(PluginLoadHookResult {
      content: String::new(),
      module_type: ModuleType::Js,
      source_map: None,
    }))
  }
}

impl FarmPluginRecooler {
  /// Returns the persistent cache config of the route scan's trace compile. Its modules may be
  /// stubbed (see [`is_trace_stubbed`]), so they are cached in a namespace of their own that is
  /// never shared with the other compiles. Modules are looked up in the cache by timestamp before
  /// they are loaded, i.e. before it is decided whether to stub them, so the namespace also depends
  /// on the options and the version of the rules that decide it, and on the options that
  /// `process_module` depends on.
  fn trace_persistent_cache_config(&self, config: &Config) -> PersistentCacheConfig {
    if !config.persistent_cache.enabled() {
      return PersistentCacheConfig::Bool(false);
    }

    let mut cache_config = match config.persistent_cache.as_ref() {
      PersistentCacheConfig::Obj(obj) => obj.clone(),
      PersistentCacheConfig::Bool(_) => config.persistent_cache.as_obj(&config.root),
    };
    let options_hash = farmfe_toolkit::hash::sha256(
      serde_json::to_string(&(&self.src_dir, &self.routes_dir, TRACE_STUB_RULES_VERSION))
        .unwrap()
        .as_bytes(),
      8,
    );
    cache_config.namespace = format!("{}-recooler-trace-{}", cache_config.namespace, options_hash);

    PersistentCacheConfig::Obj(cache_config)
  }

  fn list_dependencies_for_module(
    &self,
    traced_graph: &TracedModuleGraph,
    module_graph: &ModuleGraph,
    source: &String,
  ) -> Result<Vec<Module>> {
    let source_id = PathBuf::from(source)
      .relative_to(&traced_graph.root)
      .unwrap()
      .to_string();
    let mut modules_queue = VecDeque::<String>::new();
    modules_queue.push_back(source_id.clone());

    let mut deps = Vec::<Module>::new();
    let mut visited = HashSet::<String>::new();

    while let Some(module_id) = modules_queue.pop_front() {
      // the scanned module itself is always the first dependency, even when it is outside of
      // `src_dir`, which is the case for every file in a `routes_dir` outside of `src_dir`
      if module_id != source_id && !module_id.starts_with(&self.src_dir) {
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

      if let Some(module) = module_graph.module(&module_id) {
        // println!("module decls {}", module.meta.as_script().ast.body.len());
        if let ModuleMetaData::Script(script) = module.meta.as_ref() {
          // modules from the persistent cache were not transformed by `process_module`
          let mut ids = self.ids.lock().unwrap();
          crate::htmx::validate::ensure_registered_handlers(&module_id, ids.get_mut(), &script.ast)
            .map_err(CompilationError::GenericError)?;

          deps.push(module.clone());
        }
      } else {
        println!(
          "WARNING: module not found in module graph: {}",
          module_id.relative_path()
        );
      }
    }

    Ok(deps)
  }

  fn get_modules_dependencies(
    &self,
    source_paths: Vec<PathBuf>,
    context: &Arc<CompilationContext>,
  ) -> Result<HashMap<PathBuf, Vec<Module>>> {
    // e.g. an empty routes dir and no root component, which is an app without any routes. There is
    // nothing to trace, and the trace compile would fail without any input
    if source_paths.is_empty() {
      return Ok(HashMap::new());
    }

    let mut config = context.config.as_ref().clone();
    config.progress = false;
    config.input = nested_compile_input(
      &context.config,
      source_paths
        .iter()
        .map(|sp| {
          let s = sp.to_str().unwrap().to_string();
          if let Ok(canonical) = sp.canonicalize() {
            Ok((s, canonical.to_str().unwrap().to_string()))
          } else {
            Err(CompilationError::GenericError(format!(
              "failed to find source: {}",
              s
            )))
          }
        })
        .collect::<Result<Vec<_>>>()?,
    );
    config.persistent_cache = Box::new(self.trace_persistent_cache_config(&config));
    let canonical_path_map = config.input.clone();

    let mut plugins = shared_host_plugins(context);
    plugins.push(Arc::new(TraceStubPlugin {
      src_dir: self.src_dir.clone(),
      routes_dir: self.routes_dir.clone(),
    }) as _);

    let compiler = Compiler::new_without_internal_plugins(config, plugins)?;

    let traced_module = compiler.trace_module_graph()?;
    // unlike `compile`, tracing doesn't write the cache
    if compiler.context().config.persistent_cache.enabled() {
      compiler.context().cache_manager.write_cache();
    }

    let module_graph_rg = compiler.context().module_graph.read();
    let module_graph = &*module_graph_rg;

    source_paths
      .into_iter()
      .map(|sp| {
        let s = sp.to_str().unwrap().to_string();
        let deps = self.list_dependencies_for_module(
          &traced_module,
          module_graph,
          canonical_path_map.get(&s).unwrap(),
        )?;
        Ok((sp, deps))
      })
      .collect()
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

  let path = relpath
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
    .join("/");
  if path.starts_with(".") {
    return String::new();
  }

  format!("/{}", path)
}

lazy_static! {
  static ref ROUTE_FILE_PATTERN: Regex = Regex::new("^index\\.[jt]sx?$").unwrap();
  static ref LAYOUT_FILE_PATTERN: Regex = Regex::new("^layout\\.[jt]sx?$").unwrap();
  static ref MIDDLEWARE_FILE_PATTERN: Regex =
    Regex::new("^middleware(?:@[\\w\\d-]+)?\\.[jt]sx?$").unwrap();
}

/// The exports of a route module that [`ScanResult::add_route`] registers as request handlers.
const REQUEST_HANDLER_EXPORTS: [&str; 6] = [
  "onGet",
  "onPost",
  "onPut",
  "onDelete",
  "onPatch",
  "onOptions",
];

/// Returns the most permissive syntax for a script module of `module_type`, or `None` for other
/// module types.
///
/// The host's `config.script.parser` is deliberately not used: read through the plugin, its
/// `ts_config` has the `dts` flag set (a `serde(skip)` field the host never sets, so the host's
/// `ScriptConfig` layout evidently differs from the plugin's farmfe_core), which makes debug builds
/// of the swc parser abort on any assignment expression and makes release builds parse route files
/// as declaration files.
fn prefetch_parse_syntax(module_type: &ModuleType) -> Option<Syntax> {
  match module_type {
    ModuleType::Js | ModuleType::Jsx => Some(Syntax::Es(EsSyntax {
      jsx: matches!(module_type, ModuleType::Jsx),
      decorators: true,
      import_attributes: true,
      ..Default::default()
    })),
    ModuleType::Ts | ModuleType::Tsx => Some(Syntax::Typescript(TsSyntax {
      tsx: matches!(module_type, ModuleType::Tsx),
      decorators: true,
      ..Default::default()
    })),
    _ => None,
  }
}

/// Parses the script at `path` and returns its exports, see [`find_source_value_exports`], or
/// `None` if it can't be read or parsed.
fn read_source_value_exports(path: &Path) -> Option<HashSet<String>> {
  let id = path.to_str()?;
  let syntax = prefetch_parse_syntax(&module_type_from_id(id)?)?;
  let content = fs::read_to_string(path).ok()?;
  let parsed = parse_module(id, &content, syntax, EsVersion::latest()).ok()?;
  Some(find_source_value_exports(&parsed.ast))
}

/// Returns the warning for a layout or the root component module that exports `onLoad`, which is
/// silently ignored since only the `onLoad` of route modules is added to client scripts.
fn ignored_on_load_warning(module: &Module) -> Option<String> {
  let ModuleMetaData::Script(script) = module.meta.as_ref() else {
    return None;
  };

  find_exports(&module.id, script)
    .contains("onLoad")
    .then(|| {
      format!(
        "[recooler] {}: the onLoad export never runs, since only the onLoad of route modules (index files) is added to client scripts; move it to the route modules that need it",
        module.id.relative_path()
      )
    })
}

/// Returns whether the layout at `layout_path` wraps the page at `route_path`, i.e. whether
/// [`LayoutTree::get_layouts_for_path`] returns it, which is when the path segments of the layout
/// are a prefix of those of the page.
fn layout_wraps_route(layout_path: &str, route_path: &str) -> bool {
  let route_segments = route_path
    .strip_prefix('/')
    .unwrap_or(route_path)
    .split('/');
  // the layout tree nests layouts by the segments of their paths, except for the root layout
  let layout_segments = if layout_path.len() > 1 {
    layout_path[1..].split('/').collect::<Vec<_>>()
  } else {
    vec![]
  };

  let mut route_segments = route_segments.into_iter();
  layout_segments
    .into_iter()
    .all(|segment| route_segments.next() == Some(segment))
}

/// Returns whether `file_path` is a route module, i.e. one that `scan_dir` would find.
pub(crate) fn is_route_file(routes_dir: &str, file_path: &Path) -> bool {
  file_path.starts_with(routes_dir)
    && file_path
      .file_name()
      .and_then(|name| name.to_str())
      .is_some_and(|name| ROUTE_FILE_PATTERN.is_match(name))
}

/// Returns an error naming the files if a directory contains more than one of `files`, e.g. both a
/// `layout.tsx` and a `layout.jsx`, which would be used for the same path.
fn ensure_one_file_per_dir(kind: &str, files: &[PathBuf]) -> Result<()> {
  let mut files_by_dir = HashMap::<&Path, Vec<String>>::new();
  for file in files {
    files_by_dir
      .entry(file.parent().unwrap_or(Path::new("")))
      .or_default()
      .push(file.display().to_string());
  }

  let mut duplicates = files_by_dir
    .into_values()
    .filter(|files| files.len() > 1)
    .map(|mut files| {
      files.sort();
      files.join(", ")
    })
    .collect::<Vec<_>>();
  if duplicates.is_empty() {
    return Ok(());
  }

  duplicates.sort();
  Err(CompilationError::GenericError(format!(
    "only one {} file may be used per directory, found:\n  {}",
    kind,
    duplicates.join("\n  ")
  )))
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
    for e in WalkDir::new(&self.routes_dir).sort_by_file_name() {
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

    ensure_one_file_per_dir("route", &route_files)?;
    ensure_one_file_per_dir("layout", &layout_files)?;

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

  /// Returns the files in the routes directory and the root component that the generated server
  /// entrypoint is certain to import (see `generate_entrypoint_module`), decided without the route
  /// scan's trace compile, so that the host compile can start on them while the entrypoint is
  /// generated. Including a file that the entrypoint doesn't import would add it and its side
  /// effects to the build, so a file is only included if it is imported regardless of the scan
  /// result:
  ///
  /// - every middleware file.
  /// - a route file if its source exports a page component or a request handler. The page
  ///   component and request handlers are found with [`find_source_value_exports`], which only
  ///   parses the file, and leaves out exports that may be types.
  /// - the root component if any route file exports a page component.
  /// - a layout file if it wraps any of those pages.
  ///
  /// The scan result itself is computed from the same files after they are compiled, so its
  /// exports are a superset of these.
  pub(crate) fn entrypoint_prefetch_files(&self) -> Result<Vec<PathBuf>> {
    let dir_contents = self.scan_dir()?;

    let mut route_files = HashMap::<String, &PathBuf>::new();
    for route_file in &dir_contents.route_files {
      route_files.insert(file_to_route_path(&self.routes_dir, route_file), route_file);
    }

    // parsed in parallel, since the host compile waits for this
    let route_exports = route_files
      .into_par_iter()
      .filter_map(|(path, route_file)| {
        Some((path, route_file, read_source_value_exports(route_file)?))
      })
      .collect::<Vec<_>>();

    let mut files = dir_contents.middleware_files.clone();
    let mut page_paths = Vec::<String>::new();
    for (path, route_file, exports) in route_exports {
      let is_page = exports.contains("default");
      if is_page
        || exports.contains("onRequest")
        || REQUEST_HANDLER_EXPORTS
          .iter()
          .any(|name| exports.contains(*name))
      {
        files.push(route_file.clone());
      }

      if is_page {
        page_paths.push(path);
      }
    }

    if !page_paths.is_empty() {
      files.extend(dir_contents.root_file.iter().cloned());

      for layout_file in &dir_contents.layout_files {
        let layout_path = file_to_route_path(&self.routes_dir, layout_file);
        if page_paths
          .iter()
          .any(|page_path| layout_wraps_route(&layout_path, page_path))
        {
          files.push(layout_file.clone());
        }
      }
    }

    files.sort();
    Ok(files)
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

    let mut warnings = vec![];

    for layout_file in &dir_contents.layout_files {
      let layout_deps = deps_map.get(layout_file).unwrap();
      warnings.extend(ignored_on_load_warning(&layout_deps[0]));
      result.add_layout(
        file_to_route_path(&self.routes_dir, layout_file),
        &self.ids,
        // &self.get_module_dependencies(layout_file, context)?,
        layout_deps,
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
      warnings.extend(ignored_on_load_warning(&root_deps[0]));

      result.root_component = Some(RootComponentInfo {
        head: find_head_export_type(&root_deps[0].meta.as_script().ast),
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

    // the host prints these once its module graph is built
    let mut log_store = context.log_store.lock();
    for warning in warnings {
      log_store.add_warning(warning);
    }
    drop(log_store);

    // println!("scan found {} routes", result.routes.len());

    Ok(result)
  }
}

#[cfg(test)]
pub(crate) mod fixture {
  use std::{
    collections::HashMap,
    fs,
    path::PathBuf,
    sync::{
      atomic::{AtomicUsize, Ordering},
      Arc,
    },
  };

  use farmfe_compiler::Compiler;
  use farmfe_core::{
    config::{persistent_cache::PersistentCacheConfig, Config},
    context::CompilationContext,
    serde_json,
  };

  use crate::FarmPluginRecooler;

  /// A project in a new temporary directory, with a host compilation context that has the farm
  /// internal plugins and the plugin, so that tests can run the route scan's trace compile.
  pub(crate) struct Fixture {
    pub(crate) dir: PathBuf,
    pub(crate) plugin: Arc<FarmPluginRecooler>,
    pub(crate) context: Arc<CompilationContext>,
  }

  impl Fixture {
    /// Writes `files` (paths relative to the project dir and their contents) and a stand-in for the
    /// `farm-plugin-recooler` package, and creates the plugin with `options`, in which `src_dir`
    /// and `routes_dir` are relative to the project dir.
    pub(crate) fn new(files: &[(&str, &str)], options: serde_json::Value) -> Self {
      static COUNTER: AtomicUsize = AtomicUsize::new(0);
      let dir = std::env::temp_dir().join(format!(
        "recooler-test-{}-{}",
        std::process::id(),
        COUNTER.fetch_add(1, Ordering::SeqCst)
      ));
      let _ = fs::remove_dir_all(&dir);
      fs::create_dir_all(&dir).unwrap();
      // the transformed modules import the helpers, which the trace compile stubs out
      let helpers = [
        (
          "node_modules/farm-plugin-recooler/package.json",
          r#"{ "name": "farm-plugin-recooler", "exports": { "./helpers": "./helpers.js" } }"#,
        ),
        ("node_modules/farm-plugin-recooler/helpers.js", ""),
      ];
      for (path, content) in helpers.iter().chain(files) {
        let path = dir.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, content).unwrap();
      }

      let mut options = options;
      for option in ["src_dir", "routes_dir"] {
        if let Some(value) = options.get_mut(option) {
          *value = dir.join(value.as_str().unwrap()).to_str().unwrap().into();
        }
      }

      let config = Config {
        input: HashMap::new(),
        progress: false,
        persistent_cache: Box::new(PersistentCacheConfig::Bool(false)),
        ..Default::default()
      };
      let plugin = Arc::new(FarmPluginRecooler::new(&config, options.to_string()));
      assert!(
        plugin.option_errors.is_empty(),
        "{:?}",
        plugin.option_errors
      );
      let compiler = Compiler::new(config, vec![plugin.clone() as _]).unwrap();

      Self {
        dir,
        plugin,
        context: compiler.context().clone(),
      }
    }
  }

  impl Drop for Fixture {
    fn drop(&mut self) {
      let _ = fs::remove_dir_all(&self.dir);
    }
  }
}

#[cfg(test)]
mod tests {
  use farmfe_core::serde_json::json;
  use farmfe_core::{
    module::{Module, ModuleId, ModuleMetaData, ModuleType, ScriptModuleMetaData},
    swc_common::{Globals, GLOBALS},
    swc_ecma_ast::EsVersion,
    swc_ecma_parser::{Syntax, TsSyntax},
  };
  use farmfe_toolkit::script::parse_module;
  use std::{cmp::Ordering, collections::HashSet};

  use std::path::PathBuf;

  use super::{
    compare_route_paths, ensure_one_file_per_dir, fixture::Fixture, ignored_on_load_warning,
    is_trace_stubbed, layout_wraps_route, prefetch_parse_syntax, RouteInfo, RoutePlan, ScanResult,
  };

  fn scan_result(routes: &[&str], middlewares: &[&str]) -> ScanResult {
    let mut result = ScanResult::new();
    for path in routes {
      result.routes.insert(
        path.to_string(),
        RouteInfo {
          module_id: ModuleId::from(format!("src/routes{}/index.tsx", path).as_str()),
          page_component: None,
          request_handlers: HashSet::new(),
          on_request: false,
          on_load: false,
          head: None,
        },
      );
    }
    for path in middlewares {
      result.add_middleware(
        path.to_string(),
        &ModuleId::from(format!("src/routes{}/middleware.ts", path).as_str()),
      );
    }
    result
  }

  /// `(hono path, route module, group middleware modules)` in registration order
  fn planned_routes(plan: &RoutePlan) -> Vec<(String, String, Vec<String>)> {
    plan
      .routes
      .iter()
      .map(|r| {
        (
          r.path.clone(),
          r.route.module_id.to_string(),
          r.group_middleware.iter().map(|m| m.to_string()).collect(),
        )
      })
      .collect()
  }

  fn sorted(paths: &[&str]) -> Vec<String> {
    let mut paths = paths.iter().map(|p| p.to_string()).collect::<Vec<_>>();
    paths.sort_by(|a, b| compare_route_paths(a, b));
    paths
  }

  #[test]
  fn static_segment_before_param_before_catch_all() {
    assert_eq!(compare_route_paths("/new", "/:id"), Ordering::Less);
    assert_eq!(compare_route_paths("/:id", "/:rest{.+}"), Ordering::Less);
    assert_eq!(compare_route_paths("/new", "/:rest{.+}"), Ordering::Less);
    assert_eq!(compare_route_paths("/top/new", "/top/:id"), Ordering::Less);
    assert_eq!(
      compare_route_paths("/top/:id", "/top/:rest{.+}"),
      Ordering::Less
    );
    assert_eq!(compare_route_paths("/:a/new", "/:a/:b"), Ordering::Less);
    assert_eq!(
      compare_route_paths("/:id/:b", "/:id/:rest{.+}"),
      Ordering::Less
    );
    // an earlier segment decides before a later one
    assert_eq!(compare_route_paths("/top/:id", "/:a/new"), Ordering::Less);
    // parameter names don't matter, `/:a/:c` would shadow `/:b/x`
    assert_eq!(compare_route_paths("/:b/x", "/:a/:c"), Ordering::Less);
  }

  #[test]
  fn deeper_and_shallower_paths() {
    assert_eq!(compare_route_paths("/", "/about"), Ordering::Less);
    assert_eq!(compare_route_paths("/", "/:rest{.+}"), Ordering::Less);
    assert_eq!(compare_route_paths("/blog", "/blog/:post"), Ordering::Less);
    assert_eq!(
      compare_route_paths("/blog/:post", "/:other{.+}"),
      Ordering::Less
    );
    // a catch-all goes after the deeper routes it would shadow
    assert_eq!(
      compare_route_paths("/a/:rest{.+}", "/:rest{.+}"),
      Ordering::Less
    );
    assert_eq!(
      compare_route_paths("/:a/:rest{.+}", "/:rest{.+}"),
      Ordering::Less
    );
    assert_eq!(
      compare_route_paths("/:rest{.+}/x", "/:rest{.+}"),
      Ordering::Less
    );
  }

  #[test]
  fn route_path_order_is_total_and_deterministic() {
    let paths = [
      "/",
      "/:other{.+}",
      "/:rest{.+}",
      "/:rest{.+}/x",
      "/:id",
      "/:a/:b",
      "/:a/new",
      "/about",
      "/blog",
      "/blog/:post",
      "/blog/:slug",
      "/top/:id",
      "/top/new",
      "/top/:rest{.+}",
    ];

    for a in paths {
      for b in paths {
        let order = compare_route_paths(a, b);
        assert_eq!(order, compare_route_paths(b, a).reverse(), "{} {}", a, b);
        assert_eq!(order == Ordering::Equal, a == b, "{} {}", a, b);
      }
    }

    let expected = sorted(&paths);
    let mut reversed = paths;
    reversed.reverse();
    assert_eq!(sorted(&reversed), expected);
    let mut rotated = paths;
    rotated.rotate_left(5);
    assert_eq!(sorted(&rotated), expected);
    assert_eq!(
      expected,
      [
        "/",
        "/about",
        "/blog",
        "/blog/:post",
        "/blog/:slug",
        "/top/new",
        "/top/:id",
        "/top/:rest{.+}",
        "/:id",
        "/:a/new",
        "/:a/:b",
        "/:rest{.+}/x",
        "/:other{.+}",
        "/:rest{.+}",
      ]
    );
  }

  #[test]
  fn route_plan_orders_routes_of_all_groups_together() {
    let result = scan_result(
      &[
        "/",
        "/top/new",
        "/top/:id",
        "/(a)/docs/:slug",
        "/(m)/docs/new",
        "/(a)/:rest{.+}",
        "/(a)/(b)/nested",
        "/(z)/zed",
        "/plain",
        "/foo/(g)",
        "/foo/(g)/bar",
      ],
      &["/", "/(a)", "/(a)/(b)", "/(m)/docs", "/foo/(g)"],
    );
    let plan = result.route_plan();

    let mw = |path: &str| format!("src/routes{}/middleware.ts", path);
    let route = |path: &str| format!("src/routes{}/index.tsx", path);
    assert_eq!(
      plan.middlewares,
      [
        (String::from("*"), ModuleId::from(mw("/").as_str())),
        (
          String::from("/docs/*"),
          ModuleId::from(mw("/(m)/docs").as_str())
        ),
      ]
    );
    assert_eq!(
      planned_routes(&plan),
      [
        ("/", route("/"), vec![]),
        ("/docs/new", route("/(m)/docs/new"), vec![]),
        ("/docs/:slug", route("/(a)/docs/:slug"), vec![mw("/(a)")]),
        ("/foo", route("/foo/(g)"), vec![mw("/foo/(g)")]),
        ("/foo/bar", route("/foo/(g)/bar"), vec![mw("/foo/(g)")]),
        (
          "/nested",
          route("/(a)/(b)/nested"),
          vec![mw("/(a)"), mw("/(a)/(b)")]
        ),
        ("/plain", route("/plain"), vec![]),
        ("/top/new", route("/top/new"), vec![]),
        ("/top/:id", route("/top/:id"), vec![]),
        ("/zed", route("/(z)/zed"), vec![]),
        ("/:rest{.+}", route("/(a)/:rest{.+}"), vec![mw("/(a)")]),
      ]
      .map(|(path, route, mw)| (path.to_string(), route, mw))
    );
  }

  #[test]
  fn route_plan_orders_readme_example() {
    let result = scan_result(
      &[
        "/(public)/:other{.+}",
        "/(public)/about",
        "/(public)/blog/:post",
        "/(public)/blog",
        "/(admin)/admin",
        "/",
      ],
      &["/(admin)/admin", "/"],
    );
    let plan = result.route_plan();

    assert_eq!(
      plan
        .routes
        .iter()
        .map(|r| r.path.as_str())
        .collect::<Vec<_>>(),
      [
        "/",
        "/about",
        "/admin",
        "/blog",
        "/blog/:post",
        "/:other{.+}"
      ]
    );
    assert_eq!(
      plan
        .middlewares
        .iter()
        .map(|m| m.0.as_str())
        .collect::<Vec<_>>(),
      ["*", "/admin/*"]
    );
  }

  #[test]
  fn route_plan_orders_duplicate_routes_by_group() {
    let result = scan_result(&["/(b)/about", "/about", "/(a)/about"], &[]);

    assert_eq!(
      planned_routes(&result.route_plan())
        .into_iter()
        .map(|r| r.1)
        .collect::<Vec<_>>(),
      [
        "src/routes/(a)/about/index.tsx",
        "src/routes/(b)/about/index.tsx",
        "src/routes/about/index.tsx",
      ]
    );
  }

  fn on_load_warning(path: &str, source: &str) -> Option<String> {
    GLOBALS.set(&Globals::new(), || {
      let parsed = parse_module(
        path,
        source,
        Syntax::Typescript(TsSyntax {
          tsx: true,
          ..Default::default()
        }),
        EsVersion::EsNext,
      )
      .unwrap();
      let mut module = Module::new(ModuleId::new(path, "", "."));
      module.meta = Box::new(ModuleMetaData::Script(ScriptModuleMetaData {
        ast: parsed.ast,
        ..Default::default()
      }));
      ignored_on_load_warning(&module)
    })
  }

  #[test]
  fn prefetch_parse_syntax_never_parses_declaration_files() {
    for (module_type, tsx) in [(ModuleType::Ts, false), (ModuleType::Tsx, true)] {
      let Some(Syntax::Typescript(syntax)) = prefetch_parse_syntax(&module_type) else {
        panic!("{:?}", module_type);
      };
      assert!(!syntax.dts);
      assert!(syntax.decorators);
      assert_eq!(syntax.tsx, tsx);
    }
    for (module_type, jsx) in [(ModuleType::Js, false), (ModuleType::Jsx, true)] {
      let Some(Syntax::Es(syntax)) = prefetch_parse_syntax(&module_type) else {
        panic!("{:?}", module_type);
      };
      assert_eq!(syntax.jsx, jsx);
      assert!(syntax.import_attributes);
    }
    assert!(prefetch_parse_syntax(&ModuleType::Css).is_none());
  }

  #[test]
  fn warns_about_on_load_of_layout_and_root_modules() {
    let warning = on_load_warning(
      "src/routes/layout.tsx",
      "export default (Content) => Content;\nexport function onLoad() {}",
    )
    .unwrap();
    assert!(warning.starts_with("[recooler] src/routes/layout.tsx: the onLoad export never runs"));

    for source in [
      "export const onLoad = () => {};",
      "const load = () => {}; export { load as onLoad };",
      r#"export { onLoad } from "./client";"#,
    ] {
      assert!(
        on_load_warning("src/root.tsx", source).is_some(),
        "{}",
        source
      );
    }

    for source in [
      "export default (Content) => Content;",
      "export function onload() {}",
      "function onLoad() {} export default onLoad;",
    ] {
      assert!(
        on_load_warning("src/root.tsx", source).is_none(),
        "{}",
        source
      );
    }
  }

  const PAGE: &str = "export default () => <p />;";
  const LAYOUT: &str =
    "export default (Page: any) => (props: any) => <main><Page {...props} /></main>;";

  fn scan_error(files: &[(&str, &str)]) -> String {
    let fixture = Fixture::new(files, json!({ "src_dir": "src" }));
    match fixture.plugin.scan_routes_(&fixture.context) {
      Ok(_) => panic!("expected the scan to fail"),
      Err(e) => e.to_string(),
    }
  }

  #[test]
  fn one_file_per_dir() {
    let paths = |paths: &[&str]| paths.iter().map(PathBuf::from).collect::<Vec<_>>();
    assert!(ensure_one_file_per_dir(
      "layout",
      &paths(&["r/layout.tsx", "r/a/layout.tsx", "r/(g)/layout.jsx"])
    )
    .is_ok());

    let message = ensure_one_file_per_dir(
      "layout",
      &paths(&[
        "r/layout.tsx",
        "r/a/layout.jsx",
        "r/a/b/layout.tsx",
        "r/a/layout.tsx",
        "r/layout.js",
      ]),
    )
    .unwrap_err()
    .to_string();
    assert!(message.contains("only one layout file may be used per directory"));
    assert!(message.contains("r/a/layout.jsx, r/a/layout.tsx"));
    assert!(message.contains("r/layout.js, r/layout.tsx"));
    assert!(!message.contains("r/a/b/layout.tsx"));
  }

  #[test]
  fn scan_rejects_two_layout_or_route_files_in_a_dir() {
    let message = scan_error(&[
      ("src/routes/index.tsx", PAGE),
      ("src/routes/layout.tsx", LAYOUT),
      ("src/routes/layout.jsx", LAYOUT),
    ]);
    assert!(message.contains("only one layout file may be used per directory"));
    assert!(message.contains("src/routes/layout.jsx, "));
    assert!(message.contains("src/routes/layout.tsx"));

    let message = scan_error(&[
      ("src/routes/index.tsx", PAGE),
      ("src/routes/index.jsx", PAGE),
    ]);
    assert!(message.contains("only one route file may be used per directory"));
    assert!(message.contains("src/routes/index.jsx, "));
    assert!(message.contains("src/routes/index.tsx"));
  }

  #[test]
  fn scan_rejects_two_layouts_or_routes_for_a_path() {
    let message = scan_error(&[
      ("src/routes/[id]/index.tsx", PAGE),
      ("src/routes/[id]/layout.tsx", LAYOUT),
      ("src/routes/:id/layout.tsx", LAYOUT),
    ]);
    assert!(message.contains("only one layout may be used per path"));
    assert!(message.contains("/src/routes/:id/layout.tsx and "));
    assert!(message.contains("/src/routes/[id]/layout.tsx for /:id"));

    let message = scan_error(&[
      ("src/routes/[id]/index.tsx", PAGE),
      ("src/routes/:id/index.tsx", PAGE),
    ]);
    assert!(message.contains("only one route may be used per path"));
    assert!(message.contains("/src/routes/:id/index.tsx and "));
    assert!(message.contains("/src/routes/[id]/index.tsx for /:id"));
  }

  #[test]
  fn scan_without_route_files_or_root_component() {
    for files in [
      vec![("src/routes/.keep", "")],
      vec![("src/routes/util.ts", "export const x = 1;")],
    ] {
      let fixture = Fixture::new(&files, json!({ "src_dir": "src" }));
      let result = fixture.plugin.scan_routes_(&fixture.context).unwrap();
      assert!(result.routes.is_empty());
      assert!(result.root_component.is_none());
    }
  }

  #[test]
  fn scan_routes_dir_outside_src_dir() {
    let fixture = Fixture::new(
      &[
        (
          "src/root.tsx",
          "export default (Page: any) => (props: any) => <html><Page {...props} /></html>;",
        ),
        (
          "src/button.tsx",
          "export const Button = () => <button hx-on:click={() => alert(1)}>x</button>;",
        ),
        (
          "routes/index.tsx",
          "import { Button } from '../src/button';\n\
           export default () => <Button />;\n\
           export const onLoad = () => {};",
        ),
        (
          "routes/layout.tsx",
          "export default (Page: any) => (props: any) => <main><Page {...props} /></main>;",
        ),
      ],
      json!({ "src_dir": "src", "routes_dir": "routes" }),
    );
    let result = fixture.plugin.scan_routes_(&fixture.context).unwrap();

    let route = result.routes.get("/").unwrap();
    assert!(route.module_id.to_string().ends_with("/routes/index.tsx"));
    assert!(route.on_load);
    let page = route.page_component.as_ref().unwrap();
    assert_eq!(page.layouts.len(), 1);
    assert!(page.layouts[0]
      .module_id
      .to_string()
      .ends_with("/routes/layout.tsx"));
    // the dependencies in `src_dir` of a route outside of it are still scanned
    assert_eq!(page.handlers.event_handlers.len(), 1);
    assert!(result.root_component.is_some());
  }

  #[test]
  fn layout_without_hoc_does_not_wrap_page() {
    let fixture = Fixture::new(
      &[
        ("src/routes/index.tsx", PAGE),
        (
          "src/routes/layout.tsx",
          "export const head = { title: 'x' };",
        ),
        ("src/routes/sub/index.tsx", PAGE),
        ("src/routes/sub/layout.tsx", LAYOUT),
      ],
      json!({ "src_dir": "src" }),
    );
    let app = fixture
      .plugin
      .generate_entrypoint_impl_module(&fixture.context, &Default::default())
      .unwrap();
    let page_statement = |path: &str| {
      app
        .lines()
        .find(|line| {
          line
            .trim_start()
            .starts_with(&format!("app.get({:?}, ", path))
        })
        .unwrap()
        .to_string()
    };

    let index = page_statement("/");
    assert!(
      !index.contains("_src_routes_layout_tsx_default"),
      "{}",
      index
    );
    assert!(index.contains("_src_routes_layout_tsx_head"), "{}", index);

    let sub = page_statement("/sub");
    assert!(!sub.contains("_src_routes_layout_tsx_default"), "{}", sub);
    assert!(sub.contains("_src_routes_layout_tsx_head"), "{}", sub);
    assert!(
      sub.contains("_src_routes_sub_layout_tsx_default(_"),
      "{}",
      sub
    );
  }

  #[test]
  fn root_layout_wraps_every_route() {
    assert!(layout_wraps_route("/", "/"));
    assert!(layout_wraps_route("/", "/about"));
    assert!(layout_wraps_route("/", "/(public)/blog/:post"));
  }

  #[test]
  fn layout_wraps_routes_under_its_path() {
    assert!(layout_wraps_route("/(public)", "/(public)"));
    assert!(layout_wraps_route("/(public)", "/(public)/about"));
    assert!(layout_wraps_route("/blog", "/blog/:post"));
    assert!(layout_wraps_route("/blog/:post", "/blog/:post"));
    assert!(!layout_wraps_route("/blog", "/"));
    assert!(!layout_wraps_route("/blog", "/blogs"));
    assert!(!layout_wraps_route("/blog/:post", "/blog"));
    assert!(!layout_wraps_route("/(public)", "/(admin)/about"));
  }

  fn stubbed(module_id: &str, resolved_path: &str) -> bool {
    is_trace_stubbed(module_id, resolved_path, "src", "src/routes")
  }

  #[test]
  fn trace_stubs_files_outside_src_dir() {
    assert!(stubbed(
      "node_modules/hono/dist/index.js",
      "/app/node_modules/hono/dist/index.js"
    ));
    assert!(stubbed("../common/util.ts", "/common/util.ts"));
    assert!(stubbed(
      "node_modules/lib/style.css?inline",
      "/app/node_modules/lib/style.css"
    ));
  }

  #[test]
  fn trace_does_not_stub_files_in_src_dir() {
    assert!(!stubbed("src/root.tsx", "/app/src/root.tsx"));
    assert!(!stubbed(
      "src/routes/index.tsx",
      "/app/src/routes/index.tsx"
    ));
    assert!(!stubbed(
      "src/components/button.tsx?recooler-handlers",
      "/app/src/components/button.tsx"
    ));
    for ext in ["js", "jsx", "ts", "tsx", "mjs", "cjs", "mts", "cts"] {
      assert!(!stubbed(
        &format!("src/lib/util.{}", ext),
        &format!("/app/src/lib/util.{}", ext)
      ));
    }
    assert!(!stubbed("src/lib/util.ts?raw", "/app/src/lib/util.ts"));
  }

  #[test]
  fn trace_stubs_non_script_files_in_src_dir() {
    assert!(stubbed("src/styles.css", "/app/src/styles.css"));
    assert!(stubbed(
      "src/routes/_admin/index.css",
      "/app/src/routes/_admin/index.css"
    ));
    assert!(stubbed("src/styles.css?inline", "/app/src/styles.css"));
    assert!(stubbed("src/data.json", "/app/src/data.json"));
    assert!(stubbed("src/icon.svg", "/app/src/icon.svg"));
    assert!(stubbed("src/LICENSE", "/app/src/LICENSE"));
    assert!(stubbed(
      "src/components/button.tsx.css",
      "/app/src/components/button.tsx.css"
    ));
  }

  #[test]
  fn trace_does_not_stub_files_in_routes_dir_outside_src_dir() {
    assert!(!is_trace_stubbed(
      "pages/index.tsx",
      "/app/pages/index.tsx",
      "src",
      "pages"
    ));
    assert!(is_trace_stubbed(
      "lib/util.ts",
      "/app/lib/util.ts",
      "src",
      "pages"
    ));
    assert!(is_trace_stubbed(
      "pages/styles.css",
      "/app/pages/styles.css",
      "src",
      "pages"
    ));
  }

  #[test]
  fn trace_does_not_stub_virtual_modules() {
    assert!(!stubbed("recooler::metadata", "recooler::metadata"));
    assert!(!stubbed("virtual:module", "virtual:module"));
  }
}
