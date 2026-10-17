//! Reporting of the modules matched by the `client_banned_imports` option in compiled client
//! scripts. The imports kept for event handlers are checked separately, see
//! [`crate::htmx::transform::slice_handler_module`].

use std::collections::{HashMap, VecDeque};

use farmfe_core::{
  module::{module_graph::ModuleGraph, ModuleId},
  regex::Regex,
};

/// Returns the forms of a module id that the `client_banned_imports` patterns are matched against,
/// without the query: the module's path relative to the project root, and for a module in a
/// `node_modules` directory, its path inside of the last one, e.g. `faunadb/index.js` for
/// `../../../Library/pnpm/store/v10/links/faunadb/4.8.0/hash/node_modules/faunadb/index.js` and
/// `@scope/pkg/x.js` for `node_modules/.pnpm/@scope+pkg@1.0.0/node_modules/@scope/pkg/x.js`.
fn module_id_forms(id: &ModuleId) -> Vec<&str> {
  let path = id.relative_path();
  let mut forms = vec![path];
  let package_path = ["node_modules/", "node_modules\\"]
    .iter()
    .filter_map(|segment| {
      let i = path.rfind(segment)?;
      // a whole segment, not e.g. `my_node_modules/`
      (i == 0 || path[..i].ends_with(['/', '\\'])).then(|| &path[i + segment.len()..])
    })
    .min_by_key(|package_path| package_path.len());
  forms.extend(package_path);
  forms
}

/// Whether any of the `client_banned_imports` patterns matches a module id, see
/// [`module_id_forms`].
pub(crate) fn is_banned_module(patterns: &[Regex], id: &ModuleId) -> bool {
  let forms = module_id_forms(id);
  patterns
    .iter()
    .any(|pattern| forms.iter().any(|form| pattern.is_match(form)))
}

/// Returns the modules of a compiled client script's graph that are `banned`, sorted by id, each
/// followed by the shortest chain of modules that imports it from `entry`, e.g.
/// `node_modules/faunadb/index.js via src/routes/index.tsx → src/lib/db.ts`.
///
/// The banned modules and the chains leave out the query of the handlers variant of a module, and
/// the chain leaves out `entry` itself, which is the generated client entry. Banned modules that
/// `entry` doesn't import, e.g. those of the farm runtime, are listed without a chain.
pub(crate) fn banned_client_modules(
  graph: &ModuleGraph,
  entry: &ModuleId,
  banned: impl Fn(&ModuleId) -> bool,
) -> Vec<String> {
  let mut banned_ids = graph
    .modules()
    .into_iter()
    .map(|module| &module.id)
    .filter(|id| *id != entry && banned(id))
    .collect::<Vec<_>>();
  if banned_ids.is_empty() {
    return vec![];
  }
  banned_ids.sort_by_key(|id| id.to_string());

  // walk the imports breadth first so that the chains are the shortest; the dependencies are in
  // the order of their imports, which decides between chains of the same length
  let mut importers = HashMap::<ModuleId, ModuleId>::new();
  let mut queue = VecDeque::new();
  if graph.has_module(entry) {
    queue.push_back(entry.clone());
  }
  while let Some(importer) = queue.pop_front() {
    for dep in graph.dependencies_ids(&importer) {
      if dep != *entry && !importers.contains_key(&dep) {
        importers.insert(dep.clone(), importer.clone());
        queue.push_back(dep);
      }
    }
  }

  banned_ids
    .into_iter()
    .map(|id| {
      let mut chain = vec![];
      let mut current = importers.get(id);
      while let Some(importer) = current.filter(|importer| *importer != entry) {
        chain.push(importer.relative_path().to_string());
        current = importers.get(importer);
      }
      if chain.is_empty() {
        return id.relative_path().to_string();
      }
      chain.reverse();
      format!("{} via {}", id.relative_path(), chain.join(" → "))
    })
    .collect()
}

#[cfg(test)]
mod tests {
  use farmfe_core::module::{
    module_graph::{ModuleGraph, ModuleGraphEdge, ModuleGraphEdgeDataItem},
    Module, ModuleId,
  };

  use farmfe_core::regex::Regex;

  use super::{banned_client_modules, is_banned_module, module_id_forms};

  #[test]
  fn matches_module_ids_without_query_and_by_package_path() {
    let forms = |id: &str| {
      module_id_forms(&ModuleId::from(id))
        .into_iter()
        .map(String::from)
        .collect::<Vec<_>>()
    };
    assert_eq!(forms("src/lib/db.ts"), vec!["src/lib/db.ts"]);
    assert_eq!(
      forms("src/lib/db.ts?recooler-handlers"),
      vec!["src/lib/db.ts"]
    );
    assert_eq!(
      forms("node_modules/faunadb/index.js?raw"),
      vec!["node_modules/faunadb/index.js", "faunadb/index.js"]
    );
    // pnpm's global store, outside of the project
    assert_eq!(
      forms(
        "../../../Library/pnpm/store/v10/links/faunadb/4.8.0/abc/node_modules/faunadb/index.js"
      ),
      vec![
        "../../../Library/pnpm/store/v10/links/faunadb/4.8.0/abc/node_modules/faunadb/index.js",
        "faunadb/index.js"
      ]
    );
    // pnpm's virtual store, the last `node_modules` is the package's
    assert_eq!(
      forms("node_modules/.pnpm/@scope+pkg@1.0.0/node_modules/@scope/pkg/x.js")[1],
      "@scope/pkg/x.js"
    );
    // a workspace package's own dependencies
    assert_eq!(
      forms("../common/node_modules/faunadb/index.js")[1],
      "faunadb/index.js"
    );
    assert_eq!(forms("../common/src/db.ts"), vec!["../common/src/db.ts"]);
    assert_eq!(
      forms("src/my_node_modules/x.js"),
      vec!["src/my_node_modules/x.js"]
    );
    assert_eq!(
      forms("C:\\app\\node_modules\\faunadb\\index.js")[1],
      "faunadb\\index.js"
    );

    let patterns = [
      Regex::new("^faunadb/").unwrap(),
      Regex::new(r"lib/db\.ts$").unwrap(),
    ];
    let banned = |id: &str| is_banned_module(&patterns, &ModuleId::from(id));
    assert!(banned(
      "../../../Library/pnpm/store/v10/links/faunadb/4.8.0/abc/node_modules/faunadb/index.js"
    ));
    assert!(banned("node_modules/faunadb/index.js"));
    assert!(banned("src/lib/db.ts?recooler-handlers"));
    assert!(!banned("node_modules/not-faunadb/index.js"));
    assert!(!banned("src/faunadb/index.js"));
  }

  fn graph(edges: &[(&str, &str)]) -> ModuleGraph {
    let mut graph = ModuleGraph::new();
    for (from, to) in edges {
      for id in [from, to] {
        let id = ModuleId::from(*id);
        if !graph.has_module(&id) {
          graph.add_module(Module::new(id));
        }
      }
    }
    for (order, (from, to)) in edges.iter().enumerate() {
      graph
        .add_edge(
          &ModuleId::from(*from),
          &ModuleId::from(*to),
          ModuleGraphEdge::new(vec![ModuleGraphEdgeDataItem {
            source: to.to_string(),
            order,
            ..Default::default()
          }]),
        )
        .unwrap();
    }
    graph
  }

  #[test]
  fn lists_banned_modules_with_their_shortest_import_chain() {
    let graph = graph(&[
      (
        "recooler:@src/routes/a/index.tsx",
        "src/routes/a/index.tsx?recooler-handlers",
      ),
      (
        "recooler:@src/routes/a/index.tsx",
        "src/components/button.tsx?recooler-handlers",
      ),
      (
        "src/routes/a/index.tsx?recooler-handlers",
        "src/lib/util.ts",
      ),
      (
        "src/components/button.tsx?recooler-handlers",
        "src/lib/db.ts",
      ),
      ("src/lib/util.ts", "src/lib/db.ts"),
      ("src/lib/db.ts", "node_modules/faunadb/index.js"),
      (
        "node_modules/faunadb/index.js",
        "node_modules/faunadb/lib.js",
      ),
      (
        "@farmfe/runtime/src/index.ts.farm-runtime",
        "node_modules/fauna-runtime.js",
      ),
    ]);

    assert_eq!(
      banned_client_modules(
        &graph,
        &ModuleId::from("recooler:@src/routes/a/index.tsx"),
        |id| id.to_string().contains("fauna"),
      ),
      vec![
        // not imported by the entry
        "node_modules/fauna-runtime.js",
        "node_modules/faunadb/index.js via src/components/button.tsx → src/lib/db.ts",
        "node_modules/faunadb/lib.js via src/components/button.tsx → src/lib/db.ts → node_modules/faunadb/index.js",
      ]
    );
  }

  #[test]
  fn lists_banned_modules_without_query_through_import_cycles() {
    let graph = graph(&[
      (
        "recooler:@src/routes/a/index.tsx",
        "src/components/button.tsx?recooler-handlers",
      ),
      ("src/components/button.tsx?recooler-handlers", "src/lib/a.ts"),
      ("src/lib/a.ts", "src/lib/b.ts"),
      ("src/lib/b.ts", "src/lib/a.ts"),
      // a cycle back to the entry
      ("src/lib/b.ts", "recooler:@src/routes/a/index.tsx"),
      ("src/lib/b.ts", "node_modules/faunadb/index.js"),
    ]);

    assert_eq!(
      banned_client_modules(
        &graph,
        &ModuleId::from("recooler:@src/routes/a/index.tsx"),
        |id| id.relative_path().contains("fauna") || id.relative_path().contains("button"),
      ),
      vec![
        // imported directly by the entry
        "node_modules/faunadb/index.js via src/components/button.tsx → src/lib/a.ts → src/lib/b.ts",
        "src/components/button.tsx",
      ]
    );
  }

  #[test]
  fn lists_no_banned_modules() {
    let graph = graph(&[("recooler:@src/routes/a/index.tsx", "src/lib/db.ts")]);
    assert!(banned_client_modules(
      &graph,
      &ModuleId::from("recooler:@src/routes/a/index.tsx"),
      |id| id.to_string().contains("fauna"),
    )
    .is_empty());
  }
}
