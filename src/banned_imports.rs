//! Reporting of the modules matched by the `client_banned_imports` option in compiled client
//! scripts. The imports kept for event handlers are checked separately, see
//! [`crate::htmx::transform::slice_handler_module`].

use std::collections::{HashMap, VecDeque};

use farmfe_core::module::{module_graph::ModuleGraph, ModuleId};

/// Returns the modules of a compiled client script's graph that are `banned`, sorted by id, each
/// followed by the shortest chain of modules that imports it from `entry`, e.g.
/// `node_modules/faunadb/index.js via src/routes/index.tsx → src/lib/db.ts`.
///
/// The chain leaves out `entry` itself, which is the generated client entry, and the query of the
/// handlers variant of a module. Banned modules that `entry` doesn't import, e.g. those of the
/// farm runtime, are listed without a chain.
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
        return id.to_string();
      }
      chain.reverse();
      format!("{} via {}", id.to_string(), chain.join(" → "))
    })
    .collect()
}

#[cfg(test)]
mod tests {
  use farmfe_core::module::{
    module_graph::{ModuleGraph, ModuleGraphEdge, ModuleGraphEdgeDataItem},
    Module, ModuleId,
  };

  use super::banned_client_modules;

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
