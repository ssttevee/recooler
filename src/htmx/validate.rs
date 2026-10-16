use farmfe_core::{module::ModuleId, swc_ecma_ast::*};

use super::{parse_handler_ident, HandlerIdent};

/// Registers the form actions and event handlers of a module that was transformed by
/// [`super::transform::transform_module`] in an earlier build, i.e. one that was loaded from the
/// persistent cache so that `process_module` was skipped, exactly like the transform did.
///
/// The ids are derived from the names of the exports that the transform appends, which encode the
/// method and the value of the module local counter that the transform numbered the ids with. The
/// names are module level bindings, so unlike the ids embedded in the transformed handlers they
/// survive the other plugins (e.g. the jsx transform) unchanged. Registering a module that already
/// is registered does nothing.
pub(crate) fn ensure_registered_handlers(
  module_id: &ModuleId,
  global_ids: &mut crate::GlobalIds,
  swc_module: &Module,
) -> Result<(), String> {
  let mut actions = vec![];
  let mut events = vec![];
  for item in &swc_module.body {
    let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
      decl: Decl::Var(var_decl),
      ..
    })) = item
    else {
      continue;
    };

    for decl in &var_decl.decls {
      let Pat::Ident(ident) = &decl.name else {
        continue;
      };

      let name = ident.id.sym.to_string();
      match parse_handler_ident(&name) {
        Some(HandlerIdent::Action(method, index)) => actions.push((index, method, name)),
        Some(HandlerIdent::Event(index)) => events.push((index, name)),
        None => {}
      }
    }
  }

  // the ids must be generated in the order of the counter, which is not the order of the exports
  // because nested actions are exported before the action containing them
  actions.sort();
  events.sort();

  // without gaps, the id of each export is numbered with its counter value like in the transform
  if !actions
    .iter()
    .enumerate()
    .all(|(i, (index, ..))| i == *index)
    || !events.iter().enumerate().all(|(i, (index, _))| i == *index)
  {
    return Err(format!(
      "unexpected form action or event handler exports in module {}: {}",
      module_id.relative_path(),
      actions
        .iter()
        .map(|(.., name)| name)
        .chain(events.iter().map(|(_, name)| name))
        .cloned()
        .collect::<Vec<_>>()
        .join(", ")
    ));
  }

  for (_, method, export_name) in actions {
    let form_action_id = global_ids
      .form_action_ids
      .generate_id(module_id, &export_name);

    if let Some(existing_method) = global_ids.action_method_map.get(&form_action_id) {
      if *existing_method != method {
        return Err(format!(
          "form action {} is registered as {:?}, but exported as {:?}",
          form_action_id, existing_method, method
        ));
      }
    } else {
      global_ids.action_method_map.insert(form_action_id, method);
    }
  }

  for (_, export_name) in events {
    global_ids
      .event_handler_ids
      .generate_id(module_id, &export_name);
  }

  Ok(())
}

#[cfg(test)]
mod tests {
  use std::sync::Arc;

  use farmfe_core::{
    module::ModuleId,
    swc_common::{comments::SingleThreadedComments, Globals, Mark, SourceMap, GLOBALS},
    swc_ecma_ast::{EsVersion, Module},
    swc_ecma_parser::{Syntax, TsSyntax},
  };
  use farmfe_toolkit::{
    script::parse_module,
    swc_ecma_transforms::{react, resolver},
    swc_ecma_visit::VisitMutWith,
  };

  use crate::{
    htmx::{transform::transform_module, FormActionMethod},
    ids::GlobalIds,
    AtomStore,
  };

  use super::ensure_registered_handlers;

  #[derive(Debug, PartialEq)]
  struct Registry {
    form_action_ids: Vec<(String, String, String)>,
    action_method_map: Vec<(String, FormActionMethod)>,
    event_handler_ids: Vec<(String, String, String)>,
  }

  fn registry(ids: &GlobalIds) -> Registry {
    let mut action_method_map = ids
      .action_method_map
      .iter()
      .map(|(id, method)| (id.clone(), *method))
      .collect::<Vec<_>>();
    action_method_map.sort();

    Registry {
      form_action_ids: ids.form_action_ids.entries(),
      action_method_map,
      event_handler_ids: ids.event_handler_ids.entries(),
    }
  }

  #[derive(Clone, Copy)]
  enum Jsx {
    /// the source is not jsx, or the jsx is left as is
    None,
    /// the jsx transform runs after the recooler transform, like in farm
    After,
    /// the jsx transform runs before the recooler transform
    Before,
  }

  /// Parses the `(path, route pathname, source)` modules like farm and transforms them into one
  /// registry, then replays the transformed modules into a fresh registry and into the same one.
  /// Returns the registry of the transform after asserting that the replays match it.
  fn assert_replay_matches(modules: &[(&str, &str, &str)], jsx: Jsx) -> Registry {
    GLOBALS.set(&Globals::new(), || {
      let mut global_ids = GlobalIds::new();
      let mut transformed = Vec::<(ModuleId, Module)>::new();
      for (path, route, source) in modules {
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
        let mut module = parsed.ast;
        let mut comments = parsed.comments.into();

        let unresolved_mark = Mark::new();
        let top_level_mark = Mark::new();
        module.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark, true));

        let jsx_transform = |module: &mut Module| {
          module.visit_mut_with(&mut react::react::<SingleThreadedComments>(
            Arc::new(SourceMap::default()),
            None,
            react::Options {
              runtime: Some(react::Runtime::Automatic),
              import_source: Some("easy-jsx-html-engine".to_string()),
              throw_if_namespace: Some(false),
              development: Some(false),
              ..Default::default()
            },
            top_level_mark,
            unresolved_mark,
          ));
        };

        if let Jsx::Before = jsx {
          jsx_transform(&mut module);
        }

        let module_id = ModuleId::new(path, "", ".");
        transform_module(
          &module_id,
          route.to_string(),
          &mut global_ids,
          &mut AtomStore::default(),
          &mut module,
          &mut comments,
        )
        .unwrap();

        if let Jsx::After = jsx {
          jsx_transform(&mut module);
        }

        transformed.push((module_id, module));
      }

      let expected = registry(&global_ids);

      let mut replayed_ids = GlobalIds::new();
      for (module_id, module) in &transformed {
        ensure_registered_handlers(module_id, &mut replayed_ids, module).unwrap();
      }
      assert_eq!(registry(&replayed_ids), expected);

      // modules that were transformed in this build are replayed too
      for (module_id, module) in &transformed {
        ensure_registered_handlers(module_id, &mut global_ids, module).unwrap();
      }
      assert_eq!(registry(&global_ids), expected);

      expected
    })
  }

  fn assert_replay_matches_with_jsx(modules: &[(&str, &str, &str)]) -> Registry {
    let expected = assert_replay_matches(modules, Jsx::None);
    assert_eq!(assert_replay_matches(modules, Jsx::After), expected);
    assert_eq!(assert_replay_matches(modules, Jsx::Before), expected);
    expected
  }

  fn ids(entries: &[(String, String, String)]) -> Vec<(&str, &str)> {
    entries
      .iter()
      .map(|(_, export_name, id)| (export_name.as_str(), id.as_str()))
      .collect()
  }

  #[test]
  fn replays_event_handlers_nested_in_children() {
    let registry = assert_replay_matches_with_jsx(&[(
      "src/components/List.tsx",
      "",
      r#"
        import { helper } from "./helper";

        export default function List(props) {
          const count = props.count;
          return (
            <div>
              <section>
                <ul>
                  {props.items.map((item) => (
                    <li>
                      <button hx-on:click={() => helper(item)}>{item}</button>
                      <span hx-on-mouseover={function () { console.log(count); }} />
                    </li>
                  ))}
                </ul>
                <a hx-on:click="plain()" />
                <i hx-on--after-request={() => helper()} />
              </section>
            </div>
          );
        }
      "#,
    )]);

    assert_eq!(
      ids(&registry.event_handler_ids),
      vec![
        ("_handler$0", "src/components/List.tsx@0"),
        ("_handler$1", "src/components/List.tsx@1"),
        ("_handler$2", "src/components/List.tsx@2"),
      ]
    );
    assert!(registry.form_action_ids.is_empty());
  }

  #[test]
  fn replays_form_actions_of_every_method() {
    let registry = assert_replay_matches_with_jsx(&[(
      "src/routes/article/[id]/index.tsx",
      "/article/:id",
      r#"
        export default function Page(props) {
          const requestContext = props.ctx;
          const title = props.title;
          return (
            <main>
              <form hx-get={(c) => c.text("get")}>
                <button hx-post={async (c) => <p hx-delete={() => title} hx-on:click={() => alert(title)} />} />
              </form>
              <div>
                <div>
                  <button hx-put={() => title} ctx={requestContext} />
                  <button hx-patch={function (c) { return c.json({ title }); }} />
                  <button hx-delete={() => null} />
                </div>
              </div>
            </main>
          );
        }
      "#,
    )]);

    let module = "src/routes/article/[id]/index.tsx";
    assert_eq!(
      registry.action_method_map,
      vec![
        (format!("{}@0", module), FormActionMethod::Get),
        (format!("{}@1", module), FormActionMethod::Post),
        (format!("{}@2", module), FormActionMethod::Delete),
        (format!("{}@3", module), FormActionMethod::Put),
        (format!("{}@4", module), FormActionMethod::Patch),
        (format!("{}@5", module), FormActionMethod::Delete),
      ]
    );
    assert_eq!(registry.event_handler_ids.len(), 1);
  }

  #[test]
  fn replays_multiple_components_and_modules() {
    let registry = assert_replay_matches_with_jsx(&[
      (
        "src/components/Dialog.tsx",
        "",
        r#"
          export function Open({ id }) {
            return <button hx-on:click={() => open(id)} hx-post={() => id}>open</button>;
          }

          export const Close = ({ id }) => (
            <div><button hx-on-click={() => close(id)} hx-delete={() => id} /></div>
          );

          export default function Dialog(props) {
            return (
              <dialog>
                <Open id={props.id} />
                <Close id={props.id} />
                <form hx-patch={() => props.id}><input hx-on:change={(e) => validate(e)} /></form>
              </dialog>
            );
          }
        "#,
      ),
      (
        "src/routes/index.tsx",
        "/",
        r#"
          import Dialog from "../components/Dialog";

          export default function Page() {
            return <div hx-on:load={() => init()}><Dialog id="a" /><form hx-post={() => "ok"} /></div>;
          }
        "#,
      ),
    ]);

    assert_eq!(registry.form_action_ids.len(), 4);
    assert_eq!(registry.event_handler_ids.len(), 4);
    assert!(registry.form_action_ids.contains(&(
      "src/routes/index.tsx".into(),
      "_action$post$0".into(),
      "src/routes/index.tsx@0".into()
    )));
  }

  #[test]
  fn replays_compiled_object_props() {
    assert_replay_matches(
      &[(
        "src/routes/x/index.tsx",
        "/x",
        r#"
          export default function C({ n }) {
            return h("button", { "hx-post": () => n, "hx-on:click": () => alert(n) });
          }
        "#,
      )],
      Jsx::None,
    );
  }

  #[test]
  fn replays_ids_in_counter_order() {
    // more than ten ids, so that their radix 36 numbering and the names sort differently, with
    // nested actions which are exported before the actions containing them
    let buttons = (0..12)
      .map(|i| {
        format!(
          "<button hx-on:click={{() => f({i})}} hx-put={{() => <i hx-get={{() => {i}}} />}} />"
        )
      })
      .collect::<String>();
    let source = format!(
      "export default function C() {{ return <div>{}</div>; }}",
      buttons
    );
    let registry = assert_replay_matches_with_jsx(&[("src/components/C.tsx", "", &source)]);

    let mut event_ids = ids(&registry.event_handler_ids);
    event_ids.sort_by_key(|(export_name, _)| export_name[9..].parse::<usize>().unwrap());
    assert_eq!(event_ids[10], ("_handler$10", "src/components/C.tsx@a"));
    assert_eq!(event_ids[11], ("_handler$11", "src/components/C.tsx@b"));
    assert!(registry
      .action_method_map
      .contains(&("src/components/C.tsx@n".to_string(), FormActionMethod::Get)));
  }

  #[test]
  fn rejects_exports_with_gaps() {
    GLOBALS.set(&Globals::new(), || {
      let module = parse_module(
        "src/x.ts",
        "export const _action$post$1 = 1; export const _handler$01 = 1; export const _action$foo$0 = 1;",
        Syntax::Typescript(Default::default()),
        EsVersion::EsNext,
      )
      .unwrap()
      .ast;

      let err = ensure_registered_handlers(
        &ModuleId::new("src/x.ts", "", "."),
        &mut GlobalIds::new(),
        &module,
      )
      .unwrap_err();
      assert!(err.contains("_action$post$1"));
      // names that the transform doesn't generate are ignored
      assert!(!err.contains("_handler$01"));
      assert!(!err.contains("_action$foo$0"));
    });
  }
}
