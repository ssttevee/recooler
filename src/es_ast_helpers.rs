use std::collections::HashSet;

use farmfe_core::{
  module::{ModuleId, ScriptModuleMetaData},
  swc_ecma_ast::{
    ArrowExpr, Decl, DefaultDecl, ExportSpecifier, Expr, FnExpr, ImportSpecifier, Module,
    ModuleDecl, ModuleExportName, ModuleItem, ObjectPatProp, ParenExpr, Pat, Stmt, TsAsExpr,
    TsConstAssertion, TsNonNullExpr, TsSatisfiesExpr, TsTypeAssertion,
  },
};

use crate::head::HeadType;

pub(crate) fn find_exports(module_id: &ModuleId, script: &ScriptModuleMetaData) -> HashSet<String> {
  let (exports, has_star_export) = find_named_exports(script);
  if has_star_export {
    println!(
      "WARNING: star export not traversed while looking for exports in {}",
      module_id.to_string(),
    );
  }

  exports
}

/// Returns the exports of a module like [`find_exports`], without reporting its star exports,
/// whose names aren't included, and whether it has any.
pub(crate) fn find_named_exports(script: &ScriptModuleMetaData) -> (HashSet<String>, bool) {
  let mut exports = HashSet::<String>::new();
  let mut has_star_export = false;
  for module_item in &script.ast.body {
    if let ModuleItem::ModuleDecl(module_decl) = module_item {
      match module_decl {
        ModuleDecl::ExportDecl(export) => match &export.decl {
          Decl::Fn(f) => {
            exports.insert(f.ident.sym.to_string());
          }
          Decl::Var(var) => {
            if var.declare == false {
              for var_decl in &var.decls {
                match &var_decl.name {
                  Pat::Ident(ident) => {
                    exports.insert(ident.sym.to_string());
                  }
                  _ => {}
                }
              }
            }
          }
          _ => {}
        },
        ModuleDecl::ExportNamed(export) => {
          for spec in &export.specifiers {
            match spec {
              ExportSpecifier::Namespace(ns) => {
                exports.insert(match &ns.name {
                  ModuleExportName::Ident(ident) => ident.sym.to_string(),
                  ModuleExportName::Str(str) => str.value.to_string(),
                });
              }
              ExportSpecifier::Default(def) => {
                exports.insert(def.exported.sym.to_string());
              }
              ExportSpecifier::Named(def) => {
                if let Some(exported) = &def.exported {
                  exports.insert(match exported {
                    ModuleExportName::Ident(ident) => ident.sym.to_string(),
                    ModuleExportName::Str(str) => str.value.to_string(),
                  });
                } else {
                  exports.insert(match &def.orig {
                    ModuleExportName::Ident(ident) => ident.sym.to_string(),
                    ModuleExportName::Str(str) => str.value.to_string(),
                  });
                }
              }
            }
          }
        }
        ModuleDecl::ExportDefaultDecl(..) | ModuleDecl::ExportDefaultExpr(..) => {
          exports.insert(String::from("default"));
        }
        ModuleDecl::ExportAll(..) => {
          has_star_export = true;
        }
        _ => {}
      }
    }
  }

  (exports, has_star_export)
}

fn module_export_name(name: &ModuleExportName) -> String {
  match name {
    ModuleExportName::Ident(ident) => ident.sym.to_string(),
    ModuleExportName::Str(str) => str.value.to_string(),
  }
}

/// Returns the names exported by a module parsed straight from its typescript or javascript source,
/// leaving out the ones that may only be types, which are stripped when the module is compiled.
/// The rules are otherwise the same as [`find_exports`], so the result is a subset of what it
/// returns for the compiled module, as long as no other plugin removes exports.
pub(crate) fn find_source_value_exports(ast: &Module) -> HashSet<String> {
  // names that may refer to types only, which typescript stripping removes from exports
  let mut type_names = HashSet::<String>::new();
  for module_item in &ast.body {
    match module_item {
      ModuleItem::Stmt(stmt) => match stmt.as_decl() {
        Some(Decl::TsInterface(decl)) => {
          type_names.insert(decl.id.sym.to_string());
        }
        Some(Decl::TsTypeAlias(decl)) => {
          type_names.insert(decl.id.sym.to_string());
        }
        _ => {}
      },
      ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => match &export.decl {
        Decl::TsInterface(decl) => {
          type_names.insert(decl.id.sym.to_string());
        }
        Decl::TsTypeAlias(decl) => {
          type_names.insert(decl.id.sym.to_string());
        }
        _ => {}
      },
      ModuleItem::ModuleDecl(ModuleDecl::Import(import)) => {
        for spec in &import.specifiers {
          let (local, is_type_only) = match spec {
            ImportSpecifier::Named(named) => (&named.local, named.is_type_only),
            ImportSpecifier::Default(default) => (&default.local, false),
            ImportSpecifier::Namespace(ns) => (&ns.local, false),
          };
          if import.type_only || is_type_only {
            type_names.insert(local.sym.to_string());
          }
        }
      }
      _ => {}
    }
  }

  let mut exports = HashSet::<String>::new();
  for module_item in &ast.body {
    let ModuleItem::ModuleDecl(module_decl) = module_item else {
      continue;
    };

    match module_decl {
      ModuleDecl::ExportDecl(export) => match &export.decl {
        Decl::Fn(f) if !f.declare => {
          exports.insert(f.ident.sym.to_string());
        }
        Decl::Var(var) if !var.declare => {
          for var_decl in &var.decls {
            if let Pat::Ident(ident) = &var_decl.name {
              exports.insert(ident.sym.to_string());
            }
          }
        }
        _ => {}
      },
      ModuleDecl::ExportNamed(export) if !export.type_only => {
        for spec in &export.specifiers {
          match spec {
            ExportSpecifier::Namespace(ns) => {
              exports.insert(module_export_name(&ns.name));
            }
            ExportSpecifier::Default(def) => {
              exports.insert(def.exported.sym.to_string());
            }
            ExportSpecifier::Named(named) if !named.is_type_only => {
              // a local name may refer to a type, which is stripped
              if export.src.is_none() && type_names.contains(&module_export_name(&named.orig)) {
                continue;
              }

              exports.insert(module_export_name(
                named.exported.as_ref().unwrap_or(&named.orig),
              ));
            }
            _ => {}
          }
        }
      }
      ModuleDecl::ExportDefaultDecl(export) => match &export.decl {
        DefaultDecl::Class(..) | DefaultDecl::Fn(..) => {
          exports.insert(String::from("default"));
        }
        DefaultDecl::TsInterfaceDecl(..) => {}
      },
      ModuleDecl::ExportDefaultExpr(export) => {
        if !export
          .expr
          .as_ident()
          .is_some_and(|ident| type_names.contains(&ident.sym.to_string()))
        {
          exports.insert(String::from("default"));
        }
      }
      _ => {}
    }
  }

  exports
}

const HEAD_EXPORT_NAME: &str = "head";

/// Determines what kind of value a module exports as `head`, if any.
///
/// The kind is only known statically for function declarations and for function, arrow, or object
/// literals (optionally wrapped in parentheses or TS `as`, `satisfies`, `!` or `<T>` expressions),
/// declared either inline or as a local declaration exported with `export { .. }`. Any other value
/// (e.g. a call, an import, or a re-export) is [`HeadType::Unknown`] and is checked at runtime.
pub(crate) fn find_head_export_type(ast: &Module) -> Option<HeadType> {
  for module_item in &ast.body {
    let ModuleItem::ModuleDecl(module_decl) = module_item else {
      continue;
    };

    match module_decl {
      ModuleDecl::ExportDecl(export) => {
        if let Some(head_type) = decl_head_type(&export.decl, HEAD_EXPORT_NAME) {
          return head_type;
        }
      }
      ModuleDecl::ExportNamed(export) if !export.type_only => {
        for spec in &export.specifiers {
          let named = match spec {
            ExportSpecifier::Named(named) => named,
            // `export * as head from "./x"` exports the module namespace object
            ExportSpecifier::Namespace(ns) if module_export_name(&ns.name) == HEAD_EXPORT_NAME => {
              return Some(HeadType::Unknown);
            }
            _ => continue,
          };

          if named.is_type_only
            || module_export_name(named.exported.as_ref().unwrap_or(&named.orig))
              != HEAD_EXPORT_NAME
          {
            continue;
          }

          if export.src.is_some() {
            return Some(HeadType::Unknown);
          }

          return local_head_type(ast, &module_export_name(&named.orig));
        }
      }
      _ => {}
    }
  }

  None
}

/// Returns the head type of the local binding `name` exported with `export { name as head }`.
fn local_head_type(ast: &Module, name: &str) -> Option<HeadType> {
  for module_item in &ast.body {
    let decl = match module_item {
      ModuleItem::Stmt(Stmt::Decl(decl)) => decl,
      ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => &export.decl,
      ModuleItem::ModuleDecl(ModuleDecl::Import(import)) => {
        let spec = import.specifiers.iter().find_map(|spec| match spec {
          ImportSpecifier::Named(named) => named.local.sym.eq(name).then_some(named.is_type_only),
          ImportSpecifier::Default(default) => default.local.sym.eq(name).then_some(false),
          ImportSpecifier::Namespace(ns) => ns.local.sym.eq(name).then_some(false),
        });
        match spec {
          // type only imports are stripped along with the export
          Some(is_type_only) => {
            return (!import.type_only && !is_type_only).then_some(HeadType::Unknown)
          }
          None => continue,
        }
      }
      _ => continue,
    };

    if let Some(head_type) = decl_head_type(decl, name) {
      return head_type;
    }
  }

  // not declared in this module (or only as a type, which is stripped along with the export)
  None
}

/// Returns `Some` if `decl` declares the value `name`, with the head type of that value.
fn decl_head_type(decl: &Decl, name: &str) -> Option<Option<HeadType>> {
  match decl {
    Decl::Fn(f) if !f.declare && f.ident.sym.eq(name) => Some(Some(HeadType::Function)),
    Decl::Var(var) if !var.declare => var.decls.iter().find_map(|var_decl| match &var_decl.name {
      Pat::Ident(ident) if ident.sym.eq(name) => Some(match &var_decl.init {
        Some(init) => Some(expr_head_type(init)),
        // a `let` may be assigned later, so leave it to the runtime check
        None => Some(HeadType::Unknown),
      }),
      // destructured, e.g. `export const { head } = createPage()`
      pat if pat_binds(pat, name) => Some(Some(HeadType::Unknown)),
      _ => None,
    }),
    _ => None,
  }
}

/// Returns whether the pattern of a variable declaration declares the binding `name`.
fn pat_binds(pat: &Pat, name: &str) -> bool {
  match pat {
    Pat::Ident(ident) => ident.sym.eq(name),
    Pat::Array(array) => array
      .elems
      .iter()
      .flatten()
      .any(|elem| pat_binds(elem, name)),
    Pat::Object(object) => object.props.iter().any(|prop| match prop {
      ObjectPatProp::KeyValue(key_value) => pat_binds(&key_value.value, name),
      ObjectPatProp::Assign(assign) => assign.key.sym.eq(name),
      ObjectPatProp::Rest(rest) => pat_binds(&rest.arg, name),
    }),
    Pat::Assign(assign) => pat_binds(&assign.left, name),
    Pat::Rest(rest) => pat_binds(&rest.arg, name),
    Pat::Invalid(..) | Pat::Expr(..) => false,
  }
}

fn expr_head_type(expr: &Expr) -> HeadType {
  match expr {
    Expr::Fn(..) | Expr::Arrow(..) => HeadType::Function,
    Expr::Object(..) => HeadType::Object,
    Expr::Paren(ParenExpr { expr, .. })
    | Expr::TsAs(TsAsExpr { expr, .. })
    | Expr::TsSatisfies(TsSatisfiesExpr { expr, .. })
    | Expr::TsConstAssertion(TsConstAssertion { expr, .. })
    | Expr::TsNonNull(TsNonNullExpr { expr, .. })
    | Expr::TsTypeAssertion(TsTypeAssertion { expr, .. }) => expr_head_type(expr),
    _ => HeadType::Unknown,
  }
}

pub fn is_fn(expr: &Expr) -> bool {
  if let Expr::Fn(FnExpr { function, .. }) = expr {
    if let Some(_) = &function.body {
      // if let Some(Stmt::Expr(Expr::Lit(Lit::Str(str)))) = body.stmts.first() {
      //   return str == "use action";
      // }
      return true;
    }
  }

  if let Expr::Arrow(ArrowExpr { .. }) = expr {
    return true;
  }

  false
}

#[cfg(test)]
mod tests {
  use std::collections::HashSet;

  use farmfe_core::{config::ScriptParserConfig, module::ModuleType, swc_ecma_ast::EsVersion};
  use farmfe_toolkit::script::{parse_module, syntax_from_module_type};

  use super::{find_head_export_type, find_source_value_exports};
  use crate::head::HeadType;

  fn parse(source: &str) -> farmfe_core::swc_ecma_ast::Module {
    let syntax = syntax_from_module_type(&ModuleType::Tsx, ScriptParserConfig::default()).unwrap();
    parse_module("test.tsx", source, syntax, EsVersion::latest())
      .unwrap()
      .ast
  }

  fn exports(source: &str) -> HashSet<String> {
    find_source_value_exports(&parse(source))
  }

  fn head_type(source: &str) -> Option<HeadType> {
    find_head_export_type(&parse(source))
  }

  fn set(names: &[&str]) -> HashSet<String> {
    names.iter().map(|name| name.to_string()).collect()
  }

  #[test]
  fn finds_value_exports() {
    assert_eq!(
      exports(
        r#"
        export default function Page() { return <div />; }
        export function onGet() {}
        export const onPost = () => {}, head = {};
        const onPut = () => {};
        export { onPut, onPut as onPatch };
        export { onDelete } from "./handlers";
        export * as ns from "./ns";
        export * from "./ignored";
        export class Ignored {}
        "#
      ),
      set(&["default", "onGet", "onPost", "head", "onPut", "onPatch", "onDelete", "ns"])
    );
    assert_eq!(exports("export default {};"), set(&["default"]));
    assert_eq!(
      exports("const Page = () => <div />; export { Page as default };"),
      set(&["default"])
    );
    assert_eq!(exports("export default class {}"), set(&["default"]));
  }

  #[test]
  fn leaves_out_type_exports() {
    assert_eq!(exports("export default interface Props {}"), set(&[]));
    assert_eq!(
      exports("interface Props {} export default Props;"),
      set(&[])
    );
    assert_eq!(
      exports("type Props = {}; export { Props as default };"),
      set(&[])
    );
    assert_eq!(
      exports("import type { Page } from './page'; export { Page as default };"),
      set(&[])
    );
    assert_eq!(
      exports("import { type Page } from './page'; export default Page;"),
      set(&[])
    );
    assert_eq!(
      exports("export type { Page as default } from './page';"),
      set(&[])
    );
    assert_eq!(
      exports("export { type Page as default } from './page';"),
      set(&[])
    );
    assert_eq!(exports("export declare function onGet(): void;"), set(&[]));
    assert_eq!(
      exports("export declare const onPost: () => void;"),
      set(&[])
    );
    assert_eq!(exports("export type onGet = () => void;"), set(&[]));
  }

  #[test]
  fn finds_head_export_type() {
    use HeadType::{Function, Object, Unknown};

    let cases: &[(&str, Option<HeadType>)] = &[
      ("export function head() { return {}; }", Some(Function)),
      ("export const head = () => ({});", Some(Function)),
      (
        "export const head = function () { return {}; };",
        Some(Function),
      ),
      ("export const head = { title: 'a' };", Some(Object)),
      (
        "export const head: RouteHeadFn = (_, prev) => prev;",
        Some(Function),
      ),
      (
        "export let head: RouteHeadFn = (_, prev) => prev;",
        Some(Function),
      ),
      ("export var head: RouteHead = { title: 'a' };", Some(Object)),
      (
        "export const head = ((_, prev) => prev) satisfies RouteHeadFn;",
        Some(Function),
      ),
      (
        "export const head = ((_, prev) => prev) as RouteHeadFn;",
        Some(Function),
      ),
      ("export const head = { title: 'a' } as const;", Some(Object)),
      (
        "export const head = { title: 'a' } satisfies RouteHead;",
        Some(Object),
      ),
      ("export const head = (((_, prev) => prev));", Some(Function)),
      ("export const head = ({ title: 'a' });", Some(Object)),
      ("export const head = (() => ({}))!;", Some(Function)),
      ("export const head = makeHead();", Some(Unknown)),
      (
        "export const head = makeHead() satisfies RouteHeadFn;",
        Some(Unknown),
      ),
      ("export const head = cond ? {} : () => ({});", Some(Unknown)),
      (
        "import { other } from './x'; export const head = other;",
        Some(Unknown),
      ),
      ("export let head;", Some(Unknown)),
      ("export const head;", Some(Unknown)),
      ("export const a = 1, head = () => ({});", Some(Function)),
      // named exports of local declarations
      ("const head = () => ({}); export { head };", Some(Function)),
      (
        "export { head }; const head = { title: 'a' };",
        Some(Object),
      ),
      (
        "function head() { return {}; } export { head };",
        Some(Function),
      ),
      (
        "const fn = () => ({}); export { fn as head };",
        Some(Function),
      ),
      (
        "export const fn = () => ({}); export { fn as head };",
        Some(Function),
      ),
      ("const head = makeHead(); export { head };", Some(Unknown)),
      ("let head; export { head };", Some(Unknown)),
      (
        "import { head } from './x'; export { head };",
        Some(Unknown),
      ),
      ("import head from './x'; export { head };", Some(Unknown)),
      (
        "import * as head from './x'; export { head };",
        Some(Unknown),
      ),
      ("export { head as 'head' }; const head = {};", Some(Object)),
      // re-exports
      ("export { head } from './x';", Some(Unknown)),
      ("export { other as head } from './x';", Some(Unknown)),
      ("export * as head from './x';", Some(Unknown)),
      // destructured declarations
      ("export const { head } = createPage();", Some(Unknown)),
      ("export const { a, b: head } = createPage();", Some(Unknown)),
      (
        "export const { head = () => ({}) } = createPage();",
        Some(Unknown),
      ),
      ("export const [, head] = createPage();", Some(Unknown)),
      (
        "export const { a: [{ head }] } = createPage();",
        Some(Unknown),
      ),
      ("export const { ...head } = createPage();", Some(Unknown)),
      (
        "const { head } = createPage(); export { head };",
        Some(Unknown),
      ),
      (
        "const [x, ...head] = createPage(); export { head };",
        Some(Unknown),
      ),
      ("export const { head: other } = createPage();", None),
      // not a value export named head
      ("export const notHead = () => ({});", None),
      ("const head = () => ({}); export { head as other };", None),
      ("export declare const head: RouteHeadFn;", None),
      ("export declare function head(): RouteHead;", None),
      ("export type head = RouteHead;", None),
      ("type head = RouteHead; export { head };", None),
      ("import type { head } from './x'; export { head };", None),
      ("import { type head } from './x'; export { head };", None),
      ("export type { head } from './x';", None),
      ("export { type head } from './x';", None),
      ("const head = {}; export { type head };", None),
      ("export * from './x';", None),
      ("export * as other from './x';", None),
      ("export default {};", None),
    ];

    for (source, expected) in cases {
      assert_eq!(head_type(source), *expected, "{}", source);
    }
  }
}
