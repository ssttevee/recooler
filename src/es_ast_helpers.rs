use std::collections::HashSet;

use farmfe_core::{
  module::{ModuleId, ScriptModuleMetaData},
  swc_ecma_ast::{
    ArrowExpr, Decl, DefaultDecl, ExportSpecifier, Expr, FnExpr, ImportSpecifier, Module,
    ModuleDecl, ModuleExportName, ModuleItem, Pat,
  },
};

use crate::head::HeadType;

pub(crate) fn find_exports(module_id: &ModuleId, script: &ScriptModuleMetaData) -> HashSet<String> {
  let mut exports = HashSet::<String>::new();
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
          println!(
            "WARNING: star export not traversed while looking for exports in {}",
            module_id.to_string(),
          );
        }
        _ => {}
      }
    }
  }

  exports
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

pub(crate) fn find_head_export_type(script: &ScriptModuleMetaData) -> Option<HeadType> {
  const HEAD_FN_NAME: &str = "head";

  for module_item in &script.ast.body {
    if let ModuleItem::ModuleDecl(module_decl) = module_item {
      match module_decl {
        ModuleDecl::ExportDecl(export) => match &export.decl {
          Decl::Fn(f) => {
            if f.ident.sym.eq(HEAD_FN_NAME) {
              return Some(HeadType::Function);
            }
          }
          Decl::Var(var) => {
            if var.declare == false {
              for var_decl in &var.decls {
                match &var_decl.name {
                  Pat::Ident(ident) => {
                    if ident.sym.eq(HEAD_FN_NAME) {
                      if let Some(expr) = &var_decl.init {
                        if expr.is_fn_expr() || expr.is_arrow() {
                          return Some(HeadType::Function);
                        } else {
                          return Some(HeadType::Object);
                        }
                      }
                    }
                  }
                  _ => {}
                }
              }
            }
          }
          _ => {}
        },
        // ModuleDecl::ExportNamed(export) => {}
        // ModuleDecl::ExportDefaultDecl(export) => {}
        // ModuleDecl::ExportDefaultExpr(export) => {}
        _ => {}
      }
    }
  }

  // TODO: support other head export types

  None
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

  use super::find_source_value_exports;

  fn exports(source: &str) -> HashSet<String> {
    let syntax = syntax_from_module_type(&ModuleType::Tsx, ScriptParserConfig::default()).unwrap();
    let parsed = parse_module("test.tsx", source, syntax, EsVersion::latest()).unwrap();
    find_source_value_exports(&parsed.ast)
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
}
