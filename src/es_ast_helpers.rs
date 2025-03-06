use std::collections::HashSet;

use farmfe_core::{
  module::{ModuleId, ScriptModuleMetaData},
  swc_ecma_ast::{
    ArrowExpr, Decl, ExportSpecifier, Expr, FnExpr, ModuleDecl, ModuleExportName, ModuleItem, Pat,
  },
};
use farmfe_toolkit::swc_ecma_codegen::to_code;

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
