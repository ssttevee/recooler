use std::{collections::HashSet, iter, rc::Rc};

use farmfe_core::{module::ModuleId, serde_json, swc_common::util::take::Take, swc_ecma_ast::*};
use farmfe_toolkit::{
  swc_atoms::{Atom, AtomStore},
  swc_ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith},
};

use super::{FormActionMethod, LocalHandlerIdentGenerator};

pub(crate) fn transform_module(
  module_id: &ModuleId,
  route_pathname: String,
  global_ids: &mut crate::GlobalIds,
  atom_store: &mut AtomStore,
  swc_module: &mut farmfe_core::swc_ecma_ast::Module,
) {
  let mut visitor = ComponentTransformVisitor {
    module_id,
    route_pathname,
    atom_store,
    global_ids,
    idents: Default::default(),
    new_decls: Default::default(),
    scopes: None,
  };

  swc_module.visit_mut_with(&mut visitor);

  for decl in visitor.new_decls {
    swc_module
      .body
      .push(ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
        decl,
        span: Default::default(),
      })));
  }
}

struct ComponentTransformVisitor<'a> {
  module_id: &'a ModuleId,
  route_pathname: String,
  atom_store: &'a mut AtomStore,
  global_ids: &'a mut crate::GlobalIds,

  idents: LocalHandlerIdentGenerator,

  new_decls: Vec<Decl>,

  scopes: Option<Rc<Vec<Rc<HashSet<Atom>>>>>,
}

impl<'a> ComponentTransformVisitor<'a> {
  fn get_captured_variables(&self, expr: &Expr) -> Vec<Atom> {
    // println!("get captured variables scopes: {:?}", self.scopes);
    if let Some(scopes) = self.scopes.clone() {
      // first scope is module decls, since the function is transplanted to the
      // module level, we don't need to worry about it capture those variables
      if scopes.len() > 1 {
        let mut excluded = HashSet::<Atom>::new();
        let captured = match expr {
          Expr::Fn(FnExpr { function, .. }) => {
            grab_idents_from_pats(&mut excluded, &mut function.params.iter().map(|p| &p.pat));
            if let Some(body) = &function.body {
              grab_idents_from_decls(
                &mut excluded,
                &mut body.stmts.iter().filter_map(|stmt| stmt.as_decl()),
              );
            }
            find_variable_references(&function.body, excluded)
          }
          Expr::Arrow(ArrowExpr { params, body, .. }) => {
            grab_idents_from_pats(&mut excluded, &mut params.iter());
            if let BlockStmtOrExpr::BlockStmt(body) = body.as_ref() {
              grab_idents_from_decls(
                &mut excluded,
                &mut body.stmts.iter().filter_map(|stmt| stmt.as_decl()),
              );
            }
            find_variable_references(body, excluded)
          }
          _ => Default::default(),
        };

        // println!("found referenced variables: {:?}", captured);

        if captured.len() > 0 {
          let capturable =
            scopes
              .iter()
              .cloned()
              .skip(1)
              .fold(HashSet::<Atom>::new(), |capturable, decls| {
                capturable
                  .into_iter()
                  .chain(decls.iter().cloned())
                  .collect()
              });

          // println!("found capturable variables: {:?}", capturable);

          let mut captured = HashSet::from_iter(captured)
            .intersection(&capturable)
            .cloned()
            .collect::<Vec<_>>();
          captured.sort();
          return captured;
        }
      }
    }

    vec![]
  }

  fn add_action(&mut self, method: FormActionMethod, node: &mut KeyValueProp) {
    // println!("called add_action");
    let export_name = self.idents.next_action_ident(method);
    let form_action_id = self
      .global_ids
      .form_action_ids
      .generate_id(self.module_id, &export_name);

    if let Some(existing_method) = self.global_ids.action_method_map.get(&form_action_id) {
      std::assert!(*existing_method == method);
    } else {
      // println!(
      //   "adding into action_method_map: {} {:?} {}",
      //   &form_action_id,
      //   method,
      //   self.module_id.relative_path()
      // );

      self
        .global_ids
        .action_method_map
        .insert(form_action_id.clone(), method);
    }

    let captured_vars = self.get_captured_variables(&node.value);
    let mut replacement = Box::new(Expr::Tpl(Tpl {
      quasis: vec![
        TplElement {
          raw: format!(
            "{}?action={}&scope=",
            self.route_pathname,
            urlencoding::encode(form_action_id.as_str())
          )
          .into(),
          ..Take::dummy()
        },
        TplElement {
          raw: "".into(),
          ..Take::dummy()
        },
      ],
      exprs: vec![Box::new(Expr::Call(CallExpr {
        callee: Callee::Expr(Box::new(Expr::Ident(Ident::from("encodeURIComponent")))),
        args: vec![ExprOrSpread::from(Expr::Call(CallExpr {
          span: Default::default(),
          type_args: None,
          callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
            obj: Box::new(Expr::Ident(Ident::from("JSON"))),
            prop: MemberProp::Ident(Ident::from("stringify")),
            ..Take::dummy()
          }))),
          args: vec![ExprOrSpread::from(Box::new(Expr::Object(ObjectLit {
            span: Default::default(),
            props: captured_vars
              .iter()
              .map(|v| {
                PropOrSpread::Prop(Box::new(Prop::Shorthand(Ident::new(
                  v.clone(),
                  Default::default(),
                ))))
              })
              .collect::<Vec<_>>(),
          })))],
        }))],
        span: Default::default(),
        type_args: None,
      }))],
      span: Default::default(),
    }));

    std::mem::swap(&mut node.value, &mut replacement);

    self.new_decls.push(Decl::Var(Box::new(VarDecl {
      kind: VarDeclKind::Const,
      decls: vec![VarDeclarator {
        name: Pat::Ident(BindingIdent {
          id: Ident::new(self.atom_store.atom(export_name), Default::default()),
          type_ann: None,
        }),
        init: Some(Box::new(Expr::Arrow(ArrowExpr {
          span: Default::default(),
          params: vec![Pat::Ident(BindingIdent {
            id: Ident::new(Atom::from("_$$ctx"), Default::default()),
            type_ann: None,
          })],
          body: Box::new({
            let action_iife = Box::new(Expr::Call(CallExpr {
              span: Default::default(),
              callee: Callee::Expr(replacement),
              args: vec![
                ExprOrSpread::from(Expr::Ident(Ident::new(
                  Atom::from("_$$ctx"),
                  Default::default(),
                ))),
                // should be `Object.fromEntries(await ctx.req.formData())`
                ExprOrSpread::from(Expr::Call(CallExpr {
                  span: Default::default(),
                  callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                    span: Default::default(),
                    obj: Box::new(Expr::Ident(Ident::from("Object"))),
                    prop: MemberProp::Ident(Ident::from("fromEntries")),
                  }))),
                  args: vec![ExprOrSpread {
                    spread: None,
                    expr: Box::new(Expr::Await(AwaitExpr {
                      span: Default::default(),
                      arg: Box::new(Expr::Call(CallExpr {
                        span: Default::default(),
                        callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                          span: Default::default(),
                          obj: Box::new(Expr::Member(MemberExpr {
                            span: Default::default(),
                            obj: Box::new(Expr::Ident(Ident::from("_$$ctx"))),
                            prop: MemberProp::Ident(Ident::from("req")),
                          })),
                          prop: MemberProp::Ident(Ident::from("formData")),
                        }))),
                        args: vec![],
                        type_args: None,
                      })),
                    })),
                  }],
                  type_args: None,
                })),
              ],
              type_args: None,
            }));

            if captured_vars.is_empty() {
              BlockStmtOrExpr::Expr(action_iife)
            } else {
              BlockStmtOrExpr::BlockStmt(BlockStmt {
                span: Default::default(),
                stmts: vec![
                  Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: Default::default(),
                    kind: VarDeclKind::Const,
                    declare: false,
                    decls: vec![VarDeclarator {
                      span: Default::default(),
                      name: Pat::Object(ObjectPat {
                        span: Default::default(),
                        props: captured_vars
                          .iter()
                          .map(|v| {
                            ObjectPatProp::Assign(AssignPatProp {
                              span: Default::default(),
                              key: BindingIdent {
                                id: Ident::new(v.clone(), Default::default()),
                                type_ann: None,
                              },
                              value: None,
                            })
                          })
                          .collect(),
                        optional: false,
                        type_ann: None,
                      }),
                      // init should be `JSON.parse(ctx.req.query("scope") ?? "{}")`
                      init: Some(Box::new(Expr::Call(CallExpr {
                        span: Default::default(),
                        callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                          span: Default::default(),
                          obj: Box::new(Expr::Ident(Ident::new(
                            Atom::from("JSON"),
                            Default::default(),
                          ))),
                          prop: MemberProp::Ident(Ident::new(
                            Atom::from("parse"),
                            Default::default(),
                          )),
                        }))),
                        args: vec![ExprOrSpread::from(Expr::Bin(BinExpr {
                          span: Default::default(),
                          op: BinaryOp::NullishCoalescing,
                          left: Box::new(Expr::Call(CallExpr {
                            span: Default::default(),
                            callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                              span: Default::default(),
                              obj: Box::new(Expr::Member(MemberExpr {
                                span: Default::default(),
                                obj: Box::new(Expr::Ident(Ident::new(
                                  Atom::from("_$$ctx"),
                                  Default::default(),
                                ))),
                                prop: MemberProp::Ident(Ident::new(
                                  Atom::from("req"),
                                  Default::default(),
                                )),
                              })),
                              prop: MemberProp::Ident(Ident::new(
                                Atom::from("query"),
                                Default::default(),
                              )),
                            }))),
                            args: vec![ExprOrSpread {
                              spread: None,
                              expr: Box::new(Expr::Lit(Lit::from("scope"))),
                            }],
                            type_args: None,
                          })),
                          right: Box::new(Expr::Lit(Lit::from("{}"))),
                        }))],
                        type_args: None,
                      }))),
                      definite: false,
                    }],
                  }))),
                  Stmt::Return(ReturnStmt {
                    span: Default::default(),
                    arg: Some(action_iife),
                  }),
                ],
              })
            }
          }),
          is_async: true,
          is_generator: false,
          type_params: None,
          return_type: None,
        }))),
        ..Take::dummy()
      }],
      ..Take::dummy()
    })));
  }

  fn add_event_handler(&mut self, node: &mut KeyValueProp) {
    let export_name = self.idents.next_event_ident();

    let event_handler_id = self
      .global_ids
      .event_handler_ids
      .generate_id(self.module_id, &export_name);
    println!(
      "added event ids: {} {}",
      &event_handler_id,
      self.module_id.relative_path()
    );

    // TODO: capture scope variables

    let mut replacement = Box::<Expr>::new(Expr::Lit(Lit::from(format!(
      "$handlers[{}].call(this, event)",
      serde_json::to_string(&event_handler_id).unwrap()
    ))));

    std::mem::swap(&mut node.value, &mut replacement);

    self.new_decls.push(Decl::Var(Box::new(VarDecl {
      kind: VarDeclKind::Const,
      decls: vec![VarDeclarator {
        name: Pat::Ident(BindingIdent {
          id: Ident::new(self.atom_store.atom(export_name), Default::default()),
          type_ann: None,
        }),
        init: Some(replacement),
        ..Take::dummy()
      }],
      ..Take::dummy()
    })));
  }
}

macro_rules! visit_children_with_scope {
  ($self:ident, $current_node:expr, $new_scope:ident) => {
    let mut new_scopes = $self
      .scopes
      .as_ref()
      .map(|scopes| scopes.as_ref().clone())
      .unwrap_or_default();

    new_scopes.push(Rc::new($new_scope));

    let prev_scopes = $self.scopes.clone();
    $self.scopes = Some(Rc::new(new_scopes));

    $current_node.visit_mut_children_with($self);

    $self.scopes = prev_scopes;
  };
}

fn grab_idents_from_decls<'a>(
  idents: &mut HashSet<Atom>,
  decls: &mut impl Iterator<Item = &'a Decl>,
) {
  for decl in decls {
    match decl {
      // omit function and class decls since they cannot be serialized
      Decl::Var(var_decl) => {
        // omit declare vars since they are only type annotations
        if !var_decl.declare {
          grab_idents_from_pats(idents, &mut var_decl.decls.iter().map(|d| &d.name));
        }
      }
      Decl::Using(using_decl) => {
        grab_idents_from_pats(idents, &mut using_decl.decls.iter().map(|d| &d.name))
      }
      _ => {}
    }
  }
}

impl<'a> VisitMut for ComponentTransformVisitor<'a> {
  fn visit_mut_module(&mut self, module: &mut Module) {
    let mut idents = HashSet::<Atom>::new();
    for module_item in &module.body {
      match module_item {
        ModuleItem::ModuleDecl(ModuleDecl::Import(import)) => {
          for ident in import.specifiers.iter().map(|s| match s {
            ImportSpecifier::Named(ImportNamedSpecifier { local, .. }) => local,
            ImportSpecifier::Default(ImportDefaultSpecifier { local, .. }) => local,
            ImportSpecifier::Namespace(ImportStarAsSpecifier { local, .. }) => local,
          }) {
            idents.insert(ident.sym.clone());
          }
        }
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl { decl, .. })) => {
          grab_idents_from_decls(&mut idents, &mut iter::once(&*decl))
        }
        _ => {}
      }
    }

    visit_children_with_scope!(self, module, idents);
  }

  fn visit_mut_function(&mut self, func: &mut Function) {
    let mut idents = HashSet::<Atom>::new();
    grab_idents_from_pats(&mut idents, &mut func.params.iter().map(|p| &p.pat));
    visit_children_with_scope!(self, func.body, idents);
  }

  fn visit_mut_arrow_expr(&mut self, arrow: &mut ArrowExpr) {
    let mut idents = HashSet::<Atom>::new();
    grab_idents_from_pats(&mut idents, &mut arrow.params.iter());
    visit_children_with_scope!(self, arrow.body, idents);
  }

  fn visit_mut_block_stmt(&mut self, block: &mut BlockStmt) {
    let mut idents = HashSet::<Atom>::new();
    grab_idents_from_decls(
      &mut idents,
      &mut block.stmts.iter().filter_map(|stmt| stmt.as_decl()),
    );
    visit_children_with_scope!(self, block, idents);
  }

  fn visit_mut_key_value_prop(&mut self, node: &mut KeyValueProp) {
    if node.key.is_str() {
      let key = node.key.as_str().unwrap().value.as_str();
      if key.starts_with("hx-on:") {
        self.add_event_handler(node);
        return;
      }

      if key.starts_with("hx-") {
        macro_rules! form_action {
          ($method:ident) => {
            if &key[3..]
              == const_format::map_ascii_case!(const_format::Case::Lower, stringify!($method))
            {
              self.add_action(FormActionMethod::$method, node);
              return;
            }
          };
        }

        form_action!(Get);
        form_action!(Post);
        form_action!(Put);
        form_action!(Delete);
        form_action!(Patch);
      }
    }

    node.visit_mut_children_with(self);
  }
}

fn grab_idents_from_pats<'a>(idents: &mut HashSet<Atom>, pats: &mut impl Iterator<Item = &'a Pat>) {
  for pat in pats {
    match pat {
      Pat::Ident(ident) => {
        idents.insert(ident.sym.clone());
      }
      Pat::Array(arr) => {
        for elem in &arr.elems {
          if let Some(pat) = elem {
            grab_idents_from_pats(idents, &mut iter::once(pat));
          }
        }
      }
      Pat::Rest(rest) => {
        grab_idents_from_pats(idents, &mut iter::once(rest.arg.as_ref()));
      }
      Pat::Object(obj) => {
        for prop in &obj.props {
          match prop {
            ObjectPatProp::KeyValue(kv) => {
              grab_idents_from_pats(idents, &mut iter::once(kv.value.as_ref()));
            }
            ObjectPatProp::Assign(assign) => {
              idents.insert(assign.key.id.sym.clone());
            }
            ObjectPatProp::Rest(rest) => {
              grab_idents_from_pats(idents, &mut iter::once(rest.arg.as_ref()));
            }
          }
        }
      }
      Pat::Assign(assign) => {
        // default assignments
        grab_idents_from_pats(idents, &mut iter::once(assign.left.as_ref()));
      }
      _ => {}
    }
  }
}

/// Extracts the capture variables from a function or arrow expression.
///
/// TODO: This can be improved with proper scope analysis, but for now it's a simple
/// visitor that scans for identifiers and excludes function params.
fn find_variable_references(
  body: &impl VisitWith<CaptureVariableVisitor>,
  exclude: HashSet<Atom>,
) -> Vec<Atom> {
  let mut vars = CaptureVariableVisitor::default();
  body.visit_with(&mut vars);

  vars
    .capture
    .into_iter()
    .filter(|a| !exclude.contains(&a))
    .collect()
}

#[derive(Default)]
struct CaptureVariableVisitor {
  capture: HashSet<Atom>,
}

macro_rules! capture_expr_ident {
  ($self:ident, $boxed_expr:expr) => {
    match $boxed_expr.as_ref() {
      Expr::Ident(ident) => {
        $self.capture.insert(ident.sym.clone());
      }
      expr => expr.visit_with($self),
    }
  };
}

impl Visit for CaptureVariableVisitor {
  fn visit_private_name(&mut self, _: &PrivateName) {
    // capturing private names is probably bad?
  }

  fn visit_member_expr(&mut self, member: &MemberExpr) {
    capture_expr_ident!(self, member.obj);
    member.prop.visit_with(self);
  }

  fn visit_member_prop(&mut self, member_prop: &MemberProp) {
    match member_prop {
      MemberProp::Ident(_) => {
        // prop idents cannot be captured
      }
      _ => member_prop.visit_children_with(self),
    }
  }

  fn visit_computed_prop_name(&mut self, computed: &ComputedPropName) {
    capture_expr_ident!(self, computed.expr);
  }

  fn visit_call_expr(&mut self, call: &CallExpr) {
    // functions cannot be serialized so don't capture callees that are just identifiers,
    // however obj identifiers of method calls can be captured
    call.callee.visit_with(self);

    for arg in &call.args {
      capture_expr_ident!(self, arg.expr);
    }
  }

  fn visit_tpl(&mut self, tpl: &Tpl) {
    for expr in &tpl.exprs {
      capture_expr_ident!(self, expr);
    }
  }

  fn visit_bin_expr(&mut self, n: &BinExpr) {
    capture_expr_ident!(self, n.left);
    capture_expr_ident!(self, n.right);
  }
}
