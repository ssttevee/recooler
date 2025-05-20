use std::{collections::HashSet, iter, rc::Rc};

use farmfe_core::{module::ModuleId, serde_json, swc_common::util::take::Take, swc_ecma_ast::*};
use farmfe_toolkit::{
  swc_atoms::{Atom, AtomStore},
  swc_ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith},
};
use lazy_static::lazy_static;
use regex::Regex;

use crate::es_ast_helpers::is_fn;

use super::{FormActionMethod, LocalHandlerIdentGenerator};

lazy_static! {
  static ref RE_PATH_PARAM: Regex = Regex::new(r"/:(\w+)(?:\{.+?\}|\?)?(?:/|$)").unwrap();
}

pub(crate) fn transform_module(
  module_id: &ModuleId,
  route_pathname: String,
  global_ids: &mut crate::GlobalIds,
  atom_store: &mut AtomStore,
  swc_module: &mut farmfe_core::swc_ecma_ast::Module,
) {
  let mut visitor = ComponentTransformVisitor {
    module_id,
    route_pathname: {
      // parse the pathname for parameters
      let mut pos: usize = 0;
      let mut parts: Vec<String> = vec![];
      let mut params: Vec<Atom> = vec![];
      while pos < route_pathname.len() {
        if let Some(capture) = RE_PATH_PARAM.captures_at(route_pathname.as_str(), pos) {
          let c = capture.get(1).unwrap();
          parts.push(
            route_pathname[pos
              ..c.start()
              // subtract one to account for the colon
              - 1]
              .to_string(),
          );
          params.push(atom_store.atom(c.as_str()));

          let m = capture.get(0).unwrap();
          pos = m.end();
          if m.as_str().chars().last().unwrap() == '/' {
            // subtract one so the next param can match with the slash
            pos -= 1;
          }

          continue;
        }

        break;
      }

      if pos == 0 {
        RoutePathname::Fixed(route_pathname)
      } else {
        parts.push(route_pathname[pos..].to_string());

        RoutePathname::Parameterized(ParameterizedRoutePathname {
          parts: parts,
          params: params,
          replacement_state: None,
        })
      }
    },
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

#[derive(Default)]
struct ParameterizedRoutePathnameReplacementState {
  used_idents: HashSet<Atom>,
  prop_refs: Vec<*mut Ident>,
}

impl ParameterizedRoutePathnameReplacementState {
  fn replace_placeholder_prop_references(
    &mut self,
    atom_store: &mut &mut AtomStore,
    param_pat: &mut Pat,
    body: &mut BlockStmt,
  ) {
    let sym = if let Some(ident) = param_pat.as_ident() {
      ident.id.sym.clone()
    } else {
      // replace destructured param with an ident so it can be directly referenced
      let ident_prefix = "prop";
      let mut next_ident_number: u32 = 0;
      let mut atom = (*atom_store).atom(ident_prefix);
      while self.used_idents.contains(&atom) {
        atom = (*atom_store).atom(format!("{}_{}", ident_prefix, next_ident_number));
        next_ident_number += 1;
      }

      body.stmts.insert(
        0,
        Stmt::Decl(Decl::Var(Box::new(VarDecl {
          kind: VarDeclKind::Let,
          decls: vec![VarDeclarator {
            name: param_pat.clone(),
            init: Some(Box::new(Expr::Ident(Ident::new(
              atom.clone(),
              Take::dummy(),
            )))),
            ..Take::dummy()
          }],
          ..Take::dummy()
        }))),
      );

      atom
    };

    for ptr in &self.prop_refs {
      *unsafe { &mut **ptr } = Ident::new(sym.clone(), Take::dummy());
    }

    *param_pat = Pat::Ident(BindingIdent {
      id: Ident {
        sym: sym,
        ..Take::dummy()
      },
      ..Take::dummy()
    });
  }
}

struct ParameterizedRoutePathname {
  parts: Vec<String>,
  params: Vec<Atom>,
  replacement_state: Option<*mut ParameterizedRoutePathnameReplacementState>,
}

enum RoutePathname {
  Fixed(String),
  Parameterized(ParameterizedRoutePathname),
}

struct ComponentTransformVisitor<'a> {
  module_id: &'a ModuleId,
  route_pathname: RoutePathname,
  atom_store: &'a mut AtomStore,
  global_ids: &'a mut crate::GlobalIds,

  idents: LocalHandlerIdentGenerator,

  new_decls: Vec<Decl>,

  scopes: Option<Rc<Vec<Rc<HashSet<Atom>>>>>,
}

impl<'a> ComponentTransformVisitor<'a> {
  fn get_all_variables_in_scope(&self) -> Vec<Atom> {
    if let Some(scopes) = self.scopes.clone() {
      return scopes
        .iter()
        .fold(HashSet::<Atom>::default(), |vars, scope| {
          vars.union(scope.as_ref()).cloned().collect()
        })
        .into_iter()
        .collect::<_>();
    }

    vec![]
  }

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

  fn add_action(&mut self, method: FormActionMethod, value_node: &mut Box<Expr>) {
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

    let captured_vars = self.get_captured_variables(value_node);
    let mut tpl = Tpl {
      quasis: vec![TplElement {
        raw: self.atom_store.atom(""),
        ..Take::dummy()
      }],
      exprs: vec![Box::new(Expr::Call(CallExpr {
        callee: Callee::Expr(Box::new(Expr::Ident(Ident::new(
          self.atom_store.atom("encodeURIComponent"),
          Take::dummy(),
        )))),
        args: vec![ExprOrSpread::from(Expr::Call(CallExpr {
          span: Default::default(),
          type_args: None,
          callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
            obj: Box::new(Expr::Ident(Ident::new(
              self.atom_store.atom("JSON"),
              Take::dummy(),
            ))),
            prop: MemberProp::Ident(Ident::new(self.atom_store.atom("stringify"), Take::dummy())),
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
    };

    match &self.route_pathname {
      RoutePathname::Fixed(str) => {
        tpl.quasis.insert(
          0,
          TplElement {
            raw: self.atom_store.atom(format!(
              "{}?action={}&scope=",
              str,
              urlencoding::encode(form_action_id.as_str())
            )),
            ..Take::dummy()
          },
        );
      }
      RoutePathname::Parameterized(pathname) => {
        tpl.quasis.insert(
          0,
          TplElement {
            raw: self.atom_store.atom(format!(
              "{}?action={}&scope=",
              pathname.parts[pathname.parts.len() - 1],
              urlencoding::encode(form_action_id.as_str())
            )),
            ..Take::dummy()
          },
        );

        for i in 0..pathname.parts.len() - 1 {
          tpl.quasis.insert(
            0,
            TplElement {
              raw: self
                .atom_store
                .atom(&pathname.parts[pathname.parts.len() - 2 - i]),
              ..Take::dummy()
            },
          );

          let mut placeholder_ident = Box::new(Expr::Ident(Ident::new(
            // this is a placeholder
            self.atom_store.atom("placeholder"),
            Take::dummy(),
          )));
          if let Some(state) = pathname.replacement_state.map(|p| unsafe { &mut *p }) {
            state
              .prop_refs
              .push(&mut *placeholder_ident.as_mut_ident().unwrap());

            state.used_idents.extend(self.get_all_variables_in_scope());
          }

          tpl.exprs.insert(
            0,
            Box::new(Expr::Call(CallExpr {
              span: Default::default(),
              type_args: None,
              callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                obj: Box::new(Expr::Member(MemberExpr {
                  obj: Box::new(Expr::Member(MemberExpr {
                    obj: placeholder_ident,
                    prop: MemberProp::Ident(Ident::new(self.atom_store.atom("ctx"), Take::dummy())),
                    ..Take::dummy()
                  })),
                  prop: MemberProp::Ident(Ident::new(self.atom_store.atom("req"), Take::dummy())),
                  ..Take::dummy()
                })),
                prop: MemberProp::Ident(Ident::new(self.atom_store.atom("param"), Take::dummy())),
                ..Take::dummy()
              }))),
              args: vec![ExprOrSpread::from(Box::new(Expr::Lit(Lit::Str(Str {
                value: pathname.params[pathname.params.len() - 1 - i].clone(),
                ..Take::dummy()
              }))))],
            })),
          );
        }
      }
    }

    let mut replacement = Box::new(Expr::Tpl(tpl));

    std::mem::swap(value_node, &mut replacement);

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
            id: Ident::new(self.atom_store.atom("_$$ctx"), Default::default()),
            type_ann: None,
          })],
          body: Box::new({
            let action_iife = Box::new(Expr::Call(CallExpr {
              span: Default::default(),
              callee: Callee::Expr(replacement),
              args: vec![
                ExprOrSpread::from(Expr::Ident(Ident::new(
                  self.atom_store.atom("_$$ctx"),
                  Default::default(),
                ))),
                // should be `Object.fromEntries(await ctx.req.formData())`
                ExprOrSpread::from(Expr::Call(CallExpr {
                  span: Default::default(),
                  callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                    span: Default::default(),
                    obj: Box::new(Expr::Ident(Ident::new(
                      self.atom_store.atom("Object"),
                      Take::dummy(),
                    ))),
                    prop: MemberProp::Ident(Ident::new(
                      self.atom_store.atom("fromEntries"),
                      Take::dummy(),
                    )),
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
                            obj: Box::new(Expr::Ident(Ident::new(
                              self.atom_store.atom("_$$ctx"),
                              Take::dummy(),
                            ))),
                            prop: MemberProp::Ident(Ident::new(
                              self.atom_store.atom("req"),
                              Take::dummy(),
                            )),
                          })),
                          prop: MemberProp::Ident(Ident::new(
                            self.atom_store.atom("formData"),
                            Take::dummy(),
                          )),
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
                            self.atom_store.atom("JSON"),
                            Default::default(),
                          ))),
                          prop: MemberProp::Ident(Ident::new(
                            self.atom_store.atom("parse"),
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
                                  self.atom_store.atom("_$$ctx"),
                                  Default::default(),
                                ))),
                                prop: MemberProp::Ident(Ident::new(
                                  self.atom_store.atom("req"),
                                  Default::default(),
                                )),
                              })),
                              prop: MemberProp::Ident(Ident::new(
                                self.atom_store.atom("query"),
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

  fn add_event_handler(&mut self, value_node: &mut Box<Expr>) {
    let export_name = self.idents.next_event_ident();

    let event_handler_id = self
      .global_ids
      .event_handler_ids
      .generate_id(self.module_id, &export_name);
    // println!(
    //   "added event ids: {} {}",
    //   &event_handler_id,
    //   self.module_id.relative_path()
    // );

    // TODO: capture scope variables

    let mut replacement = Box::<Expr>::new(Expr::Lit(Lit::from(format!(
      "$handlers[{}].call(this, event)",
      serde_json::to_string(&event_handler_id).unwrap()
    ))));

    std::mem::swap(value_node, &mut replacement);

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
    let mut replacement_state: ParameterizedRoutePathnameReplacementState = Default::default();
    let mut replace_placeholders = false;
    if let RoutePathname::Parameterized(pathname) = &mut self.route_pathname {
      if pathname.replacement_state.is_none() {
        replace_placeholders = true;
        pathname.replacement_state = Some(&mut replacement_state);
      }
    }

    let mut idents = HashSet::<Atom>::new();
    grab_idents_from_pats(&mut idents, &mut func.params.iter().map(|p| &p.pat));
    visit_children_with_scope!(self, func.body, idents);

    if let RoutePathname::Parameterized(pathname) = &mut self.route_pathname {
      if replace_placeholders && pathname.replacement_state.is_some() {
        pathname.replacement_state = None;
        if let Some(body) = &mut func.body {
          if replacement_state.prop_refs.len() > 0 {
            replacement_state.replace_placeholder_prop_references(
              &mut self.atom_store,
              &mut func.params[0].pat,
              body,
            );
          }
        }
      }
    }
  }

  fn visit_mut_arrow_expr(&mut self, arrow: &mut ArrowExpr) {
    let mut replacement_state: ParameterizedRoutePathnameReplacementState = Default::default();
    let mut replace_placeholders = false;
    if let RoutePathname::Parameterized(pathname) = &mut self.route_pathname {
      if pathname.replacement_state.is_none() {
        replace_placeholders = true;
        pathname.replacement_state = Some(&mut replacement_state);
      }
    }

    let mut idents = HashSet::<Atom>::new();
    grab_idents_from_pats(&mut idents, &mut arrow.params.iter());
    visit_children_with_scope!(self, arrow.body, idents);

    if let RoutePathname::Parameterized(pathname) = &mut self.route_pathname {
      if replace_placeholders && pathname.replacement_state.is_some() {
        pathname.replacement_state = None;

        if replacement_state.prop_refs.len() > 0 {
          if arrow.body.is_expr() {
            let mut replacement = BlockStmtOrExpr::BlockStmt(BlockStmt {
              stmts: vec![],
              ..Take::dummy()
            });

            std::mem::swap(&mut *arrow.body, &mut replacement);

            arrow
              .body
              .as_mut_block_stmt()
              .unwrap()
              .stmts
              .push(Stmt::Return(ReturnStmt {
                arg: Some(replacement.expect_expr()),
                span: Take::dummy(),
              }));
          }

          replacement_state.replace_placeholder_prop_references(
            &mut self.atom_store,
            &mut arrow.params[0],
            arrow.body.as_mut_block_stmt().unwrap(),
          );
        }
      }
    }
  }

  fn visit_mut_block_stmt(&mut self, block: &mut BlockStmt) {
    let mut idents = HashSet::<Atom>::new();
    grab_idents_from_decls(
      &mut idents,
      &mut block.stmts.iter().filter_map(|stmt| stmt.as_decl()),
    );
    visit_children_with_scope!(self, block, idents);
  }

  fn visit_mut_jsx_attr(&mut self, node: &mut JSXAttr) {
    if let JSXAttrName::Ident(name) = &node.name {
      if let Some(JSXAttrValue::JSXExprContainer(JSXExprContainer {
        expr: JSXExpr::Expr(expr),
        ..
      })) = &mut node.value
      {
        let key = name.sym.as_str();
        if key.starts_with("hx-on:") || key.starts_with("hx-on-") {
          self.add_event_handler(expr);
          return;
        }

        if key.starts_with("hx-") && is_fn(expr) {
          macro_rules! form_action {
            ($method:ident) => {
              if &key[3..]
                == const_format::map_ascii_case!(const_format::Case::Lower, stringify!($method))
              {
                self.add_action(FormActionMethod::$method, expr);
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
    }

    node.visit_mut_children_with(self);
  }

  fn visit_mut_key_value_prop(&mut self, node: &mut KeyValueProp) {
    if node.key.is_str() {
      let key = node.key.as_str().unwrap().value.as_str();
      if key.starts_with("hx-on:") || key.starts_with("hx-on-") {
        self.add_event_handler(&mut node.value);
        return;
      }

      if key.starts_with("hx-") && is_fn(&*node.value) {
        macro_rules! form_action {
          ($method:ident) => {
            if &key[3..]
              == const_format::map_ascii_case!(const_format::Case::Lower, stringify!($method))
            {
              self.add_action(FormActionMethod::$method, &mut node.value);
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
