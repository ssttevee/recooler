use std::collections::{HashMap, HashSet, VecDeque};

use farmfe_core::{
  module::{CommentsMetaData, CommentsMetaDataItem, ModuleId},
  serde_json,
  swc_common::{comments::Comment, util::take::Take, BytePos, Span},
  swc_ecma_ast::*,
};
use farmfe_toolkit::{
  swc_atoms::{Atom, AtomStore},
  swc_ecma_visit::{noop_visit_type, Visit, VisitMut, VisitMutWith, VisitWith},
};
use lazy_static::lazy_static;
use regex::Regex;

use crate::es_ast_helpers::is_fn;

use super::{FormActionMethod, LocalHandlerIdentGenerator};

lazy_static! {
  static ref RE_PATH_PARAM: Regex = Regex::new(r"/:(\w+)(?:\{.+?\}|\?)?(?:/|$)").unwrap();
}

const HELPERS_MODULE: &str = "farm-plugin-recooler/helpers";
const PROPS_IDENT: &str = "__recooler_props";
const ACTION_CTX_IDENT: &str = "__recooler_ctx";

/// Hoists form actions and event handlers out of components into module level exports.
///
/// The module must have been processed by swc's `resolver` (farm does this while parsing) because
/// captured variables are found by comparing the syntax contexts of identifiers.
pub(crate) fn transform_module(
  module_id: &ModuleId,
  route_pathname: String,
  global_ids: &mut crate::GlobalIds,
  atom_store: &mut AtomStore,
  swc_module: &mut farmfe_core::swc_ecma_ast::Module,
  comments: &mut CommentsMetaData,
) -> Result<(), String> {
  let mut bindings = BindingCollector::default();
  swc_module.visit_with(&mut bindings);

  let mut visitor = ComponentTransformVisitor {
    module_id,
    route_pathname: parse_route_pathname(route_pathname, atom_store),
    atom_store,
    global_ids,
    idents: Default::default(),
    new_decls: Default::default(),
    bindings: bindings.bindings,
    top_level: top_level_bindings(swc_module),
    component: None,
    action_frames: vec![],
    explicit_action_ctx: None,
    errors: vec![],
    comments,
    synthesized_pos_counter: u32::MAX - 1000000,
  };

  swc_module.visit_mut_with(&mut visitor);

  if !visitor.errors.is_empty() {
    return Err(visitor.errors.join("\n"));
  }

  swc_module
    .body
    .push(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
      src: Box::new(Str {
        span: Default::default(),
        value: visitor.atom_store.atom(HELPERS_MODULE),
        raw: None,
      }),
      specifiers: [
        "wrapAction",
        "wrapActionWithPayload",
        "actionScope",
        "handlerScope",
      ]
      .iter()
      .map(|name| {
        ImportSpecifier::Named(ImportNamedSpecifier {
          span: Default::default(),
          local: Ident::new(visitor.atom_store.atom(*name), Default::default()),
          imported: None,
          is_type_only: false,
        })
      })
      .collect(),
      with: None,
      type_only: false,
      phase: ImportPhase::Evaluation,
      span: Default::default(),
    })));

  for decl in visitor.new_decls {
    swc_module
      .body
      .push(ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
        decl,
        span: Default::default(),
      })));
  }

  Ok(())
}

fn parse_route_pathname(route_pathname: String, atom_store: &mut AtomStore) -> RoutePathname {
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
    RoutePathname::Parameterized { parts, params }
  }
}

enum RoutePathname {
  Fixed(String),
  Parameterized {
    parts: Vec<String>,
    params: Vec<Atom>,
  },
}

/// The outermost function of a parameterized route module, whose first param is assumed to be the
/// component props containing `ctx`.
struct ComponentFrame {
  props_ident: Option<Ident>,
  /// Whether the first param is destructured and must be replaced by `props_ident`.
  rewrite_param: bool,
  used: bool,
}

/// A form action whose body is being visited, so that nested actions can use its `ctx` param.
struct ActionFrame {
  ctx_ident: Option<Ident>,
  /// Whether `ctx_ident` must be added as the first param of the action if used.
  add_param: bool,
  used: bool,
}

struct ComponentTransformVisitor<'a> {
  module_id: &'a ModuleId,
  route_pathname: RoutePathname,
  atom_store: &'a mut AtomStore,
  global_ids: &'a mut crate::GlobalIds,

  idents: LocalHandlerIdentGenerator,

  new_decls: Vec<Decl>,

  /// Every binding in the module, keyed by the resolved identifier.
  bindings: HashMap<Id, BindingKind>,
  /// Bindings in the module scope, which do not need to be captured since hoisted functions stay
  /// in the same module.
  top_level: HashSet<Id>,

  component: Option<ComponentFrame>,
  action_frames: Vec<ActionFrame>,
  explicit_action_ctx: Option<Box<Expr>>,

  errors: Vec<String>,

  comments: &'a mut CommentsMetaData,

  synthesized_pos_counter: u32,
}

impl<'a> ComponentTransformVisitor<'a> {
  fn generate_unique_span(&mut self) -> Span {
    let pos = BytePos(self.synthesized_pos_counter);
    self.synthesized_pos_counter += 1;
    Span {
      lo: pos,
      hi: pos,
      ctxt: Default::default(),
    }
  }

  fn ident(&mut self, name: &str) -> Ident {
    Ident::new(self.atom_store.atom(name), Default::default())
  }

  fn str_lit(&mut self, value: &str) -> Box<Expr> {
    Box::new(Expr::Lit(Lit::Str(Str {
      span: Default::default(),
      value: self.atom_store.atom(value),
      raw: None,
    })))
  }

  fn call(&mut self, callee: &str, args: Vec<Box<Expr>>) -> Box<Expr> {
    Box::new(Expr::Call(CallExpr {
      span: Default::default(),
      callee: Callee::Expr(Box::new(Expr::Ident(self.ident(callee)))),
      args: args.into_iter().map(ExprOrSpread::from).collect(),
      type_args: None,
    }))
  }

  fn member(&mut self, obj: Box<Expr>, prop: &str) -> Box<Expr> {
    Box::new(Expr::Member(MemberExpr {
      span: Default::default(),
      obj,
      prop: MemberProp::Ident(self.ident(prop)),
    }))
  }

  fn tpl(&mut self, quasis: Vec<String>, exprs: Vec<Box<Expr>>) -> Tpl {
    debug_assert_eq!(quasis.len(), exprs.len() + 1);
    let len = quasis.len();
    Tpl {
      span: Default::default(),
      quasis: quasis
        .into_iter()
        .enumerate()
        .map(|(i, cooked)| TplElement {
          span: Default::default(),
          tail: i == len - 1,
          raw: self.atom_store.atom(escape_tpl_raw(&cooked)),
          cooked: Some(self.atom_store.atom(cooked)),
        })
        .collect(),
      exprs,
    }
  }

  fn error(&mut self, message: String) {
    self
      .errors
      .push(format!("{}: {}", self.module_id.relative_path(), message));
  }

  /// Returns the variables from enclosing function scopes that are referenced by `func`.
  ///
  /// Module level bindings and globals are not captured because the function is hoisted to the
  /// module level of the same module. Captured local functions and classes are reported as errors
  /// since they cannot be serialized.
  fn captured_idents(&mut self, func: &Expr, description: &str) -> Vec<Ident> {
    let mut inner = BindingCollector::default();
    func.visit_with(&mut inner);

    let mut refs = IdentRefCollector::default();
    func.visit_with(&mut refs);

    let mut seen = HashSet::<Id>::new();
    let mut captured = vec![];
    for ident in refs.idents {
      let id = ident.to_id();
      if inner.bindings.contains_key(&id)
        || self.top_level.contains(&id)
        || !seen.insert(id.clone())
      {
        continue;
      }

      match self.bindings.get(&id).copied() {
        None => {
          // unresolved (global) or not a reference, such as a property name
        }
        Some(BindingKind::Value) => captured.push(Ident::new(ident.sym, ident.span)),
        Some(kind) => self.error(format!(
          "{} references the local {} `{}`, which cannot be serialized; move it to the module scope",
          description,
          match kind {
            BindingKind::Function => "function",
            _ => "class",
          },
          ident.sym,
        )),
      }
    }

    captured.sort_by(|a, b| a.sym.cmp(&b.sym));
    captured
  }

  fn scope_object(captured: &[Ident]) -> Box<Expr> {
    Box::new(Expr::Object(ObjectLit {
      span: Default::default(),
      props: captured
        .iter()
        .map(|ident| PropOrSpread::Prop(Box::new(Prop::Shorthand(ident.clone()))))
        .collect(),
    }))
  }

  /// Wraps `func` in `({ ...captured }) => func` so that the captured variables are bound by the
  /// factory params instead of rewriting the function body.
  fn scope_factory(captured: &[Ident], func: Box<Expr>) -> Box<Expr> {
    Box::new(Expr::Arrow(ArrowExpr {
      span: Default::default(),
      params: vec![Pat::Object(ObjectPat {
        span: Default::default(),
        props: captured
          .iter()
          .map(|ident| {
            ObjectPatProp::Assign(AssignPatProp {
              span: Default::default(),
              key: BindingIdent {
                id: ident.clone(),
                type_ann: None,
              },
              value: None,
            })
          })
          .collect(),
        optional: false,
        type_ann: None,
      })],
      body: Box::new(BlockStmtOrExpr::Expr(func)),
      is_async: false,
      is_generator: false,
      type_params: None,
      return_type: None,
    }))
  }

  /// Returns the expression for the request context used to build parameterized action urls.
  fn action_ctx_expr(&mut self) -> Option<Box<Expr>> {
    if let Some(explicit_action_ctx) = &self.explicit_action_ctx {
      return Some(explicit_action_ctx.clone());
    }

    if let Some(frame) = self.action_frames.last_mut() {
      frame.used = true;
      return frame
        .ctx_ident
        .clone()
        .map(|ident| Box::new(Expr::Ident(ident)));
    }

    let props_ident = self.component.as_mut().and_then(|component| {
      component.used = true;
      component.props_ident.clone()
    });

    props_ident.map(|ident| self.member(Box::new(Expr::Ident(ident)), "ctx"))
  }

  fn action_url(&mut self, form_action_id: &str, captured: &[Ident]) -> Tpl {
    let query = format!("?action={}", urlencoding::encode(form_action_id));

    let (mut quasis, mut exprs) = match &self.route_pathname {
      RoutePathname::Fixed(pathname) => (vec![format!("{}{}", pathname, query)], vec![]),
      RoutePathname::Parameterized { parts, params } => {
        let (parts, params) = (parts.clone(), params.clone());
        let mut quasis = vec![];
        let mut exprs = vec![];
        for (part, param) in parts.iter().zip(params.iter()) {
          quasis.push(part.clone());

          let Some(ctx_expr) = self.action_ctx_expr() else {
            self.error(format!(
              "form action {} is in a parameterized route but has no request context; give the component a props param that receives `ctx`, or pass `ctx={{...}}` to the element",
              form_action_id,
            ));
            return self.tpl(vec![String::new()], vec![]);
          };

          let req = self.member(ctx_expr, "req");
          let param_fn = self.member(req, "param");
          let param_lit = self.str_lit(param);
          exprs.push(Box::new(Expr::Call(CallExpr {
            span: Default::default(),
            callee: Callee::Expr(param_fn),
            args: vec![ExprOrSpread::from(param_lit)],
            type_args: None,
          })));
        }
        quasis.push(format!("{}{}", parts[parts.len() - 1], query));
        (quasis, exprs)
      }
    };

    if !captured.is_empty() {
      let id_lit = self.str_lit(form_action_id);
      exprs.push(self.call("actionScope", vec![id_lit, Self::scope_object(captured)]));
      quasis.push(String::new());
    }

    self.tpl(quasis, exprs)
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
      self
        .global_ids
        .action_method_map
        .insert(form_action_id.clone(), method);
    }

    // transform nested actions and event handlers before capturing the action's variables, so
    // that the variables captured by the nested ones are also captured by this one
    self
      .action_frames
      .push(new_action_frame(value_node, self.atom_store));
    value_node.visit_mut_children_with(self);
    let frame = self.action_frames.pop().unwrap();
    if frame.used && frame.add_param {
      add_first_param(value_node, frame.ctx_ident.unwrap());
    }

    let captured = self.captured_idents(value_node, &format!("form action {}", form_action_id));

    let mut replacement = Box::new(Expr::Tpl(self.action_url(&form_action_id, &captured)));
    std::mem::swap(value_node, &mut replacement);

    let args = if captured.is_empty() {
      vec![replacement]
    } else {
      vec![
        Self::scope_factory(&captured, replacement),
        self.str_lit(&form_action_id),
      ]
    };

    let span = self.generate_unique_span();
    let mut call_expr = self.call(
      if method == FormActionMethod::Get {
        "wrapAction"
      } else {
        "wrapActionWithPayload"
      },
      args,
    );
    if let Expr::Call(call) = call_expr.as_mut() {
      call.span = span;
    }

    self.comments.leading.push(CommentsMetaDataItem {
      byte_pos: span.lo,
      comment: vec![Comment {
        kind: farmfe_core::swc_common::comments::CommentKind::Block,
        span,
        text: "#__PURE__".into(),
      }],
    });

    self.push_export(export_name, call_expr);
  }

  fn add_event_handler(&mut self, value_node: &mut Box<Expr>) {
    let export_name = self.idents.next_event_ident();
    let event_handler_id = self
      .global_ids
      .event_handler_ids
      .generate_id(self.module_id, &export_name);

    let captured = self.captured_idents(value_node, &format!("event handler {}", event_handler_id));

    let handler_ref = format!(
      "$handlers[{}]",
      serde_json::to_string(&event_handler_id).unwrap()
    );

    let mut replacement = if captured.is_empty() {
      Box::new(Expr::Lit(Lit::from(format!(
        "{}.call(this, event)",
        handler_ref
      ))))
    } else {
      let scope = self.call("handlerScope", vec![Self::scope_object(&captured)]);
      Box::new(Expr::Tpl(self.tpl(
        vec![
          format!("{}(", handler_ref),
          String::from(").call(this, event)"),
        ],
        vec![scope],
      )))
    };

    std::mem::swap(value_node, &mut replacement);

    if !captured.is_empty() {
      replacement = Self::scope_factory(&captured, replacement);
    }

    self.push_export(export_name, replacement);
  }

  fn push_export(&mut self, export_name: String, init: Box<Expr>) {
    let id = self.ident(&export_name);
    self.new_decls.push(Decl::Var(Box::new(VarDecl {
      kind: VarDeclKind::Const,
      decls: vec![VarDeclarator {
        name: Pat::Ident(BindingIdent { id, type_ann: None }),
        init: Some(init),
        ..Take::dummy()
      }],
      ..Take::dummy()
    })));
  }

  /// Starts a component frame if `params` belong to the outermost function of a parameterized
  /// route module.
  fn enter_function<'p>(&mut self, mut params: impl Iterator<Item = &'p Pat>) -> bool {
    if self.component.is_some()
      || !self.action_frames.is_empty()
      || !matches!(self.route_pathname, RoutePathname::Parameterized { .. })
    {
      return false;
    }

    let (props_ident, rewrite_param) = match params.next() {
      None => (None, false),
      Some(Pat::Ident(binding)) => (Some(binding.id.clone()), false),
      Some(Pat::Assign(AssignPat { left, .. })) if left.is_ident() => {
        (Some(left.as_ident().unwrap().id.clone()), false)
      }
      Some(_) => (Some(self.ident(PROPS_IDENT)), true),
    };

    self.component = Some(ComponentFrame {
      props_ident,
      rewrite_param,
      used: false,
    });

    true
  }

  /// Ends the component frame and returns the props ident if the first param must be replaced.
  fn exit_function(&mut self) -> Option<Ident> {
    let component = self.component.take().unwrap();
    if component.used && component.rewrite_param {
      component.props_ident
    } else {
      None
    }
  }
}

fn new_action_frame(func: &Expr, atom_store: &mut AtomStore) -> ActionFrame {
  let first_param = match func {
    Expr::Arrow(arrow) => arrow.params.first(),
    Expr::Fn(FnExpr { function, .. }) => function.params.first().map(|param| &param.pat),
    _ => None,
  };

  let (ctx_ident, add_param) = match first_param {
    None => (
      Some(Ident::new(
        atom_store.atom(ACTION_CTX_IDENT),
        Default::default(),
      )),
      true,
    ),
    Some(Pat::Ident(binding)) => (Some(binding.id.clone()), false),
    Some(_) => (None, false),
  };

  ActionFrame {
    ctx_ident,
    add_param,
    used: false,
  }
}

fn add_first_param(func: &mut Expr, ident: Ident) {
  let pat = Pat::Ident(BindingIdent {
    id: ident,
    type_ann: None,
  });
  match func {
    Expr::Arrow(arrow) => arrow.params.insert(0, pat),
    Expr::Fn(FnExpr { function, .. }) => function.params.insert(
      0,
      Param {
        span: Default::default(),
        decorators: vec![],
        pat,
      },
    ),
    _ => {}
  }
}

/// Replaces a destructured props param with `props_ident` and moves the destructuring into the
/// function body, so that `props_ident.ctx` can be referenced.
fn replace_props_param(param: &mut Pat, body: &mut BlockStmt, props_ident: Ident) {
  let original = std::mem::replace(param, Pat::dummy());
  let (pattern, default) = match original {
    Pat::Assign(AssignPat { left, right, .. }) => (*left, Some(right)),
    pat => (pat, None),
  };

  let mut binding = BindingIdent {
    id: props_ident.clone(),
    type_ann: None,
  };
  let mut pattern = pattern;
  match &mut pattern {
    Pat::Object(ObjectPat { type_ann, .. }) | Pat::Array(ArrayPat { type_ann, .. }) => {
      binding.type_ann = type_ann.take();
    }
    _ => {}
  }

  *param = match default {
    Some(right) => Pat::Assign(AssignPat {
      span: Default::default(),
      left: Box::new(Pat::Ident(binding)),
      right,
    }),
    None => Pat::Ident(binding),
  };

  body.stmts.insert(
    0,
    Stmt::Decl(Decl::Var(Box::new(VarDecl {
      kind: VarDeclKind::Let,
      decls: vec![VarDeclarator {
        name: pattern,
        init: Some(Box::new(Expr::Ident(props_ident))),
        ..Take::dummy()
      }],
      ..Take::dummy()
    }))),
  );
}

fn escape_tpl_raw(s: &str) -> String {
  s.replace('\\', "\\\\")
    .replace('`', "\\`")
    .replace("${", "\\${")
}

fn form_action_method(name: &str) -> Option<FormActionMethod> {
  match name {
    "hx-get" => Some(FormActionMethod::Get),
    "hx-post" => Some(FormActionMethod::Post),
    "hx-put" => Some(FormActionMethod::Put),
    "hx-delete" => Some(FormActionMethod::Delete),
    "hx-patch" => Some(FormActionMethod::Patch),
    _ => None,
  }
}

fn jsx_attr_name(attr: &JSXAttr) -> Option<&str> {
  match &attr.name {
    JSXAttrName::Ident(name) => Some(name.sym.as_str()),
    JSXAttrName::JSXNamespacedName(_) => None,
  }
}

fn jsx_attr_expr(attr: &JSXAttr) -> Option<&Expr> {
  match &attr.value {
    Some(JSXAttrValue::JSXExprContainer(JSXExprContainer {
      expr: JSXExpr::Expr(expr),
      ..
    })) => Some(expr),
    _ => None,
  }
}

fn prop_name(prop: &KeyValueProp) -> Option<&str> {
  match &prop.key {
    PropName::Ident(name) => Some(name.sym.as_str()),
    PropName::Str(name) => Some(name.value.as_str()),
    _ => None,
  }
}

impl<'a> VisitMut for ComponentTransformVisitor<'a> {
  fn visit_mut_function(&mut self, func: &mut Function) {
    if func.body.is_none() || !self.enter_function(func.params.iter().map(|p| &p.pat)) {
      func.visit_mut_children_with(self);
      return;
    }

    func.visit_mut_children_with(self);

    if let Some(props_ident) = self.exit_function() {
      replace_props_param(
        &mut func.params[0].pat,
        func.body.as_mut().unwrap(),
        props_ident,
      );
    }
  }

  fn visit_mut_arrow_expr(&mut self, arrow: &mut ArrowExpr) {
    if !self.enter_function(arrow.params.iter()) {
      arrow.visit_mut_children_with(self);
      return;
    }

    arrow.visit_mut_children_with(self);

    if let Some(props_ident) = self.exit_function() {
      if let BlockStmtOrExpr::Expr(expr) = arrow.body.as_mut() {
        let expr = expr.take();
        *arrow.body = BlockStmtOrExpr::BlockStmt(BlockStmt {
          stmts: vec![Stmt::Return(ReturnStmt {
            span: Default::default(),
            arg: Some(expr),
          })],
          ..Take::dummy()
        });
      }

      replace_props_param(
        &mut arrow.params[0],
        arrow.body.as_mut_block_stmt().unwrap(),
        props_ident,
      );
    }
  }

  fn visit_mut_jsx_opening_element(&mut self, node: &mut JSXOpeningElement) {
    let has_action = node.attrs.iter().any(|attr| match attr {
      JSXAttrOrSpread::JSXAttr(attr) => jsx_attr_name(attr)
        .and_then(form_action_method)
        .is_some_and(|_| jsx_attr_expr(attr).is_some_and(is_fn)),
      JSXAttrOrSpread::SpreadElement(_) => false,
    });

    if has_action {
      let explicit_ctx_index = node.attrs.iter().position(|attr| match attr {
        JSXAttrOrSpread::JSXAttr(attr) => {
          jsx_attr_name(attr) == Some("ctx") && jsx_attr_expr(attr).is_some()
        }
        JSXAttrOrSpread::SpreadElement(_) => false,
      });

      if let Some(index) = explicit_ctx_index {
        let JSXAttrOrSpread::JSXAttr(attr) = node.attrs.remove(index) else {
          unreachable!()
        };
        let Some(JSXAttrValue::JSXExprContainer(JSXExprContainer {
          expr: JSXExpr::Expr(explicit_ctx),
          ..
        })) = attr.value
        else {
          unreachable!()
        };

        let previous_ctx = self.explicit_action_ctx.replace(explicit_ctx);
        node.visit_mut_children_with(self);
        self.explicit_action_ctx = previous_ctx;
        return;
      }
    }

    node.visit_mut_children_with(self);
  }

  fn visit_mut_object_lit(&mut self, node: &mut ObjectLit) {
    let has_action = node.props.iter().any(|prop| match prop {
      PropOrSpread::Prop(prop) => match prop.as_ref() {
        Prop::KeyValue(prop) => prop_name(prop)
          .and_then(form_action_method)
          .is_some_and(|_| is_fn(&prop.value)),
        _ => false,
      },
      PropOrSpread::Spread(_) => false,
    });

    if has_action {
      let explicit_ctx_index = node.props.iter().position(|prop| match prop {
        PropOrSpread::Prop(prop) => match prop.as_ref() {
          Prop::KeyValue(prop) => prop_name(prop) == Some("ctx"),
          _ => false,
        },
        PropOrSpread::Spread(_) => false,
      });

      if let Some(index) = explicit_ctx_index {
        let PropOrSpread::Prop(prop) = node.props.remove(index) else {
          unreachable!()
        };
        let Prop::KeyValue(prop) = *prop else {
          unreachable!()
        };

        let previous_ctx = self.explicit_action_ctx.replace(prop.value);
        node.visit_mut_children_with(self);
        self.explicit_action_ctx = previous_ctx;
        return;
      }
    }

    node.visit_mut_children_with(self);
  }

  fn visit_mut_jsx_attr(&mut self, node: &mut JSXAttr) {
    if let Some(JSXAttrValue::JSXExprContainer(JSXExprContainer {
      expr: JSXExpr::Expr(expr),
      ..
    })) = &mut node.value
    {
      if is_fn(expr) {
        let name = match &node.name {
          JSXAttrName::Ident(name) => name.sym.as_str().to_string(),
          JSXAttrName::JSXNamespacedName(namespaced_name) => {
            format!("{}:{}", namespaced_name.ns.sym, namespaced_name.name.sym)
          }
        };

        if name.starts_with("hx-on:") || name.starts_with("hx-on-") {
          self.add_event_handler(expr);
          return;
        }

        if let Some(method) = form_action_method(&name) {
          self.add_action(method, expr);
          return;
        }
      }
    }

    node.visit_mut_children_with(self);
  }

  fn visit_mut_key_value_prop(&mut self, node: &mut KeyValueProp) {
    if node.key.is_str() && is_fn(&node.value) {
      let key = node.key.as_str().unwrap().value.to_string();
      if key.starts_with("hx-on:") || key.starts_with("hx-on-") {
        self.add_event_handler(&mut node.value);
        return;
      }

      if let Some(method) = form_action_method(&key) {
        self.add_action(method, &mut node.value);
        return;
      }
    }

    node.visit_mut_children_with(self);
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BindingKind {
  Value,
  Function,
  Class,
}

/// Collects every binding in a node, classifying functions and classes since they cannot be
/// captured.
#[derive(Default)]
struct BindingCollector {
  bindings: HashMap<Id, BindingKind>,
}

impl BindingCollector {
  fn add_pat(&mut self, pat: &Pat, kind: BindingKind) {
    let mut idents = vec![];
    pat_binding_idents(pat, &mut idents);
    for ident in idents {
      self.bindings.insert(ident.to_id(), kind);
    }
  }
}

impl Visit for BindingCollector {
  noop_visit_type!();

  fn visit_var_declarator(&mut self, node: &VarDeclarator) {
    let kind = match (&node.name, node.init.as_deref()) {
      (Pat::Ident(_), Some(Expr::Fn(_) | Expr::Arrow(_))) => BindingKind::Function,
      (Pat::Ident(_), Some(Expr::Class(_))) => BindingKind::Class,
      _ => BindingKind::Value,
    };
    self.add_pat(&node.name, kind);
    node.visit_children_with(self);
  }

  fn visit_param(&mut self, node: &Param) {
    self.add_pat(&node.pat, BindingKind::Value);
    node.visit_children_with(self);
  }

  fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
    for param in &node.params {
      self.add_pat(param, BindingKind::Value);
    }
    node.visit_children_with(self);
  }

  fn visit_catch_clause(&mut self, node: &CatchClause) {
    if let Some(param) = &node.param {
      self.add_pat(param, BindingKind::Value);
    }
    node.visit_children_with(self);
  }

  fn visit_fn_decl(&mut self, node: &FnDecl) {
    self
      .bindings
      .insert(node.ident.to_id(), BindingKind::Function);
    node.visit_children_with(self);
  }

  fn visit_fn_expr(&mut self, node: &FnExpr) {
    if let Some(ident) = &node.ident {
      self.bindings.insert(ident.to_id(), BindingKind::Function);
    }
    node.visit_children_with(self);
  }

  fn visit_class_decl(&mut self, node: &ClassDecl) {
    self.bindings.insert(node.ident.to_id(), BindingKind::Class);
    node.visit_children_with(self);
  }

  fn visit_class_expr(&mut self, node: &ClassExpr) {
    if let Some(ident) = &node.ident {
      self.bindings.insert(ident.to_id(), BindingKind::Class);
    }
    node.visit_children_with(self);
  }
}

fn pat_binding_idents(pat: &Pat, idents: &mut Vec<Ident>) {
  match pat {
    Pat::Ident(binding) => idents.push(binding.id.clone()),
    Pat::Array(arr) => {
      for elem in arr.elems.iter().flatten() {
        pat_binding_idents(elem, idents);
      }
    }
    Pat::Rest(rest) => pat_binding_idents(&rest.arg, idents),
    Pat::Object(obj) => {
      for prop in &obj.props {
        match prop {
          ObjectPatProp::KeyValue(kv) => pat_binding_idents(&kv.value, idents),
          ObjectPatProp::Assign(assign) => idents.push(assign.key.id.clone()),
          ObjectPatProp::Rest(rest) => pat_binding_idents(&rest.arg, idents),
        }
      }
    }
    Pat::Assign(assign) => pat_binding_idents(&assign.left, idents),
    Pat::Invalid(_) | Pat::Expr(_) => {}
  }
}

/// Collects the bindings that live in the module scope, including hoisted `var`s.
fn top_level_bindings(module: &Module) -> HashSet<Id> {
  let mut collector = TopLevelCollector::default();
  for item in &module.body {
    let decl = match item {
      ModuleItem::ModuleDecl(ModuleDecl::Import(import)) => {
        for specifier in &import.specifiers {
          collector.ids.insert(
            match specifier {
              ImportSpecifier::Named(s) => &s.local,
              ImportSpecifier::Default(s) => &s.local,
              ImportSpecifier::Namespace(s) => &s.local,
            }
            .to_id(),
          );
        }
        None
      }
      ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => Some(&export.decl),
      ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(export)) => {
        match &export.decl {
          DefaultDecl::Fn(FnExpr {
            ident: Some(ident), ..
          })
          | DefaultDecl::Class(ClassExpr {
            ident: Some(ident), ..
          }) => {
            collector.ids.insert(ident.to_id());
          }
          _ => {}
        }
        None
      }
      ModuleItem::Stmt(Stmt::Decl(decl)) => Some(decl),
      _ => None,
    };

    match decl {
      Some(Decl::Var(var)) => {
        let mut idents = vec![];
        for declarator in &var.decls {
          pat_binding_idents(&declarator.name, &mut idents);
        }
        collector.ids.extend(idents.iter().map(|i| i.to_id()));
      }
      Some(Decl::Using(using)) => {
        let mut idents = vec![];
        for declarator in &using.decls {
          pat_binding_idents(&declarator.name, &mut idents);
        }
        collector.ids.extend(idents.iter().map(|i| i.to_id()));
      }
      Some(Decl::Fn(f)) => {
        collector.ids.insert(f.ident.to_id());
      }
      Some(Decl::Class(c)) => {
        collector.ids.insert(c.ident.to_id());
      }
      _ => {}
    }
  }

  module.visit_with(&mut collector);
  collector.ids
}

/// Finds `var` declarations hoisted to the module scope from nested statements.
#[derive(Default)]
struct TopLevelCollector {
  ids: HashSet<Id>,
}

impl Visit for TopLevelCollector {
  noop_visit_type!();

  fn visit_function(&mut self, _: &Function) {}
  fn visit_arrow_expr(&mut self, _: &ArrowExpr) {}
  fn visit_constructor(&mut self, _: &Constructor) {}
  fn visit_getter_prop(&mut self, _: &GetterProp) {}
  fn visit_setter_prop(&mut self, _: &SetterProp) {}
  fn visit_static_block(&mut self, _: &StaticBlock) {}

  fn visit_var_decl(&mut self, node: &VarDecl) {
    if node.kind == VarDeclKind::Var {
      let mut idents = vec![];
      for declarator in &node.decls {
        pat_binding_idents(&declarator.name, &mut idents);
      }
      self.ids.extend(idents.iter().map(|i| i.to_id()));
    }
    node.visit_children_with(self);
  }
}

/// Collects every identifier in a node. Identifiers that are not references, such as property
/// names, keep an empty syntax context and are filtered out by the binding lookup.
#[derive(Default)]
struct IdentRefCollector {
  idents: Vec<Ident>,
}

impl Visit for IdentRefCollector {
  noop_visit_type!();

  fn visit_ident(&mut self, ident: &Ident) {
    self.idents.push(ident.clone());
  }
}

fn ident_refs<N: VisitWith<IdentRefCollector> + ?Sized>(node: &N) -> Vec<Id> {
  let mut refs = IdentRefCollector::default();
  node.visit_with(&mut refs);
  refs.idents.iter().map(Ident::to_id).collect()
}

/// A top level declaration, import specifier or var declarator that a handler slice may keep.
struct SliceNode {
  /// Index of the module item.
  item: usize,
  /// Index of the import specifier or var declarator within the module item.
  part: Option<usize>,
  /// Identifiers referenced by the node, excluding those in types.
  refs: Vec<Id>,
}

const ON_LOAD_EXPORT: &str = "onLoad";

/// An export that a handler slice keeps, along with everything it references.
enum SliceRoot {
  /// A hoisted event handler, by export name.
  Handler(Atom),
  /// The `onLoad` export of a route module.
  OnLoad,
}

/// How a kept node was reached from the slice roots, for error messages.
struct SliceReach {
  /// The identifier that referenced the node, or `None` if the node is the root itself.
  via: Option<Atom>,
  from: Option<usize>,
  root: usize,
}

#[derive(Default)]
struct SliceGraph {
  nodes: Vec<SliceNode>,
  /// Nodes binding each top level identifier; usually one, but overloads and merged
  /// declarations may bind the same identifier more than once.
  bindings: HashMap<Id, Vec<usize>>,
}

impl SliceGraph {
  fn add(&mut self, item: usize, part: Option<usize>, binds: Vec<Id>, refs: Vec<Id>) {
    let node = self.nodes.len();
    self.nodes.push(SliceNode { item, part, refs });
    for id in binds {
      self.bindings.entry(id).or_default().push(node);
    }
  }

  fn add_decl(&mut self, item: usize, decl: &Decl) {
    match decl {
      Decl::Fn(f) => self.add(item, None, vec![f.ident.to_id()], ident_refs(f)),
      Decl::Class(c) => self.add(item, None, vec![c.ident.to_id()], ident_refs(c)),
      Decl::Var(var) => {
        for (i, declarator) in var.decls.iter().enumerate() {
          let mut binds = vec![];
          pat_binding_idents(&declarator.name, &mut binds);
          self.add(
            item,
            Some(i),
            binds.iter().map(Ident::to_id).collect(),
            ident_refs(declarator),
          );
        }
      }
      Decl::TsEnum(e) => self.add(item, None, vec![e.id.to_id()], ident_refs(e)),
      Decl::TsModule(m) => {
        if let TsModuleName::Ident(id) = &m.id {
          self.add(item, None, vec![id.to_id()], ident_refs(m));
        }
      }
      // types are erased, and top level `using` declarations are not supported
      Decl::TsInterface(_) | Decl::TsTypeAlias(_) | Decl::Using(_) => {}
    }
  }
}

fn handler_export(item: &ModuleItem) -> Option<&VarDeclarator> {
  let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
    decl: Decl::Var(var),
    ..
  })) = item
  else {
    return None;
  };

  match var.decls.as_slice() {
    [declarator] => match &declarator.name {
      Pat::Ident(binding) if binding.id.sym.starts_with(super::EVENT_DECL_PREFIX) => {
        Some(declarator)
      }
      _ => None,
    },
    _ => None,
  }
}

/// Returns whether an export specifier exports `onLoad`, matching the exports found by
/// [`crate::es_ast_helpers::find_exports`].
fn exports_on_load(specifier: &ExportSpecifier) -> bool {
  let name = match specifier {
    ExportSpecifier::Namespace(s) => &s.name,
    ExportSpecifier::Default(s) => return s.exported.sym == ON_LOAD_EXPORT,
    ExportSpecifier::Named(s) => s.exported.as_ref().unwrap_or(&s.orig),
  };
  match name {
    ModuleExportName::Ident(ident) => ident.sym == ON_LOAD_EXPORT,
    ModuleExportName::Str(s) => s.value == ON_LOAD_EXPORT,
  }
}

/// Cuts a module processed by [`transform_module`] down to its event handler exports, its
/// `onLoad` export if `keep_on_load` is set (i.e. it is a route module), and the top level
/// declarations and imports they reference, so that client scripts do not include the server side
/// code of the module.
///
/// Everything else is dropped, including the components, form actions and top level side effects.
/// Kept imports whose specifier matches one of `banned_imports` are reported as errors.
pub(crate) fn slice_handler_module(
  module_id: &ModuleId,
  global_ids: &crate::GlobalIds,
  swc_module: &mut Module,
  keep_on_load: bool,
  banned_imports: &[Regex],
) -> Result<(), String> {
  let mut graph = SliceGraph::default();
  let mut roots = vec![];
  let mut on_load_root = None;
  // nodes, or identifiers referencing nodes, where the walk starts
  let mut node_seeds: Vec<(usize, usize)> = vec![];
  let mut ref_seeds: Vec<(usize, Vec<Id>)> = vec![];
  // module items and export specifiers that keep their `export` because they export `onLoad`
  let mut on_load_items = HashSet::new();
  let mut on_load_specifiers = HashSet::new();
  for (i, item) in swc_module.body.iter().enumerate() {
    if let Some(declarator) = handler_export(item) {
      ref_seeds.push((roots.len(), ident_refs(item)));
      roots.push(SliceRoot::Handler(
        declarator.name.as_ident().unwrap().id.sym.clone(),
      ));
      continue;
    }

    let mut on_load_root_index = |roots: &mut Vec<SliceRoot>| {
      *on_load_root.get_or_insert_with(|| {
        roots.push(SliceRoot::OnLoad);
        roots.len() - 1
      })
    };

    match item {
      ModuleItem::ModuleDecl(ModuleDecl::Import(import)) if !import.type_only => {
        for (j, specifier) in import.specifiers.iter().enumerate() {
          let local = match specifier {
            ImportSpecifier::Named(s) if s.is_type_only => continue,
            ImportSpecifier::Named(s) => &s.local,
            ImportSpecifier::Default(s) => &s.local,
            ImportSpecifier::Namespace(s) => &s.local,
          };
          graph.add(i, Some(j), vec![local.to_id()], vec![]);
        }
      }
      ModuleItem::ModuleDecl(ModuleDecl::TsImportEquals(import)) if !import.is_type_only => {
        let refs = match &import.module_ref {
          TsModuleRef::TsEntityName(name) => {
            let mut name = name;
            while let TsEntityName::TsQualifiedName(qualified) = name {
              name = &qualified.left;
            }
            vec![name.as_ident().unwrap().to_id()]
          }
          TsModuleRef::TsExternalModuleRef(_) => vec![],
        };
        graph.add(i, None, vec![import.id.to_id()], refs);
      }
      ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => {
        let first = graph.nodes.len();
        graph.add_decl(i, &export.decl);
        if !keep_on_load {
          continue;
        }

        // `export function onLoad` and `export const onLoad = ...`
        let on_load_node = match &export.decl {
          Decl::Fn(f) if f.ident.sym == ON_LOAD_EXPORT => Some(first),
          Decl::Var(var) if !var.declare => var
            .decls
            .iter()
            .position(|declarator| {
              matches!(&declarator.name, Pat::Ident(binding) if binding.id.sym == ON_LOAD_EXPORT)
            })
            .map(|j| first + j),
          _ => None,
        };
        if let Some(node) = on_load_node {
          node_seeds.push((on_load_root_index(&mut roots), node));
          on_load_items.insert(i);
        }
      }
      // `export { x as onLoad }` and `export { onLoad } from "..."`
      ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(export)) if keep_on_load => {
        for (j, specifier) in export.specifiers.iter().enumerate() {
          if !exports_on_load(specifier) {
            continue;
          }
          let root = on_load_root_index(&mut roots);
          match (&export.src, specifier) {
            (
              None,
              ExportSpecifier::Named(ExportNamedSpecifier {
                orig: ModuleExportName::Ident(orig),
                ..
              }),
            ) => ref_seeds.push((root, vec![orig.to_id()])),
            (Some(_), _) => {
              node_seeds.push((root, graph.nodes.len()));
              graph.add(i, Some(j), vec![], vec![]);
            }
            _ => continue,
          }
          on_load_specifiers.insert((i, j));
        }
      }
      ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(export)) => match &export.decl {
        DefaultDecl::Fn(FnExpr {
          ident: Some(ident), ..
        })
        | DefaultDecl::Class(ClassExpr {
          ident: Some(ident), ..
        }) => graph.add(i, None, vec![ident.to_id()], ident_refs(&export.decl)),
        _ => {}
      },
      ModuleItem::Stmt(Stmt::Decl(decl)) => graph.add_decl(i, decl),
      _ => {}
    }
  }

  // walk the references breadth first so that error messages show the shortest chain
  let mut reached: Vec<Option<SliceReach>> = graph.nodes.iter().map(|_| None).collect();
  let mut queue = VecDeque::new();
  for (root, node) in node_seeds {
    if reached[node].is_none() {
      reached[node] = Some(SliceReach {
        via: None,
        from: None,
        root,
      });
      queue.push_back(node);
    }
  }
  for (root, refs) in ref_seeds {
    for id in refs {
      for &node in graph.bindings.get(&id).into_iter().flatten() {
        if reached[node].is_none() {
          reached[node] = Some(SliceReach {
            via: Some(id.0.clone()),
            from: None,
            root,
          });
          queue.push_back(node);
        }
      }
    }
  }
  while let Some(from) = queue.pop_front() {
    let root = reached[from].as_ref().unwrap().root;
    for id in &graph.nodes[from].refs {
      for &node in graph.bindings.get(id).into_iter().flatten() {
        if reached[node].is_none() {
          reached[node] = Some(SliceReach {
            via: Some(id.0.clone()),
            from: Some(from),
            root,
          });
          queue.push_back(node);
        }
      }
    }
  }

  let kept = graph
    .nodes
    .iter()
    .zip(&reached)
    .filter(|(_, reach)| reach.is_some())
    .map(|(node, _)| (node.item, node.part))
    .collect::<HashSet<_>>();

  let chain = |node: usize| {
    let mut names = vec![];
    let mut current = Some(node);
    while let Some(node) = current {
      let reach = reached[node].as_ref().unwrap();
      names.extend(reach.via.as_ref().map(Atom::to_string));
      current = reach.from;
    }
    let root = reached[node].as_ref().unwrap().root;
    names.push(match &roots[root] {
      SliceRoot::Handler(export_name) => export_name.to_string(),
      SliceRoot::OnLoad => ON_LOAD_EXPORT.to_string(),
    });
    names.reverse();
    (root, names.join(" → "))
  };

  let mut errors = vec![];
  let mut reported = HashSet::new();
  for (node, reach) in reached.iter().enumerate() {
    let item = graph.nodes[node].item;
    if reach.is_none() || reported.contains(&item) {
      continue;
    }
    let src = match &swc_module.body[item] {
      ModuleItem::ModuleDecl(ModuleDecl::Import(import)) => &import.src.value,
      ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(NamedExport { src: Some(src), .. })) => {
        &src.value
      }
      ModuleItem::ModuleDecl(ModuleDecl::TsImportEquals(import)) => match &import.module_ref {
        TsModuleRef::TsExternalModuleRef(module_ref) => &module_ref.expr.value,
        TsModuleRef::TsEntityName(_) => continue,
      },
      _ => continue,
    };
    if banned_imports.iter().any(|pattern| pattern.is_match(src)) {
      reported.insert(item);
      let (root, chain) = chain(node);
      let exporter = match &roots[root] {
        SliceRoot::Handler(export_name) => format!(
          "event handler {}",
          global_ids
            .event_handler_ids
            .existing_id(module_id, export_name)
            .cloned()
            .unwrap_or_else(|| format!("{}#{}", module_id.relative_path(), export_name)),
        ),
        SliceRoot::OnLoad => format!("{} of {}", ON_LOAD_EXPORT, module_id.relative_path()),
      };
      errors.push(format!(
        "{} imports banned module {} via {}",
        exporter,
        serde_json::to_string(src.as_str()).unwrap(),
        chain,
      ));
    }
  }
  if !errors.is_empty() {
    return Err(errors.join("\n"));
  }

  let body = std::mem::take(&mut swc_module.body);
  for (i, item) in body.into_iter().enumerate() {
    if handler_export(&item).is_some() {
      swc_module.body.push(item);
      continue;
    }

    let decl = match item {
      ModuleItem::ModuleDecl(ModuleDecl::Import(mut import)) => {
        let mut j = 0;
        import.specifiers.retain(|_| {
          j += 1;
          kept.contains(&(i, Some(j - 1)))
        });
        if !import.specifiers.is_empty() {
          swc_module
            .body
            .push(ModuleItem::ModuleDecl(ModuleDecl::Import(import)));
        }
        continue;
      }
      ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(mut export)) => {
        let mut j = 0;
        export.specifiers.retain(|_| {
          j += 1;
          on_load_specifiers.contains(&(i, j - 1))
        });
        if !export.specifiers.is_empty() {
          swc_module
            .body
            .push(ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(export)));
        }
        continue;
      }
      ModuleItem::ModuleDecl(ModuleDecl::TsImportEquals(mut import)) => {
        if kept.contains(&(i, None)) {
          import.is_export = false;
          swc_module
            .body
            .push(ModuleItem::ModuleDecl(ModuleDecl::TsImportEquals(import)));
        }
        continue;
      }
      ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => export.decl,
      ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(export)) => match export.decl {
        DefaultDecl::Fn(FnExpr {
          ident: Some(ident),
          function,
        }) => Decl::Fn(FnDecl {
          ident,
          declare: false,
          function,
        }),
        DefaultDecl::Class(ClassExpr {
          ident: Some(ident),
          class,
        }) => Decl::Class(ClassDecl {
          ident,
          declare: false,
          class,
        }),
        _ => continue,
      },
      ModuleItem::Stmt(Stmt::Decl(decl)) => decl,
      _ => continue,
    };

    let decl = match decl {
      Decl::Var(mut var) => {
        let mut j = 0;
        var.decls.retain(|_| {
          j += 1;
          kept.contains(&(i, Some(j - 1)))
        });
        if var.decls.is_empty() {
          continue;
        }
        Decl::Var(var)
      }
      decl if kept.contains(&(i, None)) => decl,
      _ => continue,
    };
    swc_module.body.push(if on_load_items.contains(&i) {
      ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
        span: Default::default(),
        decl,
      }))
    } else {
      ModuleItem::Stmt(Stmt::Decl(decl))
    });
  }

  Ok(())
}

#[cfg(test)]
mod tests {
  use std::sync::Arc;

  use farmfe_core::{
    module::ModuleId,
    swc_common::{Globals, Mark, SourceMap, GLOBALS},
    swc_ecma_ast::EsVersion,
    swc_ecma_parser::{Syntax, TsSyntax},
  };
  use farmfe_toolkit::{
    script::{codegen_module, parse_module},
    swc_ecma_transforms::{
      hygiene::{hygiene_with_config, Config as HygieneConfig},
      resolver,
    },
    swc_ecma_visit::VisitMutWith,
  };

  use crate::{ids::GlobalIds, AtomStore};

  use super::{slice_handler_module, transform_module};

  /// Mirrors farm: `resolver` runs when parsing and `hygiene` runs when rendering.
  fn try_transform(route: &str, source: &str) -> Result<String, String> {
    try_transform_and_slice(route, source, None)
  }

  /// Transforms and then slices the module like its handlers variant, with the given banned
  /// import patterns.
  fn try_slice(source: &str, banned_imports: &[&str]) -> Result<String, String> {
    try_slice_with(source, true, banned_imports)
  }

  fn try_slice_with(
    source: &str,
    keep_on_load: bool,
    banned_imports: &[&str],
  ) -> Result<String, String> {
    let banned_imports = banned_imports
      .iter()
      .map(|pattern| regex::Regex::new(pattern).unwrap())
      .collect::<Vec<_>>();
    try_transform_and_slice("/x", source, Some((keep_on_load, &banned_imports)))
  }

  fn slice(source: &str) -> String {
    compact(&try_slice(source, &[]).unwrap())
  }

  fn try_transform_and_slice(
    route: &str,
    source: &str,
    slice: Option<(bool, &[regex::Regex])>,
  ) -> Result<String, String> {
    GLOBALS.set(&Globals::new(), || {
      let parsed = parse_module(
        "src/routes/article/[id]/index.tsx",
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

      let module_id = ModuleId::new("src/routes/article/[id]/index.tsx", "", ".");
      let mut global_ids = GlobalIds::new();
      transform_module(
        &module_id,
        route.to_string(),
        &mut global_ids,
        &mut AtomStore::default(),
        &mut module,
        &mut comments,
      )?;

      if let Some((keep_on_load, banned_imports)) = slice {
        slice_handler_module(
          &module_id,
          &global_ids,
          &mut module,
          keep_on_load,
          banned_imports,
        )?;
      }

      module.visit_mut_with(&mut hygiene_with_config(HygieneConfig {
        top_level_mark,
        ..Default::default()
      }));

      Ok(
        String::from_utf8(
          codegen_module(
            &module,
            EsVersion::EsNext,
            Arc::new(SourceMap::default()),
            None,
            false,
            None,
          )
          .unwrap(),
        )
        .unwrap(),
      )
    })
  }

  fn transform_at(route: &str, source: &str) -> String {
    try_transform(route, source).unwrap()
  }

  fn transform(source: &str) -> String {
    transform_at("/article/:id", source)
  }

  /// Collapses whitespace so assertions don't depend on codegen formatting.
  fn compact(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
  }

  #[test]
  fn uses_explicit_action_context() {
    let result = transform(
      r#"
        export default function Component(props) {
          const requestContext = props.context;
          return <button hx-post={() => null} ctx={requestContext}>Submit</button>;
        }
      "#,
    );

    assert!(result.contains("requestContext.req.param(\"id\")"));
    assert!(!result.contains("ctx={requestContext}"));
    assert!(!result.contains("props.ctx.req.param"));
  }

  #[test]
  fn uses_component_context_by_default() {
    let result = transform(
      r#"
        export default function Component(props) {
          return <button hx-post={() => null}>Submit</button>;
        }
      "#,
    );

    assert!(result.contains("props.ctx.req.param(\"id\")"));
  }

  #[test]
  fn captures_without_rewriting_the_action() {
    let result = compact(&transform_at(
      "/x",
      r#"
        export default function C({ count }) {
          return <button hx-post={() => <b>{count}</b>} />;
        }
      "#,
    ));

    assert!(result.contains(
      r#"`/x?action=src%2Froutes%2Farticle%2F%5Bid%5D%2Findex.tsx%400${actionScope("src/routes/article/[id]/index.tsx@0", { count })}`"#
    ));
    // the original expression body, and therefore its return value, is preserved
    assert!(result.contains("wrapActionWithPayload(({ count })=>()=><b>{count}</b>, \"src/routes/article/[id]/index.tsx@0\")"));
  }

  #[test]
  fn captures_survive_hygiene() {
    let result = transform_at(
      "/x",
      r#"
        export default function C({ count }) {
          return <button hx-post={(c) => { const count1 = 1; return c.text(String(count + count1)); }} />;
        }
      "#,
    );

    assert!(!result.contains("count2"));
    assert!(!result.contains("count: count"));
  }

  #[test]
  fn captures_references_in_every_position() {
    let result = compact(&transform_at(
      "/x",
      r#"
        export default function C({ a, b, c, d, e, f, g, h }) {
          return <button hx-post={async () => {
            const x = a;
            if (!b) {}
            await db.del({ c });
            new Date(d);
            return [e ? f : g, <i>{h}</i>];
          }} />;
        }
      "#,
    ));

    assert!(result.contains("{ a, b, c, d, e, f, g, h }"));
  }

  #[test]
  fn does_not_capture_module_scope_globals_or_inner_bindings() {
    let result = compact(&transform_at(
      "/x",
      r#"
        import { db } from "./db";
        const LIMIT = 10;
        function fmt(n) { return n; }
        export default function C({ item, list }) {
          return <button hx-post={() => {
            if (list) { const item = 1; console.log(item); }
            return list.map((item) => fmt(item.id)).slice(0, LIMIT).concat(db);
          }} />;
        }
      "#,
    ));

    assert!(result.contains("{ list }"));
    assert!(!result.contains("item })"));
  }

  #[test]
  fn captures_loop_and_switch_bindings() {
    let result = compact(&transform_at(
      "/x",
      r#"
        export default function C({ items, k }) {
          const out = [];
          for (const item of items) {
            out.push(<button hx-post={() => remove(item.id)} />);
          }
          switch (k) {
            case 1:
              const y = k + 1;
              out.push(<button hx-post={() => f(y)} />);
          }
          return out;
        }
      "#,
    ));

    assert!(result.contains("({ item })=>()=>remove(item.id)"));
    assert!(result.contains("({ y })=>()=>f(y)"));
  }

  #[test]
  fn captured_variables_are_mutable() {
    let result = compact(&transform_at(
      "/x",
      r#"
        export default function C({ count }) {
          return <button hx-post={() => { count += 1; function helper() {} helper(); return String(count); }} />;
        }
      "#,
    ));

    assert!(result.contains("({ count })=>()=>{ count += 1;"));
  }

  #[test]
  fn rejects_local_functions_and_classes() {
    let err = try_transform(
      "/x",
      r#"
        export default function C({ n }) {
          const fmt = (v) => v * 2;
          class Thing {}
          return <button hx-post={() => [fmt(n), new Thing()]} />;
        }
      "#,
    )
    .unwrap_err();

    assert!(err.contains("local function `fmt`"));
    assert!(err.contains("local class `Thing`"));
  }

  #[test]
  fn captures_event_handler_variables() {
    let result = compact(&transform_at(
      "/x",
      r#"
        export default function C({ msg }) {
          return <button hx-on:click={() => alert(msg)} />;
        }
      "#,
    ));

    assert!(result.contains(
      r#"hx-on:click={`$handlers["src/routes/article/[id]/index.tsx@0"](${handlerScope({ msg })}).call(this, event)`}"#
    ));
    assert!(result.contains("export const _handler$0 = ({ msg })=>()=>alert(msg);"));
  }

  #[test]
  fn leaves_handlers_without_captures_unchanged() {
    let result = compact(&transform_at(
      "/x",
      r#"
        export default function C() {
          return <button hx-on:click={() => alert(1)} hx-post={(c) => c.text("hi")} />;
        }
      "#,
    ));

    assert!(result.contains(
      r#"hx-on:click={'$handlers["src/routes/article/[id]/index.tsx@0"].call(this, event)'}"#
    ));
    assert!(result.contains("export const _handler$0 = ()=>alert(1);"));
    assert!(
      result.contains("hx-post={`/x?action=src%2Froutes%2Farticle%2F%5Bid%5D%2Findex.tsx%400`}")
    );
    assert!(result.contains("wrapActionWithPayload((c)=>c.text(\"hi\"))"));
  }

  #[test]
  fn transforms_nested_actions_using_outer_action_context() {
    let result = compact(&transform(
      r#"
        export default function C(props) {
          const a = props.a;
          return <button hx-post={() => <i hx-post={() => f(a)} />} />;
        }
      "#,
    ));

    // the inner action uses the outer action's ctx, and the outer action captures `a` for it
    assert!(result.contains("_action$post$1 = wrapActionWithPayload(({ a })=>()=>f(a)"));
    assert!(result.contains("({ a })=>(__recooler_ctx)=><i hx-post={`/article/${__recooler_ctx.req.param(\"id\")}?action="));
    assert!(!result.contains("{ a, props }"));
  }

  #[test]
  fn rewrites_destructured_props_for_context() {
    let result = compact(&transform(
      r#"
        export default function C({ id }: RouteProps) {
          return <button hx-post={() => id} />;
        }
      "#,
    ));

    assert!(
      result.contains("function C(__recooler_props: RouteProps) { let { id } = __recooler_props;")
    );
    assert!(result.contains("__recooler_props.ctx.req.param(\"id\")"));
  }

  #[test]
  fn reports_missing_context_instead_of_panicking() {
    let err = try_transform(
      "/article/:id",
      r#"
        export default function C() {
          return <button hx-post={() => 1} />;
        }
      "#,
    )
    .unwrap_err();

    assert!(err.contains("has no request context"));
  }

  #[test]
  fn slices_handler_helpers_and_drops_server_code() {
    let result = slice(
      r#"
        import { query as q } from "faunadb";
        import { type BountyMultipliers, faunaClient } from "@app/backend/fql";
        import { runAdminAction } from "../lib/admin-action";
        import Button from "./Button";
        import "./styles.css";

        export interface Props { multipliers?: BountyMultipliers }

        const defaultMultipliers: BountyMultipliers = { tier: [1] };

        function formatNumber(n: number): string {
          return n.toFixed(2);
        }

        function multipliersFromFormPayload(payload: Record<string, any>): BountyMultipliers {
          return payload as unknown as BountyMultipliers;
        }

        function setElementFormattedNumber(id: string, value: number) {
          (document.getElementById(id) as HTMLElement).innerText = formatNumber(value);
        }

        export const updateMultipliersTable = () => {
          const multipliers = multipliersFromFormPayload({});
          setElementFormattedNumber("x", multipliers.tier[0]);
        };

        console.log("side effect");

        export async function ManageMultipliersButton({ multipliers }: Props) {
          multipliers ??= (await faunaClient.query(q.Get("x"))) ?? defaultMultipliers;
          return (
            <Button
              hx-on-click={function (this: HTMLButtonElement) {
                ((window as any).updateMultipliersTable = updateMultipliersTable)();
              }}
              hx-post={async (ctx) => runAdminAction(ctx, multipliers)}
            />
          );
        }
      "#,
    );

    assert!(result.contains("function formatNumber(n: number): string"));
    assert!(result.contains("function multipliersFromFormPayload("));
    assert!(result.contains("function setElementFormattedNumber("));
    assert!(result.contains("const updateMultipliersTable = ()=>{"));
    assert!(!result.contains("export const updateMultipliersTable"));
    assert!(result.contains("export const _handler$0 = function(this: HTMLButtonElement) {"));
    for dropped in [
      "faunadb",
      "fql",
      "admin-action",
      "./Button",
      "styles.css",
      "interface Props",
      "defaultMultipliers",
      "side effect",
      "ManageMultipliersButton",
      "_action$",
      "farm-plugin-recooler/helpers",
    ] {
      assert!(
        !result.contains(dropped),
        "{} was not dropped: {}",
        dropped,
        result
      );
    }
  }

  #[test]
  fn slices_handlers_with_captured_scope() {
    let result = slice(
      r#"
        import { fmt } from "./fmt";
        import { db } from "./db";
        export default async function C({ msg }) {
          await db.get();
          return <button hx-on:click={() => alert(fmt(msg))} />;
        }
      "#,
    );

    assert!(result.contains(r#"import { fmt } from "./fmt";"#));
    assert!(result.contains("export const _handler$0 = ({ msg })=>()=>alert(fmt(msg));"));
    assert!(!result.contains("./db"));
    assert!(!result.contains("handlerScope"));
    assert!(!result.contains("function C"));
  }

  #[test]
  fn slices_out_type_only_imports() {
    let result = slice(
      r#"
        import type { A } from "./a";
        import { type B, c } from "./b";
        import { D } from "./d";
        export default function C() {
          return <button hx-on:click={(e: A) => { const x: B = c; return e as unknown as D; }} />;
        }
      "#,
    );

    assert!(result.contains(r#"import { c } from "./b";"#));
    assert!(!result.contains("./a"));
    assert!(!result.contains("type B"));
    assert!(!result.contains("./d"));
  }

  #[test]
  fn slices_destructured_var_declarators() {
    let result = slice(
      r#"
        import { config } from "./config";
        import { server } from "./server";
        const { a, b: [c] } = config, unused = server();
        let other = 1;
        export default function C() {
          return <button hx-on:click={() => c} />;
        }
      "#,
    );

    assert!(result.contains(r#"import { config } from "./config";"#));
    assert!(result.contains("const { a, b: [c] } = config;"));
    assert!(!result.contains("unused"));
    assert!(!result.contains("server"));
    assert!(!result.contains("other"));
  }

  #[test]
  fn slices_partially_used_import_declarations() {
    let result = slice(
      r#"
        import { a, b, c as d } from "./m";
        import def, { e } from "./n";
        import * as ns from "./o";
        import f, * as g from "./p";
        export default function C() {
          return <button hx-on:click={() => [a, d, ns.x, f, b2]} hx-on:keyup={() => def} />;
        }
      "#,
    );

    assert!(result.contains(r#"import { a, c as d } from "./m";"#));
    assert!(result.contains(r#"import def from "./n";"#));
    assert!(result.contains(r#"import * as ns from "./o";"#));
    assert!(result.contains(r#"import f from "./p";"#));
  }

  #[test]
  fn reports_banned_imports_with_reference_chain() {
    let source = r#"
      import { faunaClient } from "../lib/fql";
      import { query as q } from "faunadb";
      function query() {
        return faunaClient.query();
      }
      export const updateTable = () => query();
      export default async function C() {
        await faunaClient.query(q.Get("x"));
        return <><button hx-on:click={() => 1} /><button hx-on:click={() => updateTable()} /></>;
      }
    "#;

    let err = try_slice(source, &["/fql$"]).unwrap_err();
    assert_eq!(
      err,
      r#"event handler src/routes/article/[id]/index.tsx@1 imports banned module "../lib/fql" via _handler$1 → updateTable → query → faunaClient"#
    );

    // imports that the handlers do not use are dropped instead of reported
    assert!(try_slice(source, &["^faunadb$"]).is_ok());
  }

  #[test]
  fn slices_on_load_without_event_handlers() {
    let source = r#"
      import { db } from "./db";
      import { fmt } from "./fmt";
      function helper() {
        return fmt(1);
      }
      export const onLoad = () => {
        helper();
      };
      export default async function Page() {
        await db.get();
        return <div />;
      }
    "#;

    let result = compact(&try_slice(source, &[]).unwrap());
    assert!(result.contains(r#"import { fmt } from "./fmt";"#));
    assert!(result.contains("function helper() {"));
    assert!(result.contains("export const onLoad = ()=>{ helper(); };"));
    assert!(!result.contains("./db"));
    assert!(!result.contains("Page"));

    // only route modules keep `onLoad`, since that of other modules never runs on the client
    let result = compact(&try_slice_with(source, false, &[]).unwrap());
    assert!(!result.contains("onLoad"));
    assert!(!result.contains("helper"));
  }

  #[test]
  fn slices_on_load_and_handlers_sharing_a_helper() {
    let result = slice(
      r#"
        import { db } from "./db";
        function shared(n) {
          return n * 2;
        }
        export async function onLoad() {
          shared(1);
        }
        export default async function Page() {
          await db.get();
          return <button hx-on:click={() => shared(2)} />;
        }
      "#,
    );

    assert!(result.contains("export async function onLoad() { shared(1); }"));
    assert!(result.contains("export const _handler$0 = ()=>shared(2);"));
    assert_eq!(result.matches("function shared(").count(), 1);
    assert!(!result.contains("./db"));
  }

  #[test]
  fn slices_on_load_export_forms() {
    // `export { local as onLoad }`
    let result = slice(
      r#"
        import { run } from "./run";
        function load() {
          run();
        }
        export { load as onLoad, other };
        const other = 1;
      "#,
    );
    assert!(result.contains(r#"import { run } from "./run";"#));
    assert!(result.contains("function load() { run(); }"));
    assert!(result.contains("export { load as onLoad };"));
    assert!(!result.contains("other"));

    // `export { onLoad } from "..."`
    let result = slice(r#"export { onLoad, other } from "./client";"#);
    assert!(result.contains(r#"export { onLoad } from "./client";"#));

    // one of several declarators
    let result = slice("export const a = 1, b = 2, onLoad = () => a;");
    assert!(result.contains("export const a = 1, onLoad = ()=>a;"));

    // `declare` is not an export that runs
    let result = slice("export declare const onLoad: () => void;");
    assert!(!result.contains("onLoad"));
  }

  #[test]
  fn reports_banned_imports_reached_from_on_load() {
    let err = try_slice(
      r#"
        import { x } from "../lib/fql";
        function helper() {
          return x.query();
        }
        export function onLoad() {
          helper();
        }
      "#,
      &["/fql$"],
    )
    .unwrap_err();
    assert_eq!(
      err,
      r#"onLoad of src/routes/article/[id]/index.tsx imports banned module "../lib/fql" via onLoad → helper → x"#
    );

    let err = try_slice(r#"export { onLoad } from "../lib/fql";"#, &["/fql$"]).unwrap_err();
    assert_eq!(
      err,
      r#"onLoad of src/routes/article/[id]/index.tsx imports banned module "../lib/fql" via onLoad"#
    );
  }
}
