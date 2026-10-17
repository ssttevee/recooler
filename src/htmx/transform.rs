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
  /// A path param, whose name is anything up to its pattern like in hono, e.g. `[user-id]`.
  static ref RE_PATH_PARAM: Regex = Regex::new(r"/:([^/{}?]+)(\{.+?\}|\?)?(?:/|$)").unwrap();
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
    in_component_element: false,
    keep_object_lit_ctx: false,
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
  let mut params: Vec<RouteParam> = vec![];
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
      params.push(RouteParam {
        name: atom_store.atom(c.as_str()),
        // a param with a pattern, e.g. `:rest{.+}`, may span several path segments
        multi_segment: capture.get(2).is_some_and(|p| p.as_str().starts_with('{')),
      });

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
    params: Vec<RouteParam>,
  },
}

#[derive(Clone)]
struct RouteParam {
  name: Atom,
  multi_segment: bool,
}

/// The outermost function of a parameterized route module, whose first param is assumed to be the
/// component props containing `ctx`.
struct ComponentFrame {
  props_ident: Option<Ident>,
  /// Whether the first param is destructured and must be replaced by `props_ident`.
  rewrite_param: bool,
  /// Whether the first param is a rest param, so that the props are `props_ident[0]`.
  rest_param: bool,
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
  /// Whether the innermost JSX element whose attributes are being visited is a component.
  in_component_element: bool,
  /// Set right before visiting an object literal spread into the props of a component, whose `ctx`
  /// prop is then kept like the `ctx` attribute of a component, see `visit_mut_jsx_opening_element`.
  keep_object_lit_ctx: bool,

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
    let callee = Box::new(Expr::Ident(self.ident(callee)));
    self.call_expr(callee, args)
  }

  fn call_expr(
    &mut self,
    callee: Box<Expr>,
    args: impl IntoIterator<Item = Box<Expr>>,
  ) -> Box<Expr> {
    Box::new(Expr::Call(CallExpr {
      span: Default::default(),
      callee: Callee::Expr(callee),
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

    let (props_ident, rest_param) = self.component.as_mut().map_or((None, false), |component| {
      component.used = true;
      (component.props_ident.clone(), component.rest_param)
    });

    props_ident.map(|ident| {
      let mut props = Box::new(Expr::Ident(ident));
      if rest_param {
        props = Box::new(Expr::Member(MemberExpr {
          span: Default::default(),
          obj: props,
          prop: MemberProp::Computed(ComputedPropName {
            span: Default::default(),
            expr: Box::new(Expr::Lit(Lit::Num(Number::from(0.0)))),
          }),
        }));
      }
      self.member(props, "ctx")
    })
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

          // `param()` returns the decoded value, which must be encoded again for the url
          let req = self.member(ctx_expr, "req");
          let param_fn = self.member(req, "param");
          let param_lit = self.str_lit(&param.name);
          let value = Box::new(Expr::Call(CallExpr {
            span: Default::default(),
            callee: Callee::Expr(param_fn),
            args: vec![ExprOrSpread::from(param_lit)],
            type_args: None,
          }));
          exprs.push(if param.multi_segment {
            // `value.split("/").map(encodeURIComponent).join("/")` keeps the segment separators
            let slash = self.str_lit("/");
            let split_fn = self.member(value, "split");
            let segments = self.call_expr(split_fn, vec![slash.clone()]);
            let map_fn = self.member(segments, "map");
            let encode_fn = Box::new(Expr::Ident(self.ident("encodeURIComponent")));
            let encoded = self.call_expr(map_fn, vec![encode_fn]);
            let join_fn = self.member(encoded, "join");
            self.call_expr(join_fn, vec![slash])
          } else {
            self.call("encodeURIComponent", vec![value])
          });
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
    // that the variables captured by the nested ones are also captured by this one; the explicit
    // `ctx` of this element is for this action's url only, nested ones use this action's ctx param
    let explicit_action_ctx = self.explicit_action_ctx.take();
    self
      .action_frames
      .push(new_action_frame(value_node, self.atom_store));
    value_node.visit_mut_children_with(self);
    let frame = self.action_frames.pop().unwrap();
    self.explicit_action_ctx = explicit_action_ctx;
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

    self.check_arrow_handler_bindings(value_node, &event_handler_id);

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

  /// Reports an arrow function event handler that uses `this` or `arguments`, which arrow
  /// functions don't bind, so unlike in a `function` handler they never refer to the element or
  /// the handler's arguments; the hoisted handler gets those of the client script instead.
  fn check_arrow_handler_bindings(&mut self, handler: &Expr, event_handler_id: &str) {
    let Expr::Arrow(arrow) = handler else {
      return;
    };

    let mut finder = LexicalBindingFinder::default();
    arrow.params.visit_with(&mut finder);
    arrow.body.visit_with(&mut finder);
    for (used, binding, fix) in [
      (
        finder.this,
        "this",
        "use a `function` expression to get the element as `this`, or `event.currentTarget`",
      ),
      (
        finder.arguments,
        "arguments",
        "use a rest parameter, e.g. `(...args) => ...`",
      ),
    ] {
      if used {
        self.error(format!(
          "event handler {} is an arrow function that uses `{}`, which arrow functions don't bind; {}",
          event_handler_id, binding, fix,
        ));
      }
    }
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
  fn enter_function<'p>(&mut self, params: impl Iterator<Item = &'p Pat>) -> bool {
    if self.component.is_some()
      || !self.action_frames.is_empty()
      || !matches!(self.route_pathname, RoutePathname::Parameterized { .. })
    {
      return false;
    }

    let mut params = params.skip_while(|pat| is_this_param(pat));

    let (props_ident, rewrite_param, rest_param) = match params.next() {
      None => (None, false, false),
      Some(Pat::Ident(binding)) => (Some(binding.id.clone()), false, false),
      Some(Pat::Assign(AssignPat { left, .. })) if left.is_ident() => {
        (Some(left.as_ident().unwrap().id.clone()), false, false)
      }
      Some(Pat::Rest(RestPat { arg, .. })) => match arg.as_ref() {
        Pat::Ident(binding) => (Some(binding.id.clone()), false, true),
        _ => (Some(self.ident(PROPS_IDENT)), true, true),
      },
      Some(_) => (Some(self.ident(PROPS_IDENT)), true, false),
    };

    self.component = Some(ComponentFrame {
      props_ident,
      rewrite_param,
      rest_param,
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
    Expr::Fn(FnExpr { function, .. }) => function
      .params
      .iter()
      .map(|param| &param.pat)
      .find(|pat| !is_this_param(pat)),
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
      function
        .params
        .iter()
        .take_while(|param| is_this_param(&param.pat))
        .count(),
      Param {
        span: Default::default(),
        decorators: vec![],
        pat,
      },
    ),
    _ => {}
  }
}

/// Returns whether `pat` is a typescript `this` param, which only declares the type of `this` and
/// is not one of the function's params.
fn is_this_param(pat: &Pat) -> bool {
  matches!(pat, Pat::Ident(binding) if binding.id.sym == "this")
}

/// Replaces a destructured props param with `props_ident` and moves the destructuring into the
/// function body, so that `props_ident.ctx` can be referenced.
fn replace_props_param(param: &mut Pat, body: &mut BlockStmt, props_ident: Ident) {
  // `...pattern` becomes `...props_ident`
  let param = match param {
    Pat::Rest(RestPat { arg, .. }) => arg.as_mut(),
    param => param,
  };
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

/// Returns the htmx attribute name without the `data-` prefix, which htmx accepts on every attribute.
fn htmx_attr_name(name: &str) -> &str {
  name.strip_prefix("data-").unwrap_or(name)
}

fn is_event_handler_attr(name: &str) -> bool {
  let name = htmx_attr_name(name);
  name.starts_with("hx-on:") || name.starts_with("hx-on-")
}

fn form_action_method(name: &str) -> Option<FormActionMethod> {
  match htmx_attr_name(name) {
    "hx-get" => Some(FormActionMethod::Get),
    "hx-post" => Some(FormActionMethod::Post),
    "hx-put" => Some(FormActionMethod::Put),
    "hx-delete" => Some(FormActionMethod::Delete),
    "hx-patch" => Some(FormActionMethod::Patch),
    _ => None,
  }
}

/// Returns whether the element is an html element rather than a component, by the same rule as
/// the jsx transform.
fn is_intrinsic_element(name: &JSXElementName) -> bool {
  match name {
    JSXElementName::Ident(ident) => ident.sym.starts_with(|c: char| c.is_ascii_lowercase()),
    JSXElementName::JSXNamespacedName(_) => true,
    JSXElementName::JSXMemberExpr(_) => false,
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
      let param = func
        .params
        .iter_mut()
        .find(|param| !is_this_param(&param.pat))
        .unwrap();
      replace_props_param(&mut param.pat, func.body.as_mut().unwrap(), props_ident);
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
    let in_component_element = std::mem::replace(
      &mut self.in_component_element,
      !is_intrinsic_element(&node.name),
    );

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
        let JSXAttrOrSpread::JSXAttr(attr) = &node.attrs[index] else {
          unreachable!()
        };
        let explicit_ctx = Box::new(jsx_attr_expr(attr).unwrap().clone());
        // `ctx` would be rendered as an attribute of an intrinsic element, but a component may use
        // the prop itself
        if is_intrinsic_element(&node.name) {
          node.attrs.remove(index);
        }

        let previous_ctx = self.explicit_action_ctx.replace(explicit_ctx);
        node.visit_mut_children_with(self);
        self.explicit_action_ctx = previous_ctx;
        self.in_component_element = in_component_element;
        return;
      }
    }

    node.visit_mut_children_with(self);
    self.in_component_element = in_component_element;
  }

  fn visit_mut_jsx_attr_or_spread(&mut self, node: &mut JSXAttrOrSpread) {
    if let JSXAttrOrSpread::SpreadElement(spread) = node {
      // `{...{ ctx, "hx-post": fn }}` passes `ctx` to a component, like `ctx={...}` does
      self.keep_object_lit_ctx = self.in_component_element && spread.expr.is_object();
    }

    node.visit_mut_children_with(self);
  }

  fn visit_mut_object_lit(&mut self, node: &mut ObjectLit) {
    let keep_ctx = std::mem::take(&mut self.keep_object_lit_ctx);
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
      // `ctx: value` or the shorthand `ctx`
      let explicit_ctx_index = node.props.iter().position(|prop| match prop {
        PropOrSpread::Prop(prop) => match prop.as_ref() {
          Prop::KeyValue(prop) => prop_name(prop) == Some("ctx"),
          Prop::Shorthand(ident) => ident.sym == "ctx",
          _ => false,
        },
        PropOrSpread::Spread(_) => false,
      });

      if let Some(index) = explicit_ctx_index {
        // like the `ctx` attribute, it is kept for a component, which may use the prop itself
        let prop = if keep_ctx {
          node.props[index].clone()
        } else {
          node.props.remove(index)
        };
        let PropOrSpread::Prop(prop) = prop else {
          unreachable!()
        };
        let explicit_ctx = match *prop {
          Prop::KeyValue(prop) => prop.value,
          Prop::Shorthand(ident) => Box::new(Expr::Ident(ident)),
          _ => unreachable!(),
        };

        let previous_ctx = self.explicit_action_ctx.replace(explicit_ctx);
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

        if is_event_handler_attr(&name) {
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
      if is_event_handler_attr(&key) {
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

/// Finds the uses of `this` and `arguments` that refer to those of the enclosing scope, i.e. outside
/// of nested functions and class bodies, which bind their own.
#[derive(Default)]
struct LexicalBindingFinder {
  this: bool,
  arguments: bool,
}

impl Visit for LexicalBindingFinder {
  noop_visit_type!();

  fn visit_function(&mut self, _: &Function) {}

  // a computed key is evaluated in the enclosing scope, unlike the body
  fn visit_getter_prop(&mut self, node: &GetterProp) {
    node.key.visit_with(self);
  }

  fn visit_setter_prop(&mut self, node: &SetterProp) {
    node.key.visit_with(self);
  }

  fn visit_class(&mut self, node: &Class) {
    node.super_class.visit_with(self);
    for member in &node.body {
      match member {
        ClassMember::Method(method) => method.key.visit_with(self),
        ClassMember::ClassProp(prop) => prop.key.visit_with(self),
        ClassMember::AutoAccessor(accessor) => accessor.key.visit_with(self),
        _ => {}
      }
    }
  }

  fn visit_expr(&mut self, node: &Expr) {
    match node {
      Expr::This(_) => self.this = true,
      Expr::Ident(ident) if ident.sym == "arguments" => self.arguments = true,
      _ => node.visit_children_with(self),
    }
  }

  fn visit_prop(&mut self, node: &Prop) {
    match node {
      Prop::Shorthand(ident) if ident.sym == "arguments" => self.arguments = true,
      _ => node.visit_children_with(self),
    }
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

/// Collects the specifiers of the `import("...")` and `require("...")` calls in a node whose
/// argument is a string literal, or a template literal without expressions.
#[derive(Default)]
struct DynamicImportCollector {
  srcs: Vec<Atom>,
}

impl Visit for DynamicImportCollector {
  noop_visit_type!();

  fn visit_call_expr(&mut self, call: &CallExpr) {
    let imports = match &call.callee {
      Callee::Import(_) => true,
      Callee::Expr(callee) => matches!(&**callee, Expr::Ident(ident) if ident.sym == "require"),
      Callee::Super(_) => false,
    };
    if let (true, Some(ExprOrSpread { spread: None, expr })) = (imports, call.args.first()) {
      let src = match &**expr {
        Expr::Lit(Lit::Str(src)) => Some(&src.value),
        Expr::Tpl(Tpl { exprs, quasis, .. }) if exprs.is_empty() => {
          quasis.first().and_then(|quasi| quasi.cooked.as_ref())
        }
        _ => None,
      };
      if let Some(src) = src.filter(|src| !self.srcs.contains(src)) {
        self.srcs.push(src.clone());
      }
    }
    call.visit_children_with(self);
  }
}

fn dynamic_imports<N: VisitWith<DynamicImportCollector> + ?Sized>(node: &N) -> Vec<Atom> {
  let mut imports = DynamicImportCollector::default();
  node.visit_with(&mut imports);
  imports.srcs
}

/// A top level declaration, import specifier or var declarator that a handler slice may keep.
struct SliceNode {
  /// Index of the module item.
  item: usize,
  /// Index of the import specifier or var declarator within the module item.
  part: Option<usize>,
  /// Identifiers referenced by the node, excluding those in types.
  refs: Vec<Id>,
  /// Specifiers of the dynamic imports and `require` calls in the node, see
  /// [`DynamicImportCollector`].
  dynamic_imports: Vec<Atom>,
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
    self.add_code(item, part, binds, refs, vec![]);
  }

  /// Adds a node with the dynamic imports in its code, see [`dynamic_imports`].
  fn add_code(
    &mut self,
    item: usize,
    part: Option<usize>,
    binds: Vec<Id>,
    refs: Vec<Id>,
    dynamic_imports: Vec<Atom>,
  ) {
    let node = self.nodes.len();
    self.nodes.push(SliceNode {
      item,
      part,
      refs,
      dynamic_imports,
    });
    for id in binds {
      self.bindings.entry(id).or_default().push(node);
    }
  }

  fn add_decl(&mut self, item: usize, decl: &Decl) {
    match decl {
      Decl::Fn(f) => self.add_code(
        item,
        None,
        vec![f.ident.to_id()],
        ident_refs(f),
        dynamic_imports(f),
      ),
      Decl::Class(c) => self.add_code(
        item,
        None,
        vec![c.ident.to_id()],
        ident_refs(c),
        dynamic_imports(c),
      ),
      Decl::Var(var) => {
        for (i, declarator) in var.decls.iter().enumerate() {
          let mut binds = vec![];
          pat_binding_idents(&declarator.name, &mut binds);
          self.add_code(
            item,
            Some(i),
            binds.iter().map(Ident::to_id).collect(),
            ident_refs(declarator),
            dynamic_imports(declarator),
          );
        }
      }
      Decl::TsEnum(e) => self.add_code(
        item,
        None,
        vec![e.id.to_id()],
        ident_refs(e),
        dynamic_imports(e),
      ),
      Decl::TsModule(m) => {
        if let TsModuleName::Ident(id) = &m.id {
          self.add_code(
            item,
            None,
            vec![id.to_id()],
            ident_refs(m),
            dynamic_imports(m),
          );
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
/// Kept imports, including the dynamic imports and `require` calls in the kept code (see
/// [`DynamicImportCollector`]), whose specifier matches one of `banned_imports` are reported as
/// errors.
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
  // the dynamic imports in the event handlers, by root
  let mut handler_dynamic_imports = vec![];
  for (i, item) in swc_module.body.iter().enumerate() {
    if let Some(declarator) = handler_export(item) {
      handler_dynamic_imports.push((roots.len(), dynamic_imports(item)));
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
        }) => graph.add_code(
          i,
          None,
          vec![ident.to_id()],
          ident_refs(&export.decl),
          dynamic_imports(&export.decl),
        ),
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

  let root_name = |root: usize| match &roots[root] {
    SliceRoot::Handler(export_name) => export_name.to_string(),
    SliceRoot::OnLoad => ON_LOAD_EXPORT.to_string(),
  };

  let chain = |node: usize| {
    let mut names = vec![];
    let mut current = Some(node);
    while let Some(node) = current {
      let reach = reached[node].as_ref().unwrap();
      names.extend(reach.via.as_ref().map(Atom::to_string));
      current = reach.from;
    }
    let root = reached[node].as_ref().unwrap().root;
    names.push(root_name(root));
    names.reverse();
    (root, names.join(" → "))
  };

  let banned = |src: &str| banned_imports.iter().any(|pattern| pattern.is_match(src));
  let banned_error = |root: usize, src: &str, chain: &str| {
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
    format!(
      "{} imports banned module {} via {}",
      exporter,
      serde_json::to_string(src).unwrap(),
      chain,
    )
  };

  let mut errors = vec![];
  for (root, srcs) in &handler_dynamic_imports {
    for src in srcs.iter().filter(|src| banned(src)) {
      errors.push(banned_error(*root, src, &root_name(*root)));
    }
  }
  let mut reported = HashSet::new();
  for (node, reach) in reached.iter().enumerate() {
    if reach.is_none() {
      continue;
    }
    for src in graph.nodes[node]
      .dynamic_imports
      .iter()
      .filter(|src| banned(src))
    {
      let (root, chain) = chain(node);
      errors.push(banned_error(root, src, &chain));
    }

    let item = graph.nodes[node].item;
    if reported.contains(&item) {
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
    if banned(src) {
      reported.insert(item);
      let (root, chain) = chain(node);
      errors.push(banned_error(root, src, &chain));
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
  fn passes_explicit_action_context_on_to_components() {
    let result = compact(&transform(
      r#"
        export default function Component(props) {
          const rc = props.context;
          return <>
            <LikeButton ctx={rc} hx-post={() => null} />
            <ui.Button ctx={rc} hx-post={() => null} />
            <svg:a ctx={rc} hx-post={() => null} />
          </>;
        }
      "#,
    ));

    assert!(result.contains(
      "<LikeButton ctx={rc} hx-post={`/article/${encodeURIComponent(rc.req.param(\"id\"))}?action="
    ));
    assert!(result.contains(
      "<ui.Button ctx={rc} hx-post={`/article/${encodeURIComponent(rc.req.param(\"id\"))}?action="
    ));
    assert!(result
      .contains("<svg:a hx-post={`/article/${encodeURIComponent(rc.req.param(\"id\"))}?action="));
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
  fn rejects_this_and_arguments_in_arrow_event_handlers() {
    let err = try_transform(
      "/x",
      r#"
        export default function C() {
          return (
            <div>
              <button hx-on:click={() => { this.classList.toggle("on"); }} />
              <button hx-on-click={(e) => [1].map(() => console.log(arguments.length, e))} />
              <button hx-on:load={(e = this) => e} />
              <button {...{ "hx-on:click": async () => this.remove() }} />
              <button hx-on:click={() => class { [this.id] = 1; static [arguments[0]]() {} }} />
              <button hx-on:click={() => ({ get [this.id]() { return 1; } })} />
              <button hx-on:click={() => ({ set [arguments[0]](v) {} })} />
            </div>
          );
        }
      "#,
    )
    .unwrap_err();

    let errors = err.lines().collect::<Vec<_>>();
    assert_eq!(errors.len(), 8, "{}", err);
    assert_eq!(
      errors[0],
      "src/routes/article/[id]/index.tsx: event handler src/routes/article/[id]/index.tsx@0 is an arrow function that uses `this`, which arrow functions don't bind; use a `function` expression to get the element as `this`, or `event.currentTarget`"
    );
    assert!(errors[1].contains("@1 is an arrow function that uses `arguments`"));
    assert!(errors[2].contains("@2 is an arrow function that uses `this`"));
    assert!(errors[3].contains("@3 is an arrow function that uses `this`"));
    // computed keys of class members and accessors are evaluated in the enclosing scope
    assert!(errors[4].contains("@4 is an arrow function that uses `this`"));
    assert!(errors[5].contains("@4 is an arrow function that uses `arguments`"));
    assert!(errors[6].contains("@5 is an arrow function that uses `this`"));
    assert!(errors[7].contains("@6 is an arrow function that uses `arguments`"));

    // `function` handlers, and functions, methods and classes nested in arrow handlers, bind their own
    transform_at(
      "/x",
      r#"
        export default function C() {
          return (
            <div>
              <button hx-on:click={function () { this.classList.toggle(arguments[0].type); }} />
              <button hx-on:click={() => [1].forEach(function () { console.log(this, arguments); })} />
              <button hx-on:click={() => ({ m() { return this; }, get g() { return this; } })} />
              <button hx-on:click={() => class { f = this; m() { return arguments; } }} />
              <button hx-on:click={(e) => e.currentTarget.arguments} />
            </div>
          );
        }
      "#,
    );
  }

  #[test]
  fn transforms_data_prefixed_attributes() {
    let result = compact(&transform_at(
      "/x",
      r#"
        export default function C({ msg }) {
          return (
            <form data-hx-post={() => msg} ctx={c}>
              <button data-hx-on:click={() => alert(msg)} data-hx-on-mouseover={() => 1} />
              <i {...{ "data-hx-delete": () => 2, "data-hx-on:load": () => 3 }} />
            </form>
          );
        }
      "#,
    ));

    assert!(result.contains(
      "data-hx-post={`/x?action=src%2Froutes%2Farticle%2F%5Bid%5D%2Findex.tsx%400${actionScope("
    ));
    assert!(!result.contains("ctx={c}"));
    assert!(result.contains(
      r#"data-hx-on:click={`$handlers["src/routes/article/[id]/index.tsx@0"](${handlerScope({ msg })}).call(this, event)`}"#
    ));
    assert!(result.contains(
      r#"data-hx-on-mouseover={'$handlers["src/routes/article/[id]/index.tsx@1"].call(this, event)'}"#
    ));
    assert!(result.contains(
      r#""data-hx-delete": `/x?action=src%2Froutes%2Farticle%2F%5Bid%5D%2Findex.tsx%401`"#
    ));
    assert!(result.contains(
      r#""data-hx-on:load": '$handlers["src/routes/article/[id]/index.tsx@2"].call(this, event)'"#
    ));
    assert!(result.contains("_action$post$0 = wrapActionWithPayload("));
    assert!(result.contains("_action$delete$1 = wrapActionWithPayload("));
    assert!(result.contains("export const _handler$1 = ()=>1;"));

    // only one `data-` prefix, and only on htmx attributes
    let result = compact(&transform_at(
      "/x",
      r#"<a data-data-hx-get={() => 1} data-on:click={() => 2} />"#,
    ));
    assert!(result.contains("data-data-hx-get={()=>1}"));
    assert!(result.contains("data-on:click={()=>2}"));
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
    assert!(result.contains("({ a })=>(__recooler_ctx)=><i hx-post={`/article/${encodeURIComponent(__recooler_ctx.req.param(\"id\"))}?action="));
    assert!(!result.contains("{ a, props }"));
  }

  #[test]
  fn nested_actions_do_not_use_outer_explicit_context() {
    let result = compact(&transform(
      r#"
        export default function C(props) {
          const rc = props.ctx;
          return <form ctx={rc} hx-post={async (ctx) => <button hx-post={() => 1} />} />;
        }
      "#,
    ));

    // the outer action's url uses `rc`, the inner one the outer action's ctx param, so the outer
    // action does not capture the hono context
    assert!(result
      .contains("<form hx-post={`/article/${encodeURIComponent(rc.req.param(\"id\"))}?action="));
    assert!(result.contains("async (ctx)=><button hx-post={`/article/${encodeURIComponent(ctx.req.param(\"id\"))}?action="));
    assert!(!result.contains("{ rc }"));
    assert!(!result.contains("actionScope("));
  }

  #[test]
  fn encodes_route_params_in_action_urls() {
    let source = r#"
      export default function C(props) {
        return <button hx-post={() => 1} />;
      }
    "#;

    let result = compact(&transform_at("/a/:id/b/:slug", source));
    assert!(result.contains(
      "`/a/${encodeURIComponent(props.ctx.req.param(\"id\"))}/b/${encodeURIComponent(props.ctx.req.param(\"slug\"))}?action="
    ));

    // the separators of a catch-all param are kept, its segments are encoded
    let result = compact(&transform_at("/files/:rest{.+}", source));
    assert!(result.contains(
      "`/files/${props.ctx.req.param(\"rest\").split(\"/\").map(encodeURIComponent).join(\"/\")}?action="
    ));
  }

  #[test]
  fn reads_route_params_with_any_name() {
    let result = compact(&transform_at(
      "/u/:user-id/:a.b/x",
      r#"
        export default function C(props) {
          return <button hx-post={() => 1} />;
        }
      "#,
    ));

    assert!(result.contains(
      "`/u/${encodeURIComponent(props.ctx.req.param(\"user-id\"))}/${encodeURIComponent(props.ctx.req.param(\"a.b\"))}/x?action="
    ));
  }

  #[test]
  fn uses_explicit_action_context_from_object_props() {
    for props in [
      r#"{ ctx, "hx-post": () => 1 }"#,
      r#"{ "hx-post": () => 1, ctx: ctx }"#,
    ] {
      let result = compact(&transform(&format!(
        r#"
          export default function C(props) {{
            const ctx = props.rc;
            return <button {{...{}}} />;
          }}
        "#,
        props,
      )));

      assert!(
        result.contains(
          "{...{ \"hx-post\": `/article/${encodeURIComponent(ctx.req.param(\"id\"))}?action="
        ),
        "{}",
        result
      );
      assert!(!result.contains("props.ctx"), "{}", result);
    }
  }

  #[test]
  fn keeps_explicit_action_context_in_object_props_of_components() {
    let result = compact(&transform(
      r#"
        export default function C(props) {
          const rc = props.rc;
          return (
            <>
              <Btn {...{ "hx-post": () => 1, ctx: rc }} />
              <Btn {...{ ctx, "hx-post": () => 2 }} />
              <a {...{ ctx: rc, "hx-post": () => 3 }} />
              <Btn {...props} {...{ x: { ctx: rc, "hx-post": () => 4 } }} />
            </>
          );
        }
      "#,
    ));

    // used for the urls, and still passed to the components
    assert!(
      result.contains("<Btn {...{ \"hx-post\": `/article/${encodeURIComponent(rc.req.param(\"id\"))}?action=src%2Froutes%2Farticle%2F%5Bid%5D%2Findex.tsx%400`, ctx: rc }}/>"),
      "{}",
      result
    );
    assert!(
      result.contains("<Btn {...{ ctx, \"hx-post\": `/article/${encodeURIComponent(ctx.req.param(\"id\"))}?action="),
      "{}",
      result
    );
    // but not rendered as an attribute of an html element
    assert!(
      result.contains("<a {...{ \"hx-post\": `/article/${encodeURIComponent(rc.req.param(\"id\"))}?action="),
      "{}",
      result
    );
    // nor kept in an object that isn't itself the spread props
    assert!(
      result.contains("x: { \"hx-post\": `/article/${encodeURIComponent(rc.req.param(\"id\"))}?action="),
      "{}",
      result
    );
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
  fn reads_context_from_rest_props_param() {
    let result = compact(&transform(
      r#"
        export default function C(...args) {
          return <button hx-post={() => 1} />;
        }
      "#,
    ));
    assert!(result.contains("function C(...args) { return <button hx-post={`/article/${encodeURIComponent(args[0].ctx.req.param(\"id\"))}?action="));

    let result = compact(&transform(
      r#"
        export default (...[{ id }]: [RouteProps]) => <button hx-post={() => id} />;
      "#,
    ));
    assert!(result.contains("(...__recooler_props: [RouteProps])=>{ let [{ id }] = __recooler_props; return <button hx-post={`/article/${encodeURIComponent(__recooler_props[0].ctx.req.param(\"id\"))}?action="));
  }

  #[test]
  fn nested_actions_use_their_own_frame_or_explicit_context() {
    let result = compact(&transform(
      r#"
        export default function C(props) {
          return <>
            <a ctx={props.ctx} hx-post={() => <b hx-post={() => <i hx-post={() => 1} />} />} />
            <p hx-post={(c1) => <u ctx={c1} hx-post={(c2) => <s hx-post={() => 2} />} />} />
          </>;
        }
      "#,
    ));

    // three levels: each nested action gets a ctx param for the url of the one it returns, and
    // none of them captures `props`
    assert!(result.contains(
      "(__recooler_ctx)=><b hx-post={`/article/${encodeURIComponent(__recooler_ctx.req.param(\"id\"))}?action="
    ), "{}", result);
    assert!(result.contains(
      "(__recooler_ctx)=><i hx-post={`/article/${encodeURIComponent(__recooler_ctx.req.param(\"id\"))}?action="
    ), "{}", result);
    // an explicit ctx inside an action is used for its element only, the action nested in it
    // uses its own ctx param
    assert!(
      result.contains(
        "(c1)=><u hx-post={`/article/${encodeURIComponent(c1.req.param(\"id\"))}?action="
      ),
      "{}",
      result
    );
    assert!(
      result.contains(
        "(c2)=><s hx-post={`/article/${encodeURIComponent(c2.req.param(\"id\"))}?action="
      ),
      "{}",
      result
    );
    assert!(!result.contains("actionScope("), "{}", result);
  }

  #[test]
  fn reads_route_params_with_symbols_and_unicode_in_names() {
    let result = compact(&transform_at(
      "/n/:a$b/:口/:rest{.+}",
      r#"
        export default function C(props) {
          return <button hx-post={() => 1} />;
        }
      "#,
    ));

    assert!(result.contains(
      "`/n/${encodeURIComponent(props.ctx.req.param(\"a$b\"))}/${encodeURIComponent(props.ctx.req.param(\"口\"))}/${props.ctx.req.param(\"rest\").split(\"/\").map(encodeURIComponent).join(\"/\")}?action="
    ), "{}", result);
  }

  #[test]
  fn skips_typescript_this_param_for_context() {
    let result = compact(&transform(
      r#"
        export default function C(this: unknown, { id }: RouteProps) {
          return <button hx-post={function (this: unknown) {
            return <button hx-post={() => id} />;
          }} />;
        }
      "#,
    ));

    assert!(
      result.contains(
        "function C(this: unknown, __recooler_props: RouteProps) { let { id } = __recooler_props;"
      ),
      "{}",
      result
    );
    assert!(
      result.contains("encodeURIComponent(__recooler_props.ctx.req.param(\"id\"))"),
      "{}",
      result
    );
    assert!(
      result.contains("function(this: unknown, __recooler_ctx) {"),
      "{}",
      result
    );
    assert!(
      result.contains("encodeURIComponent(__recooler_ctx.req.param(\"id\"))"),
      "{}",
      result
    );
    assert!(
      !result.contains("this.ctx") && !result.contains("this.req"),
      "{}",
      result
    );
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

  #[test]
  fn reports_banned_dynamic_imports_and_requires() {
    let source = r#"
      async function load() {
        const { query } = await import("../lib/fql");
        return query();
      }
      const legacy = () => require(`faunadb`);
      export default function C({ name }) {
        import("../lib/server-only");
        return <>
          <button hx-on:click={() => import("faunadb")} />
          <button hx-on:click={() => load()} />
          <button hx-on:click={() => legacy()} />
          <button hx-on:click={() => import(name)} />
        </>;
      }
    "#;

    let err = try_slice(source, &["/fql$", "^faunadb$", "server-only"]).unwrap_err();
    assert_eq!(
      err.lines().collect::<Vec<_>>(),
      vec![
        r#"event handler src/routes/article/[id]/index.tsx@0 imports banned module "faunadb" via _handler$0"#,
        r#"event handler src/routes/article/[id]/index.tsx@1 imports banned module "../lib/fql" via _handler$1 → load"#,
        r#"event handler src/routes/article/[id]/index.tsx@2 imports banned module "faunadb" via _handler$2 → legacy"#,
      ]
    );

    // the dynamic imports of the dropped component are not reported
    assert!(try_slice(source, &["server-only"]).is_ok());

    let err = try_slice(
      r#"
        export async function onLoad() {
          await import("../lib/fql");
        }
      "#,
      &["/fql$"],
    )
    .unwrap_err();
    assert_eq!(
      err,
      r#"onLoad of src/routes/article/[id]/index.tsx imports banned module "../lib/fql" via onLoad"#
    );
  }
  #[test]
  fn ignores_dynamic_imports_without_a_static_specifier_or_outside_the_slice() {
    let source = r#"
      const lazy = (name) => import(`../lib/${name}`);
      const resolve = () => require.resolve("faunadb");
      function unused() { return import("faunadb"); }
      const api = { load() { return import(`faunadb`); } };
      export default function C() {
        return <>
          <button hx-on:click={() => lazy("fql")} />
          <button hx-on:click={() => resolve()} />
          <button hx-on:click={() => api.load()} />
        </>;
      }
    "#;

    // only the template literal without expressions in a method of a kept object
    let err = try_slice(source, &["faunadb", "fql"]).unwrap_err();
    assert_eq!(
      err,
      r#"event handler src/routes/article/[id]/index.tsx@2 imports banned module "faunadb" via _handler$2 → api"#
    );
  }
}
