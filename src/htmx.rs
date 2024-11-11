use std::collections::HashSet;

use farmfe_core::{
  module::ModuleId,
  regex::Regex,
  swc_common::util::take::Take,
  swc_ecma_ast::{
    BindingIdent, CallExpr, Callee, Decl, ExportDecl, Expr, ExprOrSpread, Ident, KeyValueProp, Lit,
    MemberExpr, MemberProp, ModuleDecl, ModuleItem, ObjectLit, Pat, PropOrSpread, Tpl, TplElement,
    VarDecl, VarDeclKind, VarDeclarator,
  },
};
use farmfe_toolkit::swc_ecma_visit::{visit_module, visit_mut_module, Visit, VisitMut};
use lazy_static::lazy_static;
use swc_atoms::AtomStore;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum RequestHandlerMethod {
  Get,
  Post,
  Put,
  Delete,
  Patch,
  Options,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub(crate) enum FormActionMethod {
  Get,
  Post,
  Put,
  Delete,
  Patch,
}

lazy_static! {
  static ref ACTION_REPLACEMENT_STRING_PATTERN: Regex =
    Regex::new("^\\?action=(.+)&scope=$").unwrap();
  static ref HANDLER_REPLACEMENT_STRING_PATTERN: Regex =
    Regex::new("^\\$handlers\\[(\".+\")\\]\\.call\\(this, event\\)$").unwrap();
}

const ACTION_DECL_PREFIX: &str = "_action$";
const EVENT_DECL_PREFIX: &str = "_handler$";

struct LocalHandlerIdentGenerator {
  action_counter: usize,
  event_counter: usize,
}

impl Default for LocalHandlerIdentGenerator {
  fn default() -> Self {
    Self {
      action_counter: 0,
      event_counter: 0,
    }
  }
}

impl LocalHandlerIdentGenerator {
  fn next_action_ident(&mut self, method: FormActionMethod) -> String {
    let export_name = format!(
      "{}{}${}",
      ACTION_DECL_PREFIX,
      format!("{:?}", method).to_lowercase(),
      self.action_counter
    );
    self.action_counter += 1;
    export_name
  }

  fn next_event_ident(&mut self) -> String {
    let export_name = format!("{}{}", EVENT_DECL_PREFIX, self.event_counter);
    self.event_counter += 1;
    export_name
  }
}

struct ComponentHandlerValidationVisitor<'a> {
  module_id: &'a ModuleId,
  global_ids: &'a mut crate::GlobalIds,
  local_idents: LocalHandlerIdentGenerator,
  exported_handlers: HashSet<String>,
}

/// Ensure that all event handlers are registered.
pub(crate) fn ensure_registered_handlers(
  module_id: &ModuleId,
  global_ids: &mut crate::GlobalIds,
  swc_module: &farmfe_core::swc_ecma_ast::Module,
) {
  let mut exported_handlers = HashSet::<String>::new();
  for item in &swc_module.body {
    if let ModuleItem::ModuleDecl(decl) = item {
      if let ModuleDecl::ExportDecl(export_decl) = decl {
        if let Decl::Var(var_decl) = &export_decl.decl {
          if var_decl.kind == VarDeclKind::Const && var_decl.decls.len() == 1 {
            if let Pat::Ident(ident) = &var_decl.decls[0].name {
              let name = ident.id.sym.to_string();
              if name.starts_with(ACTION_DECL_PREFIX) || name.starts_with(EVENT_DECL_PREFIX) {
                exported_handlers.insert(name);
              }
            }
          }
        }
      }
    }
  }

  let mut visitor = ComponentHandlerValidationVisitor {
    module_id,
    global_ids,
    local_idents: Default::default(),
    exported_handlers,
  };

  visit_module(&mut visitor, swc_module);
}

impl<'a> ComponentHandlerValidationVisitor<'a> {
  fn validate_form_action(&mut self, method: FormActionMethod, node: &KeyValueProp) {
    let export_name = self.local_idents.next_action_ident(method);
    if !self.exported_handlers.contains(&export_name) {
      panic!(
        "unmatched form action export name in module: {}",
        export_name
      );
    }

    let form_action_id = self
      .global_ids
      .form_action_ids
      .generate_id(self.module_id, &export_name);

    if let Some(existing_method) = self.global_ids.action_method_map.get(&form_action_id) {
      std::assert!(*existing_method == method);
    } else {
      println!(
        "adding into action_method_map: {} {:?} {}",
        &form_action_id,
        method,
        self.module_id.relative_path()
      );

      self
        .global_ids
        .action_method_map
        .insert(form_action_id.clone(), method);
    }

    if let Expr::Tpl(tpl) = node.value.as_ref() {
      let s = tpl.quasis[0].raw.as_str();
      if let Some(captures) = ACTION_REPLACEMENT_STRING_PATTERN.captures(s) {
        let (_, [expected_form_action_id_urlencoded]) = captures.extract();
        if let Ok(expected_form_action_id) = urlencoding::decode(expected_form_action_id_urlencoded)
        {
          if form_action_id != expected_form_action_id {
            panic!(
              "unexpected form action id: expected {}, but found {}",
              expected_form_action_id, form_action_id,
            );
          }
        } else {
          panic!("unexpected form action id urlencoded: {}", s);
        }
      } else {
        panic!("unexpected form action string pattern: {}", s);
      }
    } else {
      panic!("unexpected form action value type");
    }

    println!(
      "validated form action {} {} {}",
      self.module_id.relative_path(),
      export_name,
      form_action_id
    );
  }

  fn validate_event_handler(&mut self, node: &KeyValueProp) {
    let export_name = self.local_idents.next_event_ident();
    if !self.exported_handlers.contains(&export_name) {
      panic!(
        "unmatched event handler export name in module: {}",
        export_name
      );
    }

    let event_handler_id = self
      .global_ids
      .event_handler_ids
      .generate_id(self.module_id, &export_name);

    if let Expr::Lit(Lit::Str(lit)) = node.value.as_ref() {
      let s = lit.value.as_str();
      if let Some(captures) = HANDLER_REPLACEMENT_STRING_PATTERN.captures(s) {
        let (_, [expected_event_handler_id_json]) = captures.extract();
        if let Ok(expected_event_handler_id) =
          serde_json::from_str::<String>(expected_event_handler_id_json)
        {
          if event_handler_id != expected_event_handler_id {
            panic!(
              "unexpected event handler id: expected {}, but found {} ({})",
              expected_event_handler_id, event_handler_id, expected_event_handler_id_json,
            );
          }
        } else {
          panic!(
            "unexpected event handler id json: {}",
            expected_event_handler_id_json
          );
        }
      } else {
        panic!("unexpected event handler string pattern: {}", s);
      };
    } else {
      panic!("unexpected event handler value type");
    }

    println!(
      "validated event handler {} {} {}",
      self.module_id.relative_path(),
      export_name,
      event_handler_id
    );
  }
}

impl<'a> Visit for ComponentHandlerValidationVisitor<'a> {
  fn visit_key_value_prop(&mut self, node: &KeyValueProp) {
    // println!("called visit_mut_key_value_prop {:?}", node.key);
    if !node.key.is_str() {
      return;
    }

    let key = node.key.as_str().unwrap().value.as_str();
    if key.starts_with("hx-on:") {
      self.validate_event_handler(node);
      return;
    }

    if key.starts_with("hx-") {
      macro_rules! form_action {
        ($method:ident) => {
          if &key[3..]
            == const_format::map_ascii_case!(const_format::Case::Lower, stringify!($method))
          {
            self.validate_form_action(FormActionMethod::$method, node);
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

pub(crate) fn transform_module(
  module_id: &ModuleId,
  global_ids: &mut crate::GlobalIds,
  atom_store: &mut AtomStore,
  swc_module: &mut farmfe_core::swc_ecma_ast::Module,
) {
  let mut visitor = ComponentTransformVisitor {
    module_id,
    atom_store,
    global_ids,
    idents: Default::default(),
    new_decls: Default::default(),
  };

  visit_mut_module(&mut visitor, swc_module);

  for decl in visitor.new_decls {
    swc_module
      .body
      .push(ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
        decl,
        span: Take::dummy(),
      })));
  }
}

struct ComponentTransformVisitor<'a> {
  module_id: &'a ModuleId,
  atom_store: &'a mut AtomStore,
  global_ids: &'a mut crate::GlobalIds,

  idents: LocalHandlerIdentGenerator,

  new_decls: Vec<Decl>,
}

impl<'a> ComponentTransformVisitor<'a> {
  fn add_action(&mut self, method: FormActionMethod, node: &mut KeyValueProp) {
    println!("called add_action");
    let export_name = self.idents.next_action_ident(method);
    let form_action_id = self
      .global_ids
      .form_action_ids
      .generate_id(self.module_id, &export_name);

    if let Some(existing_method) = self.global_ids.action_method_map.get(&form_action_id) {
      std::assert!(*existing_method == method);
    } else {
      println!(
        "adding into action_method_map: {} {:?} {}",
        &form_action_id,
        method,
        self.module_id.relative_path()
      );

      self
        .global_ids
        .action_method_map
        .insert(form_action_id.clone(), method);
    }

    let scope_props = Vec::<PropOrSpread>::new();
    // TODO: populate scope

    let mut replacement = Box::new(Expr::Tpl(Tpl {
      quasis: vec![
        TplElement {
          raw: format!(
            "?action={}&scope=",
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
          callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
            obj: Box::new(Expr::Ident(Ident::from("JSON"))),
            prop: MemberProp::Ident(Ident::from("stringify")),
            ..Take::dummy()
          }))),
          args: vec![ExprOrSpread::from(Box::new(Expr::Object(ObjectLit {
            props: scope_props,
            ..Take::dummy()
          })))],
          ..Take::dummy()
        }))],
        ..Take::dummy()
      }))],
      ..Take::dummy()
    }));

    std::mem::swap(&mut node.value, &mut replacement);

    self.new_decls.push(Decl::Var(Box::new(VarDecl {
      kind: VarDeclKind::Const,
      decls: vec![VarDeclarator {
        name: Pat::Ident(BindingIdent {
          id: Ident::new(self.atom_store.atom(export_name), Take::dummy()),
          type_ann: None,
        }),
        init: Some(replacement),
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
          id: Ident::new(self.atom_store.atom(export_name), Take::dummy()),
          type_ann: None,
        }),
        init: Some(replacement),
        ..Take::dummy()
      }],
      ..Take::dummy()
    })));
  }
}

impl<'a> VisitMut for ComponentTransformVisitor<'a> {
  fn visit_mut_key_value_prop(&mut self, node: &mut KeyValueProp) {
    // println!("called visit_mut_key_value_prop {:?}", node.key);
    if !node.key.is_str() {
      return;
    }

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
}
