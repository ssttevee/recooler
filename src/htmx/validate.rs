use std::collections::HashSet;

use farmfe_core::{module::ModuleId, regex::Regex, serde_json, swc_ecma_ast::*};
use farmfe_toolkit::{
  lazy_static::lazy_static,
  swc_ecma_visit::{Visit, VisitWith},
};

use crate::es_ast_helpers::is_fn;

use super::{FormActionMethod, LocalHandlerIdentGenerator, ACTION_DECL_PREFIX, EVENT_DECL_PREFIX};

lazy_static! {
  static ref ACTION_REPLACEMENT_STRING_PATTERN: Regex =
    Regex::new("\\?action=(.+)&scope=$").unwrap();
  static ref HANDLER_REPLACEMENT_STRING_PATTERN: Regex =
    Regex::new("^\\$handlers\\[(\".+\")\\]\\.call\\(this, event\\)$").unwrap();
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

  swc_module.visit_with(&mut visitor);
}

impl<'a> ComponentHandlerValidationVisitor<'a> {
  fn validate_form_action(&mut self, method: FormActionMethod, node: &KeyValueProp) {
    let export_name = self.local_idents.next_action_ident(method);
    if !self.exported_handlers.contains(&export_name) {
      panic!(
        "unmatched form action export name in module {}: {}",
        self.module_id.relative_path(),
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

    // println!(
    //   "validated form action {} {} {}",
    //   self.module_id.relative_path(),
    //   export_name,
    //   form_action_id
    // );
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
    if key.starts_with("hx-on:") || key.starts_with("hx-on-") {
      self.validate_event_handler(node);
      return;
    }

    if key.starts_with("hx-") && is_fn(&*node.value) {
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
