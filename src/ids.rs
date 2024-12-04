use std::collections::{HashMap, HashSet};

use farmfe_core::{config::Mode, module::ModuleId};

use crate::{htmx::FormActionMethod, routes::ModuleExport};

pub(crate) struct GlobalIdGenerator {
  module_name_id_map: HashMap<ModuleId, HashMap<String, String>>,
  id_module_map: HashMap<String, ModuleExport>,
}

impl GlobalIdGenerator {
  pub(crate) fn generate_id(&mut self, module_id: &ModuleId, export_name: &String) -> String {
    if !self.module_name_id_map.contains_key(module_id) {
      self
        .module_name_id_map
        .insert(module_id.clone(), Default::default());
    }

    let name_id_map = self.module_name_id_map.get_mut(module_id).unwrap();
    if let Some(existing_id) = name_id_map.get(export_name) {
      return existing_id.clone();
    }

    let id = format!(
      "{}@{}",
      module_id.id(Mode::Development),
      radix_fmt::radix_36(name_id_map.len())
    );
    name_id_map.insert(export_name.clone(), id.clone());
    self.id_module_map.insert(
      id.clone(),
      ModuleExport {
        module_id: module_id.clone(),
        export_name: export_name.clone(),
      },
    );

    id.clone()
  }

  /// for the given modules, return a map of ids to module exports
  pub(crate) fn combined_exports<'a, I: IntoIterator<Item = &'a ModuleId>>(
    &self,
    modules: I,
  ) -> HashMap<String, ModuleExport> {
    let mut exports = HashMap::<String, ModuleExport>::new();
    for module in modules {
      if let Some(name_id_map) = self.module_name_id_map.get(module) {
        for (export_name, id) in name_id_map {
          exports.insert(
            id.clone(),
            ModuleExport {
              module_id: module.clone(),
              export_name: export_name.clone(),
            },
          );
        }
      }
    }

    exports
  }

  pub(crate) fn id_export(&self, id: &String) -> Option<ModuleExport> {
    self.id_module_map.get(id).cloned()
  }
}

impl GlobalIdGenerator {
  pub(crate) fn new() -> Self {
    Self {
      module_name_id_map: Default::default(),
      id_module_map: Default::default(),
    }
  }
}

pub(crate) struct GlobalIds {
  pub(crate) form_action_ids: GlobalIdGenerator,
  pub(crate) action_method_map: HashMap<String, FormActionMethod>,
  pub(crate) event_handler_ids: GlobalIdGenerator,
}

impl GlobalIds {
  pub(crate) fn new() -> Self {
    Self {
      form_action_ids: GlobalIdGenerator::new(),
      action_method_map: Default::default(),
      event_handler_ids: GlobalIdGenerator::new(),
    }
  }

  pub(crate) fn handler_ids_for_modules<'a, I: Iterator<Item = &'a ModuleId>>(
    &self,
    modules: I,
  ) -> ComponentHandlerIds {
    let mut action_handlers = HashMap::<FormActionMethod, HashSet<String>>::new();
    let mut event_handlers = HashSet::<String>::new();

    let modules = modules.cloned().collect::<Vec<_>>();
    for (action_id, ..) in self.form_action_ids.combined_exports(&modules) {
      if let Some(method) = self.action_method_map.get(&action_id) {
        if !action_handlers.contains_key(method) {
          action_handlers.insert(*method, Default::default());
        }

        action_handlers.get_mut(method).unwrap().insert(action_id);
      }
    }

    for (handler_id, ..) in self.event_handler_ids.combined_exports(&modules) {
      event_handlers.insert(handler_id);
    }

    ComponentHandlerIds {
      action_handlers,
      event_handlers,
    }
  }
}

#[derive(Clone)]
pub(crate) struct ComponentHandlerIds {
  /// Array of action ids for each form action method
  pub(crate) action_handlers: HashMap<FormActionMethod, HashSet<String>>,

  /// Array of event ids that is used by a particular route and its dependencies
  pub(crate) event_handlers: HashSet<String>,
}

impl ComponentHandlerIds {
  pub(crate) fn merged(&self, other: &Self) -> Self {
    let mut clone = self.clone();
    for (method, actions) in &other.action_handlers {
      if let Some(existing) = clone.action_handlers.get_mut(method) {
        existing.extend(actions.iter().cloned());
      } else {
        clone.action_handlers.insert(*method, actions.clone());
      }
    }

    clone
      .event_handlers
      .extend(other.event_handlers.iter().cloned());

    clone
  }
}
