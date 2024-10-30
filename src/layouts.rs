use std::{cell::RefCell, collections::HashMap, sync::Mutex};

use farmfe_core::{
  error::Result,
  module::{Module, ModuleId, ModuleMetaData},
};

use crate::{
  es_ast_helpers::{find_exports, find_head_export_type},
  head::HeadType,
  ids::ComponentHandlerIds,
  GlobalIds,
};

pub(crate) struct LayoutInfo {
  pub(crate) module_id: ModuleId,

  /// The component handlers of the layout.
  ///
  /// None implies that the module does not export a layout HOC.
  pub(crate) layout_handlers: Option<ComponentHandlerIds>,

  /// The head type if one is exported.
  pub(crate) head: Option<HeadType>,
}

pub(crate) struct LayoutTree {
  pub(crate) module: Option<LayoutInfo>,
  pub(crate) children: HashMap<String, LayoutTree>,
}

pub(crate) struct LayoutTreeRefItem<'a> {
  pub(crate) module_id: ModuleId,
  pub(crate) handlers: Option<&'a ComponentHandlerIds>,
  pub(crate) head: Option<HeadType>,
}

impl LayoutTree {
  pub(crate) fn new() -> Self {
    Self {
      module: None,
      children: HashMap::new(),
    }
  }

  pub(crate) fn get_layouts_for_path(&self, path: &String) -> Vec<LayoutTreeRefItem> {
    // println!("looking up layout modules for path {}", path);
    let mut modules = Vec::<LayoutTreeRefItem>::new();

    let mut node = self;

    let mut segment_iterator = path[1..].split("/");

    loop {
      if let Some(module) = &node.module {
        modules.push(LayoutTreeRefItem {
          module_id: module.module_id.clone(),
          handlers: module.layout_handlers.as_ref(),
          head: module.head,
        });
      }

      if node.children.len() == 0 {
        break;
      }

      match segment_iterator.next() {
        Some(segment) => {
          if let Some(next_node) = node.children.get(&segment.to_string()) {
            // println!("found child tree for segment {}", segment);
            node = next_node;
          } else {
            // the segment isn't found in the tree
            // println!("no child tree for segment {}", segment);
            break;
          }
        }
        None => {
          // end of path
          break;
        }
      }
    }

    modules.reverse();

    modules
  }

  pub(crate) fn add_layout(
    &mut self,
    path: String,
    ids_mutex: &Mutex<RefCell<GlobalIds>>,
    modules: Vec<Module>,
  ) -> Result<()> {
    let main_module = &modules[0];

    if let ModuleMetaData::Script(script) = main_module.meta.as_ref() {
      let mut node = self;
      for segment in path[1..].split("/") {
        if !node.children.contains_key(segment) {
          node.children.insert(segment.to_string(), LayoutTree::new());
        }

        node = node.children.get_mut(segment).unwrap();
      }

      let exports = find_exports(&main_module.id, script);
      assert!(node.module.is_none());
      let head = find_head_export_type(script);
      println!("inserting layout: {} {:?}", path, head);
      node.module = Some(LayoutInfo {
        module_id: main_module.id.clone(),
        layout_handlers: if exports.contains("default") {
          let ids_lock = ids_mutex.lock().unwrap();
          let handlers = ids_lock
            .borrow()
            .handler_ids_for_modules(modules.iter().map(|m| &m.id));
          Some(handlers)
        } else {
          None
        },
        head,
      });
    } else {
      println!("skipping layout: {}", path);
    }

    Ok(())
  }
}
