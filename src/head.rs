use farmfe_core::module::ModuleId;

use crate::{
  imports::UniqueImportIdentifiers,
  routes::{PageComponentLayoutItem, RootComponentInfo, RouteInfo},
};

#[derive(Clone, Copy, Debug)]
pub(crate) enum HeadType {
  Function,
  Object,
}

struct HeadExprBuilder<'a> {
  imports: &'a mut UniqueImportIdentifiers,

  fns: Vec<String>,
  initial: Option<String>,
}

impl<'a> HeadExprBuilder<'a> {
  fn new(imports: &'a mut UniqueImportIdentifiers) -> Self {
    Self {
      imports,
      fns: Vec::new(),
      initial: None,
    }
  }

  fn add_head(&mut self, module_id: &ModuleId, head_type: &Option<HeadType>) {
    if let Some(head_type) = head_type {
      let head_ident = self.imports.identifier(module_id, "head");
      match head_type {
        HeadType::Function => {
          self.fns.push(head_ident);
        }
        HeadType::Object => {
          self.fns.clear();
          self.initial = Some(head_ident);
        }
      }
    }
  }

  fn to_string(self) -> String {
    let mut params = format!("[{}]", self.fns.join(", "));
    if let Some(initial) = self.initial {
      params = format!("{}, {}", params, initial.as_str());
    }

    format!("buildHeadFn({})", params)
  }
}

pub(crate) fn build_head_expr(
  import_idents: &mut UniqueImportIdentifiers,
  root_component: &Option<RootComponentInfo>,
  layouts: &Vec<PageComponentLayoutItem>,
  route: &RouteInfo,
) -> String {
  // head functions should be called from root to leaf

  // start with an empty object
  let mut builder = HeadExprBuilder::new(import_idents);

  if let Some(root_info) = root_component {
    builder.add_head(&root_info.module_id, &root_info.head);
  };

  // layouts are in reverse order from `get_layout_modules`, so we must clone and reverse the vec.
  let mut reversed = layouts.clone();
  reversed.reverse();

  for layout in reversed {
    builder.add_head(&layout.module_id, &layout.head);
  }

  // ensure the outermost head is from the route module
  builder.add_head(&route.module_id, &route.head);

  builder.to_string()
}
