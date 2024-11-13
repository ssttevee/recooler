pub(crate) mod transform;
pub(crate) mod validate;

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

pub(super) struct LocalHandlerIdentGenerator {
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

pub(super) const ACTION_DECL_PREFIX: &str = "_action$";
pub(super) const EVENT_DECL_PREFIX: &str = "_handler$";

impl LocalHandlerIdentGenerator {
  pub(super) fn next_action_ident(&mut self, method: FormActionMethod) -> String {
    let export_name = format!(
      "{}{}${}",
      ACTION_DECL_PREFIX,
      format!("{:?}", method).to_lowercase(),
      self.action_counter
    );
    self.action_counter += 1;
    export_name
  }

  pub(super) fn next_event_ident(&mut self) -> String {
    let export_name = format!("{}{}", EVENT_DECL_PREFIX, self.event_counter);
    self.event_counter += 1;
    export_name
  }
}
