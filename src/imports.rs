use std::collections::{HashMap, HashSet};

use farmfe_core::{module::ModuleId, regex::Regex, serde_json};
use farmfe_toolkit::lazy_static::lazy_static;

pub(crate) struct UniqueImportIdentifiers {
  identifiers: HashSet<String>,
  exports: HashMap<ModuleId, HashMap<String, String>>,
}

lazy_static! {
  static ref IDENT_PATTERN: Regex = Regex::new("(?:^[^a-zA-Z]|[^a-zA-Z0-9]+)").unwrap();
}

impl UniqueImportIdentifiers {
  pub(crate) fn new() -> Self {
    Self {
      identifiers: Default::default(),
      exports: Default::default(),
    }
  }

  fn unique_identifier(&mut self, mut ident: String) -> String {
    if self.identifiers.contains(&ident) {
      let mut suffix = 1;
      loop {
        let new_ident = format!("{}{}", ident, suffix);
        if !self.identifiers.contains(&new_ident) {
          ident = new_ident;
          break;
        }

        suffix += 1;
      }
    }

    self.identifiers.insert(ident.clone());
    ident
  }

  pub(crate) fn identifier<S: AsRef<str>>(&mut self, module: &ModuleId, export_name: S) -> String {
    let export_name_string = String::from(export_name.as_ref());
    if let Some(ident) = self
      .exports
      .entry(module.clone())
      .or_default()
      .get(&export_name_string)
    {
      return ident.clone();
    }

    let ident = self.unique_identifier(
      IDENT_PATTERN
        .replace_all(&module.relative_path(), "_")
        .to_lowercase()
        + "_"
        + export_name_string.as_str(),
    );

    self
      .exports
      .get_mut(&module)
      .unwrap()
      .insert(export_name_string, ident.clone());

    ident
  }

  pub(crate) fn imports_str(&self) -> String {
    let mut modules = self.exports.keys().collect::<Vec<_>>();
    modules.sort_by(|a, b| a.relative_path().cmp(b.relative_path()));

    let cwd = std::env::current_dir().unwrap();
    let cwd_str = cwd.to_str().unwrap();
    let mut out = String::new();
    for module in modules {
      for (export_name, ident) in self.exports.get(module).unwrap() {
        out += format!(
          "import {{ {} as {} }} from {};\n",
          export_name,
          ident,
          serde_json::to_string(&module.resolved_path(cwd_str)).unwrap()
        )
        .as_str();
      }
    }

    out
  }
}

impl Default for UniqueImportIdentifiers {
  fn default() -> Self {
    Self::new()
  }
}
