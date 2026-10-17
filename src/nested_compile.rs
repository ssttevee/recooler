//! Helpers for the compiles this plugin runs inside of the host compile, i.e. the route scan's
//! trace compile and the client script compiles.

use std::{collections::HashMap, sync::Arc};

use farmfe_core::{
  config::Config,
  context::CompilationContext,
  error::Result,
  module::{
    module_graph::ModuleGraph, module_group::ModuleGroupGraph, Module, ModuleId, ModuleMetaData,
  },
  plugin::*,
  resource::resource_pot::{ResourcePot, ResourcePotInfo, ResourcePotMetaData},
  stats::Stats,
};
use farmfe_toolkit::script::constant::RUNTIME_SUFFIX;

/// Returns the host compile's plugins for use in a nested compile.
///
/// `Compiler::new_without_internal_plugins` runs the `config` hook of every plugin it is given, but
/// these are the host's own instances, shared with the host compile, and the config of a nested
/// compile is cloned from the host's, which has already been through their `config` hooks.
/// Running them again is redundant at best and corrupts the shared state at worst: e.g.
/// `FarmPluginDefine::config` appends the configured defines to a list that it never clears, so
/// after every nested compile, every later transform of the host compile and of the other nested
/// compiles applies each define once more. So each plugin is wrapped in a [`SharedHostPlugin`],
/// which doesn't forward the `config` hook.
pub(crate) fn shared_host_plugins(context: &Arc<CompilationContext>) -> Vec<Arc<dyn Plugin>> {
  context
    .plugin_driver
    .plugins
    .iter()
    .map(|plugin| Arc::new(SharedHostPlugin(plugin.clone())) as Arc<dyn Plugin>)
    .collect()
}

/// Returns the inputs of a nested compile with entries named by `input`.
///
/// The `config` hooks of the host plugins are not run for nested compiles (see
/// [`shared_host_plugins`]), so inputs added by them to the host config must be carried over. That
/// is the farm runtime entry that `FarmPluginRuntime::config` adds for targets other than
/// `library`, which the runtime plugin later expects to find in the generated resources.
pub(crate) fn nested_compile_input(
  host_config: &Config,
  input: impl IntoIterator<Item = (String, String)>,
) -> HashMap<String, String> {
  let mut nested_input = host_config
    .input
    .iter()
    .filter(|(_, source)| source.ends_with(RUNTIME_SUFFIX))
    .map(|(name, source)| (name.clone(), source.clone()))
    .collect::<HashMap<_, _>>();
  nested_input.extend(input);
  nested_input
}

/// A host plugin shared with a nested compile that forwards every hook to the wrapped plugin,
/// except for `config`, see [`shared_host_plugins`].
///
/// Every other method of the [`Plugin`] trait must be forwarded here, including `name` (plugin
/// caches are keyed by it) and `priority` (which decides the hook order), since a missing one
/// silently falls back to the trait's default no-op implementation instead.
struct SharedHostPlugin(Arc<dyn Plugin>);

impl Plugin for SharedHostPlugin {
  fn name(&self) -> &str {
    self.0.name()
  }

  fn priority(&self) -> i32 {
    self.0.priority()
  }

  fn config(&self, _config: &mut Config) -> Result<Option<()>> {
    Ok(None)
  }

  fn plugin_cache_loaded(
    &self,
    cache: &Vec<u8>,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.plugin_cache_loaded(cache, context)
  }

  fn build_start(&self, context: &Arc<CompilationContext>) -> Result<Option<()>> {
    self.0.build_start(context)
  }

  fn resolve(
    &self,
    param: &PluginResolveHookParam,
    context: &Arc<CompilationContext>,
    hook_context: &PluginHookContext,
  ) -> Result<Option<PluginResolveHookResult>> {
    self.0.resolve(param, context, hook_context)
  }

  fn load(
    &self,
    param: &PluginLoadHookParam,
    context: &Arc<CompilationContext>,
    hook_context: &PluginHookContext,
  ) -> Result<Option<PluginLoadHookResult>> {
    self.0.load(param, context, hook_context)
  }

  fn transform(
    &self,
    param: &PluginTransformHookParam,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<PluginTransformHookResult>> {
    self.0.transform(param, context)
  }

  fn parse(
    &self,
    param: &PluginParseHookParam,
    context: &Arc<CompilationContext>,
    hook_context: &PluginHookContext,
  ) -> Result<Option<ModuleMetaData>> {
    self.0.parse(param, context, hook_context)
  }

  fn process_module(
    &self,
    param: &mut PluginProcessModuleHookParam,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.process_module(param, context)
  }

  fn analyze_deps(
    &self,
    param: &mut PluginAnalyzeDepsHookParam,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.analyze_deps(param, context)
  }

  fn finalize_module(
    &self,
    param: &mut PluginFinalizeModuleHookParam,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.finalize_module(param, context)
  }

  fn build_end(&self, context: &Arc<CompilationContext>) -> Result<Option<()>> {
    self.0.build_end(context)
  }

  fn generate_start(&self, context: &Arc<CompilationContext>) -> Result<Option<()>> {
    self.0.generate_start(context)
  }

  fn optimize_module_graph(
    &self,
    module_graph: &mut ModuleGraph,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.optimize_module_graph(module_graph, context)
  }

  fn analyze_module_graph(
    &self,
    module_graph: &mut ModuleGraph,
    context: &Arc<CompilationContext>,
    hook_context: &PluginHookContext,
  ) -> Result<Option<ModuleGroupGraph>> {
    self
      .0
      .analyze_module_graph(module_graph, context, hook_context)
  }

  fn partial_bundling(
    &self,
    modules: &Vec<ModuleId>,
    context: &Arc<CompilationContext>,
    hook_context: &PluginHookContext,
  ) -> Result<Option<Vec<ResourcePot>>> {
    self.0.partial_bundling(modules, context, hook_context)
  }

  fn process_resource_pots(
    &self,
    resource_pots: &mut Vec<&mut ResourcePot>,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.process_resource_pots(resource_pots, context)
  }

  fn render_start(&self, config: &Config, context: &Arc<CompilationContext>) -> Result<Option<()>> {
    self.0.render_start(config, context)
  }

  fn render_resource_pot_modules(
    &self,
    resource_pot: &ResourcePot,
    context: &Arc<CompilationContext>,
    hook_context: &PluginHookContext,
  ) -> Result<Option<ResourcePotMetaData>> {
    self
      .0
      .render_resource_pot_modules(resource_pot, context, hook_context)
  }

  fn render_resource_pot(
    &self,
    resource_pot: &PluginRenderResourcePotHookParam,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<PluginRenderResourcePotHookResult>> {
    self.0.render_resource_pot(resource_pot, context)
  }

  fn augment_resource_hash(
    &self,
    render_pot_info: &ResourcePotInfo,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<String>> {
    self.0.augment_resource_hash(render_pot_info, context)
  }

  fn optimize_resource_pot(
    &self,
    resource: &mut ResourcePot,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.optimize_resource_pot(resource, context)
  }

  fn generate_resources(
    &self,
    resource_pot: &mut ResourcePot,
    context: &Arc<CompilationContext>,
    hook_context: &PluginHookContext,
  ) -> Result<Option<PluginGenerateResourcesHookResult>> {
    self
      .0
      .generate_resources(resource_pot, context, hook_context)
  }

  fn process_generated_resources(
    &self,
    resources: &mut PluginGenerateResourcesHookResult,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.process_generated_resources(resources, context)
  }

  fn handle_entry_resource(
    &self,
    resource: &mut PluginHandleEntryResourceHookParams,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.handle_entry_resource(resource, context)
  }

  fn finalize_resources(
    &self,
    param: &mut PluginFinalizeResourcesHookParams,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.finalize_resources(param, context)
  }

  fn generate_end(&self, context: &Arc<CompilationContext>) -> Result<Option<()>> {
    self.0.generate_end(context)
  }

  fn finish(&self, stat: &Stats, context: &Arc<CompilationContext>) -> Result<Option<()>> {
    self.0.finish(stat, context)
  }

  fn update_modules(
    &self,
    params: &mut PluginUpdateModulesHookParams,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.update_modules(params, context)
  }

  fn module_graph_updated(
    &self,
    param: &PluginModuleGraphUpdatedHookParams,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<()>> {
    self.0.module_graph_updated(param, context)
  }

  fn update_finished(&self, context: &Arc<CompilationContext>) -> Result<Option<()>> {
    self.0.update_finished(context)
  }

  fn handle_persistent_cached_module(
    &self,
    module: &Module,
    context: &Arc<CompilationContext>,
  ) -> Result<Option<bool>> {
    self.0.handle_persistent_cached_module(module, context)
  }

  fn write_plugin_cache(&self, context: &Arc<CompilationContext>) -> Result<Option<Vec<u8>>> {
    self.0.write_plugin_cache(context)
  }
}

#[cfg(test)]
mod tests {
  use std::{collections::HashMap, sync::Arc};

  use farmfe_core::{config::Config, error::Result, plugin::Plugin};

  use super::{nested_compile_input, SharedHostPlugin};

  struct ConfigPlugin;

  impl Plugin for ConfigPlugin {
    fn name(&self) -> &str {
      "ConfigPlugin"
    }

    fn priority(&self) -> i32 {
      -99
    }

    fn config(&self, config: &mut Config) -> Result<Option<()>> {
      config
        .input
        .insert("added".to_string(), "added".to_string());
      Ok(Some(()))
    }
  }

  #[test]
  fn shared_host_plugin_does_not_forward_config() {
    let plugin = SharedHostPlugin(Arc::new(ConfigPlugin));
    let mut config = Config {
      input: HashMap::new(),
      ..Default::default()
    };
    assert_eq!(plugin.config(&mut config).unwrap(), None);
    assert!(config.input.is_empty());
  }

  #[test]
  fn shared_host_plugin_forwards_name_and_priority() {
    let plugin = SharedHostPlugin(Arc::new(ConfigPlugin));
    assert_eq!(plugin.name(), "ConfigPlugin");
    assert_eq!(plugin.priority(), -99);
  }

  #[test]
  fn nested_compile_input_keeps_runtime_entry() {
    let config = Config {
      input: HashMap::from([
        ("main".to_string(), "src/main.ts".to_string()),
        (
          "runtime".to_string(),
          "@farmfe/runtime/src/index.ts.farm-runtime".to_string(),
        ),
      ]),
      ..Default::default()
    };

    let input = nested_compile_input(
      &config,
      [(
        "client".to_string(),
        "recooler:@src/routes/index.tsx".to_string(),
      )],
    );
    assert_eq!(
      input,
      HashMap::from([
        (
          "runtime".to_string(),
          "@farmfe/runtime/src/index.ts.farm-runtime".to_string()
        ),
        (
          "client".to_string(),
          "recooler:@src/routes/index.tsx".to_string()
        ),
      ])
    );
  }
}
