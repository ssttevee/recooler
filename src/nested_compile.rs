//! Helpers for the compiles this plugin runs inside of the host compile, i.e. the route scan's
//! trace compile and the client script compiles.

use std::{collections::HashMap, sync::Arc};

use farmfe_core::{
  config::{custom::CUSTOM_CONFIG_EXTERNAL_RECORD, Config, ModuleFormat, TargetEnv},
  context::CompilationContext,
  error::Result,
  module::{
    module_graph::ModuleGraph, module_group::ModuleGroupGraph, Module, ModuleId, ModuleMetaData,
  },
  plugin::*,
  resource::{
    resource_pot::{ResourcePot, ResourcePotInfo, ResourcePotMetaData},
    Resource, ResourceType,
  },
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

/// The name of farm's `FarmPluginLazyCompilation`.
const LAZY_COMPILATION_PLUGIN_NAME: &str = "FarmPluginLazyCompilation";

/// Returns the plugins of a client script compile with the config `config`, given the
/// `host_plugins` shared with it, see [`shared_host_plugins`].
///
/// The resolve plugin of the host is replaced by one of its own: it caches the externals of the
/// host config on first use, which a client script must not have (see
/// [`force_client_script_output`]), and its resolve cache isn't keyed by the target, so e.g. the
/// `node` condition of package exports resolved for a node host would be reused.
///
/// The lazy compilation plugin, which the host has in dev, is left out: it doesn't check whether
/// lazy compilation is enabled, and it would turn every dynamic import into a request to the dev
/// server through the farm runtime, which a client script doesn't have.
pub(crate) fn client_script_plugins(
  host_plugins: Vec<Arc<dyn Plugin>>,
  config: &Config,
) -> Vec<Arc<dyn Plugin>> {
  let resolve_plugin = farmfe_plugin_resolve::FarmPluginResolve::new(config);

  let mut plugins = host_plugins
    .into_iter()
    .filter(|plugin| {
      plugin.name() != resolve_plugin.name() && plugin.name() != LAZY_COMPILATION_PLUGIN_NAME
    })
    .collect::<Vec<_>>();
  plugins.push(Arc::new(resolve_plugin));
  plugins.push(Arc::new(farmfe_plugin_minify::FarmPluginMinify::new(
    config,
  )));
  plugins
}

/// Returns the inputs of a nested compile with entries named by `input`.
///
/// The `config` hooks of the host plugins are not run for nested compiles (see
/// [`shared_host_plugins`]), so inputs added by them to the host config must be carried over. That
/// is the farm runtime entry that `FarmPluginRuntime::config` adds for targets other than
/// `library`, which the runtime plugin later expects to find in the generated resources. Client
/// script compiles always use the `library` target and don't need it, see
/// [`force_client_script_output`].
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

/// Forces the output settings of a client script compile, whose config is cloned from the host's.
///
/// Client scripts are inlined into a classic `<script>` element, so whatever the host builds (e.g.
/// a cjs bundle for node), they must be self-contained browser scripts:
///
/// - the `library` target, which is an alias of `library-browser`, with esm output. Unlike the
///   `browser` and `node` targets, it doesn't include the farm runtime (its module system and
///   resource loader, and for node `global.nodeRequire = require`), and since the client entry
///   exports nothing, the output has no `export` statements either. The inputs of a client script
///   compile must therefore not include the runtime entry of the host, see
///   [`nested_compile_input`].
/// - no externals: an inline script can't import anything, so every import is bundled.
pub(crate) fn force_client_script_output(config: &mut Config) {
  config.output.target_env = TargetEnv::Library;
  config.output.format = ModuleFormat::EsModule;
  config.external.clear();
  config.custom.remove(CUSTOM_CONFIG_EXTERNAL_RECORD);
}

/// Returns the script generated for the entry named `entry_name` by a nested compile, given its
/// module graph `entries`, `resource_pots` and `resources`.
///
/// Resources are named by the host's `output.entryFilename` and `output.filename` (e.g.
/// `[entryName].[hash].js`), so the script is found through the resource pot of the entry module
/// instead of by its name.
pub(crate) fn entry_script<'a>(
  entries: &HashMap<ModuleId, String>,
  resource_pots: &[&ResourcePot],
  resources: &'a HashMap<String, Resource>,
  entry_name: &str,
) -> Option<&'a Resource> {
  resource_pots
    .iter()
    .filter(|pot| {
      pot
        .entry_module
        .as_ref()
        .and_then(|module_id| entries.get(module_id))
        .is_some_and(|name| name == entry_name)
    })
    .flat_map(|pot| pot.resources())
    .filter_map(|name| resources.get(name))
    .find(|resource| matches!(resource.resource_type, ResourceType::Js))
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

  use farmfe_core::{
    config::{
      config_regex::ConfigRegex, custom::CUSTOM_CONFIG_EXTERNAL_RECORD, Config, ModuleFormat,
      OutputConfig, TargetEnv,
    },
    error::Result,
    module::ModuleId,
    plugin::Plugin,
    resource::{
      resource_pot::{ResourcePot, ResourcePotType},
      Resource, ResourceType,
    },
  };

  use super::{
    client_script_plugins, entry_script, force_client_script_output, nested_compile_input,
    SharedHostPlugin,
  };

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

  fn resource(name: &str, resource_type: ResourceType) -> (String, Resource) {
    (
      name.to_string(),
      Resource {
        name: name.to_string(),
        bytes: name.as_bytes().to_vec(),
        resource_type,
        ..Default::default()
      },
    )
  }

  #[test]
  fn entry_script_finds_the_script_of_the_entry_by_its_pot() {
    let client_module = ModuleId::from("recooler:@src/routes/index.tsx");
    let entries = HashMap::from([(client_module.clone(), "client".to_string())]);

    // named like `entryFilename: "[entryName].[hash].js"`, with a source map
    let mut client_pot = ResourcePot::new("main".to_string(), ResourcePotType::Js);
    client_pot.entry_module = Some(client_module);
    client_pot.add_resource("client.0123abcd.js.map".to_string());
    client_pot.add_resource("client.0123abcd.js".to_string());

    // e.g. a dynamically imported chunk
    let mut other_pot = ResourcePot::new("chunk".to_string(), ResourcePotType::Js);
    other_pot.add_resource("chunk.js".to_string());

    let resources = HashMap::from([
      resource(
        "client.0123abcd.js.map",
        ResourceType::SourceMap("js".to_string()),
      ),
      resource("client.0123abcd.js", ResourceType::Js),
      resource("chunk.js", ResourceType::Js),
    ]);

    let script = entry_script(&entries, &[&other_pot, &client_pot], &resources, "client").unwrap();
    assert_eq!(script.name, "client.0123abcd.js");

    assert!(entry_script(&entries, &[&other_pot, &client_pot], &resources, "other").is_none());
  }

  #[test]
  fn force_client_script_output_builds_a_self_contained_browser_script() {
    let mut config = Config {
      output: Box::new(OutputConfig {
        target_env: TargetEnv::Node,
        format: ModuleFormat::CommonJs,
        entry_filename: "[entryName].[hash].cjs".to_string(),
        ..Default::default()
      }),
      external: vec![ConfigRegex::new("^hono")],
      custom: Box::new(HashMap::from([
        (
          CUSTOM_CONFIG_EXTERNAL_RECORD.to_string(),
          r#"{"jquery":"$"}"#.to_string(),
        ),
        ("other".to_string(), "kept".to_string()),
      ])),
      ..Default::default()
    };

    force_client_script_output(&mut config);

    assert_eq!(config.output.target_env, TargetEnv::Library);
    assert_eq!(config.output.format, ModuleFormat::EsModule);
    assert!(config.external.is_empty());
    assert_eq!(
      *config.custom,
      HashMap::from([("other".to_string(), "kept".to_string())])
    );
    // the script is found by its entry, see `entry_script`
    assert_eq!(config.output.entry_filename, "[entryName].[hash].cjs");
  }

  struct NamedPlugin(&'static str);

  impl Plugin for NamedPlugin {
    fn name(&self) -> &str {
      self.0
    }
  }

  fn plugin_names(plugins: &[Arc<dyn Plugin>]) -> Vec<&str> {
    plugins.iter().map(|plugin| plugin.name()).collect()
  }

  #[test]
  fn client_script_plugins_replace_the_host_resolve_plugin() {
    let host_resolve: Arc<dyn Plugin> = Arc::new(NamedPlugin("FarmPluginResolve"));
    let host_plugins: Vec<Arc<dyn Plugin>> = vec![
      Arc::new(NamedPlugin("FarmPluginScript")),
      host_resolve.clone(),
      Arc::new(NamedPlugin("FarmPluginRecooler")),
    ];

    let plugins = client_script_plugins(host_plugins, &Config::default());
    let resolve_plugins = plugins
      .iter()
      .filter(|plugin| plugin.name() == "FarmPluginResolve")
      .collect::<Vec<_>>();
    assert_eq!(resolve_plugins.len(), 1);
    assert!(!Arc::ptr_eq(resolve_plugins[0], &host_resolve));

    let names = plugin_names(&plugins);
    assert!(names.contains(&"FarmPluginScript"));
    assert!(names.contains(&"FarmPluginRecooler"));
  }

  #[test]
  fn client_script_plugins_leave_out_lazy_compilation() {
    let host_plugins: Vec<Arc<dyn Plugin>> = vec![
      Arc::new(NamedPlugin("FarmPluginLazyCompilation")),
      Arc::new(NamedPlugin("FarmPluginScript")),
    ];

    let plugins = client_script_plugins(host_plugins, &Config::default());
    let names = plugin_names(&plugins);
    assert!(!names.contains(&"FarmPluginLazyCompilation"));
    assert!(names.contains(&"FarmPluginScript"));
  }
}
