// The require condition resolves to the binary path, which Farm loads when a config names the
// package as a plain string.
type Required = typeof import("farm-plugin-recooler", { with: { "resolution-mode": "require" } });

export const binPath: Required["default"] = "/path/to/index.farm";
// @ts-expect-error not the plugin function
export const notPlugin: Required["default"] = () => ["", undefined];
