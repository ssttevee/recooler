import { defineConfig } from "@farmfe/core";
import recooler from "farm-plugin-recooler";

export default defineConfig({
  compilation: {
    // the plugin generates the hono app as this virtual module
    input: { main: "recooler::app" },
    output: {
      path: "build",
      format: "esm",
      targetEnv: "library",
      filename: "[resourceName].[ext]",
    },
    // emit a single build/main.js so the tests can import the app
    partialBundling: { enforceResources: [{ name: "main", test: [".+"] }] },
    minify: false,
    sourcemap: false,
    persistentCache: false,
    progress: false,
  },
  plugins: [
    [
      "@farmfe/plugin-react",
      {
        runtime: "automatic",
        importSource: "easy-jsx-html-engine",
        useBuiltins: true,
        throwIfNamespace: false,
        refresh: false,
        development: false,
      },
    ],
    // src/lib/db.ts is only imported by page components, so it never reaches a client script
    recooler({ client_banned_imports: ["/lib/db"] }),
  ],
});
