export interface IPluginOptions {
  /**
   * The source directory, relative to the current working directory. Only modules in it are
   * transformed for form actions and event handlers. Defaults to `"src"`.
   */
  src_dir?: string;

  /**
   * The routes directory, relative to the current working directory. Defaults to `"routes"` inside
   * of `src_dir`.
   */
  routes_dir?: string;

  /**
   * The module that exports the `JSXRouteHandlerFactory` (see `farm-plugin-recooler/helpers`) used
   * to render page components, e.g. one made with `CreateJSXRouteHandlerFactory`. Either a module,
   * whose default export is used, or a tuple of a module and the name of the export, where a `null`
   * name also uses the default export. The module is imported by the generated entrypoint, so it
   * should be a package name or an absolute path. Defaults to the `jsxRouteHandler` export of
   * `farm-plugin-recooler/helpers`.
   */
  route_handler_factory?: string | [module: string] | [module: string, name: string | null];

  /**
   * Secret used to sign the variables captured by form actions. When set, requests with a missing
   * or invalid signature are rejected with a 403. When unset, captured variables are not signed and
   * can be modified by the client.
   *
   * The secret is inlined into the generated server entrypoint.
   */
  action_secret?: string;

  /**
   * Regular expressions for modules that must never be included in client scripts, such as
   * database clients. Unset by default, which disables the checks.
   *
   * Each pattern is searched for (unanchored) in the specifier of every import kept for an event
   * handler or route `onLoad`, e.g. `"faunadb"` or `"../lib/db"`, and in every module included in
   * each compiled client script. A module is matched by its path relative to the project root, e.g.
   * `"node_modules/faunadb/index.js"`, `"src/lib/db.ts"` or `"../common/src/db.ts"`, and if it is
   * in a `node_modules` directory, also by its path inside of the last one, e.g.
   * `"faunadb/index.js"` or `"@scope/pkg/x.js"`, both without any `?query`. So `"^faunadb/"` matches
   * the package wherever it is installed, e.g. in pnpm's global store outside of the project, whose
   * modules' paths relative to the project root start with `"../"`. Any match fails the build.
   */
  client_banned_imports?: string[];
}
