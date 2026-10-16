export interface IPluginOptions {
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
   * handler or route `onLoad`, e.g. `"faunadb"` or `"../lib/db"`, and in the id of every module in
   * each compiled client script, which is its path relative to the project root, e.g.
   * `"node_modules/faunadb/index.js"`. Any match fails the build.
   */
  client_banned_imports?: string[];
}
