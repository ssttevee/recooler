export interface IPluginOptions {
  /**
   * Secret used to sign the variables captured by form actions. When set, requests with a missing
   * or invalid signature are rejected with a 403. When unset, captured variables are not signed and
   * can be modified by the client.
   *
   * The secret is inlined into the generated server entrypoint.
   */
  action_secret?: string;
}
