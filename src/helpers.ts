import {
  ErrorBoundary as _ErrorBoundary,
  type Component,
  createElement,
  dangerouslyPreventEscaping,
  defaultCatch,
  type ErrorBoundaryProps,
  Fragment,
  type ResolvedTemplateProps,
  Suspense,
  type SuspenseProps,
  type SuspenseRequestID,
} from "easy-jsx-html-engine";
import { renderToStream } from "easy-jsx-html-engine/stream-webapi";
import type { Context, Handler, Hono, MiddlewareHandler } from "hono";

export function buildHeadFn(
  headFns: RouteHeadFn[],
  initial?: RouteHead,
): (ctx: Context) => Promise<RouteHead> {
  return async (ctx: Context) => {
    let head = initial ?? {};
    for (const headFn of headFns) {
      head = (await headFn(ctx, head)) ?? {};
    }

    return head;
  };
}

export type JSXRouteHandlerFactory = (
  rootContainer: (
    component: Component<RouteProps>,
    script?: string,
  ) => Component<RouteProps>,
  component: Component<RouteProps>,
  headFn: ReturnType<typeof buildHeadFn>,
  script?: string,
) => Handler;

export interface CreateJSXRouteHandlerFactoryOptions {
  ResolvedTemplate?: Component<ResolvedTemplateProps>;
  suspenseCleanupScriptMiddleware?: (script: string) => string;
}

export function CreateJSXRouteHandlerFactory(
  options: CreateJSXRouteHandlerFactoryOptions = {},
): JSXRouteHandlerFactory {
  return (
      rootContainer: (
        component: Component<RouteProps>,
        script?: string,
      ) => Component<RouteProps>,
      component: Component<RouteProps>,
      headFn: ReturnType<typeof buildHeadFn>,
      script?: string,
    ): Handler =>
    async (ctx: Context) => {
      const url = new URL(ctx.req.url);
      if (url.search === "?script") {
        if (script) {
          return ctx.newResponse(script, {
            headers: { "content-type": "application/javascript" },
          });
        }

        url.search = "";
        return ctx.redirect(url.href.slice(url.origin.length));
      }

      let waitUntil: ((promise: Promise<unknown>) => void) | undefined;
      try {
        waitUntil = ctx.executionCtx.waitUntil?.bind(ctx.executionCtx);
      } catch {
        // ignore
      }

      try {
        const head = await headFn(ctx);
        return ctx.newResponse(
          await renderToStream(
            (rid: SuspenseRequestID) =>
              createElement(rootContainer(component, script), {
                url,
                ctx,
                head,
                rid,
              }),
            {
              waitUntil,
              ResolvedTemplate: options.ResolvedTemplate,
              cleanupScriptMiddleware: options.suspenseCleanupScriptMiddleware,
            },
          ),
          {
            headers: { "content-type": "text/html" },
          },
        );
      } catch (err) {
        if (err instanceof Response) {
          return err;
        }

        throw err;
      }
    };
}

export const jsxRouteHandler = /* @__PURE__ */ CreateJSXRouteHandlerFactory();

let actionSecret: Uint8Array | undefined;

export interface ConfigureActionsOptions {
  /**
   * Secret used to sign the variables captured by form actions.
   */
  secret?: string;
}

/**
 * Called by the generated app module with the plugin's `action_secret` option.
 */
export function configureActions(options: ConfigureActionsOptions): void {
  actionSecret = options.secret
    ? new TextEncoder().encode(options.secret)
    : undefined;
}

type ScopedFormActionFactory<TPayload extends {}, TContext extends HonoContext> = (
  scope: any,
) => FormActionFn<TPayload, TContext>;

/**
 * Returns the query string that carries the variables captured by a form action.
 */
export function actionScope(
  actionId: string,
  scope: Record<string, unknown>,
): string {
  const json = serializeScope(scope);
  let query = `&scope=${encodeURIComponent(json)}`;
  if (actionSecret) {
    query += `&sig=${signScope(actionSecret, actionId, json)}`;
  }

  return query;
}

/**
 * Returns the javascript expression that carries the variables captured by an event handler.
 */
export function handlerScope(scope: Record<string, unknown>): string {
  return serializeScope(scope);
}

function resolveAction<TPayload extends {}, TContext extends HonoContext>(
  ctx: TContext,
  action: FormActionFn<TPayload, TContext> | ScopedFormActionFactory<TPayload, TContext>,
  actionId: string | undefined,
): FormActionFn<TPayload, TContext> | Response {
  if (actionId === undefined) {
    return action as FormActionFn<TPayload, TContext>;
  }

  const json = ctx.req.query("scope");
  if (json === undefined) {
    return ctx.text("missing action scope", 400);
  }

  if (actionSecret) {
    const sig = ctx.req.query("sig");
    if (!sig || !timingSafeEqual(sig, signScope(actionSecret, actionId, json))) {
      return ctx.text("invalid action scope signature", 403);
    }
  }

  let scope: unknown;
  try {
    scope = JSON.parse(json);
  } catch {
    // handled below
  }

  if (typeof scope !== "object" || scope === null || Array.isArray(scope)) {
    return ctx.text("malformed action scope", 400);
  }

  return (action as ScopedFormActionFactory<TPayload, TContext>)(scope);
}

export function wrapAction<TContext extends HonoContext = HonoContext>(
  action: FormActionFn<{}, TContext>,
): (ctx: TContext) => ReturnType<FormActionFn>;
export function wrapAction<TContext extends HonoContext = HonoContext>(
  factory: ScopedFormActionFactory<{}, TContext>,
  actionId: string,
): (ctx: TContext) => ReturnType<FormActionFn>;
export function wrapAction<TContext extends HonoContext = HonoContext>(
  action: FormActionFn<{}, TContext> | ScopedFormActionFactory<{}, TContext>,
  actionId?: string,
) {
  return (ctx: TContext) => {
    const resolved = resolveAction(ctx, action, actionId);
    return resolved instanceof Response ? resolved : resolved(ctx, {});
  };
}

export function wrapActionWithPayload<
  TContext extends HonoContext = HonoContext,
>(
  action: FormActionFn<any, TContext>,
): (ctx: TContext) => Promise<Awaited<ReturnType<FormActionFn>>>;
export function wrapActionWithPayload<
  TContext extends HonoContext = HonoContext,
>(
  factory: ScopedFormActionFactory<any, TContext>,
  actionId: string,
): (ctx: TContext) => Promise<Awaited<ReturnType<FormActionFn>>>;
export function wrapActionWithPayload<
  TContext extends HonoContext = HonoContext,
>(
  action: FormActionFn<any, TContext> | ScopedFormActionFactory<any, TContext>,
  actionId?: string,
) {
  return async (ctx: TContext) => {
    const resolved = resolveAction(ctx, action, actionId);
    if (resolved instanceof Response) {
      return resolved;
    }

    const fd = await ctx.req.formData();
    return resolved(ctx, { ...Object.fromEntries(fd), __formData: fd });
  };
}

/**
 * Serializes captured variables as json, throwing for values that json cannot round trip, such as
 * functions, class instances (including the hono context), and dates.
 */
function serializeScope(scope: Record<string, unknown>): string {
  for (const [name, value] of Object.entries(scope)) {
    assertSerializable(value, name, new Set());
  }

  return JSON.stringify(scope);
}

function assertSerializable(
  value: unknown,
  path: string,
  ancestors: Set<object>,
): void {
  switch (typeof value) {
    case "string":
    case "boolean":
    case "undefined":
      return;
    case "number":
      if (Number.isFinite(value)) {
        return;
      }
      break;
    case "object": {
      if (value === null) {
        return;
      }

      if (ancestors.has(value)) {
        throw new TypeError(`cannot capture \`${path}\`: circular reference`);
      }

      const proto = Object.getPrototypeOf(value);
      if (
        !Array.isArray(value) &&
        proto !== Object.prototype &&
        proto !== null
      ) {
        break;
      }

      ancestors.add(value);
      for (const [key, child] of Object.entries(value)) {
        assertSerializable(
          child,
          Array.isArray(value) ? `${path}[${key}]` : `${path}.${key}`,
          ancestors,
        );
      }
      ancestors.delete(value);
      return;
    }
  }

  const kind =
    typeof value === "object"
      ? (Object.getPrototypeOf(value)?.constructor?.name ?? "object")
      : typeof value === "number"
        ? String(value)
        : typeof value;
  throw new TypeError(
    `cannot capture \`${path}\` (${kind}): only json compatible values may be captured by form actions and event handlers`,
  );
}

function signScope(secret: Uint8Array, actionId: string, json: string): string {
  return base64url(
    hmacSha256(secret, new TextEncoder().encode(`${actionId}\n${json}`)),
  );
}

function timingSafeEqual(a: string, b: string): boolean {
  if (a.length !== b.length) {
    return false;
  }

  let diff = 0;
  for (let i = 0; i < a.length; i++) {
    diff |= a.charCodeAt(i) ^ b.charCodeAt(i);
  }

  return diff === 0;
}

function base64url(bytes: Uint8Array): string {
  let binary = "";
  for (const byte of bytes) {
    binary += String.fromCharCode(byte);
  }

  return btoa(binary).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

// Synchronous HMAC-SHA256, because WebCrypto is async and attribute values are rendered
// synchronously.

const SHA256_K = /* @__PURE__ */ new Uint32Array([
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
  0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
  0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
  0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
  0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
  0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
]);

function rotr(x: number, n: number): number {
  return (x >>> n) | (x << (32 - n));
}

function sha256(data: Uint8Array): Uint8Array {
  const length = data.length;
  const padded = new Uint8Array(((length + 9 + 63) >> 6) << 6);
  padded.set(data);
  padded[length] = 0x80;

  const view = new DataView(padded.buffer);
  view.setUint32(padded.length - 8, Math.floor(length / 0x20000000));
  view.setUint32(padded.length - 4, (length << 3) >>> 0);

  const h = new Uint32Array([
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c,
    0x1f83d9ab, 0x5be0cd19,
  ]);
  const w = new Uint32Array(64);

  for (let offset = 0; offset < padded.length; offset += 64) {
    for (let i = 0; i < 16; i++) {
      w[i] = view.getUint32(offset + i * 4);
    }
    for (let i = 16; i < 64; i++) {
      const s0 = rotr(w[i - 15], 7) ^ rotr(w[i - 15], 18) ^ (w[i - 15] >>> 3);
      const s1 = rotr(w[i - 2], 17) ^ rotr(w[i - 2], 19) ^ (w[i - 2] >>> 10);
      w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }

    let [a, b, c, d, e, f, g, hh] = h;
    for (let i = 0; i < 64; i++) {
      const s1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25);
      const ch = (e & f) ^ (~e & g);
      const t1 = (hh + s1 + ch + SHA256_K[i] + w[i]) | 0;
      const s0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22);
      const maj = (a & b) ^ (a & c) ^ (b & c);
      const t2 = (s0 + maj) | 0;
      hh = g;
      g = f;
      f = e;
      e = (d + t1) | 0;
      d = c;
      c = b;
      b = a;
      a = (t1 + t2) | 0;
    }

    h[0] += a;
    h[1] += b;
    h[2] += c;
    h[3] += d;
    h[4] += e;
    h[5] += f;
    h[6] += g;
    h[7] += hh;
  }

  const out = new Uint8Array(32);
  const outView = new DataView(out.buffer);
  for (let i = 0; i < 8; i++) {
    outView.setUint32(i * 4, h[i]);
  }

  return out;
}

function hmacSha256(key: Uint8Array, message: Uint8Array): Uint8Array {
  const block = new Uint8Array(64);
  block.set(key.length > 64 ? sha256(key) : key);

  const inner = new Uint8Array(64 + message.length);
  const outer = new Uint8Array(64 + 32);
  for (let i = 0; i < 64; i++) {
    inner[i] = block[i] ^ 0x36;
    outer[i] = block[i] ^ 0x5c;
  }
  inner.set(message, 64);
  outer.set(sha256(inner), 64);

  return sha256(outer);
}

export function actionsMiddleware(
  actions: Record<string, Handler>,
): MiddlewareHandler {
  return async (ctx, next) => {
    const params = new URL(ctx.req.url).searchParams;
    const action = actions[params.get("action")!];
    if (action) {
      const result = await action(ctx, next);
      if (result === undefined || result === null) {
        return ctx.newResponse(null, 204);
      }
      if (typeof result === "object" && "toHTML" in result) {
        return ctx.html(result.toHTML());
      }
      return result;
    }

    await next();
  };
}

export function makeCloudflarePagesHandler(
  app: Hono,
): (ctx: unknown) => Promise<Response> | Response {
  return (ctx: any) => app.fetch(ctx, ctx.env, ctx);
}

export function renderHeadElements(
  head: RouteHead,
): ReturnType<typeof createElement> {
  return createElement(
    Fragment,
    {},
    head.title ? createElement("title", {}, head.title) : null,
    head.base &&
      createElement(
        "base",
        typeof head.base === "string" ? { href: head.base } : head.base,
      ),
    head.metas?.map((props) => createElement("meta", props)),
    head.links?.map((props) => createElement("link", props)),
    head.scripts?.map((props) => createElement("script", props)),
  );
}

export function DefaultRootHOC(
  Content: Component<RouteProps>,
  clientScript?: string,
) {
  return (props: RouteProps) => {
    return createElement(
      Fragment,
      {},
      dangerouslyPreventEscaping("<!DOCTYPE html>"),
      createElement(
        "html",
        {},
        createElement("head", {}, renderHeadElements(props.head)),
        createElement(
          "body",
          {},
          createElement(Content, props),
          clientScript && createElement("script", {}, clientScript),
        ),
      ),
    );
  };
}

export type { ErrorBoundaryProps, SuspenseProps };

function wrapCatchFn(catchFn: ErrorBoundaryProps["catch"]) {
  return (err: any) => {
    if (err instanceof Response) {
      // propagate the response up
      throw err;
    }

    if (catchFn === null || catchFn === undefined) {
      return defaultCatch(err);
    }

    if (typeof catchFn === "function") {
      return catchFn(err);
    }

    return createElement(Fragment, {}, catchFn);
  };
}

export function ErrorBoundary(props: ErrorBoundaryProps): JSX.Element {
  return _ErrorBoundary({ ...props, catch: wrapCatchFn(props.catch) });
}

export { defaultCatch, Suspense };
