import {
  type Component,
  createElement,
  Fragment,
  type SuspenseRequestID,
} from "easy-jsx-html-engine";
import { renderToStream } from "easy-jsx-html-engine/stream-webapi";
import { type Context, type Handler, Hono, type MiddlewareHandler } from "hono";

function renderStream<P extends { rid: SuspenseRequestID }>(
  waitUntil: ((promise: Promise<unknown>) => void) | undefined,
  component: Component<P>,
  props: Omit<P, "rid">,
) {
  return renderToStream(
    (rid: SuspenseRequestID) => createElement(component, { ...props, rid }),
    waitUntil,
  );
}

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

export function jsxRouteHandler(
  rootContainer: (
    component: Component<RouteProps>,
    script?: string,
  ) => Component<RouteProps>,
  component: Component<RouteProps>,
  headFn: ReturnType<typeof buildHeadFn>,
  script?: string,
): Handler {
  return async (ctx: Context) => {
    const url = new URL(ctx.req.url);
    if (url.search === "?script") {
      if (script) {
        return ctx.newResponse(script, {
          headers: { "content-type": "application/javascript" },
        });
      } else {
        url.search = "";
        return ctx.redirect(url.href.slice(url.origin.length));
      }
    }

    let waitUntil: ((promise: Promise<unknown>) => void) | undefined;
    try {
      waitUntil = ctx.executionCtx.waitUntil?.bind(ctx.executionCtx);
    } catch {
      // ignore
    }

    const head = await headFn(ctx);
    try {
      return ctx.newResponse(
        await renderStream(waitUntil, rootContainer(component, script), {
          url,
          ctx,
          head,
        }),
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

export function actionsMiddleware(
  actions: Record<string, Handler>,
): MiddlewareHandler {
  return async (ctx, next) => {
    const params = new URL(ctx.req.url).searchParams;
    const action = actions[params.get("action")!];
    if (action) {
      const result = await action(ctx, next);
      if ("toHTML" in result) {
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
