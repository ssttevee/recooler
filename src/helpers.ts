import {
  type Component,
  createElement,
  SuspenseRequestID,
} from "easy-jsx-html-engine";
import { renderToStream } from "easy-jsx-html-engine/stream-webapi";
import { Context, Handler, MiddlewareHandler } from "hono";

function renderStream<P extends { rid: SuspenseRequestID }>(
  waitUntil: ((promise: Promise<unknown>) => void) | undefined,
  component: Component<P>,
  props: Omit<P, "rid">,
) {
  return renderToStream(
    (rid) => createElement(component, { ...props, rid }),
    waitUntil,
  );
}

export function buildHeadFn(
  headFns: HeadFn[],
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
  };
}

export function actionsMiddleware(
  actions: Record<string, Handler>,
): MiddlewareHandler {
  return async (ctx, next) => {
    const params = new URL(ctx.req.url).searchParams;
    const action = actions[params.get("action")!];
    if (action) {
      return await action(ctx, next);
    }

    await next();
  };
}
