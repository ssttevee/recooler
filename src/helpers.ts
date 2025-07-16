import {
  type Component,
  createElement,
  Fragment,
  type SuspenseRequestID,
  ErrorBoundary as _ErrorBoundary,
  type ErrorBoundaryProps,
  defaultCatch,
  Suspense,
  type SuspenseProps,
  dangerouslyPreventEscaping,
  type ResolvedTemplateProps,
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
