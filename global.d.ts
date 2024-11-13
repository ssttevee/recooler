type HonoContext = import("hono").Context;

declare interface RouteProps<TContext = HonoContext> {
  rid: string | number;
  url: URL;
  ctx: TContext;
  head: RouteHead;
}

declare interface RouteHead {
  title?: string;
  base?: string | JSX.IntrinsicElements["base"];
  metas?: JSX.IntrinsicElements["meta"][];
  links?: JSX.IntrinsicElements["link"][];
  scripts?: JSX.IntrinsicElements["script"][];
}

declare interface ServerActionFn<TPayload = any, TContext = HonoContext> {
  (
    ctx: TContext,
    payload: TPayload,
  ):
    | import("hono/types").HandlerResponse<any>
    | Promise<JSX.Element>
    | JSX.Element;
}

declare interface RouteHeadFn<TContext = HonoContext> {
  (ctx: TContext, prev: RouteHead): Promise<RouteHead> | RouteHead;
}

declare namespace JSX {
  interface HtmlGlobalAttributes {
    [event: `hx-on:${string}`]: (event: Event) => any;
    [`hx-get`]?: ServerActionFn;
    [`hx-post`]?: ServerActionFn;
    [`hx-patch`]?: ServerActionFn;
    [`hx-put`]?: ServerActionFn;
    [`hx-delete`]?: ServerActionFn;
  }
}

declare module "recooler::app" {
  const app: import("hono").Hono;
  export default app;
  export const onRequest: (ctx: unknown) => Promise<Response> | Response;
}

declare module "recooler::metadata" {
  export const pages: Array<{ path: string; metadata?: Record<string, any> }>;
}
