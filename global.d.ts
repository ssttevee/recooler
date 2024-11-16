type HonoContext = import("hono").Context<RouteEnv>;

declare interface RouteEnv {}

declare interface RouteProps<TContext = HonoContext> {
  /**
   * The request ID for the use with <Suspense />
   */
  rid: string | number;

  /**
   * The current url, pre-parsed for convenience.
   */
  url: URL;

  /**
   * The hono request context.
   */
  ctx: TContext;

  /**
   * The head object used to render the head elements.
   */
  head: RouteHead;
}

declare interface RouteHead {
  title?: string;
  base?: string | JSX.IntrinsicElements["base"];
  metas?: JSX.IntrinsicElements["meta"][];
  links?: JSX.IntrinsicElements["link"][];
  scripts?: JSX.IntrinsicElements["script"][];
}

declare interface RouteHeadFn<TContext = HonoContext> {
  (ctx: TContext, prev: RouteHead): Promise<RouteHead> | RouteHead;
}

declare interface FormActionFn<TPayload = any, TContext = HonoContext> {
  (
    ctx: TContext,
    payload: TPayload,
  ): import("hono/types").HandlerResponse<any> | JSX.Element;
}

declare interface EventHandlerFn<
  TElement extends Element = any,
  TEvent extends Event = any,
> {
  (this: TElement, event: TEvent): any;
}

declare namespace JSX {
  interface HtmlGlobalAttributes {
    [event: `hx-on:${string}`]: EventHandlerFn;
    [`hx-get`]?: FormActionFn;
    [`hx-post`]?: FormActionFn;
    [`hx-patch`]?: FormActionFn;
    [`hx-put`]?: FormActionFn;
    [`hx-delete`]?: FormActionFn;
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
