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

type FormActionResult =
  | Awaited<import('hono/types').HandlerResponse<any>>
  | Exclude<
      import('easy-jsx-html-engine').Children,
      Promise<import('easy-jsx-html-engine').Children>
    >
  | void;

declare interface FormActionFn<
  TPayload extends {} = Record<string, any> & { __formData: FormData },
  TContext extends HonoContext = HonoContext,
> {
  (
    ctx: TContext,
    payload: TPayload,
  ): Promise<FormActionResult> | FormActionResult;
}

declare interface EventHandlerFn<
  TElement extends Element = any,
  TEvent extends Event = any,
> {
  (this: TElement, event: TEvent): any;
}

declare namespace JSX {
  interface HtmlGlobalAttributes {
    /**
     * issues a `GET` to the specified URL
     *
     * @see https://htmx.org/attributes/hx-get/
     */
    'hx-get'?: FormActionFn<any> | string;
    /**
     * issues a `POST` to the specified URL
     *
     * @see https://htmx.org/attributes/hx-post/
     */
    'hx-post'?: FormActionFn<any> | string;
    /**
     * issues a `PATCH` to the specified URL
     *
     * @see https://htmx.org/attributes/hx-patch/
     */
    'hx-patch'?: FormActionFn<any> | string;
    /**
     * issues a `PUT` to the specified URL
     *
     * @see https://htmx.org/attributes/hx-put/
     */
    'hx-put'?: FormActionFn<any> | string;
    /**
     * issues a `DELETE` to the specified URL
     *
     * @see https://htmx.org/attributes/hx-delete/
     */
    'hx-delete'?: FormActionFn<any> | string;
    /**
     * handle events with inline scripts on elements
     *
     * @see https://htmx.org/attributes/hx-on/
     */
    [event: `hx-on:${string}`]: EventHandlerFn | string;
    /**
     * handle events with inline scripts on elements
     *
     * @see https://htmx.org/attributes/hx-on/
     */
    [event: `hx-on-${string}`]: EventHandlerFn | string;
    /**
     * handle events with inline scripts on elements
     *
     * @see https://htmx.org/attributes/hx-on/
     */
    'hx-on'?: string;
    /**
     * push a URL into the browser location bar to create history
     *
     * @see https://htmx.org/attributes/hx-push-url/
     */
    'hx-push-url'?: string;
    /**
     * select content to swap in from a response
     *
     * @see https://htmx.org/attributes/hx-select/
     */
    'hx-select'?: string;
    /**
     * select content to swap in from a response, somewhere other than the target (out of band)
     *
     * @see https://htmx.org/attributes/hx-select-oob/
     */
    'hx-select-oob'?: string;
    /**
     * controls how content will swap in (`outerHTML`, `beforeend`, `afterend`, …)
     *
     * @see https://htmx.org/attributes/hx-swap/
     */
    'hx-swap'?: string;
    /**
     * mark element to swap in from a response (out of band)
     *
     * @see https://htmx.org/attributes/hx-swap-oob/
     */
    'hx-swap-oob'?: string;
    /**
     * specifies the target element to be swapped
     *
     * @see https://htmx.org/attributes/hx-target/
     */
    'hx-target'?: string;
    /**
     * specifies the event that triggers the request
     *
     * @see https://htmx.org/attributes/hx-trigger/
     */
    'hx-trigger'?: string;
    /**
     * add values to submit with the request (JSON format)
     *
     * @see https://htmx.org/attributes/hx-vals/
     */
    'hx-vals'?: string;
    /**
     * add progressive enhancement for links and forms
     *
     * @see https://htmx.org/attributes/hx-boost/
     */
    'hx-boost'?: string;
    /**
     * shows a confirm() dialog before issuing a request
     *
     * @see https://htmx.org/attributes/hx-confirm/
     */
    'hx-confirm'?: string;
    /**
     * disables htmx processing for the given node and any children nodes
     *
     * @see https://htmx.org/attributes/hx-disable/
     */
    'hx-disable'?: string;
    /**
     * adds the disabled attribute to the specified elements while a request is in flight
     *
     * @see https://htmx.org/attributes/hx-disable-elt/
     */
    'hx-disable-elt'?: string;
    /**
     * control and disable automatic attribute inheritance for child nodes
     *
     * @see https://htmx.org/attributes/hx-disinherit/
     */
    'hx-disinherit'?: string;
    /**
     * changes the request encoding type
     *
     * @see https://htmx.org/attributes/hx-encoding/
     */
    'hx-encoding'?: string;
    /**
     * extensions to use for this element
     *
     * @see https://htmx.org/attributes/hx-ext/
     */
    'hx-ext'?: string;
    /**
     * adds to the headers that will be submitted with the request
     *
     * @see https://htmx.org/attributes/hx-headers/
     */
    'hx-headers'?: string;
    /**
     * prevent sensitive data being saved to the history cache
     *
     * @see https://htmx.org/attributes/hx-history/
     */
    'hx-history'?: string;
    /**
     * the element to snapshot and restore during history navigation
     *
     * @see https://htmx.org/attributes/hx-history-elt/
     */
    'hx-history-elt'?: string;
    /**
     * include additional data in requests
     *
     * @see https://htmx.org/attributes/hx-include/
     */
    'hx-include'?: string;
    /**
     * the element to put the htmx-request class on during the request
     *
     * @see https://htmx.org/attributes/hx-indicator/
     */
    'hx-indicator'?: string;
    /**
     * control and enable automatic attribute inheritance for child nodes if it has been disabled by default
     *
     * @see https://htmx.org/attributes/hx-inherit/
     */
    'hx-inherit'?: string;
    /**
     * filters the parameters that will be submitted with a request
     *
     * @see https://htmx.org/attributes/hx-params/
     */
    'hx-params'?: string;
    /**
     * specifies elements to keep unchanged between requests
     *
     * @see https://htmx.org/attributes/hx-preserve/
     */
    'hx-preserve'?: string;
    /**
     * shows a prompt() before submitting a request
     *
     * @see https://htmx.org/attributes/hx-prompt/
     */
    'hx-prompt'?: string;
    /**
     * replace the URL in the browser location bar
     *
     * @see https://htmx.org/attributes/hx-replace-url/
     */
    'hx-replace-url'?: string;
    /**
     * configures various aspects of the request
     *
     * @see https://htmx.org/attributes/hx-request/
     */
    'hx-request'?: string;
    /**
     * control how requests made by different elements are synchronized
     *
     * @see https://htmx.org/attributes/hx-sync/
     */
    'hx-sync'?: string;
    /**
     * force elements to validate themselves before a request
     *
     * @see https://htmx.org/attributes/hx-validate/
     */
    'hx-validate'?: string;
    /**
     * adds values dynamically to the parameters to submit with the request
     *
     * @see https://htmx.org/attributes/hx-vars/
     *
     * @deprecated please use hx-vals
     */
    'hx-vars'?: string;
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
