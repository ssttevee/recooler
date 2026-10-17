# Recooler

Recooler is a server-side-only framework designed around [htmx](https://htmx.org/) and [hono](https://hono.dev/) with async JSX components made to feel like PHP.

The name recooler is an homage to the htmx predecessor [intercooler](https://intercoolerjs.org/).

## Quick Start

https://github.com/ssttevee/recooler-cloudflare-workers-template

TODO: create a `npm create` command to scaffold a new project with a basic setup.

## Configuration

By default, an esm module is emitted with a hono instance as the default export and a thin adapter function for the cloudflare pages' `onRequest` hook.

Alternatively, a custom entrypoint module may be provided using the "recooler::app" to import the app for additional configuration of the hono instance and exports. Once such use case for this is to add additional handlers for extra cloudflare workers features, such as [cron triggers](https://developers.cloudflare.com/workers/configuration/cron-triggers/), [event queues](https://developers.cloudflare.com/queues/reference/how-queues-works/#create-a-consumer-worker), or even [email routing](https://developers.cloudflare.com/email-routing/email-workers/).

```ts
// entrypoint.ts
import app from "recooler::app";

export default {
  fetch: app.fetch.bind(app),
  async scheduled(event: ScheduledEvent) {
    // handle cron job event
  },
};
```

## Features

- [Routing](#Routing)
- [Components](#Components)
- [Page Components](#Page%20Components)
- [Head Functions](#Head%20Functions)
- [Request Handlers](#Request%20Handlers)
- [Layouts](#Layouts)
- [Middleware](#Middleware)
- [Form Actions](#Form%20Actions)
- [Event Handlers](#Event%20Handlers)
- [Root Component](#Root%20Component)
- [Error Boundaries](#Error%20Boundaries)
- [Suspense](#Suspense)

### Routing

A special directory structure is used to build the hono app routes. By default, the route directory is `routes` inside the source directory, which is `src` by default. The directory structure is as follows:

```
src
├── components
│   └── ...
├── ...
├── routes
│   ├── (public)
│   │   ├── [[other]]/index.tsx
│   │   ├── about/index.tsx
│   │   ├── blog
│   │   │   ├── [post]/index.tsx
│   │   │   └── index.tsx
│   │   └── layout.tsx
│   ├── (admin)
│   │   ├── admin
│   │   │   ├── index.tsx
│   │   │   ├── middleware@10-db.tsx
│   │   │   └── middleware@20-auth.tsx
│   │   └── layout.tsx
│   ├── index.tsx
│   └── middleware.ts
├── ...
```

This will generate the following hono routes, in this order:

```
/
/about
/admin
/blog
/blog/:post
/:other{.+}
```

Every route is registered on the same hono app, ordered segment by segment so that a more specific route is matched first: a static segment goes before a `[param]`, which goes before a `[[wildcard]]`. For example, `/top/new` goes before `/top/:id`, and every other route goes before `/:other{.+}`, regardless of their groups.

The `index.tsx` files are route modules used to indicate a route.

The `layout.tsx` files are used as the default layout for all routes in the directory and sub-directories. Only one layout file may be used per directory, and they are applied additively. (i.e. they build upon each other.)

The `middleware[@{name}].ts` files are used to define hono middleware for the route. Middleware files may contain JSX and have the `.tsx` extension. Middleware file name may include a name to use more than one middleware for a particular route using an `@` after the word `middleware`. Middleware is executed in the alphabetical order of the file name. It is convention to add a number to the name to guarantee a certain order of execution.

The `(parenthesized)` directory names are used to group routes together for layout purposes and are completely removed from the final route. Any duplicate routes resulting from the parenthesized directory names are added to hono like any other route, ordered by their directory paths (e.g. `(a)/about` before `(b)/about` before `about`), it is up to hono (i.e. the first one) to pick which one to use.

The `[param]` directory name are replaced with a path parameter. Please see the [hono](https://hono.dev/docs/api/routing#path-parameter) for more information.

The `[[wildcard]]` directory name is a special case that is of the path parameter that matches multiple path segments. In hono terms, it is replaced with `:wildcard{.+}`. Please see the [hono](https://hono.dev/docs/api/routing#path-parameter) for more information.

### Components

While not strictly a feature of Recooler, components are optionally async functions that return JSX elements. This is useful for composition and abstractions.

Component are "uncolored" or "colorblind" with a few exceptions. This meaning that async and non-async components may be freely interleaved without any additional effort. However, Suspense may be used as a boundary to defer rendering of long-running async components to improve time-to-first-byte.

### Page Components

Page components are the default export of a route module. Whatever is returned is rendered as the page HTML. They are given `RouteProps` as the properties.

```ts
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
```

Within in any component (page, layout, root, or other component) or head function, a `Response` may be thrown to immediately stop rendering and return the response. This is useful for handling redirects, 404s, and other edge cases. One exception to this is when within a `<Suspense />` boundary, because the response will have already been sent to the client. Note that this will still work through an `<ErrorBoundary />`, but only if imported through `"farm-plugin-recooler/helpers"` because the catch function is overridden to handle this case.

### Head Functions

Route modules, layout modules, and the root component module may optionally export a `head` object (`RouteHead`) or function (`RouteHeadFn`), used to set the head elements for the page. If an object is exported, it will overwrite all prior head elements. If a function is exported, then the implementor will have access to the prior head object and may choose to extend or overwrite it. Head functions are applied out to in, starting from the root component, applying the head function from each layout module, and finally the route module. This means the route module's head export will have the final say on the head elements.

The `head` export may be declared in any form, e.g. `export function head`, `export const head = ...` (with or without `as`/`satisfies`), `export { head }`, or `export { head } from "./head"`. If the plugin cannot tell at build time whether it is an object or a function (e.g. `export const head = makeHead()`), this is checked when the page is rendered.

```ts
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
```

### Request Handlers

Request handlers are an escape hatch to use regular hono request handlers. They are defined by exporting `onGet`, `onPost`, `onPatch`, `onPut`, `onDelete`, `onOptions`, or `onRequest` functions. `onRequest` will handle all methods, while the other will handle their own respective methods.

### Layouts

Layout are implemented as "Higher Order Components", or HOCs, that wrap the page component. They are used to apply common layout elements to all pages in the directory and sub-directories. Only one layout file may be used per directory, and they are applied additively. (i.e. they build upon each other.) A layout HOC must be exported as the default export of a layout module to take effect.

### Middleware

Regular hono middleware may be the default export of a middleware module to take effect for the particular route. The behavior is the same as calling `app.use("/route/*", middleware)` on a hono instance, so it also handles the requests to the paths under the route. The only special sauce from Recooler is the folder directory structure and ordering by file name.

Middleware files placed directly in a group directory (i.e. a parenthesized directory name) will only apply to the routes in the group. This is unlike regular middleware which will handle all requests matching the prefix regardless of whether there is a route handler.

### Form Actions

Any `hx-{method}` attributes with function values will be converted to a form action. This works for all components within the configured source directory (which is `src` by default), including layouts and the root component. The function may optionally be async and return a hono response, or anything that can be rendered as JSX children (elements, strings, numbers, arrays and promises of them), which is sent as html. Returning `undefined` or `null` sends an empty `204 No Content` response, while `false` and `""` render nothing and send an empty `200` response, just like `{false}` in JSX. A `Response` thrown by the action or by a component it renders is sent as is.

This is a little similar to next.js's `"use server"` functions, but specifically for htmx.

Form actions run after the middleware of the page, and the request of an action is handled by the action alone: the `onRequest` and `on{Method}` handlers of the route don't run. See [hono handler order](#hono-handler-order).

```ts
declare interface FormActionFn<
  TPayload extends {} = Record<string, any> & { __formData: FormData },
  TContext extends HonoContext = HonoContext,
> {
  (
    ctx: TContext,
    payload: TPayload,
  ): Promise<FormActionResult> | FormActionResult;
}

type FormActionResult =
  | Awaited<import("hono/types").HandlerResponse<any>>
  | Exclude<import("easy-jsx-html-engine").Children, PromiseLike<any>>
  | void;
```

Examples:

```tsx
return (
  <button
    hx-swap="outerHTML"
    hx-post={() => {
      return <div>you clicked me!</div>;
    }}
  >
    click me
  </button>
);
```

```tsx
return (
  <button
    hx-post={(ctx: HonoContext) => {
      ctx.header("hx-push-url", "/pressed");
      return ctx.text("you clicked me!");
    }}
  >
    click me
  </button>
);
```

Form actions can also read request bodies from form submissions.

Example:

```tsx
return (
  <form
    hx-post={(ctx: HonoContext, payload: { remember?: "on"; name: string }) => {
      return ctx.text(`you entered ${payload.name} and ${payload.remember}`);
    }}
  >
    <input type="checkbox" name="remember" />
    <input type="text" name="name" value={name} />
    <button>hello</button>
  </form>
);
```

Form actions are requested with an `action` query parameter on the page's url. Requests without one are handled by the route's own handlers (e.g. `onPost`), and requests whose `action` does not name any form action of the app are rejected with a 404.

Form actions in a route module post to the path of the route, e.g. `/blog/my-post?action=...`, but those in any other module, such as a layout or the root component, render on many pages, so they post to the relative url `?action=...` to be handled by the current page. Like any relative url, it is resolved against the base url of the document, which is a known limitation: on a page with a `<base href>` (e.g. the `base` of a head function), it posts to the base url, and after the url changes without rendering the page again (e.g. with `hx-push-url` or `history.pushState`), it posts to the new url. Either fails unless that url is a page that renders the same action.

NOTE: Routes with path parameters must have `ctx` available as a prop. See [gotchas and workarounds](#form-actions-in-parameterized-routes).

#### Captured variables

Form actions are hoisted out of the component and run in a separate request, so variables from the enclosing component are captured when the page is rendered and sent back with the action request.

```tsx
export default function TodoList({ todos }: { todos: Todo[] }) {
  return todos.map((todo) => (
    <button
      hx-delete={async (ctx: HonoContext) => {
        await deleteTodo(ctx, todo.id);
        return <></>;
      }}
    >
      delete {todo.title}
    </button>
  ));
}
```

- Only values that can round trip through JSON may be captured: strings, finite numbers, booleans, `null`, arrays and plain objects. Capturing anything else, such as a `Date`, a class instance or the hono context, throws when the page is rendered. Capture the specific values instead, e.g. `const id = props.ctx.req.param("id")` rather than `props`.
- Local functions and classes cannot be captured and fail the build. Move them to the module scope, where they can be referenced freely.
- Module level variables and imports are not captured since the action stays in the same module.
- Captured values are a copy: reassigning them inside the action does not affect the component.

Captured values are part of the action url, so they are visible to the client. They can also be modified by the client unless an `action_secret` is configured, in which case they are signed and requests with a missing or invalid signature are rejected with a 403.

```ts
// farm.config.ts
import { defineConfig } from "@farmfe/core";
import recooler from "farm-plugin-recooler";

export default defineConfig({
  plugins: [recooler({ action_secret: process.env.RECOOLER_ACTION_SECRET })],
});
```

The secret is inlined into the generated server entrypoint, so treat the build output as sensitive.

### Event Handlers

Any `hx-on:{event}` attributes with function values will be extracted and bundled into a specialized client script for each page. Just like form actions, this works for all components within the configured source directory (which is `src` by default), including layouts and the root component. These function can be expected to run on the client, as if simply added with `elem.addEventListener("event", fn)`. It does not have access to the hono context because it is executed on the client side, but it does have access to the element that triggered the event, as well as the event itself.

Variables from the enclosing component are captured in the same way as [form actions](#captured-variables), except that they are embedded in the page instead of an action url, and are never signed.

```ts
declare interface EventHandlerFn<TElement extends Element = any, TEvent extends  Event = any> {
  (this: TElement, event: TEvent): any;
}
```

Client scripts only include the event handlers, the `onLoad` export of the route module, and the module level functions, variables and imports that they reference. The rest of each module, including the components, form actions, unused imports and module level statements with side effects, is left out. Imports that the handlers and `onLoad` do use are bundled as usual, so keep server side code out of the modules that they import.

To make sure server side code never ends up in a client script, `client_banned_imports` may be set to a list of regular expressions. The build fails if an event handler or route `onLoad` imports a matching module, with the chain of references that led to the import, or if any module in a client script has a matching path (relative to the project root) through some other import.

```ts
// farm.config.ts
export default defineConfig({
  plugins: [recooler({ client_banned_imports: ["(^|/)faunadb(/|$)", "/lib/db"] })],
});
```

```
event handler src/components/X.tsx@2 imports banned module "../lib/db" via _handler$2 → updateTable → dbClient
```

Because only declarations are carried over to the client script, there are a few limitations:

- Module level variables are only initialized by their declaration. Assignments in separate statements are left out, so `export let onLoad;` followed by `onLoad = () => { ... };` is `undefined` on the client.
- `var` declarations nested inside module level blocks (e.g. `if (...) { var x = 1; }`) and module level `using` declarations are left out.
- `onLoad` must be exported by name from a route module (`index.tsx`), as a function declaration, a variable, or an export specifier such as `export { load as onLoad }`. `export default function onLoad` is not recognized.
- If `routes_dir` is outside of `src_dir`, route modules are not transformed, so their whole module is bundled into the client script along with `onLoad`.

### Root Component

The root component is an optional HOC that wraps the entire page. It is meant to be used for rendering the basic HTML document structure, including the `<html>`, `<head>`, and `<body>` elements. The client script is also passed to the root component which should be embedded inline after the content or just before the `</body>` tag.

The default root component will render the content and the head elements in a basic HTML template with the client script (if any) embedded, inline, just before the `</body> tag. It also adds `<meta charset="utf-8">` unless one of the head's `metas` already declares a charset.

Since the client script is only passed to the root component, it's not very straightforward to load it as a separate script or caching purposes. However, since client scripts are fairly likely to be small, no prescriptions will ad be made for this.

```ts
declare interface RootHOC {
  (Content: Component<RouteProps>, clientScript?: string): Component<RouteProps>;
}
```

### Error Boundaries

`<ErrorBoundary />` is a special component that is able to handle any errors thrown by its async children. This is a leak in the colorblind abstraction.

If a non-sync component can throw and the error must be caught with an `<ErrorBoundary />`, then it can either be converted to an async component (by adding `async` to the function declaration) or by wrapping the child in a simple function that will be subsequently be called the `<ErrorBoundary />`.

```tsx
return (
  <ErrorBoundary>
    <AsyncFallibleComponent />
    {() => <NonAsyncFallibleComponent />}
  </ErrorBoundary>
)
```

For more information about `<ErrorBoundary />`, please see the easy-jsx-html-engine documentation.

### Suspense

`<Suspense />` works similarly to `<ErrorBoundary />` but is used to defer rendering of long-running async components to improve time-to-first-byte.

When any of the children are async, the fallback component will be immediately rendered and sent to the client. Once all of the async children are resolved, the fallback will be replaced with the final rendered children.

Note that this requires some level of server support and may not work in every server environment.

For more information about `<Suspense />`, please see the easy-jsx-html-engine documentation.

## hono handler order

Hono middleware/handlers have an execution order.

Here is the order for a request to a route:

```
1. middleware
2. form actions (only for requests with an `?action=` query of the route, which end here)
3. onRequest
   ├── onPost, onPatch, onPut, onDelete, onOptions
   └── onGet
       └── (page handler)
```

## htmx

While recooler is designed around htmx, it is not a requirement. It is fully functional as a standalong routing framework.

This also means that htmx is not imported by default. So please make sure to include it in either your root component or head functions where necessary.

```ts
export const head: RouteHeadFn = (_ctx, prev) => ({
  ...prev,
  scripts: [
    ...(prev.scripts ?? []),
    {
      src: "https://cdn.jsdelivr.net/npm/htmx.org@1.9.12/dist/htmx.min.js",
      integrity: "sha256-RJMXreeIHpSVENthSZHhlcOgmcTHkcJNrOxV+fSipFI=",
      crossorigin: "anonymous",
    },
  ],
});
```

## SSG

Recooler can also be used for static site generation using the "farm-plugin-ssg" module.

```ts
// farm.config.ts
import { defineConfig } from "@farmfe/core";
import ssg from "farm-plugin-ssg";

export default defineConfig({
  // ...
  plugins: [
    // ...
    ssg(),
    // ...
  ],
  // ...
});
```

## Gotchas and workarounds

Here is a running list of rough edges that you may encounter and how they can be worked around:

### form actions in parameterized routes

```
TypeError: Cannot read properties of undefined (reading 'req')
```

For pages with a parameterized route (i.e. `/article/[id]/index.tsx`), there needs a way to get the path parameter value to get the actual path. As such, the plugin assumes that the component containing the form action function to have `ctx` passed in with the props. This assumption is made because it is true for the page component, which makes it very easy for simple apps. If the outermost function in the module has no params, the build fails instead.

Form actions nested inside another form action's returned JSX use the outer action's `ctx` param instead.

```tsx
// /article/[id]/index.tsx

export default function (props: RouteProps) {
  return (
    <>
      {/* ... */}
      <LikeButton ctx={props.ctx} />
      {/* ... */}
    </>
  );
}

function LikeButton({ ...otherProps }: OtherProps & { ctx: HonoContext }) {
  return (
    <button
      hx-get={async () => {
        await doSomething();
        return <></>;
      }}
    >
      Like +1
    </button>
  );
}
```

`ctx` does not need to be destructured or pulled out. The plugin can get the path parameter when the component receives a prop named `ctx`.

You can also supply the context on the same element as the server action. The plugin uses this value instead of the component prop and does not render the `ctx` attribute:

```tsx
function LikeButton({ requestContext }: { requestContext: HonoContext }) {
  return (
    <button
      ctx={requestContext}
      hx-get={async () => {
        await doSomething();
        return <></>;
      }}
    >
      Like +1
    </button>
  );
}
```

## About

The inspiration for this project came from my nostalgia for my childhood days as a PHP main. I wanted to bring back the experience of writing server-side logic in-line with the client-side markup without the mess of interleaving JS and PHP. Recooler is the manifestation of that idea built on top of modern technologies and tooling for JS.

## Disclaimer

This is my side project that I'm using for my other personal projects. While I do use it in a "production" capacity, it is nowhere near "battle-tested". I am not responsible for any damage that may occur from using this project. Use at your own risk.

Pull requests are welcome, but please be nice.
