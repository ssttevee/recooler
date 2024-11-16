# Recooler

Recooler is a server-side-only framework designed around [htmx](https://htmx.org/) and [hono](https://hono.dev/) with async JSX components made to feel like PHP.

The name recooler is an homage to the htmx predecessor [intercooler](https://intercoolerjs.org/).

## Quick Start

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
└── entrypoint.ts
├── ...
```

This will generate the following hono routes:

```
/
/about
/admin
/blog
/blog/:post
```

The `index.tsx` files are route modules used to indicate a route.

The `layout.tsx` files are used as the default layout for all routes in the directory and sub-directories. Only one layout file may be used per directory, and they are applied additively. (i.e. they build upon each other.)

The `middleware[@{name}].ts` files are used to define hono middleware for the route. Middleware files may contain JSX and have the `.tsx` extension. Middleware file name may include a name to use more than one middleware for a particular route using an `@` after the word `middleware`. Middleware is executed in the alphabetical order of the file name. It is convention to add a number to the name to guarantee a certain order of execution.

The `(parenthesized)` directory names are used to group routes together for layout purposes and are completely removed from the final route. Any duplicate routes resulting from the parenthesized directory names are added to hono like any other route, it is up to hono (i.e. the first one) to pick which one to use.

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

Request handlers are an escape hatch to use regular hono request handlers. They are defined by exporting `onGet`, `onPost`, `onPatch`, `onPut`, `onDelete`, or `onOptions` functions. There is an edge case when using `onGet` with a default page component export, in which case the `onGet` function will take priority, but may use hono's `next` function to continue to the regular page component handler. In this way, they are very similar to middleware but only run on a specific request method.

### Layouts

Layout are implemented as "Higher Order Components", or HOCs, that wrap the page component. They are used to apply common layout elements to all pages in the directory and sub-directories. Only one layout file may be used per directory, and they are applied additively. (i.e. they build upon each other.) A layout HOC must be exported as the default export of a layout module to take effect.

### Middleware

Regular hono middleware may be exported from a middleware module to take effect for the particular route. The behavior is the same as calling `app.use("/route", middleware)` on a hono instance. The only special sauce from Recooler is the folder directory structure and ordering by file name.

### Form Actions

Any `hx-{method}` attributes with function values will be converted to a form action. This works for all components within the configured source directory (which is `src` by default), including layouts and the root component. The function may optionally be async and return a JSX element or a hono response.

This is a little similar to next.js's `"use server"` functions, but specifically for htmx.

```ts
declare interface FormActionFn<TPayload = any, TContext = HonoContext> {
  (
    ctx: TContext,
    payload: TPayload,
  ): import("hono/types").HandlerResponse<any> | JSX.Element;
}
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

### Event Handlers

Any `hx-on:{event}` attributes with function values will be extracted and bundled into a specialized client script for each page. Just like form actions, this works for all components within the configured source directory (which is `src` by default), including layouts and the root component. These function can be expected to run on the client, as if simply added with `elem.addEventListener("event", fn)`. It does not have access to the hono context because it is executed on the client side, but it does have access to the element that triggered the event, as well as the event itself.

```ts
declare interface EventHandlerFn<TElement extends Element = any, TEvent extends  Event = any> {
  (this: TElement, event: TEvent): any;
}
```

### Root Component

The root component is an optional HOC that wraps the entire page. It is meant to be used for rendering the basic HTML document structure, including the `<html>`, `<head>`, and `<body>` elements. The client script is also passed to the root component which should be embedded inline after the content or just before the `</body>` tag.

The default root component will render the content and the head elements in a basic HTML template with the client script (if any) embedded, inline, just before the `</body> tag.

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

## About

The inspiration for this project came from my nostalgia for my childhood days as a PHP main. I wanted to bring back the experience of writing server-side logic in-line with the client-side markup without the mess of interleaving JS and PHP. Recooler is the manifestation of that idea built on top of modern technologies and tooling for JS.

## Disclaimer

This is my side projec that I'm using for my other personal projects. While I do use it in a "production" capacity, it is nowhere near "battle-tested". I am not responsible for any damage that may occur from using this project. Use at your own risk.

Pull requests are welcome, but please be nice.
