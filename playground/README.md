# Recooler playground

A minimal app built with the plugin from this repository, used by CI as an end-to-end check. It
loads the plugin through the `farm-plugin-recooler` workspace package, so it needs a plugin binary
for your platform in `npm/<platform>/index.farm` and the compiled helpers (`src/helpers.js`).

## Running it locally

From the repository root:

```sh
# build the plugin binary and copy it where scripts/index.js looks first
# (use your platform's directory and library name, e.g. npm/linux-x64-gnu and libfarm_plugin_recooler.so)
cargo build --release
cp target/release/libfarm_plugin_recooler.dylib npm/darwin-arm64/index.farm

pnpm install --frozen-lockfile
pnpm exec tsc # emits src/helpers.js

pnpm --filter playground build # farm build -> playground/build/main.js
pnpm --filter playground check # node --test against the built hono app
```

## What it covers

| Feature | Where |
| --- | --- |
| static, `[param]` and `[[catchall]]` routes | `src/routes/posts/{new,[id],[[rest]]}` |
| group middleware | `src/routes/(admin)/middleware.ts` (sets `x-group: admin` on `/admin` only) |
| a blocking group guard, incl. fall-through from a public page's form actions, the same path in two groups, mounting under a prefix and re-dispatching a request | `src/routes/(staff)/middleware.ts` with `(staff)/posts/[postId]` and `(staff)/admin` (`onPost` only) |
| a page next to a same-shaped `onPost`-only route of another group | `src/routes/posts/[id]` (page with an action) and `(staff)/posts/[postId]` |
| layout with a `head` function | `src/routes/layout.tsx` |
| root component and `head` object (with its own charset, which must not be duplicated) | `src/root.tsx` |
| route `head` function using params | `src/routes/posts/[id]/index.tsx` |
| form actions (`hx-post`) returning JSX (200) and nothing (204) | `src/routes/index.tsx` |
| `hx-on:click` event handler inlined as a client script | `src/routes/index.tsx` |
| `client_banned_imports` | `farm.config.ts` bans `src/lib/db.ts`, which pages import but handlers must not |

The tests in `test/app.test.mjs` import `build/main.js` and assert on `app.request()` responses.
