// Checks the app built by `pnpm build` (build/main.js) through hono's `app.request()`.
import assert from "node:assert/strict";
import { test } from "node:test";

const { default: app } = await import("../build/main.js");

async function get(path) {
  const res = await app.request(path);
  return { res, html: await res.text() };
}

function actionUrls(html) {
  return [...html.matchAll(/hx-post="([^"]+)"/g)].map((m) => m[1].replaceAll("&amp;", "&"));
}

test("static route renders through the layout and root component", async () => {
  const { res, html } = await get("/");
  assert.equal(res.status, 200);
  assert.match(html, /^<!DOCTYPE html>/);
  assert.match(html, /<div id="app"><main data-layout="root"><h1>home<\/h1>/);
});

test("root head object and layout head function build the title", async () => {
  const { html } = await get("/");
  assert.match(html, /<title>Playground - Recooler<\/title>/);
});

test("route head function runs last and sees the route params", async () => {
  const { html } = await get("/posts/1");
  assert.match(html, /<title>Hello world - Playground - Recooler<\/title>/);
});

test("[param] route matches a single segment", async () => {
  const { res, html } = await get("/posts/42");
  assert.equal(res.status, 200);
  assert.match(html, /<h1>posts-id 42<\/h1>/);
});

test("[[catchall]] route matches multiple segments", async () => {
  const { res, html } = await get("/posts/a/b");
  assert.equal(res.status, 200);
  assert.match(html, /<h1>posts-rest a\/b<\/h1>/);
});

// TODO: today `[param]` routes are registered before their static siblings, so /posts/new
// renders posts/[id]. Drop `todo` once the route ordering fix (static > param > catch-all) lands.
test("static route takes precedence over a [param] sibling", { todo: true }, async () => {
  const { html } = await get("/posts/new");
  assert.match(html, /<h1>posts-new<\/h1>/);
});

test("unknown routes outside the catch-all are not found", async () => {
  const { res } = await get("/nope");
  assert.equal(res.status, 404);
});

test("group middleware applies to the group's routes only", async () => {
  const admin = await get("/admin");
  assert.equal(admin.res.status, 200);
  assert.match(admin.html, /<h1>admin<\/h1>/);
  assert.equal(admin.res.headers.get("x-group"), "admin");

  const home = await get("/");
  assert.equal(home.res.headers.get("x-group"), null);
});

test("form action returning JSX responds 200 with the rendered html", async () => {
  const { html } = await get("/");
  const [greet] = actionUrls(html);
  assert.ok(greet, "the form has an hx-post action url");

  const body = new FormData();
  body.set("name", "Ada");
  const res = await app.request(greet, { method: "POST", body });
  assert.equal(res.status, 200);
  assert.match(await res.text(), /<p id="greeting">hello Ada<\/p>/);
});

test("form action returning nothing responds 204", async () => {
  const { html } = await get("/");
  const [, ping] = actionUrls(html);
  assert.ok(ping, "the ping button has an hx-post action url");

  const res = await app.request(ping, { method: "POST", body: new FormData() });
  assert.equal(res.status, 204);
});

test("hx-on:click handler is inlined as a client script without server-only code", async () => {
  const { html } = await get("/");
  assert.match(html, /<button id="counter" hx-on:click="\$handlers\[[^"]+\]\.call\(this, event\)">/);

  const script = html.match(/<script>([\s\S]*?)<\/script>/)?.[1];
  assert.ok(script, "the page has an inline client script");
  assert.match(script, /window\.\$handlers=/);
  assert.match(script, /clicked \$\{/);
  assert.doesNotMatch(script, /server-only-db-module/);
});

test("pages without event handlers have no client script", async () => {
  const { html } = await get("/admin");
  assert.doesNotMatch(html, /<script>/);
});
