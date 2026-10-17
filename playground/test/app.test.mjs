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

test("a charset declared by the root head is not duplicated", async () => {
  const { html } = await get("/");
  assert.equal(html.match(/<meta charset=/gi)?.length, 1);
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

test("static route takes precedence over a [param] sibling", async () => {
  const { res, html } = await get("/posts/new");
  assert.equal(res.status, 200);
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

const post = (path, headers = {}) => app.request(path, { method: "POST", headers });

test("a page's form actions run before a same-shaped route of another group", async () => {
  // /posts/[id] (page) and (staff)/posts/[postId] (onPost) match the same requests: the page's
  // action registrations go first, and a POST without a matching action passes along
  const { html } = await get("/posts/7");
  const [like] = actionUrls(html);
  assert.ok(like, "the like button has an hx-post action url");
  const res = await app.request(like, { method: "POST", body: new FormData() });
  assert.equal(res.status, 200);
  assert.match(await res.text(), /<p id="liked">liked<\/p>/);
});

test("a blocking group guard runs when a request falls through to the group's route", async () => {
  // the public /posts/new page registers its form action for POST, which passes a POST without
  // a matching action along to (staff)/posts/[postId]; /posts/7 is a page too
  const { html } = await get("/posts/new");
  const [draft] = actionUrls(html);
  assert.ok(draft, "the draft form has an hx-post action url");
  const drafted = await app.request(draft, { method: "POST", body: new FormData() });
  assert.equal(drafted.status, 200);
  assert.match(await drafted.text(), /<p id="drafted">drafted<\/p>/);

  for (const path of ["/posts/new", "/posts/7"]) {
    const denied = await post(path);
    assert.equal(denied.status, 401, path);
    assert.equal(await denied.text(), "staff only");

    const allowed = await post(path, { "x-staff": "yes" });
    assert.equal(allowed.status, 200, path);
    assert.equal(await allowed.text(), `staff post ${path.slice("/posts/".length)}`);
  }
  // the guard doesn't run for the public pages themselves
  assert.equal((await get("/posts/7")).res.status, 200);
});

test("a blocking group guard runs for the same path registered by another group", async () => {
  assert.equal((await get("/admin")).res.status, 200);
  assert.equal((await post("/admin")).status, 401);
  assert.equal(await (await post("/admin", { "x-staff": "yes" })).text(), "staff admin post");
});

test("a blocking group guard runs when the app is mounted under a prefix", async () => {
  const { Hono } = await import("hono");
  const main = new Hono({ strict: true });
  main.route("/app", app);
  assert.equal((await main.request("/app/posts/7", { method: "POST" })).status, 401);
  assert.equal((await main.request("/app/posts/7", { method: "POST", headers: { "x-staff": "yes" } })).status, 200);
});

test("a blocking group guard runs again when the same request is dispatched again", async () => {
  const req = new Request("http://localhost/posts/7", { method: "POST" });
  assert.equal((await app.request(req)).status, 401);
  assert.equal((await app.request(req)).status, 401);
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

test("unknown form actions are rejected by the actions middleware", async () => {
  for (const action of ["nope", "constructor"]) {
    const res = await app.request(`/?action=${action}`, { method: "POST", body: new FormData() });
    assert.equal(res.status, 404);
    // the middleware's own 404, not hono's fallback for a POST nothing else handles
    assert.equal(await res.text(), "unknown action");
  }
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
