import assert from "node:assert/strict";
import { describe, test } from "node:test";
import { register } from "node:module";
import { Hono } from "hono";

register("./extensionless-hooks.mjs", import.meta.url);
const { actionsMiddleware } = await import("../src/helpers.js");
const { createElement } = await import("easy-jsx-html-engine");

function post(app, path) {
  return app.request(path, { method: "POST" });
}

describe("actionsMiddleware", () => {
  function actionsApp() {
    const app = new Hono();
    app.post("/page", actionsMiddleware({ "page@0": (ctx) => ctx.text("action") }));
    app.post("/page", (ctx) => ctx.text("onPost"));
    // registered later and matches every path, like a catch-all route or group middleware
    app.post("/*", (ctx) => ctx.text("catch-all"));
    return app;
  }

  test("runs the named action", async () => {
    const res = await post(actionsApp(), "/page?action=page%400");
    assert.equal(res.status, 200);
    assert.equal(await res.text(), "action");
  });

  test("passes requests without an action to the route's handlers", async () => {
    const res = await post(actionsApp(), "/page");
    assert.equal(res.status, 200);
    assert.equal(await res.text(), "onPost");
  });

  test("rejects unknown actions instead of falling through", async () => {
    for (const id of ["bogus", "", "null", "constructor", "toString"]) {
      const res = await post(actionsApp(), `/page?action=${id}`);
      assert.equal(res.status, 404, id);
      assert.equal(await res.text(), "unknown action", id);
    }
  });

  test("passes actions of a later middleware along", async () => {
    // the generated app chains the root component's actions before the page's, and an
    // overlapping route (e.g. a catch-all mounted by a group) may be registered first
    const app = new Hono();
    app.post("/:rest{.+}", actionsMiddleware({ "other@0": (ctx) => ctx.text("other") }));
    app.post(
      "/page",
      actionsMiddleware({ "root@0": (ctx) => ctx.text("root") }),
      actionsMiddleware({ "page2@0": (ctx) => ctx.text("page") }),
    );

    assert.equal(await (await post(app, "/page?action=root%400")).text(), "root");
    assert.equal(await (await post(app, "/page?action=page2%400")).text(), "page");
    assert.equal(await (await post(app, "/page?action=other%400")).text(), "other");
    assert.equal((await post(app, "/page?action=nope")).status, 404);
  });
});

describe("actionsMiddleware results", () => {
  async function run(action) {
    const app = new Hono();
    app.post("/page", actionsMiddleware({ "result@0": action }));
    return app.request("/page?action=result%400", { method: "POST" });
  }

  async function html(action) {
    const res = await run(action);
    assert.equal(res.status, 200);
    assert.match(res.headers.get("content-type"), /^text\/html/);
    return res.text();
  }

  test("renders strings escaped", async () => {
    assert.equal(await html(() => "a <b> & c"), "a &lt;b&gt; &amp; c");
  });

  test("renders numbers", async () => {
    assert.equal(await html(() => 42), "42");
  });

  test("renders arrays", async () => {
    assert.equal(
      await html(() => ["a", createElement("b", {}, "b"), 3, [null, "d"]]),
      "a<b>b</b>3d",
    );
  });

  test("renders jsx", async () => {
    assert.equal(await html(() => createElement("i", {}, "jsx")), "<i>jsx</i>");
  });

  test("renders promises, including nested ones", async () => {
    assert.equal(await html(async () => "async"), "async");
    assert.equal(
      await html(() => [Promise.resolve(createElement("i", {}, Promise.resolve("p"))), "!"]),
      "<i>p</i>!",
    );
  });

  test("renders false and empty strings as an empty body", async () => {
    assert.equal(await html(() => false), "");
    assert.equal(await html(() => ""), "");
  });

  test("returns no content for undefined and null", async () => {
    for (const action of [() => {}, () => null, async () => undefined]) {
      const res = await run(action);
      assert.equal(res.status, 204);
      assert.equal(await res.text(), "");
    }
  });

  test("returns responses as is", async () => {
    const res = await run((ctx) => ctx.text("plain", 201));
    assert.equal(res.status, 201);
    assert.equal(await res.text(), "plain");
  });

  test("returns a response thrown while rendering", async () => {
    const Redirect = async () => {
      throw new Response(null, { status: 302, headers: { location: "/elsewhere" } });
    };
    for (const action of [
      () => createElement(Redirect, {}),
      () => ["before", createElement("div", {}, createElement(Redirect, {}))],
      () => {
        throw new Response(null, { status: 302, headers: { location: "/elsewhere" } });
      },
    ]) {
      const res = await run(action);
      assert.equal(res.status, 302);
      assert.equal(res.headers.get("location"), "/elsewhere");
    }
  });
});
