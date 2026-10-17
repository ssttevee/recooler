import assert from "node:assert/strict";
import { describe, test } from "node:test";
import { register } from "node:module";
import { Hono } from "hono";

register("./extensionless-hooks.mjs", import.meta.url);
const { actionsMiddleware } = await import("../src/helpers.js");

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
