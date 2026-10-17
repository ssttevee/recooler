import assert from "node:assert/strict";
import { describe, test } from "node:test";
import { register } from "node:module";
import { Hono } from "hono";

register("./extensionless-hooks.mjs", import.meta.url);
const { actionsMiddleware, buildHeadFn, DefaultRootHOC, jsxRouteHandler } = await import(
  "../src/helpers.js"
);
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

function pageApp(headFn, Page = () => createElement("p", {}, "page")) {
  const app = new Hono();
  app.get("/", jsxRouteHandler(DefaultRootHOC, Page, headFn));
  return app;
}

async function getText(app, path = "/") {
  const res = await app.request(path);
  assert.equal(res.status, 200);
  return res.text();
}

describe("DefaultRootHOC", () => {
  function charsetCount(html) {
    return html.match(/<meta[^>]*charset/gi)?.length ?? 0;
  }

  test("emits a utf-8 charset by default", async () => {
    for (const head of [undefined, {}, { metas: [{ name: "viewport", content: "width=device-width" }] }]) {
      const html = await getText(pageApp(buildHeadFn([], head)));
      assert.equal(charsetCount(html), 1, html);
      assert.match(html, /<meta charset="utf-8"\/>/);
    }
  });

  test("does not duplicate a charset declared by the head", async () => {
    for (const meta of [{ charset: "utf-8" }, { charSet: "utf-8" }, { CHARSET: "iso-8859-1" }]) {
      const html = await getText(
        pageApp(buildHeadFn([], { metas: [meta, { name: "x", content: "y" }] })),
      );
      assert.equal(charsetCount(html), 1, html);
      assert.match(html, /<meta name="x" content="y"\/>/);
    }
  });
});

describe("buildHeadFn", () => {
  test("does not let head functions mutate the exported head object", async () => {
    const head = {
      title: "title",
      metas: [{ name: "a", content: "a" }],
      links: [{ rel: "icon", href: "/icon.png" }],
      scripts: [{ src: "/a.js" }],
    };
    const snapshot = structuredClone(head);
    const headFn = buildHeadFn(
      [
        (_ctx, prev) => {
          prev.title += "!";
          prev.metas.push({ name: "b", content: "b" });
          prev.links.push({ rel: "stylesheet", href: "/b.css" });
          prev.scripts.push({ src: "/b.js" });
          return prev;
        },
      ],
      head,
    );
    const app = pageApp(headFn);

    for (let i = 0; i < 2; i++) {
      const html = await getText(app);
      assert.equal(html.match(/<title>title!<\/title>/g)?.length, 1, html);
      assert.equal(html.match(/name="b"/g)?.length, 1, html);
      assert.equal(html.match(/href="\/b\.css"/g)?.length, 1, html);
      assert.equal(html.match(/src="\/b\.js"/g)?.length, 1, html);
    }
    assert.deepEqual(head, snapshot);
  });

  test("passes the exported head object's values through", async () => {
    const head = { title: "t", base: "/base/", metas: [{ name: "a", content: "a" }] };
    const headFn = buildHeadFn([], head);
    const first = await headFn({});
    assert.deepEqual(first, head);
    assert.notEqual(first, head);
    assert.notEqual(first.metas, head.metas);
    // element props are not cloned
    assert.equal(first.metas[0], head.metas[0]);
  });
});
