import type { Context } from "hono";

// no page: a POST to /posts/<anything>, including the public /posts/new page and the
// public /posts/[id] page, falls through to this handler and must pass the (staff) guard
export async function onPost(ctx: Context) {
  return ctx.text(`staff post ${ctx.req.param("postId")}`);
}
