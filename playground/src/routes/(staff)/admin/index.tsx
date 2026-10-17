import type { Context } from "hono";

// the same path as the (admin)/admin page, with a different method
export async function onPost(ctx: Context) {
  return ctx.text("staff admin post");
}
