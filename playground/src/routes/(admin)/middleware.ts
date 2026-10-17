import type { MiddlewareHandler } from "hono";

// group middleware: only applies to the routes inside (admin)
const middleware: MiddlewareHandler = async (ctx, next) => {
  await next();
  ctx.header("x-group", "admin");
};

export default middleware;
