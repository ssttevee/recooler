import type { MiddlewareHandler } from "hono";

// a blocking group guard: every request handled by a (staff) route needs the x-staff header,
// including requests that another route passed along with `next()`
const middleware: MiddlewareHandler = async (ctx, next) => {
  if (ctx.req.header("x-staff") !== "yes") {
    return ctx.text("staff only", 401);
  }
  await next();
};

export default middleware;
