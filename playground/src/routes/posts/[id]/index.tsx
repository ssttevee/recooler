import { postTitle } from "../../../lib/db";

export const head: RouteHeadFn = (ctx, prev) => ({
  ...prev,
  title: `${postTitle(ctx.req.param("id")!)} - ${prev.title}`,
});

export default function Post({ ctx }: RouteProps) {
  return <h1>posts-id {ctx.req.param("id")}</h1>;
}
