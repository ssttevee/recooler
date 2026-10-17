import { postTitle } from "../../../lib/db";

export const head: RouteHeadFn = (ctx, prev) => ({
  ...prev,
  title: `${postTitle(ctx.req.param("id")!)} - ${prev.title}`,
});

export default function Post({ ctx }: RouteProps) {
  return (
    <div>
      <h1>posts-id {ctx.req.param("id")}</h1>
      {/* the same shape as the (staff) route at /posts/:postId, which only handles POST: this
          page's form actions run first, and other POSTs fall through to the staff route */}
      <button hx-post={() => <p id="liked">liked</p>}>like</button>
    </div>
  );
}
