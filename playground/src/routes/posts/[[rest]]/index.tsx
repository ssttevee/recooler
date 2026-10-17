export default function PostsCatchAll({ ctx }: RouteProps) {
  return <h1>posts-rest {ctx.req.param("rest")}</h1>;
}
