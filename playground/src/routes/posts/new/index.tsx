export default function NewPost(_props: RouteProps) {
  return (
    <div>
      <h1>posts-new</h1>
      {/* a form action on the public page: a POST without a matching `?action=` passes along to
          the (staff) route at /posts/:postId and must hit the staff guard there */}
      <form hx-post={() => <p id="drafted">drafted</p>}>
        <button>draft</button>
      </form>
    </div>
  );
}
