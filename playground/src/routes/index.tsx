import { SERVER_ONLY_MARKER } from "../lib/db";

export default function Home(_props: RouteProps) {
  return (
    <>
      <h1>home</h1>
      <p data-marker={SERVER_ONLY_MARKER}>rendered on the server</p>

      <form
        hx-post={(ctx: HonoContext, payload: { name: string }) => (
          <p id="greeting">hello {payload.name}</p>
        )}
      >
        <input type="text" name="name" />
        <button>greet</button>
      </form>

      <button id="ping" hx-post={() => {}}>
        ping
      </button>

      <button
        id="counter"
        hx-on:click={function (this: HTMLButtonElement) {
          this.textContent = `clicked ${Number(this.dataset.count ?? 0) + 1}`;
        }}
      >
        click me
      </button>
    </>
  );
}
