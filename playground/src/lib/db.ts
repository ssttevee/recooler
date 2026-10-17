// Stands in for a server-only module (e.g. a database client). It is listed in
// `client_banned_imports`, so the build fails if it ever ends up in a client script.
export const SERVER_ONLY_MARKER = "server-only-db-module";

const posts: Record<string, string> = { "1": "Hello world" };

export function postTitle(id: string): string {
  return posts[id] ?? `Untitled ${id}`;
}
