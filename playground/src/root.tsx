import type { Component } from "easy-jsx-html-engine";
import { DefaultRootHOC } from "farm-plugin-recooler/helpers";

// declares its own charset, so DefaultRootHOC must not add a second one
export const head: RouteHead = { title: "Recooler", metas: [{ charset: "utf-8" }] };

export default function Root(Content: Component<RouteProps>, clientScript?: string) {
  return DefaultRootHOC(
    (props: RouteProps) => (
      <div id="app">
        <Content {...props} />
      </div>
    ),
    clientScript,
  );
}
