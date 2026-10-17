import type { Component } from "easy-jsx-html-engine";
import { DefaultRootHOC } from "farm-plugin-recooler/helpers";

export const head: RouteHead = { title: "Recooler" };

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
