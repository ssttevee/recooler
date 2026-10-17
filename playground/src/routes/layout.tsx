import type { Component } from "easy-jsx-html-engine";

export const head: RouteHeadFn = (_ctx, prev) => ({
  ...prev,
  title: `Playground - ${prev.title}`,
});

export default function Layout(Page: Component<RouteProps>) {
  return (props: RouteProps) => (
    <main data-layout="root">
      <Page {...props} />
    </main>
  );
}
