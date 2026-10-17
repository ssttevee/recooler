// Type checks of the package's entry points as a consumer imports them, by its own name. Checked
// by types.test.mjs under each moduleResolution.
import recooler from "farm-plugin-recooler";
import { jsxRouteHandler, type JSXRouteHandlerFactory } from "farm-plugin-recooler/helpers";

type Options = NonNullable<Parameters<typeof recooler>[0]>;

export const plugin: [binPath: string, options: Options | undefined] = recooler({
  src_dir: "src",
  routes_dir: "src/routes",
  action_secret: "secret",
  client_banned_imports: ["/lib/db"],
});
export const factory: JSXRouteHandlerFactory = jsxRouteHandler;

export const factoryForms: Options["route_handler_factory"][] = [
  "/abs/factory",
  ["/abs/factory"],
  ["/abs/factory", "named"],
  // a null name imports the default export
  ["/abs/factory", null],
];

// @ts-expect-error unknown option
recooler({ route_handler_factroy: "/abs/factory" });
// @ts-expect-error the export name is a string or null
recooler({ route_handler_factory: ["/abs/factory", 1] });
// @ts-expect-error only a module and an export name
recooler({ route_handler_factory: ["/abs/factory", "named", "extra"] });
// @ts-expect-error a module is required
recooler({ route_handler_factory: [] });
