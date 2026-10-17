import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

const tsc = createRequire(import.meta.url).resolve("typescript/bin/tsc");
const project = fileURLToPath(new URL("./types/tsconfig.json", import.meta.url));

// the import condition types the plugin function and its options, the require condition the
// binary path, under every moduleResolution that reads the exports map
for (const [module, moduleResolution] of [
  ["esnext", "bundler"],
  ["node16", "node16"],
  ["nodenext", "nodenext"],
]) {
  test(`package types check under moduleResolution ${moduleResolution}`, () => {
    const args = [tsc, "-p", project, "--module", module, "--moduleResolution", moduleResolution];
    try {
      execFileSync(process.execPath, args, { encoding: "utf8" });
    } catch (err) {
      assert.fail(err.stdout || err.message);
    }
  });
}
