// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.102.0/testing/asserts.ts";
import { createGraph, load, parseModule } from "./mod.ts";

Deno.test({
  name: "createGraph - basic usage",
  async fn() {
    const graph = await createGraph("https://example.com/a.ts", {
      load(specifier, isDynamic) {
        assert(!isDynamic);
        assertEquals(specifier, "https://example.com/a.ts");
        return Promise.resolve({
          specifier,
          headers: {
            "content-type": "application/typescript",
          },
          content: `console.log("hello deno_graph!")`,
        });
      },
    });
    console.log(graph.toJSON());
  },
});

Deno.test({
  name: "createGraph with load - remote modules",
  async fn() {
    const graph = await createGraph(
      "https://deno.land/std@0.103.0/examples/chat/server.ts",
      {
        load,
      },
    );
    console.log(JSON.stringify(graph.toJSON(), undefined, "  "));
  },
});

Deno.test({
  name: "parseModule - basic usage",
  fn() {
    const module = parseModule(
      "file:///a/test01.ts",
      `
        import { a } from "./a.ts";
        import * as b from "./b.ts";
        export { c } from "./c.ts";
        const d = await import("./d.ts");
      `,
    );
    console.log(JSON.stringify(module.toJSON(), undefined, "  "));
  },
});
