// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.102.0/testing/asserts.ts";
import { createGraph } from "./mod.ts";

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
    console.log(graph);
  },
});
