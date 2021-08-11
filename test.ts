// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.102.0/testing/asserts.ts";
import { createGraph, load, parseModule } from "./mod.ts";

Deno.test({
  name: "createGraph()",
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
    assertEquals(graph.toJSON(), {
      root: "https://example.com/a.ts",
      modules: [
        {
          specifier: "https://example.com/a.ts",
          dependencies: [],
          mediaType: "TypeScript",
          size: 32,
        },
      ],
      redirects: {},
    });
    assertEquals(
      graph.toString(true),
      `type: TypeScript
dependencies: 0 unique (total 32B)

https://example.com/a.ts (32B)
`,
    );
  },
});

Deno.test({
  name: "createGraph with load - remote modules",
  async fn() {
    const graph = await createGraph(
      "https://deno.land/std@0.103.0/examples/chat/server.ts",
      { load },
    );
    assertEquals(graph.modules.length, 37);
    const rootModule = graph.get(graph.root);
    assert(rootModule);
    assertEquals(rootModule.mediaType, "TypeScript");
    assertEquals(Object.entries(rootModule.dependencies).length, 3);
  },
});

Deno.test({
  name: "parseModule()",
  fn() {
    const module = parseModule(
      "file:///a/test01.js",
      `
        /// <reference types="./test01.d.ts" />
        import { a } from "./a.ts";
        import * as b from "./b.ts";
        export { c } from "./c.ts";
        const d = await import("./d.ts");
      `,
    );
    assertEquals(module.toJSON(), {
      "specifier": "file:///a/test01.js",
      "mediaType": "JavaScript",
      "size": 206,
      "dependencies": [{
        "specifier": "./a.ts",
        "code": {
          "specifier": "file:///a/a.ts",
          "span": {
            "start": { "line": 2, "character": 26 },
            "end": { "line": 2, "character": 34 },
          },
        },
      }, {
        "specifier": "./b.ts",
        "code": {
          "specifier": "file:///a/b.ts",
          "span": {
            "start": { "line": 3, "character": 27 },
            "end": { "line": 3, "character": 35 },
          },
        },
      }, {
        "specifier": "./c.ts",
        "code": {
          "specifier": "file:///a/c.ts",
          "span": {
            "start": { "line": 4, "character": 26 },
            "end": { "line": 4, "character": 34 },
          },
        },
      }, {
        "specifier": "./d.ts",
        "code": {
          "specifier": "file:///a/d.ts",
          "span": {
            "start": { "line": 5, "character": 31 },
            "end": { "line": 5, "character": 39 },
          },
        },
      }],
      "typesDependency": {
        "specifier": "./test01.d.ts",
        "dependency": {
          "specifier": "file:///a/test01.d.ts",
          "span": {
            "start": { "line": 1, "character": 29 },
            "end": { "line": 1, "character": 42 },
          },
        },
      },
    });
  },
});
