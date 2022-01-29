// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import {
  assert,
  assertEquals,
  assertThrows,
  assertThrowsAsync,
} from "https://deno.land/std@0.104.0/testing/asserts.ts";
import { createGraph, load, parseModule } from "./mod.ts";
import type { LoadResponse } from "./mod.ts";

Deno.test({
  name: "createGraph()",
  async fn() {
    const graph = await createGraph("https://example.com/a", {
      load(specifier, isDynamic) {
        assert(!isDynamic);
        assertEquals(specifier, "https://example.com/a");
        return Promise.resolve({
          specifier,
          headers: {
            "content-type": "application/typescript; charset=utf-8",
          },
          content: `declare interface A {
            a: string;
          }`,
        });
      },
    });
    assertEquals(graph.toJSON(), {
      roots: ["https://example.com/a"],
      modules: [
        {
          specifier: "https://example.com/a",
          kind: "esm",
          mediaType: "TypeScript",
          size: 56,
        },
      ],
      redirects: {},
    });
    assertEquals(
      graph.toString(true),
      `type: TypeScript
dependencies: 0 unique (total 56B)

https://example.com/a (56B)
`,
    );
  },
});

Deno.test({
  name: "createGraph() - data url",
  async fn() {
    const graph = await createGraph(
      `data:application/javascript;base64,Y29uc29sZS5sb2coImhlbGxvIGRlbm9fZ3JhcGgiKTs=`,
    );
    assertEquals(graph.modules.length, 1);
    assertEquals(graph.modules[0].source, `console.log("hello deno_graph");`);
  },
});

Deno.test({
  name: "createGraph() - root module missing",
  async fn() {
    const graph = await createGraph("file:///a/test.ts", {
      load() {
        return Promise.resolve(undefined);
      },
    });
    assertEquals(graph.modules.length, 0);
  },
});

Deno.test({
  name: "createGraph() - invalid URL",
  fn() {
    return assertThrowsAsync(
      async () => {
        await createGraph("./bad.ts");
      },
      Error,
      "relative URL without a base",
    );
  },
});

Deno.test({
  name: "createGraph() - load rejects",
  async fn() {
    const graph = await createGraph("file:///a/test.ts", {
      load() {
        return Promise.reject(new Error("something bad happened"));
      },
    });
    assertEquals(graph.modules.length, 0);
  },
});

Deno.test({
  name: "createGraph() - load throws",
  async fn() {
    const graph = await createGraph("file:///a/test.ts", {
      load() {
        throw new Error("something bad happened");
      },
    });
    assertEquals(graph.modules.length, 0);
  },
});

Deno.test({
  name: "createGraph() - default load - remote modules",
  async fn() {
    const graph = await createGraph(
      "https://deno.land/std@0.103.0/examples/chat/server.ts",
    );
    assertEquals(graph.modules.length, 37);
    const rootModule = graph.get(graph.roots[0]);
    assert(rootModule);
    assertEquals(rootModule.mediaType, "TypeScript");
    assertEquals(Object.entries(rootModule.dependencies ?? {}).length, 3);
  },
});

Deno.test({
  name: "createGraph() - resolve - specifier only",
  async fn() {
    const fixtures: Record<string, LoadResponse> = {
      "file:///a/test.js": {
        specifier: "file:///a/test.js",
        content: `import * as b from "./b.js";`,
      },
      "file:///a/b.js": {
        specifier: "file:///a/b.js",
        content: `export const b = "b";`,
      },
    };
    let resolveCount = 0;
    const graph = await createGraph("file:///a/test.js", {
      resolve(specifier, referrer) {
        resolveCount++;
        return new URL(specifier, referrer).toString();
      },
      load(specifier) {
        return Promise.resolve(fixtures[specifier]);
      },
    });
    assertEquals(resolveCount, 1);
    assertEquals(graph.toJSON(), {
      "roots": [
        "file:///a/test.js",
      ],
      "modules": [
        {
          "kind": "esm",
          "size": 21,
          "mediaType": "JavaScript",
          "specifier": "file:///a/b.js",
        },
        {
          "dependencies": [
            {
              "specifier": "./b.js",
              "code": {
                "specifier": "file:///a/b.js",
                "span": {
                  "start": {
                    "line": 0,
                    "character": 19,
                  },
                  "end": {
                    "line": 0,
                    "character": 27,
                  },
                },
              },
            },
          ],
          "kind": "esm",
          "size": 28,
          "mediaType": "JavaScript",
          "specifier": "file:///a/test.js",
        },
      ],
      "redirects": {},
    });
  },
});

Deno.test({
  name: "createGraph() - resolve - resolve result",
  async fn() {
    const fixtures: Record<string, LoadResponse> = {
      "file:///a/test.js": {
        specifier: "file:///a/test.js",
        content: `import * as b from "./b.js";`,
      },
      "file:///a/b.js": {
        specifier: "file:///a/b.js",
        content: `export const b = "b";`,
      },
    };
    let resolveCount = 0;
    const graph = await createGraph("file:///a/test.js", {
      resolve(specifier, referrer) {
        resolveCount++;
        return {
          specifier: new URL(specifier, referrer).toString(),
          kind: "esm",
        };
      },
      load(specifier) {
        return Promise.resolve(fixtures[specifier]);
      },
    });
    assertEquals(resolveCount, 1);
    assertEquals(graph.toJSON(), {
      "roots": [
        "file:///a/test.js",
      ],
      "modules": [
        {
          "kind": "esm",
          "size": 21,
          "mediaType": "JavaScript",
          "specifier": "file:///a/b.js",
        },
        {
          "dependencies": [
            {
              "specifier": "./b.js",
              "code": {
                "specifier": "file:///a/b.js",
                "span": {
                  "start": {
                    "line": 0,
                    "character": 19,
                  },
                  "end": {
                    "line": 0,
                    "character": 27,
                  },
                },
              },
            },
          ],
          "kind": "esm",
          "size": 28,
          "mediaType": "JavaScript",
          "specifier": "file:///a/test.js",
        },
      ],
      "redirects": {},
    });
  },
});

Deno.test({
  name: "createGraph() - resolveTypes",
  async fn() {
    const graph = await createGraph(
      "file:///a.js",
      {
        load(specifier) {
          if (specifier === "file:///a.js") {
            return Promise.resolve({
              specifier: "file:///a.js",
              content: `export const a = "a";`,
            });
          } else {
            return Promise.resolve({
              specifier: "file:///a.d.ts",
              content: `export const a: "a";`,
            });
          }
        },
        resolveTypes(specifier) {
          assertEquals(specifier, "file:///a.js");
          return {
            types: "file:///a.d.ts",
          };
        },
      },
    );
    assertEquals(graph.modules.length, 2);
  },
});

Deno.test({
  name: "createGraph() - parsed source",
  async fn() {
    const graph = await createGraph(
      "https://deno.land/std@0.103.0/examples/chat/server.ts",
    );
    const rootModule = graph.get(graph.roots[0]);
    assert(rootModule);
    assertEquals(rootModule.mediaType, "TypeScript");
    const parsedSource = rootModule.parsedSource;
    assert(parsedSource);
    const transpiledSource = parsedSource.transpile();
    assert(transpiledSource);
    assert(typeof transpiledSource.text == "string");
  },
});

Deno.test({
  name: "load() - remote module",
  async fn() {
    const response = await load(
      "https://deno.land/std@0.103.0/examples/chat/server.ts",
    );
    assert(response);
    assertEquals(
      response.specifier,
      "https://deno.land/std@0.103.0/examples/chat/server.ts",
    );
    assertEquals(response.content.length, 2197);
  },
});

Deno.test({
  name: "load() - remote module - not found",
  async fn() {
    const response = await load(
      "https://deno.land/x/bad-url",
    );
    assertEquals(response, undefined);
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
      "kind": "esm",
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
        "isDynamic": true,
      }],
      "typesDependency": {
        "specifier": "./test01.d.ts",
        "dependency": {
          "specifier": "file:///a/test01.d.ts",
          "span": {
            "start": { "line": 1, "character": 30 },
            "end": { "line": 1, "character": 43 },
          },
        },
      },
    });
  },
});

Deno.test({
  name: "parseModule() - with headers",
  fn() {
    const module = parseModule(
      `https://example.com/a`,
      `declare interface A {
      a: string;
    }`,
      {
        headers: {
          "content-type": "application/typescript; charset=utf-8",
        },
      },
    );
    assertEquals(module.mediaType, "TypeScript");
  },
});

Deno.test({
  name: "parseModule() - with jsxImportSource pragma",
  fn() {
    const module = parseModule(
      `file:///a/test01.tsx`,
      `/* @jsxImportSource http://example.com/preact */
    export function A() {
      <div>Hello Deno</div>
    }`,
      {
        jsxImportSourceModule: "jsx-dev-runtime",
      },
    );
    assert(
      module.dependencies &&
        module.dependencies["http://example.com/preact/jsx-dev-runtime"],
    );
  },
});

Deno.test({
  name: "parseModule() - invalid URL",
  fn() {
    return assertThrows(
      () => {
        parseModule("./bad.ts", `console.log("hello");`);
      },
      Error,
      "relative URL without a base",
    );
  },
});

Deno.test({
  name: "parseModule() - syntax error",
  fn() {
    return assertThrows(
      () => {
        parseModule("file:///a/test.md", `# Some Markdown\n\n**bold**`);
      },
      Error,
      "The module's source code could not be parsed",
    );
  },
});

Deno.test({
  name: "parseModule() - import assertions",
  fn() {
    const module = parseModule(
      "file:///a/test01.js",
      `
        import a from "./a.json" assert { type: "json" };
        await import("./b.json", { assert: { type: "json" } });
      `,
    );
    assertEquals(module.toJSON(), {
      "dependencies": [
        {
          "specifier": "./a.json",
          "code": {
            "specifier": "file:///a/a.json",
            "span": {
              "start": {
                "line": 1,
                "character": 22,
              },
              "end": {
                "line": 1,
                "character": 32,
              },
            },
          },
          "assertionType": "json",
        },
        {
          "specifier": "./b.json",
          "code": {
            "specifier": "file:///a/b.json",
            "span": {
              "start": {
                "line": 2,
                "character": 21,
              },
              "end": {
                "line": 2,
                "character": 31,
              },
            },
          },
          "isDynamic": true,
          "assertionType": "json",
        },
      ],
      "kind": "esm",
      "mediaType": "JavaScript",
      "size": 129,
      "specifier": "file:///a/test01.js",
    });
  },
});
