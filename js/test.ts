// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

import {
  assert,
  assertEquals,
  assertRejects,
  assertThrows,
} from "https://deno.land/std@0.181.0/testing/asserts.ts";
import { LoadResponseModule } from "./types.ts";
import {
  createGraph,
  init,
  load,
  LoadResponse,
  MediaType,
  parseModule,
} from "./mod.ts";

Deno.test({
  name: "createGraph()",
  async fn() {
    const graph = await createGraph("https://example.com/a", {
      load(specifier, isDynamic) {
        assert(!isDynamic);
        assertEquals(specifier, "https://example.com/a");
        return Promise.resolve({
          kind: "module",
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
    assertEquals(graph, {
      roots: ["https://example.com/a"],
      modules: [
        {
          specifier: "https://example.com/a",
          kind: "esm",
          mediaType: MediaType.TypeScript,
          size: 56,
        },
      ],
      redirects: {},
    });
  },
});

Deno.test({
  name: "createGraph() - data url",
  async fn() {
    const graph = await createGraph(
      "data:application/javascript;base64,Y29uc29sZS5sb2coImhlbGxvIGRlbm9fZ3JhcGgiKTs=",
    );
    assertEquals(graph.modules.length, 1);
    assertEquals(graph, {
      modules: [{
        specifier:
          "data:application/javascript;base64,Y29uc29sZS5sb2coImhlbGxvIGRlbm9fZ3JhcGgiKTs=",
        size: 32,
        kind: "esm",
        mediaType: MediaType.JavaScript,
      }],
      redirects: {},
      roots: [
        "data:application/javascript;base64,Y29uc29sZS5sb2coImhlbGxvIGRlbm9fZ3JhcGgiKTs=",
      ],
    });
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
    assertEquals(graph.modules.length, 1);
    assertEquals(
      graph.modules[0].error,
      'Module not found "file:///a/test.ts".',
    );
  },
});

Deno.test({
  name: "createGraph() - invalid URL",
  async fn() {
    await assertRejects(
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
    assertEquals(graph.modules.length, 1);
    assertEquals(
      graph.modules[0].error,
      "load rejected or errored",
    );
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
    assertEquals(graph.modules.length, 1);
    assertEquals(
      graph.modules[0].error,
      "load rejected or errored",
    );
  },
});

Deno.test({
  name: "createGraph() - default load - remote modules",
  async fn() {
    const graph = await createGraph(
      "https://deno.land/std@0.103.0/examples/chat/server.ts",
    );
    assertEquals(graph.modules.length, 37);
    const rootModule = graph.modules.find((m) =>
      m.specifier === graph.roots[0]
    )!;
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
        kind: "module",
        specifier: "file:///a/test.js",
        content: `import * as b from "./b.js";`,
      },
      "file:///a/b.js": {
        kind: "module",
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
    assertEquals(resolveCount, 2);
    assertEquals(graph, {
      "roots": [
        "file:///a/test.js",
      ],
      "modules": [
        {
          "kind": "esm",
          "size": 21,
          "mediaType": MediaType.JavaScript,
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
          "mediaType": MediaType.JavaScript,
          "specifier": "file:///a/test.js",
        },
      ],
      "redirects": {},
    });
  },
});

Deno.test({
  name: "createGraph() - imports",
  async fn() {
    const fixtures: Record<string, LoadResponse> = {
      "file:///a/test.ts": {
        kind: "module",
        specifier: "file:///a/test.ts",
        content: `import config from "./deno.json" assert { type: "json" };`,
      },
      "file:///a/deno.json": {
        kind: "module",
        specifier: "file:///a/deno.json",
        content: `{}`,
      },
      "https://esm.sh/preact/jsx-runtime": {
        kind: "module",
        specifier: "https://esm.sh/preact/jsx-runtime",
        headers: { "content-type": "application/javascript " },
        content: `export function jsx() {}`,
      },
    };
    let resolveCount = 0;
    const graph = await createGraph("file:///a/test.ts", {
      resolve(specifier, referrer) {
        resolveCount++;
        return new URL(specifier, referrer).toString();
      },
      load(specifier) {
        return Promise.resolve(fixtures[specifier]);
      },
      imports: {
        "file:///a/deno.json": ["https://esm.sh/preact/jsx-runtime"],
      },
    });
    assertEquals(resolveCount, 3);
    assertEquals(graph, {
      "roots": [
        "file:///a/test.ts",
      ],
      "modules": [
        {
          "specifier": "file:///a/deno.json",
          "kind": "asserted",
          "mediaType": MediaType.Json,
          "size": 2,
        },
        {
          "specifier": "file:///a/test.ts",
          "kind": "esm",
          "dependencies": [
            {
              "specifier": "./deno.json",
              "code": {
                "specifier": "file:///a/deno.json",
                "span": {
                  "start": {
                    "line": 0,
                    "character": 19,
                  },
                  "end": {
                    "line": 0,
                    "character": 32,
                  },
                },
              },
              "assertionType": "json",
            },
          ],
          "mediaType": MediaType.TypeScript,
          "size": 57,
        },
        {
          "specifier": "https://esm.sh/preact/jsx-runtime",
          "kind": "esm",
          "mediaType": MediaType.JavaScript,
          "size": 24,
        },
      ],
      "imports": [
        {
          "referrer": "file:///a/deno.json",
          "dependencies": [
            {
              "specifier": "https://esm.sh/preact/jsx-runtime",
              "type": {
                "span": {
                  "start": {
                    "character": 0,
                    "line": 0,
                  },
                  "end": {
                    "character": 0,
                    "line": 0,
                  },
                },
                "specifier": "https://esm.sh/preact/jsx-runtime",
              },
            },
          ],
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
        kind: "module",
        specifier: "file:///a/test.js",
        content: `import * as b from "./b.js";`,
      },
      "file:///a/b.js": {
        kind: "module",
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
    assertEquals(resolveCount, 2);
    assertEquals(graph, {
      "roots": [
        "file:///a/test.js",
      ],
      "modules": [
        {
          "kind": "esm",
          "size": 21,
          "mediaType": MediaType.JavaScript,
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
          "mediaType": MediaType.JavaScript,
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
              kind: "module",
              specifier: "file:///a.js",
              content: `export const a = "a";`,
            });
          } else {
            return Promise.resolve({
              kind: "module",
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
  name: "createGraph() - load - external",
  async fn() {
    const fixtures: Record<string, LoadResponse> = {
      "file:///a/test.js": {
        kind: "module",
        specifier: "file:///a/test.js",
        content: `import * as fs from "builtin:fs";
          import * as bundle from "https://example.com/bundle";`,
      },
      "builtin:fs": {
        kind: "external",
        specifier: "builtin:fs",
      },
      "https://example.com/bundle": {
        kind: "external",
        specifier: "https://example.com/bundle",
      },
    };
    const graph = await createGraph("file:///a/test.js", {
      load(specifier) {
        return Promise.resolve(fixtures[specifier]);
      },
    });
    assertEquals(graph, {
      "roots": [
        "file:///a/test.js",
      ],
      "modules": [
        {
          "kind": "external",
          "specifier": "builtin:fs",
        },
        {
          "dependencies": [
            {
              "specifier": "builtin:fs",
              "code": {
                "specifier": "builtin:fs",
                "span": {
                  "start": {
                    "line": 0,
                    "character": 20,
                  },
                  "end": {
                    "line": 0,
                    "character": 32,
                  },
                },
              },
            },
            {
              "specifier": "https://example.com/bundle",
              "code": {
                "specifier": "https://example.com/bundle",
                "span": {
                  "start": {
                    "line": 1,
                    "character": 34,
                  },
                  "end": {
                    "line": 1,
                    "character": 62,
                  },
                },
              },
            },
          ],
          "kind": "esm",
          "size": 97,
          "mediaType": MediaType.JavaScript,
          "specifier": "file:///a/test.js",
        },
        {
          "kind": "external",
          "specifier": "https://example.com/bundle",
        },
      ],
      "redirects": {},
    });
  },
});

Deno.test({
  name: "load() - remote module",
  async fn() {
    const response = await load(
      "https://deno.land/std@0.103.0/examples/chat/server.ts",
    ) as LoadResponseModule;
    assert(response);
    assert(response.kind === "module");
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
  async fn() {
    await init();
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
    assertEquals(module, {
      "specifier": "file:///a/test01.js",
      "mediaType": MediaType.JavaScript,
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
            "start": { "line": 1, "character": 29 },
            "end": { "line": 1, "character": 44 },
          },
        },
      },
    });
  },
});

Deno.test({
  name: "parseModule() - with headers",
  async fn() {
    await init();
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
  async fn() {
    await init();
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
      module.dependencies?.find((d) =>
        d.specifier === "http://example.com/preact/jsx-dev-runtime"
      ),
    );
  },
});

Deno.test({
  name: "parseModule() - with defaultJsxImportSource",
  async fn() {
    await init();
    const module = parseModule(
      `file:///a/test01.tsx`,
      `
    export function A() {
      <div>Hello Deno</div>
    }`,
      {
        defaultJsxImportSource: "http://example.com/preact",
      },
    );
    assert(
      module.dependencies?.find((d) =>
        d.specifier === "http://example.com/preact/jsx-runtime"
      ),
    );
  },
});

Deno.test({
  name: "parseModule() - invalid URL",
  async fn() {
    await init();
    assertThrows(
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
  async fn() {
    await init();
    assertThrows(
      () => {
        parseModule("file:///a/test.md", `# Some Markdown\n\n**bold**`);
      },
      Error,
      "The module's source code could not be parsed",
    );
  },
});

Deno.test({
  name: "parseModule() - import attributes",
  async fn() {
    await init();
    const module = parseModule(
      "file:///a/test01.js",
      `
        import a from "./a.json" with { type: "json" };
        await import("./b.json", { with: { type: "json" } });
      `,
    );
    assertEquals(module, {
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
      "mediaType": MediaType.JavaScript,
      "size": 125,
      "specifier": "file:///a/test01.js",
    });
  },
});

Deno.test({
  name: "parseModule() - triple slash directives in typescript",
  async fn() {
    await init();
    const module = parseModule(
      "file:///a/foo.ts",
      `
        /// <reference path="./a.d.ts" />
        /// <reference types="./b.d.ts" />
      `,
    );
    assertEquals(module, {
      "dependencies": [
        {
          "specifier": "./a.d.ts",
          "type": {
            "specifier": "file:///a/a.d.ts",
            "span": {
              "start": {
                "line": 1,
                "character": 28,
              },
              "end": {
                "line": 1,
                "character": 38,
              },
            },
          },
        },
        {
          "specifier": "./b.d.ts",
          "type": {
            "specifier": "file:///a/b.d.ts",
            "span": {
              "start": {
                "line": 2,
                "character": 29,
              },
              "end": {
                "line": 2,
                "character": 39,
              },
            },
          },
        },
      ],
      "kind": "esm",
      "mediaType": MediaType.TypeScript,
      "size": 92,
      "specifier": "file:///a/foo.ts",
    });
  },
});
