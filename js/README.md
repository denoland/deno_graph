# deno_graph

[![jsr](https://jsr.io/badges/@deno/graph)](https://jsr.io/@deno/graph)

This repository includes a compiled version of the Rust crate as Web Assembly
and exposes an interface which is available via the `mod.ts` in this repository
and can be imported like:

```shell
deno add @deno/graph
```

```js
import * as denoGraph from "@deno/graph";
```

Where `{VERSION}` should be substituted with the specific version you want to
use.

### `createGraph()`

The `createGraph()` function allows a module graph to be built asynchronously
and returns a single JavaScript object containing the module graph information.
It requires a root specifier or any array of root specifiers to be passed, which
will serve as the roots of the module graph.

There are several options that can be passed the function in the optional
`options` argument:

- `load` - a callback function that takes a URL string and a flag indicating if
  the dependency was required dynamically (e.g.
  `const m = await import("mod.ts")`) and resolves with a `LoadResponse`. By
  default a `load()` function that will attempt to load local modules via
  `Deno.readFile()` and load remote modules via `fetch()`.
- `cacheInfo` - a callback function that takes a URL string and returns a
  `CacheInfo` object. In the Deno CLI, the `DENO_DIR` cache info is passed back
  using this interface. If the function is not provided, the information is not
  present in the module graph.
- `resolve` - a callback function that takes a string and a referring URL string
  and returns a fully qualified URL string. In the Deno CLI, import maps provide
  this callback functionality of potentially resolving modules differently than
  the default resolution.
- `resolveTypes` - a callback function that takes a URL string and returns the
  types dependency for the specifier, along with optionally the source of the
  types dependency. This only gets called in situations where the module is
  potentially untyped (e.g. JavaScript, JSX) and no other type dependency was
  detected. This is intended to enrich the module graph with external types that
  are resolved in some other fashion, like from a `package.json` or via
  detecting an `@types` typings package is available.

## Usage with `import_map`

You can use [`import_map`](https://deno.land/x/import_map) in combination with
`deno_graph` in order to fully resolve the specifiers in your graph.

```ts
import { createGraph } from "@deno/graph";
import { resolve, toFileUrl } from "@std/path";
import { parseFromJson } from "https://deno.land/x/import_map@v0.18.3/mod.ts";

const path = resolve("./entrypoint.ts");
const importMap = Deno.readTextFileSync(
  new URL("./deno.json", import.meta.url),
);
// resolves the import map and provides a function we can use to resolve specifiers
const resolvedImportMap = await parseFromJson(
  toFileUrl(resolve("deno.json")),
  importMap,
);
const graph = await createGraph(toFileUrl(path).href, {
  /** use the callback in order to resolve specifiers like this:
   * dependencies: [ { specifier: "@/someFiles/d.ts", code: [Object] } ],
   * this allows for the entire module graph to be explored */
  resolve: resolvedImportMap.resolve.bind(resolvedImportMap),
});
console.log(graph);
```
