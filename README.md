# deno_graph

The module graph logic for the Deno CLI.

This repository is a Rust crate which provides the foundational code to be able
to build a module graph, following the Deno CLI's module resolution logic. It
also provides a web assembly interface to the built code, making it possible to
leverage the logic outside of the Deno CLI from JavaScript/TypeScript.

## Usage from Deno CLI or Deploy

This repository includes a compiled version of the Rust crate as Web Assembly
and exposes an interface which is availble via the `mod.ts` in this repository
and can be imported like:

```js
import * as denoGraph from "https://deno.land/x/deno_graph@{VERSION}/mod.ts";
```

Where `{VERSION}` should be subtituted with the specific version you want to
use.

### `createGraph()`

The `createGraph()` function allows a module graph to be built asyncronously. It
requires a root specifier to be passed, which will serve as the root of the
module graph.

There are several options that can be passed the function in the optional
`options` argument:

- `load` - a callback function that takes a URL string and a flag indicating if
  the dependency was required dynamically (e.g.
  `const m = await import("mod.ts")`) and resolves with a `LoadResponse`. By
  default a `load()` function that will attempt to load local modules via
  `Deno.readFile()` and load remote modules via `fetch()`.
- `cacheInfo` - a callback function that takes a URL string and returns a
  `CaheInfo` object. In the Deno CLI, the `DENO_DIR` cache info is passed back
  using this interface. If the function is not provided, the information is not
  present in the module graph.
- `resolve` - a callback function that takes a string and a referring URL string
  and returns a fully qualified URL string. In the Deno CLI, import maps provide
  this callback functionality of potentially resolving modules differently than
  the default resolution.
- `check` - a callback function that takes a URL string and an `Uint8Array` of
  the byte content of a module to validate if that module sub resource integrity
  is valid. The callback should return `true` if it is, otherwise `false`. If
  the callback is not provided, all checks will pass.
- `getChecksum` - a callback function that takes an `Uint8Array` of the byte
  content of a module in order to be able to return a string which represent a
  checksum of the provided data. If the callback is not provided, the checksum
  will be generated in line with the way the Deno CLI generates the checksum.

### Replicating the Deno CLI

`deno_graph` is essentially a refactor of the module graph and related code as a
stand alone crate which also targets Web Assembly and provides a
JavaScript/TypeScript interface. This permits users of the package to be able to
replicate the `deno info` command in Deno CLI within the runtime environment
without requiring a `Deno` namesapce API.

The module graph has two methods which provide the output of `deno info`. The
method `toString()` provides the text output from `deno info` and `toJSON()`
provides the JSON object structure from `deno info --json`.

> ℹ️ currently, the Deno CLI hasn't been refactored to consume the `deno_graph`
> crate and there are minor differences in the output, specifically with the
> JSON output, and the current `deno info` command.

> ℹ️ currently, there is no runtime access to the `DENO_DIR`/Deno cache, and
> there is no default implementation of the API when creating a module graph,
> therefore cache information is missing from the output. An implementation of
> something that read the `DENO_DIR` cache is possible from CLI, but not
> currently implemented.

To replicate `deno info https://deno.land/x/std/testing/asserts.ts`, you would
want to do something like this:

```ts
import { createGraph } from "https://deno.land/x/deno_graph@{VERSION}/mod.ts";

const graph = await createGraph("https://deno.land/x/std/testing/asserts.ts");

console.log(graph.toString());
```

This would output to stdout and would respect the `NO_COLOR`/`Deno.noColor`
setting. If colors are allowed, the string will include the ANSI color escape
sequences, otherwise they will be omitted. This behaviour can be explicitly
overriden by passing `true` to always remove ANSI colors or `false` to force
them.

To replicate `deno info --json https://deno.land/x/std/testing/asserts.ts`, you
would want to do something like this:

```ts
import { createGraph } from "https://deno.land/x/deno_graph@{VERSION}/mod.ts";

const graph = await createGraph("https://deno.land/x/std/testing/asserts.ts");

console.log(JSON.stringify(graph, undefined, "  "));
```

This would output to stdout the JSON structure of the module graph.

## Building Web Assembly

To build the web assembly library, you need a couple pre-requisites, which can
be added as follows:

```
> rustup target add wasm32-unknown-unknown
> cargo install -f wasm-bindgen-cli
```

Note that the `wasm-bindgen-cli` should match the version of `wasm-bindgen` in
this crate and be explicitly set using the `--version` flag on install.

Also, the build script (`_build.ts`) requires the Deno CLI to be installed and
available in the path. If it is, then script should _just work_:

```
> ./_build.ts
```

But can be manually invoked like:

```
> deno run --unstable _build.ts
```

And you will be prompted for the permissions that Deno needs to perform the
build.
