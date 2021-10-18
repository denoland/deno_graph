# deno_graph

[![Build Status - Cirrus][]][Build status] [![Twitter handle][]][Twitter badge]
[![Discord Chat](https://img.shields.io/discord/684898665143206084?logo=discord&style=social)](https://discord.gg/deno)

The module graph/dependency logic for the Deno CLI.

This repository is a Rust crate which provides the foundational code to be able
to build a module graph, following the Deno CLI's module resolution logic. It
also provides a web assembly interface to the built code, making it possible to
leverage the logic outside of the Deno CLI from JavaScript/TypeScript.

## Rust usage

### `create_graph()`

`create_graph()` is the main way of interfacing with the crate. It requires the
root module specifiers/URLs for the graph and an implementation of the
`source::Loader` trait. It also optionally takes implementations of the
`source::Resolver` and `source::Locker` traits. It will load and parse the root
module and recursively all of its dependencies, returning asynchronously a
resulting `ModuleGraph`.

### `source::Loader` trait

Implementing this trait requires the `load()` method and optionally the
`get_cache_into()` method. The `load()` method returns a future with the
requested module specifier and the resulting load response. This allows the
module graph to deal with redirections, error conditions, and local and remote
content.

The `get_cache_info()` is the API for exposing additional meta data about a
module specifier as it resides in the cache so it can be part of a module graphs
information. When the graph is built, the method will be called with each
resolved module in the graph to see if the additional information is available.

### `source::Resolver` trait

This trait "replaces" the default resolution logic of the module graph. It is
intended to allow concepts like import maps and alternative resolution logic to
"plug" into the module graph.

It has two methods, `resolve` and `resolve_types`, which both have default
implementations. `resolve` takes the string specifier from the source and the
referring specifier and is expected to return a result with the resolved
specifier.

`resolve_types` takes a specifier and is expected to return a result with an
optional module specifier and optional source span of the types that should be
used. For example, if you are trying represent the `"typings"` field from a
`package.json` file, when you receive the request on `resolve_types` for the
main module of the package, you would respond with the absolute specifier to the
types along with a span that indicates the file URL to the `package.json` and
the range where it was specified. Including the span is useful to allow errors
produced from the graph to indicate "where" the dependency came from.

### `source::Locker` trait

This trait allows the module graph to perform sub-resource integrity checks on a
module graph.

### `source::MemoryLoader` struct

`MemoryLoader` is a structure that implements the `source::Loader` trait and is
designed so that a cache of modules can be stored in memory to be parsed and
retrieved when building a module graph. This is useful for testing purposes or
in situations where the module contents is already available and _dynamically_
loading them is not practical or desirable.

A minimal example would look like this:

```rust
use deno_graph::create_graph;
use deno_graph::ModuleSpecifier;
use deno_graph::source::MemoryLoader;
use futures::executor::block_on;

fn main() {
  let mut loader = MemoryLoader::new(
    vec![
      ("file:///test.ts", Ok(("file:///test.ts", None, "import * as a from \"./a.ts\";"))),
      ("file:///a.ts", Ok(("file:///a.ts", None, "export const a = \"a\";"))),
    ]
  );
  let roots = vec![ModuleSpecifier::parse("file:///test.ts").unwrap()];
  let future = async move {
    let graph = create_graph(roots, &mut loader, None, None).await;
    println!("{}", graph);
  };
  block_on()
}
```

### Other core concepts

#### `ModuleSpecifier` type alias

Currently part of the the `deno_core` crate. `deno_graph` explicitly doesn't
depend on `deno_core` or any part of the Deno CLI. It exports the type alias
publicably for re-use by other crates.

#### `MediaType` enum

Currently part of the `deno_cli` crate, this enum represents the various media
types that the Deno CLI can resolve and handle. Since `deno_graph` doesn't rely
upon any part of the Deno CLI, it was necessary to implement this in this crate,
and the implementation here will eventually replace the implementation in
`deno_cli`.

## Usage from Deno CLI or Deploy

This repository includes a compiled version of the Rust crate as Web Assembly
and exposes an interface which is availble via the `mod.ts` in this repository
and can be imported like:

```js
import * as denoGraph from "https://deno.land/x/deno_graph@{VERSION}/mod.ts";
```

Where `{VERSION}` should be substituted with the specific version you want to
use.

### `createGraph()`

The `createGraph()` function allows a module graph to be built asynchronously.
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
without requiring a `Deno` namespace API.

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

> ⚠️ Note that the `wasm-bindgen-cli` should match the version of `wasm-bindgen`
> in this crate and be explicitly set using the `--version` flag on install.

Also, the build script (`_build.ts`) requires the Deno CLI to be installed and
available in the path. If it is, the script should _just work_:

```
> ./_build.ts
```

But can be manually invoked like:

```
> deno run --unstable _build.ts
```

And you will be prompted for the permissions that Deno needs to perform the
build.

### Contributing

We appreciate your help!

To contribute, please read our
[contributing instructions](https://deno.land/manual/contributing).

This repository includes `.devcontainer` metadata which will allow a development
container to be built which has all the development pre-requisites available to
make contribution easier.

[Build Status - Cirrus]: https://github.com/denoland/deno_graph/workflows/ci/badge.svg?branch=main&event=push
[Build status]: https://github.com/denoland/deno_graph/actions
[Twitter badge]: https://twitter.com/intent/follow?screen_name=deno_land
[Twitter handle]: https://img.shields.io/twitter/follow/deno_land.svg?style=social&label=Follow
