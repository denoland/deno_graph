# deno_graph

The module graph logic for the Deno CLI.

This repository is a Rust crate which provides the foundational code to be able
to build a module graph, following the Deno CLI's module resolution logic. It
also provides a web assembly interface to the built code, making it possible to
leverage the logic outside of the Deno CLI from JavaScript/TypeScript.

## Usage from Deno CLI

This repository includes a compiled version of the Rust crate as Web Assembly
and exposes an interface which is availble via the `mod.ts` in this repository
and can be imported like:

```js
import { createGraph } from "https://deno.land/x/deno_graph@{VERSION}/mod.ts";
```

Where `{VERSION}` should be subtituted with the specific version you want to
use.

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
> deno run _build.ts
```

And you will be prompted for the permissions that Deno needs to perform the
build.
