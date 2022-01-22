import { build } from "https://deno.land/x/dnt@0.16.1/mod.ts";

Deno.mkdirSync("npm/esm/lib", { recursive: true });
Deno.copyFileSync("lib/deno_graph_bg.wasm", "npm/esm/lib/deno_graph_bg.wasm");

await build({
  entryPoints: ["./mod.ts"],
  outDir: "./npm",
  cjs: false, // todo(#112): enable
  shims: {
    deno: true,
    undici: true,
  },
  package: {
    name: "@deno/graph",
    version: Deno.args[0],
    description: "The module graph logic for Deno CLI.",
    license: "MIT",
    repository: {
      type: "git",
      url: "git+https://github.com/deno/deno_graph.git",
    },
    bugs: {
      url: "https://github.com/deno/deno_graph/issues",
    },
  },
});

Deno.copyFileSync("LICENSE", "npm/LICENSE");
Deno.copyFileSync("README.md", "npm/README.md");
