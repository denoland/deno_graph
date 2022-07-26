import { build, emptyDir } from "https://deno.land/x/dnt@0.28.0/mod.ts";

await emptyDir("./npm");
Deno.mkdirSync("npm/esm/lib", { recursive: true });
Deno.mkdirSync("npm/script/lib", { recursive: true });
// todo(dsherret): don't include this twice
Deno.copyFileSync("lib/deno_graph_bg.wasm", "npm/esm/lib/deno_graph_bg.wasm");
Deno.copyFileSync(
  "lib/deno_graph_bg.wasm",
  "npm/script/lib/deno_graph_bg.wasm",
);

await build({
  entryPoints: ["./mod.ts"],
  outDir: "./npm",
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
