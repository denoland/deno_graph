import { build, emptyDir } from "https://deno.land/x/dnt@0.37.0/mod.ts";

await emptyDir("./npm");
Deno.mkdirSync("npm/esm/lib", { recursive: true });
Deno.mkdirSync("npm/script/lib", { recursive: true });
// todo(dsherret): don't include this twice
Deno.copyFileSync(
  "js/deno_graph_wasm_bg.wasm",
  "npm/esm/deno_graph_wasm_bg.wasm",
);
Deno.copyFileSync(
  "js/deno_graph_wasm_bg.wasm",
  "npm/script/deno_graph_wasm_bg.wasm",
);

await build({
  entryPoints: ["./js/mod.ts"],
  outDir: "./npm",
  shims: {
    deno: true,
    undici: true,
  },
  rootTestDir: "./js",
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
    devDependencies: {
      "@types/node": "^18.16.16",
    },
  },
  compilerOptions: {
    lib: ["dom", "es2021"],
  },
  filterDiagnostic(diagnostic) {
    if (
      diagnostic.file?.fileName.endsWith("wasmbuild@0.14.1/loader.ts")
    ) {
      return false;
    }
    return true;
  },
});

Deno.copyFileSync("LICENSE", "npm/LICENSE");
Deno.copyFileSync("README.md", "npm/README.md");
