#!/usr/bin/env -S deno run --allow-run --allow-read --allow-write --allow-env
// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import * as base64 from "https://deno.land/std@0.102.0/encoding/base64.ts";
import * as colors from "https://deno.land/std@0.102.0/fmt/colors.ts";

const STD_VERSION = `0.102.0`;

console.log(
  `${colors.bold(colors.green("Building"))} deno_graph web assembly...`,
);

const home = Deno.env.get("HOME");
const root = new URL(".", import.meta.url).pathname;
const copyrightHeader = `// Copyright 2018-${
  new Date().getFullYear()
} the Deno authors. All rights reserved. MIT license.`;

if (new URL(import.meta.url).protocol === "file:") {
  Deno.chdir(root);
} else {
  console.error("The build script can only be run from a local file system");
  Deno.exit(1);
}

const cargoFmtCmd = ["cargo", "fmt"];
console.log(`  ${colors.bold(colors.gray(cargoFmtCmd.join(" ")))}`);
const cargoFmtCmdStatus = Deno.run({ cmd: cargoFmtCmd }).status();
if (!(await cargoFmtCmdStatus).success) {
  console.error(`cargo fmt failed`);
  Deno.exit(1);
}

const cargoBuildCmd = [
  "cargo",
  "build",
  "--release",
  "--target",
  "wasm32-unknown-unknown",
];
console.log(`  ${colors.bold(colors.gray(cargoBuildCmd.join(" ")))}`);
const cargoBuildReleaseCmdStatus = Deno.run({
  cmd: cargoBuildCmd,
  env: {
    "SOURCE_DATE_EPOCH": "1600000000",
    "TZ": "UTC",
    "LC_ALL": "C",
    "RUSTFLAGS": `--remap-path-prefix=${root}=. --remap-path-prefix=${home}=~`,
  },
}).status();
if (!(await cargoBuildReleaseCmdStatus).success) {
  console.error(`cargo build failed`);
  Deno.exit(1);
}

const wasmBindGenCmd = [
  "wasm-bindgen",
  "./target/wasm32-unknown-unknown/release/deno_graph.wasm",
  "--target",
  "deno",
  "--weak-refs",
  "--out-dir",
  "./target/wasm32-bindgen-deno-js",
];
console.log(`  ${colors.bold(colors.gray(wasmBindGenCmd.join(" ")))}`);
const wasmBindgenCmdStatus = Deno.run({ cmd: wasmBindGenCmd }).status();
if (!(await wasmBindgenCmdStatus).success) {
  console.error(`wasm-bindgen failed`);
  Deno.exit(1);
}

console.log(
  `${colors.bold(colors.green("Generating"))} lib files...`,
);

const generatedWasm = await Deno.readFile(
  "./target/wasm32-bindgen-deno-js/deno_graph_bg.wasm",
);

const generatedWasmLength = generatedWasm.length.toString();
const formattedWasmSize = generatedWasmLength.padStart(
  Math.ceil(generatedWasmLength.length / 3) * 3,
).replace(/...\B/g, "$&_").trim();

const wasmIntegrity = `sha256-${
  base64.encode(await crypto.subtle.digest("SHA-256", generatedWasm))
}`;

const wasmJs = `${copyrightHeader}
// @generated file from build script, do not edit

import * as base64 from "https://deno.land/std@${STD_VERSION}/encoding/base64.ts";

export const size = ${formattedWasmSize};
export const name = "deno_graph.wasm";
export const type = "application/wasm";
export const hash = ${JSON.stringify(wasmIntegrity)};
export const data = base64.decode("\\\n${
  base64.encode(generatedWasm).replace(/.{78}/g, "$&\\\n")
}\\\n");
`;
const libDenoGraphWasm = "./lib/deno_graph.wasm.js";
console.log(`  write ${colors.yellow(libDenoGraphWasm)}`);
await Deno.writeTextFile(libDenoGraphWasm, wasmJs);

const generatedJs = await Deno.readTextFile(
  "./target/wasm32-bindgen-deno-js/deno_graph.js",
);
const bindingJs = `${copyrightHeader}
// @generated file from build script, do not edit
// deno-lint-ignore-file

import { data as wasmBytes } from "./deno_graph.wasm.js";

${
  generatedJs.replace(
    /^const file =.*?;\nconst wasmFile =.*?;\nconst wasmModule =.*?;\n/sm,
    "const wasmModule = new WebAssembly.Module(wasmBytes);",
  )
}

/* for testing and debugging */
export const _wasm = wasm;
export const _wasmModule = wasmModule;
export const _wasmInstance = wasmInstance;
export const _wasmBytes = wasmBytes;
`;
const libDenoGraphJs = "./lib/deno_graph.js";
console.log(`  write ${colors.yellow(libDenoGraphJs)}`);
await Deno.writeTextFile(libDenoGraphJs, bindingJs);

const denoFmtCmd = [
  "deno",
  "fmt",
  "--quiet",
  "./lib/deno_graph.wasm.js",
  "./lib/deno_graph.js",
];
console.log(`  ${colors.bold(colors.gray(denoFmtCmd.join(" ")))}`);
const denoFmtCmdStatus = Deno.run({ cmd: denoFmtCmd }).status();
if (!(await denoFmtCmdStatus).success) {
  console.error("deno fmt command failed");
  Deno.exit(1);
}

console.log(
  `${colors.bold(colors.green("Finished"))} deno_graph web assembly.`,
);
