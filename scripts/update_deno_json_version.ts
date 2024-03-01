import * as path from "@std/path";

const version = Deno.args[0];
if (version == null || version.length === 0) {
  throw new Error("Please provide a version.");
}

const rootDir = path.dirname(import.meta.dirname!);
const denoJsonPath = path.join(rootDir, "/js/deno.json");
const data = JSON.parse(Deno.readTextFileSync(denoJsonPath));
data.version = version;
Deno.writeTextFileSync(denoJsonPath, JSON.stringify(data, undefined, 2) + "\n");
