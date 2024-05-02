import * as path from "jsr:@std/path";
import { pooledMap, retry } from "jsr:@std/async";
import { format } from "jsr:@std/fmt/bytes";

const ROOT_DIR = path.dirname(path.fromFileUrl(import.meta.url));

const VERSIONS_JSON = path.join(ROOT_DIR, "jsr_versions.json");

const MIRROR_DIR = path.join(ROOT_DIR, "jsr_mirror");
try {
  await Deno.mkdir(MIRROR_DIR, { recursive: true });
} catch (err) {
  if (err instanceof Deno.errors.AlreadyExists) {
    // Ignore
  } else {
    throw err;
  }
}

const VERSIONS: { scope: string; name: string; version: string }[] = JSON.parse(
  await Deno.readTextFile(VERSIONS_JSON),
);

let packagesDone = 0;
let filesDone = 0;
let sizeTotal = 0;

const start = performance.now();

const CI = Deno.env.get("CI") !== undefined;

const interval = setInterval(() => {
  if (CI) {
    console.log(
      `Packages done: ${packagesDone} / ${VERSIONS.length} (${
        (packagesDone / VERSIONS.length * 100).toFixed(2)
      }%), files done: ${filesDone} (${format(sizeTotal)})`,
    );
  } else {
    console.clear();
    console.log(
      `Packages done: ${packagesDone} / ${VERSIONS.length} (${
        (packagesDone / VERSIONS.length * 100).toFixed(2)
      }%)`,
    );
    console.log(`Files done: ${filesDone} (${format(sizeTotal)})`);
    const passed = performance.now();
    const bytesPerSecond = sizeTotal / (passed - start) * 1000;
    console.log(`Speed: ${format(bytesPerSecond)}/s`);
  }
}, CI ? 2000 : 100);

for await (
  const _ of pooledMap(1024, tasks(), async (task) => {
    await task.promise;
  })
) {
  // Empty
}

clearInterval(interval);

async function* tasks() {
  const manifests = pooledMap(128, VERSIONS, async (row) => {
    const { scope, name, version } = row;
    const manifest = await retry(() => fetchManifest(scope, name, version));
    return { scope, name, version, manifest };
  });

  for await (const { scope, name, version, manifest } of manifests) {
    yield* downloadPackageVersion(scope, name, version, manifest);
  }
}

interface Manifest {
  manifest: Record<string, { size: number; checksum: string }>;
  exports: Record<string, string>;
}

async function fetchManifest(
  scope: string,
  name: string,
  version: string,
): Promise<Manifest> {
  const resp = await fetch(
    `https://jsr.io/@${scope}/${name}/${version}_meta.json`,
  );
  if (!resp.ok) {
    throw new Error(
      `Failed to fetch manifest for ${scope}/${name}@${version}: ${resp.statusText}`,
    );
  }
  return await resp.json();
}

async function* downloadPackageVersion(
  scope: string,
  name: string,
  version: string,
  manifest: Manifest,
): AsyncGenerator<{ promise: Promise<void> }, void, void> {
  const packageDir = path.join(MIRROR_DIR, scope, name);
  const versionDir = path.join(packageDir, version);
  try {
    await Deno.mkdir(versionDir, { recursive: true });
  } catch (err) {
    if (err instanceof Deno.errors.AlreadyExists) {
      // Ignore
    } else {
      throw err;
    }
  }

  const manifestFile = path.join(packageDir, `${version}_meta.json`);
  await Deno.writeTextFile(manifestFile, JSON.stringify(manifest, null, 2));

  const files: [string, { size: number; checksum: string }][] = Object.entries(
    manifest.manifest,
  );

  for (const [file, { size, checksum }] of files) {
    yield {
      promise: retry(() => {
        downloadFile(
          scope,
          name,
          version,
          versionDir,
          file,
          size,
          checksum,
        );
      }),
    };
  }

  packagesDone++;
}

async function downloadFile(
  scope: string,
  name: string,
  version: string,
  versionDir: string,
  file: string,
  size: number,
  _checksum: string,
): Promise<void> {
  const fileUrl = `https://jsr.io/@${scope}/${name}/${version}/${file}`;
  const resp = await fetch(fileUrl);
  if (!resp.ok) {
    throw new Error(
      `Failed to fetch file ${fileUrl}: ${resp.statusText}`,
    );
  }

  const fileDest = path.join(versionDir, file);
  const fileDir = path.dirname(fileDest);
  try {
    await Deno.mkdir(fileDir, { recursive: true });
  } catch (err) {
    if (err instanceof Deno.errors.AlreadyExists) {
      // Ignore
    } else {
      throw err;
    }
  }
  await Deno.writeFile(fileDest, resp.body!);

  filesDone++;
  sizeTotal += size;
}
