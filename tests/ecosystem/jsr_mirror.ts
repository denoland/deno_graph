import * as path from "@std/path";
import { pooledMap, retry } from "@std/async";
import { format } from "@std/fmt/bytes";

const ROOT_DIR = path.dirname(path.fromFileUrl(import.meta.url));

const VERSIONS_JSON = path.join(ROOT_DIR, "jsr_versions.json");

const MIRROR_DIR = path.join(ROOT_DIR, "jsr_mirror");
await Deno.mkdir(MIRROR_DIR, { recursive: true });

const VERSIONS: { scope: string; name: string; version: string }[] = JSON.parse(
  await Deno.readTextFile(VERSIONS_JSON),
);

/** How many package versions to mirror at once. */
const PACKAGE_CONCURRENCY = 128;
/** How many files to download at once within a single package version. */
const FILE_CONCURRENCY = 8;

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

const failures: string[] = [];

for await (
  const _ of pooledMap(PACKAGE_CONCURRENCY, VERSIONS, async (row) => {
    const { scope, name, version } = row;
    try {
      await mirrorPackageVersion(scope, name, version);
    } catch (err) {
      // Keep going so that one bad package version doesn't abandon the rest of
      // the mirror; every failure is reported together at the end.
      failures.push(`@${scope}/${name}@${version}: ${describeError(err)}`);
    }
    packagesDone++;
  })
) {
  // Empty
}

clearInterval(interval);

if (failures.length > 0) {
  console.error(`Failed to mirror ${failures.length} package versions:`);
  for (const failure of failures.slice(0, 50)) {
    console.error(`  ${failure}`);
  }
  if (failures.length > 50) {
    console.error(`  ... and ${failures.length - 50} more`);
  }
  Deno.exit(1);
}

interface Manifest {
  manifest: Record<string, { size: number; checksum: string }>;
  exports: Record<string, string>;
}

async function mirrorPackageVersion(
  scope: string,
  name: string,
  version: string,
): Promise<void> {
  const packageDir = path.join(MIRROR_DIR, scope, name);
  const versionDir = path.join(packageDir, version);
  const manifestFile = path.join(packageDir, `${version}_meta.json`);

  if (await exists(manifestFile)) {
    return; // Already mirrored, skip package
  }

  const manifest = await retry(() => fetchManifest(scope, name, version));

  await Deno.mkdir(versionDir, { recursive: true });

  const files = Object.entries(manifest.manifest);
  for await (
    const _ of pooledMap(
      FILE_CONCURRENCY,
      files,
      ([file, { size }]) =>
        retry(() => downloadFile(scope, name, version, versionDir, file, size)),
    )
  ) {
    // Empty
  }

  // Written only once every file above is on disk: the presence of this file is
  // what marks the version as mirrored, so writing it any earlier would make an
  // interrupted run skip a package whose files never finished downloading.
  await Deno.writeTextFile(manifestFile, JSON.stringify(manifest, null, 2));
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
    await resp.body?.cancel();
    throw new Error(
      `Failed to fetch manifest for ${scope}/${name}@${version}: ${resp.statusText}`,
    );
  }
  return await resp.json();
}

async function downloadFile(
  scope: string,
  name: string,
  version: string,
  versionDir: string,
  file: string,
  size: number,
): Promise<void> {
  const fileUrl = `https://jsr.io/@${scope}/${name}/${version}/${file}`;
  const resp = await fetch(fileUrl);
  if (!resp.ok) {
    await resp.body?.cancel();
    throw new Error(
      `Failed to fetch file ${fileUrl}: ${resp.statusText}`,
    );
  }

  const fileDest = path.join(versionDir, file);
  await Deno.mkdir(path.dirname(fileDest), { recursive: true });
  await Deno.writeFile(fileDest, resp.body!);

  // A connection dropped mid-body still writes a well-formed but truncated
  // file, which surfaces much later as a bogus syntax error in some spec far
  // away from the actual problem. Only a short file means a truncated transfer:
  // jsr serves a handful of files (readmes, mostly) that are larger than the
  // size their manifest records, so this deliberately isn't an equality check.
  const written = await Deno.stat(fileDest);
  if (written.size < size) {
    await Deno.remove(fileDest);
    throw new Error(
      `Truncated download of ${fileUrl}: expected ${size} bytes, got ${written.size}`,
    );
  }

  filesDone++;
  sizeTotal += written.size;
}

/**
 * `pooledMap` wraps whatever an item threw in an `AggregateError` and `retry`
 * wraps it again in a `RetryError`, so the message that actually says what went
 * wrong is a couple of layers down.
 */
function describeError(err: unknown): string {
  if (err instanceof AggregateError) {
    return err.errors.map(describeError).join("; ");
  }
  if (err instanceof Error && err.cause !== undefined) {
    return `${err.message}: ${describeError(err.cause)}`;
  }
  return String(err);
}

async function exists(filePath: string): Promise<boolean> {
  try {
    await Deno.stat(filePath);
    return true;
  } catch (err) {
    if (err instanceof Deno.errors.NotFound) {
      return false;
    }
    throw err;
  }
}
