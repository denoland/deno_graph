// Copyright 2018-2024 the Deno authors. MIT license.

/**
 * A JavaScript/TypeScript interface to the Deno CLI's module graph logic.
 *
 * ### Example
 *
 * To build and output a graph as a JSON structure to the console:
 *
 * ```ts
 * import { createGraph } from "jsr:@deno/graph/@{VERSION}";
 *
 * const graph = await createGraph("https://deno.land/x/std/testing/asserts.ts");
 *
 * console.log(JSON.stringify(graph, undefined, "  "));
 * ```
 *
 * @module
 */

import * as wasm from "./deno_graph_wasm.generated.js";
import { load as defaultLoad } from "./loader.ts";
import type {
  CacheInfo,
  LoadResponse,
  ModuleGraphJson,
  ModuleJson,
  TypesDependency,
} from "./types.ts";

export { load } from "./loader.ts";
export { MediaType } from "./media_type.ts";
export type {
  CacheInfo,
  Dependency,
  LoadResponse,
  ModuleGraphJson,
  ModuleJson,
  ModuleKind,
  TypesDependency,
} from "./types.ts";

const encoder = new TextEncoder();

// note: keep this in line with deno_cache
export type CacheSetting = "only" | "use" | "reload";

export interface CreateGraphOptions {
  /**
   * A callback that is called with the URL string of the resource to be loaded
   * and a flag indicating if the module was required dynamically. The callback
   * should resolve with a `LoadResponse` or `undefined` if the module is not
   * found. If there are other errors encountered, a rejected promise should be
   * returned.
   *
   * @param specifier The URL string of the resource to be loaded and resolved
   * @param isDynamic A flag that indicates if the module was being loaded
   *   dynamically
   */
  load?(
    specifier: string,
    isDynamic: boolean,
    cacheSetting: CacheSetting,
    checksum: string | undefined,
  ): Promise<LoadResponse | undefined>;
  /** The type of graph to build. `"all"` includes all dependencies of the
   * roots. `"typesOnly"` skips any code only dependencies that do not impact
   * the types of the graph, and `"codeOnly"` only includes dependencies that
   * are runnable code. */
  kind?: "all" | "typesOnly" | "codeOnly";
  /** The default jsxImportSource to use in JSX/TSX files when no
   * `@jsxImportSource` pragma is specified. In Deno, this is set to the
   * `compilerOptions.jsxImportSource` value if `compilerOptions.jsx` is set to
   * `react-jsx` or `react-jsxdev`. */
  defaultJsxImportSource?: string;
  /** When identifying a `@jsxImportSource` pragma, what module name will be
   * appended to the import source. This defaults to `jsx-runtime`. */
  jsxImportSourceModule?: string;
  /** An optional callback that will be called with a URL string of the resource
   * to provide additional meta data about the resource to enrich the module
   * graph. */
  cacheInfo?(specifier: string): CacheInfo;
  /** An optional callback that allows the default resolution logic of the
   * module graph to be "overridden". This is intended to allow items like an
   * import map to be used with the module graph. The callback takes the string
   * of the module specifier from the referrer and the string URL of the
   * referrer. The callback then returns a fully qualified resolved URL string
   * specifier. */
  resolve?(specifier: string, referrer: string): string;
  /** An optional callback that can allow custom logic of how type dependencies
   * of a module to be provided. This will be called if a module is being added
   * to the graph that is is non-typed source code (e.g. JavaScript/JSX) and
   * allow resolution of a type only dependency for the module (e.g. `@types`
   * or a `.d.ts` file). */
  resolveTypes?(specifier: string): TypesDependency | undefined;
  /** An optional record of "injected" dependencies to the module graph. This
   * allows adding things like TypeScript's `"types"` values into the graph or
   * a JSX runtime. The key is the referrer specifier to use as a base when
   * resolving relative specifiers. The value is any module specifiers that are
   * being imported. */
  imports?: Record<string, string[]>;
}

/** Create a module graph using the same algorithms that are used in the Deno
 * CLI, resolving with the module graph for further processing.
 *
 * A default `load()` function is provided which will attempt to load local
 * modules via `Deno.readFile()` and will use `fetch()` to load remote
 * modules. An alternative `load()` function can be provided via the options.
 *
 * ### Example
 *
 * ```ts
 * import { createGraph } from "https://deno.land/x/deno_graph/mod.ts";
 *
 * const graph = await createGraph("https://example.com/a.ts");
 *
 * console.log(graph.toString());
 * ```
 *
 * @param rootSpecifiers A URL string of the root module specifier to build the
 * graph from or array of URL strings.
 * @param options A set of options for building the graph
 */
export async function createGraph(
  rootSpecifiers: string | string[],
  options: CreateGraphOptions = {},
): Promise<ModuleGraphJson> {
  rootSpecifiers = Array.isArray(rootSpecifiers)
    ? rootSpecifiers
    : [rootSpecifiers];
  const {
    load = defaultLoad,
    defaultJsxImportSource,
    jsxImportSourceModule,
    cacheInfo,
    resolve,
    resolveTypes,
    kind,
    imports,
  } = options;
  const { createGraph } = await wasm.instantiate();
  return await createGraph(
    rootSpecifiers,
    async (
      specifier: string,
      options: {
        isDynamic: boolean;
        cacheSetting: CacheSetting;
        checksum: string | undefined;
      },
    ) => {
      const result = await load(
        specifier,
        options.isDynamic,
        options.cacheSetting,
        options.checksum,
      );
      if (result?.kind === "module") {
        if (typeof result.content === "string") {
          result.content = encoder.encode(result.content);
        }
        // need to convert to an array for serde_wasm_bindgen to work
        // deno-lint-ignore no-explicit-any
        (result as any).content = Array.from(result.content);
      }
      return result;
    },
    defaultJsxImportSource,
    jsxImportSourceModule,
    cacheInfo,
    resolve,
    resolveTypes,
    kind,
    imports,
  ) as ModuleGraphJson;
}

export interface ParseModuleOptions {
  /** For remote resources, a record of headers should be set, where the key's
   * have been normalized to be lower case values. */
  headers?: Record<string, string>;
  /** The default jsxImportSource to use in JSX/TSX files when no
   * `@jsxImportSource` pragma is specified. In Deno, this is set to the
   * `compilerOptions.jsxImportSource` value if `compilerOptions.jsx` is set to
   * `react-jsx` or `react-jsxdev`. */
  defaultJsxImportSource?: string;
  /** When identifying a `@jsxImportSource` pragma, what module name will be
   * appended to the import source. This defaults to `jsx-runtime`. */
  jsxImportSourceModule?: string;
  /** An optional callback that allows the default resolution logic of the
   * module graph to be "overridden". This is intended to allow items like an
   * import map to be used with the module graph. The callback takes the string
   * of the module specifier from the referrer and the string URL of the
   * referrer. The callback then returns a fully qualified resolved URL string
   * specifier. */
  resolve?(specifier: string, referrer: string): string;
  /** An optional callback that can allow custom logic of how type dependencies
   * of a module to be provided. This will be called if a module is being added
   * to the graph that is is non-typed source code (e.g. JavaScript/JSX) and
   * allow resolution of a type only dependency for the module (e.g. `@types`
   * or a `.d.ts` file). */
  resolveTypes?(specifier: string): string | undefined;
}

/** Instantiates the Wasm module used within deno_graph. */
export async function init(opts?: wasm.InstantiateOptions) {
  await wasm.instantiate(opts);
}

/** Parse a module based on the supplied information and return its analyzed
 * representation. If an error is encountered when parsing, the function will
 * throw.
 *
 * @param specifier The URL text specifier to use when parsing the module.
 * @param content The content of the module to be parsed.
 * @param options Options to use when parsing the module.
 */
export function parseModule(
  specifier: string,
  content: Uint8Array,
  options: ParseModuleOptions = {},
): ModuleJson {
  const {
    headers,
    defaultJsxImportSource,
    jsxImportSourceModule,
    resolve,
    resolveTypes,
  } = options;

  if (!wasm.isInstantiated()) {
    throw new Error(
      "Please call `init()` at least once before calling `parseModule`.",
    );
  }

  return wasm.parseModule(
    specifier,
    headers,
    defaultJsxImportSource,
    jsxImportSourceModule,
    content,
    resolve,
    resolveTypes,
  ) as ModuleJson;
}
