// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

/**
 * A JavaScript/TypeScript interface to the Deno CLI's module graph logic.
 *
 * ### Example
 *
 * To build and output a graph as a JSON structure to the console:
 *
 * ```ts
 * import { createGraph } from "https://deno.land/x/deno_graph@{VERSION}/mod.ts";
 *
 * const graph = await createGraph("https://deno.land/x/std/testing/asserts.ts");
 *
 * console.log(JSON.stringify(graph, undefined, "  "));
 * ```
 *
 * @module
 */

import {
  createGraph as jsCreateGraph,
  parseModule as jsParseModule,
} from "./lib/deno_graph.generated.js";
import { load as defaultLoad } from "./lib/loader.ts";
import type {
  CacheInfo,
  LoadResponse,
  Module,
  ModuleGraph,
  ModuleKind,
  ResolveResult,
  TypesDependency,
} from "./lib/types.d.ts";

export { load } from "./lib/loader.ts";
export type {
  CacheInfo,
  Dependency,
  LoadResponse,
  Module,
  ModuleGraph,
  ModuleGraphJson,
  ModuleKind,
  ResolveResult,
  TypesDependency,
} from "./lib/types.d.ts";

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
  ): Promise<LoadResponse | undefined>;
  /** The type of graph to build. `"all"` includes all dependencies of the
   * roots. `"typesOnly"` skips any code only dependencies that do not impact
   * the types of the graph, and `"codeOnly"` only includes dependencies that
   * are runnable code. */
  kind?: "all" | "typesOnly" | "codeOnly";
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
   * specifier or an object which contains the URL string and the module kind.
   * If just the string is returned, the module kind is inferred to be ESM. */
  resolve?(specifier: string, referrer: string): string | ResolveResult;
  /** An optional callback that can allow custom logic of how type dependencies
   * of a module to be provided. This will be called if a module is being added
   * to the graph that is is non-typed source code (e.g. JavaScript/JSX) and
   * allow resolution of a type only dependency for the module (e.g. `@types`
   * or a `.d.ts` file). */
  resolveTypes?(specifier: string): TypesDependency | undefined;
  /** An optional callback that returns `true` if the sub-resource integrity of
   * the provided specifier and content is valid, otherwise `false`. This allows
   * for items like lock files to be applied to the module graph. */
  check?(specifier: string, content: Uint8Array): boolean;
  /** An optional callback that returns the sub-resource integrity checksum for
   * a given set of content. */
  getChecksum?(content: Uint8Array): string;
  /** An optional string to be used when generating an error when the integrity
   * check of the module graph fails. */
  lockFilename?: string;
  /** An optional record of "injected" dependencies to the module graph. This
   * allows adding things like TypeScript's `"types"` values into the graph. */
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
 * @param rootSpecifier A URL string of the root module specifier to build the
 *                      graph from.
 * @param options A set of options for building the graph
 */
export function createGraph(
  rootSpecifier: string,
  options?: CreateGraphOptions,
): Promise<ModuleGraph>;
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
 * const graph = await createGraph([
 *   ["https://example.com/a.ts", "esm"],
 *   ["https://example.com/a.ts", "esm"],
 * ]);
 *
 * console.log(graph.toJSON());
 * ```
 *
 * @param rootSpecifiers  An array of URL strings or tuples of URL strings and
 *                        module kinds of the root module specifiers to build
 *                        the graph from.
 * @param options A set of options for building the graph
 */
export function createGraph(
  rootSpecifiers: string[] | [string, ModuleKind][],
  options?: CreateGraphOptions,
): Promise<ModuleGraph>;
export function createGraph(
  rootSpecifiers: string | string[] | [string, ModuleKind][],
  options: CreateGraphOptions = {},
): Promise<ModuleGraph> {
  rootSpecifiers = Array.isArray(rootSpecifiers)
    ? rootSpecifiers
    : [rootSpecifiers];
  const {
    load = defaultLoad,
    jsxImportSourceModule,
    cacheInfo,
    resolve,
    resolveTypes,
    check,
    getChecksum,
    lockFilename,
    kind,
    imports,
  } = options;
  return jsCreateGraph(
    rootSpecifiers,
    load,
    jsxImportSourceModule,
    cacheInfo,
    resolve,
    resolveTypes,
    check,
    getChecksum,
    lockFilename,
    kind,
    imports,
    // deno-lint-ignore no-explicit-any
  ) as any;
}

export interface ParseModuleOptions {
  /** For remote resources, a record of headers should be set, where the key's
   * have been normalized to be lower case values. */
  headers?: Record<string, string>;
  /** When identifying a `@jsxImportSource` pragma, what module name will be
   * appended to the import source. This defaults to `jsx-runtime`. */
  jsxImportSourceModule?: string;
  /** The kind of module to set on the resulting parsed module. */
  kind?: ModuleKind;
  /** An optional callback that allows the default resolution logic of the
   * module graph to be "overridden". This is intended to allow items like an
   * import map to be used with the module graph. The callback takes the string
   * of the module specifier from the referrer and the string URL of the
   * referrer. The callback then returns a fully qualified resolved URL string
   * specifier or an object which contains the URL string and the module kind.
   * If just the string is returned, the module kind is inferred to be ESM. */
  resolve?(specifier: string, referrer: string): string | ResolveResult;
  /** An optional callback that can allow custom logic of how type dependencies
   * of a module to be provided. This will be called if a module is being added
   * to the graph that is is non-typed source code (e.g. JavaScript/JSX) and
   * allow resolution of a type only dependency for the module (e.g. `@types`
   * or a `.d.ts` file). */
  resolveTypes?(specifier: string): string | undefined;
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
  content: string,
  options: ParseModuleOptions = {},
): Module {
  const { headers, jsxImportSourceModule, kind, resolve, resolveTypes } =
    options;
  return jsParseModule(
    specifier,
    headers,
    jsxImportSourceModule,
    content,
    kind,
    resolve,
    resolveTypes,
  ) as Module;
}
