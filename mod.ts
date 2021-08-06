// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import { createGraph as jsCreateGraph } from "./lib/deno_graph.js";
import type { CacheInfo, LoadResponse, ModuleGraph } from "./lib/types.d.ts";

export { load } from "./lib/loader.ts";
export type {
  CacheInfo,
  LoadResponse,
  ModuleGraph,
  ModuleGraphJson,
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
   * @param isDynamic A flag that indicates if the module was being loaded dynamically
   */
  load(
    specifier: string,
    isDynamic: boolean,
  ): Promise<LoadResponse | undefined>;
  /** An optional callback that will be called with a URL string of the resource
   * to provide additional meta data about the resource to enrich the module
   * graph. */
  cacheInfo?(specifier: string): CacheInfo;
  /** An optional callback that allows the default resolution logic of the
   * module graph to be "overridden". This is intended to allow items like an
   * import map to be used with the module graph. The callback takes the string
   * of the module specifier from the referrer and the string URL of the
   * referrer. The callback then returns a resolved URL string specifier. */
  resolve?(specifier: string, referrer: string): string;
  /** An optional callback that returns `true` if the sub-resource integrity of
   * the provided specifier and content is valid, otherwise `false`. This allows
   * for items like lock files to be applied to the module graph. */
  check?(specifier: string, content: Uint8Array): boolean;
  /** An optional callback that returns the sub-resource integrity checksum for
   * a given set of content. */
  getChecksum?(content: Uint8Array): string;
}

/** Create a module graph using the same algorithms that are used in the Deno
 * CLI, resolving with the module graph for further processing.
 *
 * Users need to provide a `load()` callback at a minimum, which fetches the
 * source files and returns them to the graph builder.
 *
 * ### Example
 *
 * ```ts
 * import { createGraph } from "https://deno.land/x/deno_graph/mod.ts";
 *
 * const graph = await createGraph("https://example.com/a.ts", {
 *   load(specifier) {
 *     return {
 *       specifier,
 *       headers: {
 *         "content-type": "application/typescript"
 *       },
 *       content: `console.log("hello deno_graph");`
 *     }
 *   }
 * });
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
  options: CreateGraphOptions,
): Promise<ModuleGraph> {
  const { load, cacheInfo, resolve, check, getChecksum } = options;
  return jsCreateGraph(
    rootSpecifier,
    load,
    cacheInfo,
    resolve,
    check,
    getChecksum,
  );
}
