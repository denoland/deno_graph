// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import { js_create_graph as jsCreateGraph } from "./lib/deno_graph.js";

/** Additional meta data that is used to enrich the output of the module
 * graph. */
export interface CacheInfo {
  /** The string path to where the local version of the content is located. For
   * non `file:` URLs, this is the location of the cached content, otherwise it
   * is the absolute path to the local file. */
  local?: string;
  /** The string path to where a transpiled version of the source content is
   * located, if any. */
  emit?: string;
  /** The string path to where an external source map of the transpiled source
   * content is located, if any. */
  map?: string;
}

export interface LoadResponse {
  /** The string URL of the resource. If there were redirects, the final
   * specifier should be set here, otherwise the requested specifier. */
  specifier: string;
  /** For remote resources, a record of headers should be set, where the key's
   * have been normalized to be lower case values. */
  headers?: Record<string, string>;
  /** The string value of the loaded resources. */
  content: string;
}

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
 * console.log(graph);
 * ```
 *
 * @param rootSpecifier A URL string of the root module specifier to build the
 *                      graph from.
 * @param options A set of options for building the graph
 */
export function createGraph(
  rootSpecifier: string,
  options: CreateGraphOptions,
): Promise<string> {
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
