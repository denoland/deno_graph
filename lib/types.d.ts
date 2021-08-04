// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

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
