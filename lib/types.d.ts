// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import type { MediaType } from "./media_type.ts";

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

export interface LocationJson {
  /** The line number of a location within a source file. The number is a zero
   * based index. */
  line: number;
  /** The character number of a location within a source file. The number is a
   * zero based index. */
  character: number;
}

export interface SpanJson {
  /** The start location of a span of text in a source file. */
  start: LocationJson;
  /** The end location of a span of text in a source file. */
  end: LocationJson;
}

export interface ResolvedDependencyJson {
  /** The fully resolved string URL of the dependency, which should be
   * resolvable in the module graph. If there was an error, `error` will be set
   * and this will be undefined. */
  specifier?: string;
  /** Any error encountered when trying to resolved the specifier. If this is
   * defined, `specifier` will be undefined. */
  error?: string;
  /** The span within the source code where the specifier was identified. */
  span: SpanJson;
}

export interface TypesDependencyJson {
  /** The string specifier that was used for the dependency. */
  specifier: string;
  /** An object point to the resolved dependency. */
  dependency: ResolvedDependencyJson;
}

export interface DependencyJson {
  /** The string specifier that was used for the dependency. */
  specifier: string;
  /** An object pointing to the resolved _code_ dependency. */
  code?: ResolvedDependencyJson;
  /** An object pointing to the resolved _type_ dependency of a module. This is
   * populated when the `@deno-types` directive was used to supply a type
   * definition file for a dependency. */
  type?: ResolvedDependencyJson;
  /** A flag indicating if the dependency was dynamic. (e.g.
   * `await import("mod.ts")`) */
  isDynamic?: true;
}

export interface ModuleJson extends CacheInfo {
  /** The string URL of the module. */
  specifier: string;
  /** Any error encountered when attempting to load the module. */
  error?: string;
  /** An array of dependendencies that were identified in the module. */
  dependencies?: DependencyJson[];
  /** If the module had a types dependency, the information about that
   * dependency. */
  typesDependency?: TypesDependencyJson;
  /** The resolved media type of the module, which determines how Deno will
   * handle the module. */
  mediaType?: MediaType;
  /** The size of the source content of the module in bytes. */
  size?: number;
  /** If available, the calculated checksum of the module which can be used for
   * validating the integrity of the module. */
  checksum?: string;
}

/** The plain-object representation of a module graph that is suitable for
 * serialization to JSON. */
export interface ModuleGraphJson {
  /** The module specifier (URL string) of the _root_ of the module graph of
   * which the module graph was built for. */
  root: string;
  /** An array of modules that are part of the module graph. */
  modules: ModuleJson[];
  /** A record/map of any redirects encountered when resolving modules. The
   * key was the requested module specifier and the value is the redirected
   * module specifier. */
  redirects: Record<string, string>;
}

/** An interface to the web assembly structure of a built module graph. */
export class ModuleGraph {
  private constructor();

  /** Explictly free the memory used by the module graph. The web assembly
   * bindings does use weak references, meaning that the memory should be
   * automatically garbage collected when the graph falls out of use. */
  free(): void;

  /** Return a plain-object representation of the module graph suitable for
   * serialization as JSON, similiar to the `deno info --json` output. */
  toJSON(): ModuleGraphJson;

  /** Provides a string output representation of the module graph similar to
   * `deno info` with or without ANSI color escape sequences. If `noColor` is
   * expressly `true`, the string will be returned without color escape
   * sequences. If `noColor` is expressly `false` the returned string will
   * include ANSI color escape sequences. If not expressly set, `Deno.noColor`
   * will be used, or if the `Deno` namespace isn't present, will default to
   * `true` and not provide ANSI color escape sequences.
   *
   * @param noColor An optional flag indicating if ANSI color escape codes
   *                should be included in the returned string.*/
  toString(noColor?: boolean): string;
}
