// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

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

export interface TypesDependency {
  /** The string URL to the type information for the module. */
  types: string;
  /** An optional range which indicates the source of the dependency. */
  source?: Range;
}

export interface ResolveResult {
  /** The string URL of the fully qualified specifier for a module. */
  specifier: string;
  /** The module kind of the resolved module. */
  kind: ModuleKind;
}

export interface LoadResponseModule {
  /** A module with code has been loaded. */
  kind: "module";
  /** The string URL of the resource. If there were redirects, the final
   * specifier should be set here, otherwise the requested specifier. */
  specifier: string;
  /** For remote resources, a record of headers should be set, where the key's
   * have been normalized to be lower case values. */
  headers?: Record<string, string>;
  /** The string value of the loaded resources. */
  content: string;
}

export interface LoadResponseExternalBuiltIn {
  /** The loaded module is either _external_ or _built-in_ to the runtime. */
  kind: "external" | "builtIn";
  /** The strung URL of the resource. If there were redirects, the final
   * specifier should be set here, otherwise the requested specifier. */
  specifier: string;
}

export type LoadResponse = LoadResponseModule | LoadResponseExternalBuiltIn;

export interface PositionJson {
  /** The line number of a position within a source file. The number is a zero
   * based index. */
  line: number;
  /** The character number of a position within a source file. The number is a
   * zero based index. */
  character: number;
}

export interface Range {
  /** A string URL representing a source of a dependency. */
  specifier: string;
  /** The start location of a range of text in a source file. */
  start?: PositionJson;
  /** The end location of a range of text in a source file. */
  end?: PositionJson;
}

export interface RangeJson {
  /** The start location of a range of text in a source file. */
  start: PositionJson;
  /** The end location of a range of text in a source file. */
  end: PositionJson;
}

export interface ResolvedDependency {
  /** The fully resolved string URL of the dependency, which should be
   * resolvable in the module graph. If there was an error, `error` will be set
   * and this will be undefined. */
  specifier?: string;
  /** Any error encountered when trying to resolved the specifier. If this is
   * defined, `specifier` will be undefined. */
  error?: string;
  /** The range within the source code where the specifier was identified. */
  span: RangeJson;
}

export interface TypesDependencyJson {
  /** The string specifier that was used for the dependency. */
  specifier: string;
  /** An object pointing to the resolved dependency. */
  dependency: ResolvedDependency;
}

/** The kind of module.
 *
 * For asserted modules, the value of the `asserted` property is set to the
 * `type` value of the assertion.
 *
 * Dependency analysis is not performed for asserted, AMD, Script, SystemJS, or
 * UMD modules currently. Synthetic modules were injected into the graph with
 * their own dependencies provided. */
export type ModuleKind =
  | "amd"
  | "asserted"
  | "commonJs"
  | "esm"
  | "script"
  | "synthetic"
  | "systemJs"
  | "umd";

export interface DependencyJson {
  /** The string specifier that was used for the dependency. */
  specifier: string;
  /** An object pointing to the resolved _code_ dependency. */
  code?: ResolvedDependency;
  /** An object pointing to the resolved _type_ dependency of a module. This is
   * populated when the `@deno-types` directive was used to supply a type
   * definition file for a dependency. */
  type?: ResolvedDependency;
  /** A flag indicating if the dependency was dynamic. (e.g.
   * `await import("mod.ts")`) */
  isDynamic?: true;
}

export interface ModuleJson extends CacheInfo {
  /** The string URL of the module. */
  specifier: string;
  /** Any error encountered when attempting to load the module. */
  error?: string;
  /** The module kind that was determined when the module was resolved. This is
   * used by loaders to indicate how a module needs to be loaded at runtime. */
  kind?: ModuleKind;
  /** An array of dependencies that were identified in the module. */
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
  /** The module specifiers (URL string) of the _roots_ of the module graph of
   * which the module graph was built for. */
  roots: string[];
  /** An array of modules that are part of the module graph. */
  modules: ModuleJson[];
  /** A record/map of any redirects encountered when resolving modules. The
   * key was the requested module specifier and the value is the redirected
   * module specifier. */
  redirects: Record<string, string>;
}

export interface Dependency {
  /** An object pointing to the resolved _code_ dependency. */
  code?: ResolvedDependency;
  /** An object pointing to the resolved _type_ dependency of a module. This is
   * populated when the `@deno-types` directive was used to supply a type
   * definition file for a dependency. */
  type?: ResolvedDependency;
  /** A flag indicating if the dependency was dynamic. (e.g.
   * `await import("mod.ts")`) */
  isDynamic?: true;
}

export class Module {
  private constructor();

  /** Any cache information that was available on the module when the graph
   * was built. */
  readonly cacheInfo?: CacheInfo;
  /** The calculated checksum of the source of the module if available when the
   * graph was built. */
  readonly checksum?: string;
  /** A record of the dependencies, where the key is the string specifier of
   * the dependency and the value is the dependency object. */
  readonly dependencies?: Record<string, Dependency>;
  /** A module kind that can be used to determine how a module should be loaded
   * at runtime. */
  readonly kind: ModuleKind;
  /** The media type assigned to the module. This determines how Deno will
   * handle the module. */
  readonly mediaType: MediaType;
  /** The size of the source content in bytes. */
  readonly size: number;
  /** The source content of the module. */
  readonly source: string;
  /** The fully qualified string URL of the module. */
  readonly specifier: string;
  /** The types dependency for the module, where the first value in the tuple
   * was the string specifier used and the second value is the resolved
   * dependency. */
  readonly typesDependency?: [string, ResolvedDependency];

  /** Explicitly free the memory used by the module. */
  free(): void;

  /** Returns a plain-object representation of the module suitable for
   * serialization as JSON. */
  toJSON(): ModuleJson;
}

/** An interface to the web assembly structure of a built module graph. */
export class ModuleGraph {
  private constructor();

  /** The modules that are part of the module graph. */
  readonly modules: Module[];

  /** The root specifiers that were used to build the module graph from. */
  readonly roots: string[];

  /** Explicitly free the memory used by the module graph. The web assembly
   * bindings does use weak references, meaning that the memory should be
   * automatically garbage collected when the graph falls out of use. */
  free(): void;

  /** Retrieve a module from the module graph, if an error was encountered when
   * loading the module, this method will throw with that error. */
  get(specifier: string): Module | undefined;

  /** Determine if the graph sources are valid by calling the passed `check()`
   * function. If any of the modules in the graph fail the check, then an
   * error is thrown. */
  lock(): void;

  /** Given a string URL, return the resolved string URL accounting for any
   * redirections that might have occurred when resolving the module graph. */
  resolve(specifier: string): string;

  /** Given a string specifier of a module's dependency and the referring
   * module's string URL, return the string URL of the dependency, otherwise
   * return undefined. */
  resolveDependency(specifier: string, referrer: string): string | undefined;

  /** Returns a plain-object representation of the module graph suitable for
   * serialization as JSON, similar to the `deno info --json` output. */
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
   *                should be included in the returned string. */
  toString(noColor?: boolean): string;
}
