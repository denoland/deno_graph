// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.
// deno-lint-ignore-file

export function get_no_color() {
  return "Deno" in globalThis ? Boolean(Deno.noColor) : true;
}
