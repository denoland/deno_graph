// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use crate::module_specifier::ModuleSpecifier;

use lazy_static::lazy_static;
use regex::Regex;
use serde_json::from_slice;
use serde_json::Value;
use sourcemap::decode_data_url;
use sourcemap::DecodedMap;

lazy_static! {
  static ref SOURCE_MAP_URL_RE: Regex =
    Regex::new(r#"(?i)^//# sourceMappingURL=([^\s'"]+)"#).unwrap();
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ParsedSourceMap {
  Value(Value),
  Url(ModuleSpecifier),
}

fn find_source_map_url(code: &str) -> Option<String> {
  let last_line = code.rsplit(|u| u == '\n').next()?;
  let caps = SOURCE_MAP_URL_RE.captures(last_line)?;
  let m = caps.get(1)?;
  Some(m.as_str().to_string())
}

/// Attempt to identify and parse a source map URL from a source file, returning
/// the parsed source map (if inlined) or the specifier to the source map.
pub(crate) fn parse_sourcemap(
  specifier: &ModuleSpecifier,
  code: &str,
) -> Option<ParsedSourceMap> {
  let source_map_url = find_source_map_url(code)?;
  let source_map_specifier = ModuleSpecifier::parse(&source_map_url)
    .or_else(|_| specifier.join(&source_map_url))
    .ok()?;
  if source_map_specifier.scheme() == "data" {
    let decoded_map = decode_data_url(source_map_specifier.as_str()).ok()?;
    if let DecodedMap::Regular(source_map) = decoded_map {
      let mut output = Vec::<u8>::new();
      source_map.to_writer(&mut output).ok()?;
      let value: Value = from_slice(&output).ok()?;
      Some(ParsedSourceMap::Value(value))
    } else {
      None
    }
  } else {
    Some(ParsedSourceMap::Url(source_map_specifier))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json::json;

  #[test]
  fn test_parse_sourcemap_data() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/tst/mod.ts").unwrap();
    let source = r#"console.log("hello deno");
export {};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5wdXQuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyJpbnB1dC50c3giXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQ0EsT0FBTyxDQUFDLEdBQUcsQ0FBQyxZQUFZLENBQUMsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbImV4cG9ydCB7fTtcbmNvbnNvbGUubG9nKFwiaGVsbG8gZGVub1wiKTtcbiJdfQ=="#;
    let maybe_parsed_source_map = parse_sourcemap(&specifier, source);
    assert_eq!(
      maybe_parsed_source_map,
      Some(ParsedSourceMap::Value(json!({
        "version": 3,
        "file": "input.js",
        "sources": vec!["input.tsx"],
        "sourcesContent": vec!["export {};\nconsole.log(\"hello deno\");\n"],
        "names": Vec::<String>::new(),
        "mappings": "AACA,OAAO,CAAC,GAAG,CAAC,YAAY,CAAC,CAAC"
      })))
    );
  }

  #[test]
  fn test_parse_sourcemap_url() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/tst/input.js").unwrap();
    let source = r#"console.log("hello deno");
export {};
//# sourceMappingURL=./input.js.map"#;
    let maybe_parsed_source_map = parse_sourcemap(&specifier, source);
    assert_eq!(
      maybe_parsed_source_map,
      Some(ParsedSourceMap::Url(
        ModuleSpecifier::parse("https://deno.land/x/tst/input.js.map").unwrap()
      ))
    )
  }
}
