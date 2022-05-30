// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use crate::module_specifier::ModuleSpecifier;

use deno_ast::ParsedSource;
use deno_ast::SourceRangedForSpanned;
use lazy_static::lazy_static;
use regex::Regex;
use serde_json::from_slice;
use serde_json::Value;
use sourcemap::decode_data_url;
use sourcemap::DecodedMap;

lazy_static! {
  static ref SOURCE_MAP_URL_RE: Regex =
    Regex::new(r#"(?i)^# sourceMappingURL=([^\s'"]+)"#).unwrap();
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ParsedSourceMap {
  Value(Value),
  Url(ModuleSpecifier),
}

fn find_source_map_url(parsed_source: &ParsedSource) -> Option<String> {
  let program = parsed_source.program_ref();
  let pos = program.range().end;
  let comments = parsed_source.comments().get_trailing(pos)?;
  for item in comments.iter().rev() {
    if let Some(caps) = SOURCE_MAP_URL_RE.captures(&item.text) {
      if let Some(m) = caps.get(1) {
        return Some(m.as_str().to_string());
      }
    }
  }
  None
}

/// Attempt to identify and parse a source map URL from a source file, returning
/// the parsed source map (if inlined) or the specifier to the source map.
pub(crate) fn parse_sourcemap(
  specifier: &ModuleSpecifier,
  parsed_source: &ParsedSource,
) -> Option<ParsedSourceMap> {
  let source_map_url = find_source_map_url(parsed_source)?;
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
  use crate::ast;
  use crate::SourceParser;
  use deno_ast::MediaType;
  use serde_json::json;

  #[test]
  fn test_parse_sourcemap_data() {
    let source_parser = ast::DefaultSourceParser::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/tst/mod.ts").unwrap();
    let source = r#"console.log("hello deno");
export {};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5wdXQuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyJpbnB1dC50c3giXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQ0EsT0FBTyxDQUFDLEdBQUcsQ0FBQyxZQUFZLENBQUMsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbImV4cG9ydCB7fTtcbmNvbnNvbGUubG9nKFwiaGVsbG8gZGVub1wiKTtcbiJdfQ=="#;
    let res = source_parser.parse_module(
      &specifier,
      source.into(),
      MediaType::JavaScript,
    );
    assert!(res.is_ok());
    let parsed_source = res.unwrap();
    let maybe_parsed_source_map = parse_sourcemap(&specifier, &parsed_source);
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
    let source_parser = ast::DefaultSourceParser::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/tst/input.js").unwrap();
    let source = r#"console.log("hello deno");
export {};
//# sourceMappingURL=./input.js.map"#;
    let res = source_parser.parse_module(
      &specifier,
      source.into(),
      MediaType::JavaScript,
    );
    assert!(res.is_ok());
    let parsed_source = res.unwrap();
    let maybe_parsed_source_map = parse_sourcemap(&specifier, &parsed_source);
    assert_eq!(
      maybe_parsed_source_map,
      Some(ParsedSourceMap::Url(
        ModuleSpecifier::parse("https://deno.land/x/tst/input.js.map").unwrap()
      ))
    )
  }
}
