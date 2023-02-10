// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use monch::*;

pub struct ParsedDenoTypes<'a> {
  pub text: &'a str,
  pub quote_start: usize,
  pub quote_end: usize,
}

pub fn parse_deno_types(input: &str) -> Result<ParsedDenoTypes, ParseError> {
  let original_input = input;
  let (input, _) = skip_whitespace(input)?;
  let (input, _) = tag("@deno-types")(input)?;
  let (input, _) = ch('=')(input)?;
  let quote_start_input = input;
  let (input, quote_char) = or(ch('"'), ch('\"'))(input)?;
  let (input, text) = take_while(|c| c != quote_char)(input)?;
  let (input, _) = ch(quote_char)(input)?;
  Ok(ParsedDenoTypes {
    text,
    quote_start: original_input.len() - quote_start_input.len(),
    quote_end: original_input.len() - input.len(),
  })
}

pub struct ParsedTripleSlashReference<'a> {
  pub text: &'a str,
  pub quote_start: usize,
  pub quote_end: usize,
}

/// Matches a `/// <reference ... />` comment reference based on the kind (ex. path or types).
pub fn parse_triple_slash_reference<'a>(
  kind: &str,
  input: &'a str,
) -> Result<ParsedTripleSlashReference<'a>, ParseError<'a>> {
  // regex in TS codebase: /^(\/\/\/\s*<reference\s+path\s*=\s*)(('[^']*')|("[^"]*")).*?\/>/

  let original_input = input;
  let (input, _) = ch('/')(input)?; // only one, because we're starting from within a comment line
  let (input, _) = skip_whitespace(input)?;
  let (input, _) = tag("<reference")(input)?;
  let (input, _) = skip_whitespace(input)?;
  let (input, _) = tag(kind)(input)?; // "path" or "types"
  let (input, _) = skip_whitespace(input)?;
  let (input, _) = ch('=')(input)?;
  let (input, _) = skip_whitespace(input)?;
  let quote_start_input = input;
  let (input, quote_char) = or(ch('"'), ch('\"'))(input)?;
  let (input, text) = take_while(|c| c != quote_char)(input)?;
  let (input, _) = ch(quote_char)(input)?;
  let quote_end_input = input;
  if !input.contains("/>") {
    return Err(monch::ParseError::Backtrace);
  }
  Ok(ParsedTripleSlashReference {
    text,
    quote_start: original_input.len() - quote_start_input.len(),
    quote_end: original_input.len() - quote_end_input.len(),
  })
}
