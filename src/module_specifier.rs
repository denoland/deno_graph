// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use anyhow::Result;
use std::error::Error;
use std::fmt;
use url::ParseError;

pub type ModuleSpecifier = deno_ast::ModuleSpecifier;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpecifierError {
  InvalidUrl(ParseError),
  ImportPrefixMissing(String, Option<ModuleSpecifier>),
}

use SpecifierError::*;

impl Error for SpecifierError {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    match self {
      InvalidUrl(ref err) => Some(err),
      _ => None,
    }
  }
}

impl fmt::Display for SpecifierError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      InvalidUrl(ref err) => write!(f, "invalid URL: {}", err),
      ImportPrefixMissing(ref specifier, _) => write!(
        f,
        "Relative import path \"{}\" not prefixed with / or ./ or ../",
        specifier,
      ),
    }
  }
}

/// Given a specifier string and a referring module specifier, try to resolve
/// the target module specifier, erroring if it cannot be resolved.
pub fn resolve_import(
  specifier: &str,
  referrer: &ModuleSpecifier,
) -> Result<ModuleSpecifier, SpecifierError> {
  let url = match ModuleSpecifier::parse(specifier) {
    // 1. Apply the URL parser to specifier.
    //    If the result is not failure, return he result.
    Ok(url) => url,

    // 2. If specifier does not start with the character U+002F SOLIDUS (/),
    //    the two-character sequence U+002E FULL STOP, U+002F SOLIDUS (./),
    //    or the three-character sequence U+002E FULL STOP, U+002E FULL STOP,
    //    U+002F SOLIDUS (../), return failure.
    Err(ParseError::RelativeUrlWithoutBase)
      if !(specifier.starts_with('/')
        || specifier.starts_with("./")
        || specifier.starts_with("../")) =>
    {
      return Err(ImportPrefixMissing(
        specifier.to_string(),
        Some(referrer.clone()),
      ));
    }

    // 3. Return the result of applying the URL parser to specifier with base
    //    URL as the base URL.
    Err(ParseError::RelativeUrlWithoutBase) => {
      referrer.join(specifier).map_err(InvalidUrl)?
    }

    // If parsing the specifier as a URL failed for a different reason than
    // it being relative, always return the original error. We don't want to
    // return `ImportPrefixMissing` or `InvalidBaseUrl` if the real
    // problem lies somewhere else.
    Err(err) => return Err(InvalidUrl(err)),
  };

  Ok(url)
}
