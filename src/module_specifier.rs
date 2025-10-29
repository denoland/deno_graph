// Copyright 2018-2024 the Deno authors. MIT license.

pub type ModuleSpecifier = url::Url;

pub use import_map::specifier::SpecifierError;
pub use import_map::specifier::resolve_import;

pub fn is_fs_root_specifier(url: &ModuleSpecifier) -> bool {
  if url.scheme() != "file" {
    return false;
  }

  let path = url.path();
  let path = path.trim_start_matches('/').trim_end_matches('/');
  let mut parts = path.split('/');
  let Some(first_part) = parts.next() else {
    return true;
  };
  if parts.next().is_some() {
    return false;
  }
  let mut first_part_chars = first_part.chars();
  let Some(first_char) = first_part_chars.next() else {
    return true;
  };
  let Some(second_char) = first_part_chars.next() else {
    return false;
  };

  // Windows path: file:///C:/example
  first_part_chars.next().is_none()
    && first_char.is_ascii_alphabetic()
    && second_char == ':'
}

#[cfg(test)]
mod test {
  use crate::ModuleSpecifier;

  use super::*;

  #[test]
  fn test_is_fs_root_specifier() {
    let cases = [
      ("https://deno.land", false),
      ("file:///", true),
      ("file://", true),
      ("file:///C:/", true),
      ("file:///V:/", true),
      ("file:///V:/test/", false),
    ];
    for (specifier, expected) in cases {
      let url = ModuleSpecifier::parse(specifier).unwrap();
      assert_eq!(is_fs_root_specifier(&url), expected, "{:?}", specifier);
    }
  }
}
