// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::cmp::Ordering;

use deno_ast::ModuleSpecifier;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

use crate::semver::NpmVersionReqSpecifierParseError;
use crate::semver::Version;
use crate::semver::VersionReq;

#[derive(Debug, Error)]
#[error("Invalid npm package id '{text}'. {message}")]
pub struct NpmPackageIdDeserializationError {
  message: String,
  text: String,
}

/// A resolved unique identifier for an npm package. This contains
/// the resolved name, version, and peer dependency resolution identifiers.
#[derive(
  Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize,
)]
pub struct NpmPackageId {
  pub name: String,
  pub version: Version,
  pub peer_dependencies: Vec<NpmPackageId>,
}

impl NpmPackageId {
  #[allow(unused)]
  pub fn scope(&self) -> Option<&str> {
    if self.name.starts_with('@') && self.name.contains('/') {
      self.name.split('/').next()
    } else {
      None
    }
  }

  pub fn as_serialized(&self) -> String {
    self.as_serialized_with_level(0)
  }

  fn as_serialized_with_level(&self, level: usize) -> String {
    // WARNING: This should not change because it's used in the CLI's lockfile
    let mut result = format!(
      "{}@{}",
      if level == 0 {
        self.name.to_string()
      } else {
        self.name.replace('/', "+")
      },
      self.version
    );
    for peer in &self.peer_dependencies {
      // unfortunately we can't do something like `_3` when
      // this gets deep because npm package names can start
      // with a number
      result.push_str(&"_".repeat(level + 1));
      result.push_str(&peer.as_serialized_with_level(level + 1));
    }
    result
  }

  pub fn from_serialized(
    id: &str,
  ) -> Result<Self, NpmPackageIdDeserializationError> {
    use monch::*;

    fn parse_name(input: &str) -> ParseResult<&str> {
      if_not_empty(substring(move |input| {
        for (pos, c) in input.char_indices() {
          // first character might be a scope, so skip it
          if pos > 0 && c == '@' {
            return Ok((&input[pos..], ()));
          }
        }
        ParseError::backtrace()
      }))(input)
    }

    fn parse_version(input: &str) -> ParseResult<&str> {
      if_not_empty(substring(skip_while(|c| c != '_')))(input)
    }

    fn parse_name_and_version(input: &str) -> ParseResult<(String, Version)> {
      let (input, name) = parse_name(input)?;
      let (input, _) = ch('@')(input)?;
      let at_version_input = input;
      let (input, version) = parse_version(input)?;
      match Version::parse_from_npm(version) {
        Ok(version) => Ok((input, (name.to_string(), version))),
        Err(err) => ParseError::fail(at_version_input, format!("{err:#}")),
      }
    }

    fn parse_level_at_level<'a>(
      level: usize,
    ) -> impl Fn(&'a str) -> ParseResult<'a, ()> {
      fn parse_level(input: &str) -> ParseResult<usize> {
        let level = input.chars().take_while(|c| *c == '_').count();
        Ok((&input[level..], level))
      }

      move |input| {
        let (input, parsed_level) = parse_level(input)?;
        if parsed_level == level {
          Ok((input, ()))
        } else {
          ParseError::backtrace()
        }
      }
    }

    fn parse_peers_at_level<'a>(
      level: usize,
    ) -> impl Fn(&'a str) -> ParseResult<'a, Vec<NpmPackageId>> {
      move |mut input| {
        let mut peers = Vec::new();
        while let Ok((level_input, _)) = parse_level_at_level(level)(input) {
          input = level_input;
          let peer_result = parse_id_at_level(level)(input)?;
          input = peer_result.0;
          peers.push(peer_result.1);
        }
        Ok((input, peers))
      }
    }

    fn parse_id_at_level<'a>(
      level: usize,
    ) -> impl Fn(&'a str) -> ParseResult<'a, NpmPackageId> {
      move |input| {
        let (input, (name, version)) = parse_name_and_version(input)?;
        let name = if level > 0 {
          name.replace('+', "/")
        } else {
          name
        };
        let (input, peer_dependencies) =
          parse_peers_at_level(level + 1)(input)?;
        Ok((
          input,
          NpmPackageId {
            name,
            version,
            peer_dependencies,
          },
        ))
      }
    }

    with_failure_handling(parse_id_at_level(0))(id).map_err(|err| {
      NpmPackageIdDeserializationError {
        message: format!("{err:#}"),
        text: id.to_string(),
      }
    })
  }

  pub fn display(&self) -> String {
    // Don't implement std::fmt::Display because we don't
    // want this to be used by accident in certain scenarios.
    format!("{}@{}", self.name, self.version)
  }
}

#[derive(Error, Debug)]
pub enum NpmPackageReferenceParseError {
  #[error("Not an npm specifier.")]
  NotNpmSpecifier,
  #[error("Not a valid package: {0}")]
  InvalidPackage(String),
  #[error("Invalid npm specifier '{specifier}'. {source:#}")]
  Invalid {
    specifier: String,
    #[source]
    source: VersionReqPartsParseError,
  },
  #[error("Invalid package specifier 'npm:{current}'. Did you mean to write 'npm:{suggested}'?")]
  InvalidPathWithVersion { current: String, suggested: String },
}

/// A reference to an npm package's name, version constraint, and potential sub path.
///
/// This contains all the information found in an npm specifier.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct NpmPackageReference {
  pub req: NpmPackageReq,
  pub sub_path: Option<String>,
}

impl NpmPackageReference {
  pub fn from_specifier(
    specifier: &ModuleSpecifier,
  ) -> Result<NpmPackageReference, NpmPackageReferenceParseError> {
    Self::from_str(specifier.as_str())
  }

  #[allow(clippy::should_implement_trait)]
  pub fn from_str(
    specifier: &str,
  ) -> Result<NpmPackageReference, NpmPackageReferenceParseError> {
    let original_text = specifier;
    let specifier = match specifier.strip_prefix("npm:") {
      Some(s) => {
        // Strip leading slash, which might come from import map
        s.strip_prefix('/').unwrap_or(s)
      }
      None => {
        // don't allocate a string here and instead use a static string
        // because this is hit a lot when a url is not an npm specifier
        return Err(NpmPackageReferenceParseError::NotNpmSpecifier);
      }
    };
    let parts = specifier.split('/').collect::<Vec<_>>();
    let name_part_len = if specifier.starts_with('@') { 2 } else { 1 };
    if parts.len() < name_part_len {
      return Err(NpmPackageReferenceParseError::InvalidPackage(
        specifier.to_string(),
      ));
    }
    let name_parts = &parts[0..name_part_len];
    let req = match NpmPackageReq::parse_from_parts(name_parts) {
      Ok(pkg_req) => pkg_req,
      Err(err) => {
        return Err(NpmPackageReferenceParseError::Invalid {
          specifier: original_text.to_string(),
          source: err,
        });
      }
    };
    let sub_path = if parts.len() == name_parts.len() {
      None
    } else {
      let sub_path = parts[name_part_len..].join("/");
      if sub_path.is_empty() {
        None
      } else {
        Some(sub_path)
      }
    };

    if let Some(sub_path) = &sub_path {
      if let Some(at_index) = sub_path.rfind('@') {
        let (new_sub_path, version) = sub_path.split_at(at_index);
        return Err(NpmPackageReferenceParseError::InvalidPathWithVersion {
          current: format!("{req}/{sub_path}"),
          suggested: format!("{req}{version}/{new_sub_path}"),
        });
      }
    }

    Ok(NpmPackageReference { req, sub_path })
  }
}

impl std::fmt::Display for NpmPackageReference {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if let Some(sub_path) = &self.sub_path {
      write!(f, "npm:{}/{}", self.req, sub_path)
    } else {
      write!(f, "npm:{}", self.req)
    }
  }
}

#[derive(Error, Debug)]
pub enum VersionReqPartsParseError {
  #[error("Did not contain a package name.")]
  NoPackageName,
  #[error("Invalid version requirement. {source:#}")]
  VersionReq {
    #[source]
    source: NpmVersionReqSpecifierParseError,
  },
}

#[derive(Error, Debug)]
#[error("Invalid npm package requirement '{text}'. {source:#}")]
pub struct NpmPackageReqParseError {
  pub text: String,
  #[source]
  source: VersionReqPartsParseError,
}

/// The name and version constraint component of an `NpmPackageReference`.
#[derive(
  Clone, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize,
)]
pub struct NpmPackageReq {
  pub name: String,
  pub version_req: Option<VersionReq>,
}

impl std::fmt::Display for NpmPackageReq {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.version_req {
      Some(req) => write!(f, "{}@{}", self.name, req),
      None => write!(f, "{}", self.name),
    }
  }
}

impl NpmPackageReq {
  #[allow(clippy::should_implement_trait)]
  pub fn from_str(text: &str) -> Result<Self, NpmPackageReqParseError> {
    let parts = text.split('/').collect::<Vec<_>>();
    match NpmPackageReq::parse_from_parts(&parts) {
      Ok(req) => Ok(req),
      Err(err) => Err(NpmPackageReqParseError {
        text: text.to_string(),
        source: err,
      }),
    }
  }

  fn parse_from_parts(
    name_parts: &[&str],
  ) -> Result<Self, VersionReqPartsParseError> {
    assert!(!name_parts.is_empty()); // this should be provided the result of a string split
    let last_name_part = &name_parts[name_parts.len() - 1];
    let (name, version_req) = if let Some(at_index) = last_name_part.rfind('@')
    {
      let version = &last_name_part[at_index + 1..];
      let last_name_part = &last_name_part[..at_index];
      let version_req = VersionReq::parse_from_specifier(version)
        .map_err(|err| VersionReqPartsParseError::VersionReq { source: err })?;
      let name = if name_parts.len() == 1 {
        last_name_part.to_string()
      } else {
        format!("{}/{}", name_parts[0], last_name_part)
      };
      (name, Some(version_req))
    } else {
      (name_parts.join("/"), None)
    };
    if name.is_empty() {
      Err(VersionReqPartsParseError::NoPackageName)
    } else {
      Ok(Self { name, version_req })
    }
  }
}

impl PartialOrd for NpmPackageReq {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

// Sort the package requirements alphabetically then the version
// requirement in a way that will lead to the least number of
// duplicate packages (so sort None last since it's `*`), but
// mostly to create some determinism around how these are resolved.
impl Ord for NpmPackageReq {
  fn cmp(&self, other: &Self) -> Ordering {
    fn cmp_specifier_version_req(a: &VersionReq, b: &VersionReq) -> Ordering {
      match a.tag() {
        Some(a_tag) => match b.tag() {
          Some(b_tag) => b_tag.cmp(a_tag), // sort descending
          None => Ordering::Less,          // prefer a since tag
        },
        None => {
          match b.tag() {
            Some(_) => Ordering::Greater, // prefer b since tag
            None => {
              // At this point, just sort by text descending.
              // We could maybe be a bit smarter here in the future.
              b.to_string().cmp(&a.to_string())
            }
          }
        }
      }
    }

    match self.name.cmp(&other.name) {
      Ordering::Equal => {
        match &other.version_req {
          Some(b_req) => {
            match &self.version_req {
              Some(a_req) => cmp_specifier_version_req(a_req, b_req),
              None => Ordering::Greater, // prefer b, since a is *
            }
          }
          None => Ordering::Less, // prefer a, since b is *
        }
      }
      ordering => ordering,
    }
  }
}

#[cfg(test)]
mod tests {
  use pretty_assertions::assert_eq;

  use super::*;

  #[test]
  fn parse_npm_package_ref() {
    assert_eq!(
      NpmPackageReference::from_str("npm:@package/test").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "@package/test".to_string(),
          version_req: None,
        },
        sub_path: None,
      }
    );

    assert_eq!(
      NpmPackageReference::from_str("npm:@package/test@1").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "@package/test".to_string(),
          version_req: Some(VersionReq::parse_from_specifier("1").unwrap()),
        },
        sub_path: None,
      }
    );

    assert_eq!(
      NpmPackageReference::from_str("npm:@package/test@~1.1/sub_path").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "@package/test".to_string(),
          version_req: Some(VersionReq::parse_from_specifier("~1.1").unwrap()),
        },
        sub_path: Some("sub_path".to_string()),
      }
    );

    assert_eq!(
      NpmPackageReference::from_str("npm:@package/test/sub_path").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "@package/test".to_string(),
          version_req: None,
        },
        sub_path: Some("sub_path".to_string()),
      }
    );

    assert_eq!(
      NpmPackageReference::from_str("npm:test").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "test".to_string(),
          version_req: None,
        },
        sub_path: None,
      }
    );

    assert_eq!(
      NpmPackageReference::from_str("npm:test@^1.2").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "test".to_string(),
          version_req: Some(VersionReq::parse_from_specifier("^1.2").unwrap()),
        },
        sub_path: None,
      }
    );

    assert_eq!(
      NpmPackageReference::from_str("npm:test@~1.1/sub_path").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "test".to_string(),
          version_req: Some(VersionReq::parse_from_specifier("~1.1").unwrap()),
        },
        sub_path: Some("sub_path".to_string()),
      }
    );

    assert_eq!(
      NpmPackageReference::from_str("npm:@package/test/sub_path").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "@package/test".to_string(),
          version_req: None,
        },
        sub_path: Some("sub_path".to_string()),
      }
    );

    assert_eq!(
      NpmPackageReference::from_str("npm:@package")
        .err()
        .unwrap()
        .to_string(),
      "Not a valid package: @package"
    );

    // should parse leading slash
    assert_eq!(
      NpmPackageReference::from_str("npm:/@package/test/sub_path").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "@package/test".to_string(),
          version_req: None,
        },
        sub_path: Some("sub_path".to_string()),
      }
    );
    assert_eq!(
      NpmPackageReference::from_str("npm:/test").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "test".to_string(),
          version_req: None,
        },
        sub_path: None,
      }
    );
    assert_eq!(
      NpmPackageReference::from_str("npm:/test/").unwrap(),
      NpmPackageReference {
        req: NpmPackageReq {
          name: "test".to_string(),
          version_req: None,
        },
        sub_path: None,
      }
    );

    // should error for no name
    assert_eq!(
      NpmPackageReference::from_str("npm:/")
        .err()
        .unwrap()
        .to_string(),
      "Invalid npm specifier 'npm:/'. Did not contain a package name."
    );
    assert_eq!(
      NpmPackageReference::from_str("npm://test")
        .err()
        .unwrap()
        .to_string(),
      "Invalid npm specifier 'npm://test'. Did not contain a package name."
    );
  }

  #[test]
  fn sorting_package_reqs() {
    fn cmp_req(a: &str, b: &str) -> Ordering {
      let a = NpmPackageReq::from_str(a).unwrap();
      let b = NpmPackageReq::from_str(b).unwrap();
      a.cmp(&b)
    }

    // sort by name
    assert_eq!(cmp_req("a", "b@1"), Ordering::Less);
    assert_eq!(cmp_req("b@1", "a"), Ordering::Greater);
    // prefer non-wildcard
    assert_eq!(cmp_req("a", "a@1"), Ordering::Greater);
    assert_eq!(cmp_req("a@1", "a"), Ordering::Less);
    // prefer tag
    assert_eq!(cmp_req("a@tag", "a"), Ordering::Less);
    assert_eq!(cmp_req("a", "a@tag"), Ordering::Greater);
    // sort tag descending
    assert_eq!(cmp_req("a@latest-v1", "a@latest-v2"), Ordering::Greater);
    assert_eq!(cmp_req("a@latest-v2", "a@latest-v1"), Ordering::Less);
    // sort version req descending
    assert_eq!(cmp_req("a@1", "a@2"), Ordering::Greater);
    assert_eq!(cmp_req("a@2", "a@1"), Ordering::Less);
  }

  #[test]
  fn serialize_npm_package_id() {
    let id = NpmPackageId {
      name: "pkg-a".to_string(),
      version: Version::parse_from_npm("1.2.3").unwrap(),
      peer_dependencies: vec![
        NpmPackageId {
          name: "pkg-b".to_string(),
          version: Version::parse_from_npm("3.2.1").unwrap(),
          peer_dependencies: vec![
            NpmPackageId {
              name: "pkg-c".to_string(),
              version: Version::parse_from_npm("1.3.2").unwrap(),
              peer_dependencies: vec![],
            },
            NpmPackageId {
              name: "pkg-d".to_string(),
              version: Version::parse_from_npm("2.3.4").unwrap(),
              peer_dependencies: vec![],
            },
          ],
        },
        NpmPackageId {
          name: "pkg-e".to_string(),
          version: Version::parse_from_npm("2.3.1").unwrap(),
          peer_dependencies: vec![NpmPackageId {
            name: "pkg-f".to_string(),
            version: Version::parse_from_npm("2.3.1").unwrap(),
            peer_dependencies: vec![],
          }],
        },
      ],
    };

    // this shouldn't change because it's used in the CLI's lockfile
    let serialized = id.as_serialized();
    assert_eq!(serialized, "pkg-a@1.2.3_pkg-b@3.2.1__pkg-c@1.3.2__pkg-d@2.3.4_pkg-e@2.3.1__pkg-f@2.3.1");
    assert_eq!(NpmPackageId::from_serialized(&serialized).unwrap(), id);
  }
}
