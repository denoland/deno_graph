// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::cmp::Ordering;
use std::fmt;

use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

mod npm;
mod range;
mod specifier;

pub use self::npm::NpmVersionParseError;
pub use self::npm::NpmVersionReqParseError;
pub use self::range::Partial;
pub use self::range::VersionBoundKind;
pub use self::range::VersionRange;
pub use self::range::VersionRangeSet;
pub use self::range::XRange;
pub use self::specifier::NpmVersionReqSpecifierParseError;

use self::npm::parse_npm_version_req;
use self::specifier::parse_version_req_from_specifier;

#[derive(Error, Debug)]
#[error("Invalid version. {source}")]
pub struct VersionParseError {
  #[source]
  source: monch::ParseErrorFailureError,
}

#[derive(Clone, Debug, PartialEq, Eq, Default, Hash)]
pub struct Version {
  pub major: u64,
  pub minor: u64,
  pub patch: u64,
  pub pre: Vec<String>,
  pub build: Vec<String>,
}

impl Serialize for Version {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_str(&self.to_string())
  }
}

impl<'de> Deserialize<'de> for Version {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    let text = String::deserialize(deserializer)?;
    match Version::parse_standard(&text) {
      Ok(version) => Ok(version),
      Err(err) => Err(serde::de::Error::custom(err)),
    }
  }
}

impl Version {
  /// Parse a version.
  pub fn parse_standard(text: &str) -> Result<Version, VersionParseError> {
    // re-use npm's loose version parsing
    Self::parse_from_npm(text)
      .map_err(|err| VersionParseError { source: err.source })
  }

  /// Parse a version from npm.
  pub fn parse_from_npm(text: &str) -> Result<Version, NpmVersionParseError> {
    npm::parse_npm_version(text)
  }
}

impl fmt::Display for Version {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;
    if !self.pre.is_empty() {
      write!(f, "-")?;
      for (i, part) in self.pre.iter().enumerate() {
        if i > 0 {
          write!(f, ".")?;
        }
        write!(f, "{part}")?;
      }
    }
    if !self.build.is_empty() {
      write!(f, "+")?;
      for (i, part) in self.build.iter().enumerate() {
        if i > 0 {
          write!(f, ".")?;
        }
        write!(f, "{part}")?;
      }
    }
    Ok(())
  }
}

impl std::cmp::PartialOrd for Version {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl std::cmp::Ord for Version {
  fn cmp(&self, other: &Self) -> Ordering {
    let cmp_result = self.major.cmp(&other.major);
    if cmp_result != Ordering::Equal {
      return cmp_result;
    }

    let cmp_result = self.minor.cmp(&other.minor);
    if cmp_result != Ordering::Equal {
      return cmp_result;
    }

    let cmp_result = self.patch.cmp(&other.patch);
    if cmp_result != Ordering::Equal {
      return cmp_result;
    }

    // only compare the pre-release and not the build as node-semver does
    if self.pre.is_empty() && other.pre.is_empty() {
      Ordering::Equal
    } else if !self.pre.is_empty() && other.pre.is_empty() {
      Ordering::Less
    } else if self.pre.is_empty() && !other.pre.is_empty() {
      Ordering::Greater
    } else {
      let mut i = 0;
      loop {
        let a = self.pre.get(i);
        let b = other.pre.get(i);
        if a.is_none() && b.is_none() {
          return Ordering::Equal;
        }

        // https://github.com/npm/node-semver/blob/4907647d169948a53156502867ed679268063a9f/internal/identifiers.js
        let a = match a {
          Some(a) => a,
          None => return Ordering::Less,
        };
        let b = match b {
          Some(b) => b,
          None => return Ordering::Greater,
        };

        // prefer numbers
        if let Ok(a_num) = a.parse::<u64>() {
          if let Ok(b_num) = b.parse::<u64>() {
            return a_num.cmp(&b_num);
          } else {
            return Ordering::Less;
          }
        } else if b.parse::<u64>().is_ok() {
          return Ordering::Greater;
        }

        let cmp_result = a.cmp(b);
        if cmp_result != Ordering::Equal {
          return cmp_result;
        }
        i += 1;
      }
    }
  }
}

pub(super) fn is_valid_tag(value: &str) -> bool {
  // we use the same rules as npm tags
  npm::is_valid_npm_tag(value)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RangeSetOrTag {
  RangeSet(VersionRangeSet),
  Tag(String),
}

/// A version requirement found in an npm package's dependencies.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VersionReq {
  raw_text: String,
  inner: RangeSetOrTag,
}

impl VersionReq {
  /// Creates a version requirement without examining the raw text.
  pub fn from_raw_text_and_inner(
    raw_text: String,
    inner: RangeSetOrTag,
  ) -> Self {
    Self { raw_text, inner }
  }

  pub fn parse_from_specifier(
    specifier: &str,
  ) -> Result<Self, NpmVersionReqSpecifierParseError> {
    parse_version_req_from_specifier(specifier)
  }

  pub fn parse_from_npm(text: &str) -> Result<Self, NpmVersionReqParseError> {
    parse_npm_version_req(text)
  }

  #[cfg(test)]
  pub(crate) fn inner(&self) -> &RangeSetOrTag {
    &self.inner
  }

  pub fn tag(&self) -> Option<&str> {
    match &self.inner {
      RangeSetOrTag::RangeSet(_) => None,
      RangeSetOrTag::Tag(tag) => Some(tag.as_str()),
    }
  }

  pub fn matches(&self, version: &Version) -> bool {
    match &self.inner {
      RangeSetOrTag::RangeSet(range_set) => range_set.satisfies(version),
      RangeSetOrTag::Tag(_) => panic!(
        "programming error: cannot use matches with a tag: {}",
        self.raw_text
      ),
    }
  }

  pub fn version_text(&self) -> &str {
    &self.raw_text
  }
}

impl fmt::Display for VersionReq {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", &self.raw_text)
  }
}

#[cfg(test)]
mod test {
  use crate::semver::Version;

  #[test]
  fn serialize_deserialize() {
    // should deserialize and serialize with loose parsing
    let text = "= v 1.2.3-pre.other+build.test";
    let version: Version =
      serde_json::from_str(&format!("\"{}\"", text)).unwrap();
    let serialized_version = serde_json::to_string(&version).unwrap();
    assert_eq!(serialized_version, "\"1.2.3-pre.other+build.test\"");
  }
}
