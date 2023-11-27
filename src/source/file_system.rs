// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;

use indexmap::IndexSet;

use crate::ModuleSpecifier;

#[derive(Debug)]
pub enum DirEntryKind {
  File,
  Dir,
  Symlink,
  Error(anyhow::Error),
}

#[derive(Debug)]
pub struct DirEntry {
  pub kind: DirEntryKind,
  pub url: ModuleSpecifier,
}

pub trait FileSystem {
  fn read_dir(&self, dir_path: &ModuleSpecifier) -> Vec<DirEntry>;
}

#[derive(Debug, Clone, Default)]
pub struct NullFileSystem;

impl FileSystem for NullFileSystem {
  fn read_dir(&self, _dir_path: &ModuleSpecifier) -> Vec<DirEntry> {
    vec![]
  }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum InMemoryDirEntry {
  Dir(ModuleSpecifier),
  File(ModuleSpecifier),
}

#[derive(Debug, Clone, Default)]
pub struct MemoryFileSystem {
  directories: HashMap<ModuleSpecifier, IndexSet<InMemoryDirEntry>>,
}

impl MemoryFileSystem {
  pub fn add_dir(&mut self, dir_url: ModuleSpecifier) {
    assert_eq!(dir_url.scheme(), "file");
    self.directories.entry(dir_url.clone()).or_default();
    let mut dir = dir_url;
    while dir.path().len() > 1 {
      let parent_dir = dir.join("..").unwrap();
      self
        .directories
        .entry(parent_dir.clone())
        .or_default()
        .insert(InMemoryDirEntry::Dir(dir.clone()));
      dir = parent_dir;
    }
  }

  pub fn add_file(&mut self, file_url: ModuleSpecifier) {
    assert_eq!(file_url.scheme(), "file");
    let dir = file_url.join(".").unwrap();
    self.add_dir(dir.clone());
    self
      .directories
      .entry(dir)
      .or_default()
      .insert(InMemoryDirEntry::File(file_url));
  }
}

impl FileSystem for MemoryFileSystem {
  fn read_dir(&self, dir_url: &ModuleSpecifier) -> Vec<DirEntry> {
    assert_eq!(dir_url.scheme(), "file");
    let Some(entries) = self.directories.get(dir_url) else {
      return Vec::new();
    };
    let mut result = Vec::with_capacity(entries.len());
    for entry in entries {
      result.push(match entry {
        InMemoryDirEntry::Dir(url) => DirEntry {
          kind: DirEntryKind::Dir,
          url: url.clone(),
        },
        InMemoryDirEntry::File(url) => DirEntry {
          kind: DirEntryKind::File,
          url: url.clone(),
        },
      });
    }
    result
  }
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Clone, Default)]
pub struct RealFileSystem;

#[cfg(not(target_arch = "wasm32"))]
impl FileSystem for RealFileSystem {
  fn read_dir(&self, dir_url: &ModuleSpecifier) -> Vec<DirEntry> {
    let dir_path = match dir_url.to_file_path() {
      Ok(path) => path,
      Err(()) => {
        return vec![DirEntry {
          kind: DirEntryKind::Error(anyhow::anyhow!(
            "Failed converting url to path."
          )),
          url: dir_url.clone(),
        }]
      }
    };
    let dir = match std::fs::read_dir(&dir_path) {
      Ok(dir) => dir,
      Err(err)
        if matches!(
          err.kind(),
          std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::NotFound
        ) =>
      {
        return vec![];
      }
      Err(err) => {
        return vec![DirEntry {
          kind: DirEntryKind::Error(
            anyhow::Error::from(err)
              .context("Failed to read directory.".to_string()),
          ),
          url: dir_url.clone(),
        }];
      }
    };
    let mut dir_entries = vec![];
    for entry in dir {
      let entry = match entry {
        Ok(entry) => {
          match entry.file_type() {
            Ok(file_type) if file_type.is_file() => DirEntry {
              kind: DirEntryKind::File,
              url: ModuleSpecifier::from_file_path(entry.path()).unwrap(),
            },
            Ok(file_type) if file_type.is_dir() => DirEntry {
              kind: DirEntryKind::Dir,
              url: ModuleSpecifier::from_directory_path(entry.path()).unwrap(),
            },
            Ok(file_type) if file_type.is_symlink() => DirEntry {
              kind: DirEntryKind::Symlink,
              url: ModuleSpecifier::from_file_path(entry.path()).unwrap(),
            },
            Ok(_) => continue, // unknown
            Err(err) if err.kind() == std::io::ErrorKind::PermissionDenied => {
              continue;
            }
            Err(err) => DirEntry {
              kind: DirEntryKind::Error(
                anyhow::Error::from(err).context("Failed to read file type."),
              ),
              url: ModuleSpecifier::from_file_path(entry.path()).unwrap(),
            },
          }
        }
        Err(err)
          if matches!(
            err.kind(),
            std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::NotFound
          ) =>
        {
          continue;
        }
        Err(err) => DirEntry {
          url: dir_url.clone(),
          kind: DirEntryKind::Error(
            anyhow::Error::from(err)
              .context("Failed to read entry in directory.".to_string()),
          ),
        },
      };

      dir_entries.push(entry);
    }

    dir_entries
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn real_file_system() {
    let temp_dir = tempfile::tempdir().unwrap();
    let dir_path = temp_dir.path();
    std::fs::create_dir_all(dir_path.join("foo/bar")).unwrap();
    std::fs::write(dir_path.join("foo/testing.ts"), "").unwrap();
    let file_system = RealFileSystem;
    let dir_uri = ModuleSpecifier::from_directory_path(dir_path).unwrap();
    // read dir with only dir
    {
      let entries = file_system.read_dir(&dir_uri);
      assert_eq!(entries.len(), 1);
      assert_eq!(entries[0].url, dir_uri.join("foo/").unwrap());
      assert!(matches!(entries[0].kind, DirEntryKind::Dir));
    }
    // read dir with dir and file
    {
      let dir_uri = dir_uri.join("foo/").unwrap();
      let mut entries = file_system.read_dir(&dir_uri);
      entries.sort_by(|a, b| a.url.cmp(&b.url));
      assert_eq!(entries.len(), 2);
      assert_eq!(entries[0].url, dir_uri.join("bar/").unwrap());
      assert!(matches!(entries[0].kind, DirEntryKind::Dir));
      assert_eq!(entries[1].url, dir_uri.join("testing.ts").unwrap());
      assert!(matches!(entries[1].kind, DirEntryKind::File));
    }
  }
}
