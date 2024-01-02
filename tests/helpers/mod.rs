// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::path::Path;
use std::path::PathBuf;

mod test_builder;

use deno_graph::WorkspaceMember;
use indexmap::IndexMap;
use serde::de::DeserializeOwned;
pub use test_builder::*;
use url::Url;

pub struct Spec {
  pub files: Vec<SpecFile>,
  pub output_file: SpecFile,
  pub diagnostics: Vec<serde_json::Value>,
  pub workspace_members: Vec<WorkspaceMember>,
}

impl Spec {
  pub fn emit(&self) -> String {
    let mut text = String::new();
    if !self.workspace_members.is_empty() {
      text.push_str("# workspace_members\n");
      text.push_str(
        &serde_json::to_string_pretty(&self.workspace_members).unwrap(),
      );
      text.push_str("\n\n");
    }
    for file in &self.files {
      text.push_str(&file.emit());
      text.push('\n');
    }
    text.push_str(&self.output_file.emit());
    if !text.ends_with('\n') {
      text.push('\n');
    }
    if !self.diagnostics.is_empty() {
      text.push_str("\n# diagnostics\n");
      text.push_str(&serde_json::to_string_pretty(&self.diagnostics).unwrap());
      text.push('\n');
    }
    text
  }
}

#[derive(Debug)]
pub struct SpecFile {
  pub specifier: String,
  pub text: String,
  pub headers: IndexMap<String, String>,
}

impl SpecFile {
  pub fn emit(&self) -> String {
    let mut text = format!("# {}\n", self.specifier);
    if !self.headers.is_empty() {
      text.push_str(&format!(
        "HEADERS: {}\n",
        serde_json::to_string(&self.headers).unwrap()
      ));
    }
    text.push_str(&self.text);
    text
  }

  pub fn url(&self) -> Url {
    let specifier = self
      .specifier
      .strip_prefix("cache:")
      .unwrap_or(&self.specifier);
    if !specifier.starts_with("http") && !specifier.starts_with("file") {
      Url::parse(&format!("file:///{}", specifier)).unwrap()
    } else {
      Url::parse(specifier).unwrap()
    }
  }

  pub fn is_cache(&self) -> bool {
    self.specifier.starts_with("cache:")
  }
}

pub fn get_specs_in_dir(path: &Path) -> Vec<(PathBuf, Spec)> {
  let files = collect_files_in_dir_recursive(path);
  let files: Vec<_> = if files
    .iter()
    .any(|file| file.path.to_string_lossy().to_lowercase().contains("_only"))
  {
    files
      .into_iter()
      .filter(|file| {
        file.path.to_string_lossy().to_lowercase().contains("_only")
      })
      .collect()
  } else {
    files
      .into_iter()
      .filter(|file| {
        !file.path.to_string_lossy().to_lowercase().contains("_skip")
      })
      .collect()
  };
  files
    .into_iter()
    .map(|file| (file.path, parse_spec(file.text)))
    .collect()
}

fn parse_spec(text: String) -> Spec {
  let mut files = Vec::new();
  let mut current_file = None;
  for line in text.split('\n') {
    if let Some(specifier) = line.strip_prefix("# ") {
      if let Some(file) = current_file.take() {
        files.push(file);
      }
      current_file = Some(SpecFile {
        specifier: specifier.to_string(),
        text: String::new(),
        headers: Default::default(),
      });
    } else if let Some(headers) = line.strip_prefix("HEADERS: ") {
      current_file.as_mut().unwrap().headers =
        serde_json::from_str(headers).unwrap();
    } else {
      let current_file = current_file.as_mut().unwrap();
      if !current_file.text.is_empty() {
        current_file.text.push('\n');
      }
      current_file.text.push_str(line);
    }
  }
  files.push(current_file.unwrap());
  let output_file =
    files.remove(files.iter().position(|f| f.specifier == "output").unwrap());
  let diagnostics = take_vec_file(&mut files, "diagnostics");
  let workspace_members = take_vec_file(&mut files, "workspace_members");
  Spec {
    files,
    output_file,
    diagnostics,
    workspace_members,
  }
}

fn take_vec_file<T: DeserializeOwned>(
  files: &mut Vec<SpecFile>,
  name: &str,
) -> Vec<T> {
  if let Some(index) = files.iter().position(|f| f.specifier == name) {
    let file = files.remove(index);
    serde_json::from_str(&file.text).unwrap()
  } else {
    Vec::new()
  }
}

struct CollectedFile {
  pub path: PathBuf,
  pub text: String,
}

fn collect_files_in_dir_recursive(path: &Path) -> Vec<CollectedFile> {
  let mut result = Vec::new();

  for entry in path.read_dir().unwrap().flatten() {
    let entry_path = entry.path();
    if entry_path.is_file() {
      let text = std::fs::read_to_string(&entry_path).unwrap();
      result.push(CollectedFile {
        path: entry_path,
        text,
      });
    } else {
      result.extend(collect_files_in_dir_recursive(&entry_path));
    }
  }

  result
}
