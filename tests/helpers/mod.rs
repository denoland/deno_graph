// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

mod test_builder;

use deno_graph::source::recommended_registry_package_url;
use deno_graph::source::recommended_registry_package_url_to_nv;
use deno_graph::source::LoaderChecksum;
use deno_graph::source::DEFAULT_JSR_URL;
use deno_graph::WorkspaceMember;
use deno_semver::package::PackageNv;
use indexmap::IndexMap;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;
pub use test_builder::*;
use url::Url;

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SpecOptions {
  #[serde(default)]
  #[serde(skip_serializing_if = "is_false")]
  pub workspace_fast_check: bool,
  #[serde(default)]
  #[serde(skip_serializing_if = "is_false")]
  pub fast_check_cache: bool,
}

fn is_false(v: &bool) -> bool {
  !v
}

pub struct Spec {
  pub options: Option<SpecOptions>,
  pub files: Vec<SpecFile>,
  pub output_file: SpecFile,
  pub diagnostics: Vec<serde_json::Value>,
  pub workspace_members: Vec<WorkspaceMember>,
}

impl Spec {
  pub fn emit(&self) -> String {
    let mut text = String::new();
    if let Some(options) = &self.options {
      text.push_str("~~ ");
      text.push_str(&serde_json::to_string(options).unwrap());
      text.push_str(" ~~");
      text.push('\n');
    }
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

  /// Fills the `manifest` field in the `_meta.json` files with the checksums
  /// so that we don't need to bother having them in the tests.
  pub fn fill_jsr_meta_files_with_checksums(&mut self) {
    for (nv, checksums_by_files) in self.get_jsr_checksums() {
      let base_specifier =
        recommended_registry_package_url(&DEFAULT_JSR_URL, &nv);
      let meta_file = base_specifier
        .join(&format!("../{}_meta.json", nv.version))
        .unwrap();

      let meta_file = self
        .files
        .iter_mut()
        .find(|f| f.url() == meta_file)
        .unwrap_or_else(|| panic!("Could not find in specs: {}", meta_file));
      let mut meta_value = serde_json::from_str::<
        HashMap<String, serde_json::Value>,
      >(&meta_file.text)
      .unwrap();
      let manifest = meta_value
        .entry("manifest".to_string())
        .or_insert_with(|| serde_json::Value::Object(Default::default()))
        .as_object_mut()
        .unwrap();
      for (file, checksum) in checksums_by_files {
        if !manifest.contains_key(&file) {
          manifest.insert(file, checksum);
        }
      }
      // use the original text as the emit text so we don't
      // end up with these hashes in the output
      meta_file.emit_text = Some(std::mem::take(&mut meta_file.text));
      meta_file.text = serde_json::to_string_pretty(&meta_value).unwrap();
    }
  }

  pub fn get_jsr_checksums(
    &self,
  ) -> HashMap<PackageNv, HashMap<String, serde_json::Value>> {
    let mut checksums_by_package: HashMap<
      PackageNv,
      HashMap<String, serde_json::Value>,
    > = Default::default();
    for file in &self.files {
      if let Some(nv) =
        recommended_registry_package_url_to_nv(&DEFAULT_JSR_URL, &file.url())
      {
        let base_specifier =
          recommended_registry_package_url(&DEFAULT_JSR_URL, &nv);
        let relative_url = file
          .url()
          .to_string()
          .strip_prefix(base_specifier.to_string().strip_suffix('/').unwrap())
          .unwrap()
          .to_string();
        checksums_by_package.entry(nv.clone()).or_default().insert(
          relative_url,
          serde_json::json!({
            "size": file.text.len(),
            "checksum": format!("sha256-{}", LoaderChecksum::gen(file.text.as_bytes())),
          }),
        );
      }
    }
    checksums_by_package
  }
}

#[derive(Debug)]
pub struct SpecFile {
  pub specifier: String,
  pub text: String,
  /// Text to use when emitting the spec file.
  pub emit_text: Option<String>,
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
    text.push_str(self.emit_text.as_ref().unwrap_or(&self.text));
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
    .map(|file| {
      let mut spec = parse_spec(file.text);
      // always do this as we want this for the spec tests
      spec.fill_jsr_meta_files_with_checksums();
      (file.path, spec)
    })
    .collect()
}

fn parse_spec(text: String) -> Spec {
  let mut files = Vec::new();
  let mut current_file = None;
  let mut options: Option<SpecOptions> = None;
  for (i, line) in text.split('\n').enumerate() {
    if i == 0 && line.starts_with("~~ ") {
      let line = line.replace("~~", "").trim().to_string(); // not ideal, being lazy
      options = Some(serde_json::from_str(&line).unwrap());
      continue;
    }
    if let Some(specifier) = line.strip_prefix("# ") {
      if let Some(file) = current_file.take() {
        files.push(file);
      }
      current_file = Some(SpecFile {
        specifier: specifier.to_string(),
        text: String::new(),
        emit_text: None,
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
    options,
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
