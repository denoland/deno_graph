use std::path::Path;
use std::path::PathBuf;

use pretty_assertions::assert_eq;

use super::test_builder::TestBuilder;
use deno_graph::type_tracer::TypeTraceDiagnostic;

#[tokio::test]
async fn test_type_tracing_specs() {
  for (test_file_path, spec) in
    get_specs_in_dir(&PathBuf::from("./tests/specs/type_tracing"))
  {
    eprintln!("Running {}", test_file_path.display());
    let mut builder = TestBuilder::new();
    builder.with_loader(|loader| {
      for file in &spec.files {
        loader.add_file(&file.specifier, &file.text);
      }
    });

    let result = builder.trace().await.unwrap();
    let update_var = std::env::var("UPDATE");
    let spec = if update_var.as_ref().map(|v| v.as_str()) == Ok("1") {
      let mut spec = spec;
      spec.output_file.text = result.output.clone();
      spec.diagnostics = result.diagnostics.clone();
      std::fs::write(&test_file_path, spec.emit()).unwrap();
      spec
    } else {
      spec
    };
    assert_eq!(
      result.output,
      spec.output_file.text,
      "Should be same for {}",
      test_file_path.display()
    );
    assert_eq!(
      result.diagnostics,
      spec.diagnostics,
      "Should be same for {}",
      test_file_path.display()
    );
  }
}

struct Spec {
  files: Vec<File>,
  output_file: File,
  diagnostics: Vec<TypeTraceDiagnostic>,
}

impl Spec {
  pub fn emit(&self) -> String {
    let mut text = String::new();
    for file in &self.files {
      text.push_str(&file.emit());
      text.push('\n');
    }
    text.push_str(&self.output_file.emit());
    if !self.diagnostics.is_empty() {
      text.push_str("\n# diagnostics\n");
      text.push_str(&serde_json::to_string_pretty(&self.diagnostics).unwrap());
      text.push('\n');
    }
    text
  }
}

struct File {
  specifier: String,
  text: String,
}

impl File {
  pub fn emit(&self) -> String {
    format!("# {}\n{}", self.specifier, self.text)
  }
}

fn get_specs_in_dir(path: &Path) -> Vec<(PathBuf, Spec)> {
  let files = get_files_in_dir_recursive(path);
  let files = if files
    .iter()
    .any(|(s, _)| s.to_string_lossy().to_lowercase().contains("_only"))
  {
    files
      .into_iter()
      .filter(|(s, _)| s.to_string_lossy().to_lowercase().contains("_only"))
      .collect()
  } else {
    files
  };
  files
    .into_iter()
    .map(|(file_path, text)| (file_path, parse_spec(text)))
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
      current_file = Some(File {
        specifier: specifier.to_string(),
        text: String::new(),
      });
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
  let diagnostics = if let Some(index) =
    files.iter().position(|f| f.specifier == "diagnostics")
  {
    let diagnostic_file = files.remove(index);
    serde_json::from_str(&diagnostic_file.text).unwrap()
  } else {
    Vec::new()
  };
  Spec {
    files,
    output_file,
    diagnostics,
  }
}

fn get_files_in_dir_recursive(path: &Path) -> Vec<(PathBuf, String)> {
  let mut result = Vec::new();

  for entry in path.read_dir().unwrap().flatten() {
    let entry_path = entry.path();
    if entry_path.is_file() {
      let text = std::fs::read_to_string(&entry_path).unwrap();
      result.push((entry_path, text));
    } else {
      result.extend(get_files_in_dir_recursive(&entry_path));
    }
  }

  result
}
