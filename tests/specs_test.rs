// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::BTreeMap;
use std::panic::AssertUnwindSafe;
use std::sync::Arc;

use std::collections::HashMap;

use deno_ast::emit;
use deno_ast::EmitOptions;
use deno_ast::EmittedSource;
use deno_ast::SourceMap;
use deno_graph::source::recommended_registry_package_url;
use deno_graph::source::recommended_registry_package_url_to_nv;
use deno_graph::source::LoaderChecksum;
use deno_graph::source::Source;
use deno_graph::source::DEFAULT_JSR_URL;
use deno_graph::FastCheckCacheModuleItem;
use deno_graph::WorkspaceMember;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use file_test_runner::collect_and_run_tests;
use file_test_runner::collection::strategies::TestPerFileCollectionStrategy;
use file_test_runner::collection::CollectOptions;
use file_test_runner::collection::CollectedTest;
use file_test_runner::RunOptions;
use file_test_runner::TestResult;
use helpers::TestLoader;
use indexmap::IndexMap;
use pretty_assertions::assert_eq;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;
use url::Url;

use crate::helpers::TestBuilder;

mod helpers;

fn main() {
  collect_and_run_tests(
    CollectOptions {
      base: "tests/specs".into(),
      strategy: Box::new(TestPerFileCollectionStrategy {
        file_pattern: Some(
          "^*.[/\\\\]specs[/\\\\](:?graph|symbols)[/\\\\].*$".to_owned(),
        ),
      }),
      filter_override: None,
    },
    RunOptions { parallel: true },
    Arc::new(|test| {
      if test.name.starts_with("specs::graph") {
        TestResult::from_maybe_panic(AssertUnwindSafe(|| {
          run_graph_test(test);
        }))
      } else if test.name.starts_with("specs::symbols") {
        TestResult::from_maybe_panic(AssertUnwindSafe(|| {
          run_symbol_test(test);
        }))
      } else {
        TestResult::Failed {
          output: format!("Unknown test kind: {}", test.name).into_bytes(),
        }
      }
    }),
  )
}

fn run_graph_test(test: &CollectedTest) {
  let file_text = test.read_to_string().unwrap();
  let mut spec = parse_spec(file_text);
  spec.fill_jsr_meta_files_with_checksums();

  let mut builder = TestBuilder::new();
  builder.with_loader(|loader| {
    add_spec_files_to_loader(&spec.files, loader);
  });
  builder.workspace_members(spec.workspace_members.clone());
  builder.lockfile_jsr_packages(spec.lockfile_jsr_packages.clone());

  if let Some(options) = &spec.options {
    builder.workspace_fast_check(options.workspace_fast_check);
    builder.fast_check_cache(options.fast_check_cache);
  }

  let rt = tokio::runtime::Builder::new_current_thread()
    .enable_all()
    .build()
    .unwrap();
  let result = rt.block_on(async { builder.build().await });
  let mut output_text = serde_json::to_string_pretty(&result.graph).unwrap();
  output_text.push('\n');
  // include the list of jsr dependencies
  let jsr_deps = result
    .graph
    .packages
    .packages_with_checksum_and_deps()
    .map(|(k, _checksum, deps)| {
      (k.to_string(), {
        let mut deps = deps.map(|d| d.to_string()).collect::<Vec<_>>();
        deps.sort();
        deps
      })
    })
    .filter(|(_, v)| !v.is_empty())
    .collect::<BTreeMap<_, _>>();
  if !jsr_deps.is_empty() {
    output_text.push_str("\njsr deps: ");
    output_text.push_str(&format!("{:#?}", jsr_deps));
    output_text.push('\n');
  }
  // now the fast check modules
  let fast_check_modules = result.graph.modules().filter_map(|module| {
    let module = module.js()?;
    let fast_check = module.fast_check.as_ref()?;
    Some((module, fast_check))
  });
  for (module, fast_check) in fast_check_modules {
    output_text.push_str(&format!("\nFast check {}:\n", module.specifier));
    match fast_check {
      deno_graph::FastCheckTypeModuleSlot::Module(fast_check) => {
        output_text.push_str(&format!(
          "{}\n{}",
          indent(
            &serde_json::to_string_pretty(&fast_check.dependencies).unwrap()
          ),
          if fast_check.source.is_empty() {
            "  <empty>".to_string()
          } else {
            indent(&fast_check.source)
          },
        ));

        if let Some(dts) = &fast_check.dts {
          let source_map = SourceMap::single(
            module.specifier.clone(),
            module.source.to_string(),
          );
          let EmittedSource { text, .. } = emit(
            &dts.program,
            &dts.comments.as_single_threaded(),
            &source_map,
            &EmitOptions {
              keep_comments: true,
              source_map: deno_ast::SourceMapOption::None,
              ..Default::default()
            },
          )
          .unwrap();
          if !text.is_empty() {
            output_text.push_str(&indent("--- DTS ---\n"));
            output_text.push_str(&indent(&text));
          }
          if !dts.diagnostics.is_empty() {
            output_text.push_str(&indent("--- DTS Diagnostics ---\n"));
            let message = dts
              .diagnostics
              .iter()
              .map(|d| match d.range() {
                Some(range) => {
                  format!(
                    "{}\n    at {}@{}",
                    d, range.specifier, range.range.start
                  )
                }
                None => format!("{}\n    at {}", d, d.specifier()),
              })
              .collect::<Vec<_>>()
              .join("\n");
            output_text.push_str(&indent(&message));
          }
        }
      }
      deno_graph::FastCheckTypeModuleSlot::Error(diagnostics) => {
        let message = diagnostics
          .iter()
          .map(|d| match d.range() {
            Some(range) => {
              format!("{}\n    at {}@{}", d, range.specifier, range.range.start)
            }
            None => format!("{}\n    at {}", d, d.specifier()),
          })
          .collect::<Vec<_>>()
          .join("\n");
        output_text.push_str(&indent(&message));
      }
    }
  }
  if let Some(fast_check_cache) = result.fast_check_cache.as_ref() {
    output_text.push_str("\n== fast check cache ==\n");
    for (key, item) in fast_check_cache.inner.borrow().iter() {
      output_text.push_str(&format!(
        "{:?}:\n    Deps - {}\n    Modules: {}\n",
        key,
        serde_json::to_string(&item.dependencies).unwrap(),
        serde_json::to_string(
          &item
            .modules
            .iter()
            .map(|(url, module_item)| (
              url.as_str(),
              match module_item {
                FastCheckCacheModuleItem::Info(_) => "info",
                FastCheckCacheModuleItem::Diagnostic(_) => "diagnostic",
              }
            ))
            .collect::<Vec<_>>()
        )
        .unwrap()
      ));
    }
  }
  if !output_text.ends_with('\n') {
    output_text.push('\n');
  }
  let diagnostics = result
    .diagnostics
    .iter()
    .map(|d| serde_json::to_value(d.to_string()).unwrap())
    .collect::<Vec<_>>();
  let update = std::env::var("UPDATE").as_deref() == Ok("1");
  let spec = if update {
    let mut spec = spec;
    spec.output_file.text = output_text.clone();
    spec.diagnostics = diagnostics.clone();
    std::fs::write(&test.path, spec.emit()).unwrap();
    spec
  } else {
    spec
  };
  assert_eq!(
    output_text,
    spec.output_file.text,
    "Should be same for {}",
    test.path.display()
  );
  assert_eq!(
    diagnostics,
    spec.diagnostics,
    "Should be same for {}",
    test.path.display()
  );
}

fn run_symbol_test(test: &CollectedTest) {
  let file_text = test.read_to_string().unwrap();
  let mut spec = parse_spec(file_text);
  spec.fill_jsr_meta_files_with_checksums();
  let mut builder = TestBuilder::new();

  if spec.files.iter().any(|f| f.specifier == "mod.js") {
    // this is for the TypesEntrypoint test
    builder.entry_point("file:///mod.js");
    builder.entry_point_types("file:///mod.d.ts");
  }

  if let Some(options) = &spec.options {
    builder.workspace_fast_check(options.workspace_fast_check);
  }

  builder.with_loader(|loader| {
    add_spec_files_to_loader(&spec.files, loader);
  });

  let rt = tokio::runtime::Builder::new_current_thread()
    .enable_all()
    .build()
    .unwrap();
  let result = rt.block_on(async { builder.symbols().await.unwrap() });
  let spec = if std::env::var("UPDATE").as_deref() == Ok("1") {
    let mut spec = spec;
    spec.output_file.text = result.output.clone();
    std::fs::write(&test.path, spec.emit()).unwrap();
    spec
  } else {
    spec
  };
  assert_eq!(
    result.output,
    spec.output_file.text,
    "Should be same for {}",
    test.path.display()
  );
}

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
  pub lockfile_jsr_packages: BTreeMap<PackageReq, PackageNv>,
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
    if !self.lockfile_jsr_packages.is_empty() {
      text.push_str("\n# lockfile_jsr_packages\n");
      text.push_str(
        &serde_json::to_string_pretty(&self.lockfile_jsr_packages).unwrap(),
      );
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

fn add_spec_files_to_loader(
  files: &[crate::SpecFile],
  loader: &mut TestLoader,
) {
  for file in files {
    let source = match file.headers.get("location") {
      Some(location) => {
        let location = if location.starts_with("./") {
          file.url().join(location).unwrap().to_string()
        } else {
          location.to_string()
        };
        Source::Redirect(location)
      }
      None => Source::Module {
        specifier: file.url().to_string(),
        maybe_headers: Some(file.headers.clone().into_iter().collect()),
        content: file.text.clone(),
      },
    };
    if file.is_cache() {
      loader.cache.add_source(file.url(), source);
    } else {
      loader.remote.add_source(file.url(), source);
    }
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

pub fn parse_spec(text: String) -> Spec {
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
  let diagnostics = take_file(&mut files, "diagnostics");
  let workspace_members = take_file(&mut files, "workspace_members");
  let lockfile_jsr_packages = take_file(&mut files, "lockfile_jsr_packages");
  Spec {
    options,
    files,
    output_file,
    diagnostics,
    workspace_members,
    lockfile_jsr_packages,
  }
}

fn take_file<T: Default + DeserializeOwned>(
  files: &mut Vec<SpecFile>,
  name: &str,
) -> T {
  if let Some(index) = files.iter().position(|f| f.specifier == name) {
    let file = files.remove(index);
    serde_json::from_str(&file.text).unwrap()
  } else {
    Default::default()
  }
}

fn indent(text: &str) -> String {
  text
    .split('\n')
    .map(|l| format!("  {}", l).trim_end().to_string())
    .collect::<Vec<_>>()
    .join("\n")
}
