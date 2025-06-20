// Copyright 2018-2024 the Deno authors. MIT license.

#![allow(clippy::disallowed_methods)]

use std::collections::BTreeMap;
use std::panic::AssertUnwindSafe;

use std::collections::HashMap;
use std::fmt::Write;

use deno_ast::diagnostics::Diagnostic;
use deno_ast::emit;
use deno_ast::EmitOptions;
use deno_ast::EmittedSourceText;
use deno_ast::SourceMap;
use deno_graph::fast_check::FastCheckCacheModuleItem;
use deno_graph::source::recommended_registry_package_url;
use deno_graph::source::recommended_registry_package_url_to_nv;
use deno_graph::source::LoaderChecksum;
use deno_graph::source::Source;
use deno_graph::source::DEFAULT_JSR_URL;
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
  // set log level with RUST_LOG env var (ex. `RUST_LOG=trace`)
  env_logger::builder()
    .filter(Some("tracing::span"), log::LevelFilter::Off)
    .filter(Some("swc_ecma_codegen"), log::LevelFilter::Off)
    .init();
  // Disable colors so that deno_ast diagnostics do not contain escape sequences.
  std::env::set_var("NO_COLOR", "true");

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
    |test| {
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
    },
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
    if let Some(entrypoint) = &options.entrypoint {
      builder.entry_point(entrypoint);
    }
    builder.skip_dynamic_deps(options.skip_dynamic_deps);
    builder
      .unstable_bytes_and_text_imports(options.unstable_bytes_and_text_imports);
    builder.workspace_fast_check(options.workspace_fast_check);
    builder.fast_check_cache(options.fast_check_cache);
    if let Some(checksums) = options.remote_checksums.as_ref() {
      builder.ensure_locker();
      for (specifier, checksum) in checksums {
        builder.add_remote_checksum(specifier, checksum);
      }
    }
    if let Some(checksums) = options.pkg_checksums.as_ref() {
      builder.ensure_locker();
      for (pkg_nv, checksum) in checksums {
        builder.add_pkg_manifest_checksum(pkg_nv, checksum);
      }
    }
  }

  let rt = tokio::runtime::Builder::new_current_thread()
    .enable_all()
    .build()
    .unwrap();
  let result = rt.block_on(async { builder.build().await });
  let mut output_text = serde_json::to_string_pretty(&result.graph).unwrap();
  output_text.push('\n');
  // include the checksums if non-empty
  if let Some(locker) = &result.locker {
    {
      let sorted_checksums = locker.remote().iter().collect::<BTreeMap<_, _>>();
      if !sorted_checksums.is_empty() {
        output_text.push_str("\nremote checksums:\n");
        output_text
          .push_str(&serde_json::to_string_pretty(&sorted_checksums).unwrap());
        output_text.push('\n');
      }
    }
    {
      let sorted_checksums =
        locker.pkg_manifests().iter().collect::<BTreeMap<_, _>>();
      if !sorted_checksums.is_empty() {
        output_text.push_str("\npkg manifest checksums:\n");
        output_text
          .push_str(&serde_json::to_string_pretty(&sorted_checksums).unwrap());
        output_text.push('\n');
      }
    }
  }
  // include the list of jsr dependencies
  let jsr_deps = result
    .graph
    .packages
    .packages_with_deps()
    .map(|(k, deps)| {
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
            module.source.text.to_string(),
          );
          let EmittedSourceText { text, .. } = emit(
            (&dts.program).into(),
            &dts.comments.as_single_threaded(),
            &source_map,
            &EmitOptions {
              remove_comments: false,
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
              .map(|d| {
                let range = d.range();

                format!(
                  "{}\n    at {}@{}",
                  d, range.specifier, range.range.start
                )
              })
              .collect::<Vec<_>>()
              .join("\n\n");
            output_text.push_str(&indent(&message));
          }
        }
      }
      deno_graph::FastCheckTypeModuleSlot::Error(diagnostics) => {
        let mut printed_diagnostics = "".to_owned();
        for diagnostic in diagnostics {
          write!(&mut printed_diagnostics, "{}", diagnostic.display()).unwrap();
        }
        output_text.push_str(&indent(&printed_diagnostics));
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
  let update = std::env::var("UPDATE").as_deref() == Ok("1");
  let spec = if update {
    let mut spec = spec;
    spec.output_file.text.clone_from(&output_text);
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
    if let Some(entrypoint) = &options.entrypoint {
      builder.entry_point(entrypoint);
    }
    builder.workspace_fast_check(options.workspace_fast_check);
  }

  builder.with_loader(|loader| {
    add_spec_files_to_loader(&spec.files, loader);
  });

  let rt = tokio::runtime::Builder::new_current_thread()
    .enable_all()
    .build()
    .unwrap();
  let result = rt.block_on(async { builder.symbols().await });
  let spec = if std::env::var("UPDATE").as_deref() == Ok("1") {
    let mut spec = spec;
    spec.output_file.text.clone_from(&result.output);
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

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SpecOptions {
  #[serde(default)]
  #[serde(skip_serializing_if = "Option::is_none")]
  pub entrypoint: Option<String>,
  #[serde(default)]
  #[serde(skip_serializing_if = "Option::is_none")]
  pub remote_checksums: Option<HashMap<String, String>>,
  #[serde(default)]
  #[serde(skip_serializing_if = "Option::is_none")]
  pub pkg_checksums: Option<HashMap<String, String>>,
  #[serde(default)]
  #[serde(skip_serializing_if = "is_false")]
  pub workspace_fast_check: bool,
  #[serde(default)]
  #[serde(skip_serializing_if = "is_false")]
  pub fast_check_cache: bool,
  #[serde(default)]
  #[serde(skip_serializing_if = "is_false")]
  pub skip_dynamic_deps: bool,
  #[serde(skip_serializing_if = "is_false")]
  pub unstable_bytes_and_text_imports: bool,
}

fn is_false(v: &bool) -> bool {
  !v
}

pub struct Spec {
  pub options: Option<SpecOptions>,
  pub files: Vec<SpecFile>,
  pub output_file: SpecFile,
  pub workspace_members: Vec<WorkspaceMember>,
  pub lockfile_jsr_packages: BTreeMap<PackageReq, PackageNv>,
}

impl Spec {
  pub fn emit(&self) -> String {
    let mut text = String::new();
    if let Some(options) = &self.options {
      text.push_str("~~ ");
      if options.remote_checksums.is_some() || options.pkg_checksums.is_some() {
        text.push_str(&serde_json::to_string_pretty(options).unwrap());
      } else {
        text.push_str(&serde_json::to_string(options).unwrap());
      }
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
        BTreeMap<String, serde_json::Value>,
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
  ) -> BTreeMap<PackageNv, BTreeMap<String, serde_json::Value>> {
    let mut checksums_by_package: BTreeMap<
      PackageNv,
      BTreeMap<String, serde_json::Value>,
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
    if !specifier.starts_with("http:")
      && !specifier.starts_with("https:")
      && !specifier.starts_with("file:")
    {
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
  let mut text = text.as_str();
  if text.starts_with("~~ ") {
    let end = text.find(" ~~\n").unwrap();
    options = Some(serde_json::from_str(&text[3..end]).unwrap());
    text = &text[end + 4..];
  }
  for line in text.split('\n') {
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
  let workspace_members = take_file(&mut files, "workspace_members");
  let lockfile_jsr_packages = take_file(&mut files, "lockfile_jsr_packages");
  Spec {
    options,
    files,
    output_file,
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
