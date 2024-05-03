use std::io::Write as _;
use std::path::PathBuf;
use std::sync::Arc;

use deno_ast::diagnostics::Diagnostic;
use deno_graph::source::LoadResponse;
use deno_graph::source::NullFileSystem;
use deno_graph::BuildFastCheckTypeGraphOptions;
use deno_graph::BuildOptions;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::WorkspaceFastCheckOption;
use deno_graph::WorkspaceMember;
use deno_semver::package::PackageNv;
use file_test_runner::collection::strategies::TestPerFileCollectionStrategy;
use file_test_runner::collection::CollectedCategoryOrTest;
use file_test_runner::collection::CollectedTest;
use file_test_runner::RunOptions;
use file_test_runner::TestResult;
use futures::FutureExt;
use indexmap::IndexMap;
use serde::Deserialize;
use std::fmt::Write;
use tempfile::tempdir;
use url::Url;

#[derive(Debug, Clone, Deserialize)]
struct Version {
  scope: String,
  name: String,
  version: String,
}

fn main() {
  if std::fs::metadata("./tests/ecosystem/jsr_mirror").is_err() {
    println!("skipping, ecosystem mirror not found. run `deno run -A ./tests/ecosystem/jsr_mirror.ts` to populate");
    return;
  }

  std::env::set_var("NO_COLOR", "1");

  let versions_str = include_str!("./ecosystem/jsr_versions.json");
  let versions: Vec<Version> = serde_json::from_str(versions_str).unwrap();

  if std::env::var("UPDATE").is_ok() {
    for version in versions {
      let path = PathBuf::from(format!(
        "./tests/specs/ecosystem/{}/{}/{}.test",
        version.scope.replace('-', "_"),
        version.name.replace('-', "_"),
        version.version.replace(['.', '-', '+'], "_")
      ));
      std::fs::create_dir_all(path.parent().unwrap()).ok();

      if let Ok(mut file) = std::fs::OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(path)
      {
        file
          .write_all(
            format!(
              "{}/{}/{}\n===\n\n",
              version.scope, version.name, version.version
            )
            .as_bytes(),
          )
          .unwrap();
      }
    }
  }

  let mut category = file_test_runner::collection::collect_tests_or_exit(
    file_test_runner::collection::CollectOptions {
      base: PathBuf::from("./tests/specs/ecosystem"),
      strategy: Box::new(TestPerFileCollectionStrategy { file_pattern: None }),
      filter_override: None,
    },
  );

  let shard_index: Option<u8> = std::env::var("SHARD_INDEX")
    .ok()
    .map(|s| s.parse().unwrap());
  let shard_count: Option<u8> = std::env::var("SHARD_COUNT")
    .ok()
    .map(|s| s.parse().unwrap());

  if let (Some(shard_index), Some(shard_count)) = (shard_index, shard_count) {
    let tests_per_shard = category.test_count() / shard_count as usize;
    let mut current_shard_index = 0;
    let mut tests_in_current_shard = 0;
    category.children.retain_mut(|category| match category {
      CollectedCategoryOrTest::Test(_) => todo!(),
      CollectedCategoryOrTest::Category(category) => {
        category.children.retain(|category| match category {
          CollectedCategoryOrTest::Test(_) => todo!(),
          CollectedCategoryOrTest::Category(category) => {
            let test_count = category.test_count();
            tests_in_current_shard += test_count;
            let retain = current_shard_index == shard_index;
            if tests_in_current_shard > tests_per_shard {
              current_shard_index += 1;
              tests_in_current_shard = 0;
            }
            retain
          }
        });
        !category.children.is_empty()
      }
    });
  };

  file_test_runner::run_tests(
    &category,
    RunOptions { parallel: true },
    Arc::new(run_test),
  )
}

fn run_test(test: &CollectedTest) -> TestResult {
  TestResult::from_maybe_panic(|| {
    let file = test.read_to_string().unwrap();
    let Some((scope_name_version, rest)) = file.split_once('\n') else {
      panic!("first line of test file must be scope/name/version");
    };
    let (scope, name_version) = scope_name_version.split_once('/').unwrap();
    let (name, version) = name_version.split_once('/').unwrap();

    let expected = rest.strip_prefix("===\n\n").unwrap();

    test_version(scope, name, version, &test.path, expected)
  })
}

#[derive(Debug, Clone, Deserialize)]
struct VersionMeta {
  exports: IndexMap<String, String>,
}

struct Loader<'a> {
  scope: &'a str,
  name: &'a str,
  version: &'a str,
}

impl deno_graph::source::Loader for Loader<'_> {
  fn load(
    &self,
    specifier: &deno_ast::ModuleSpecifier,
    _options: deno_graph::source::LoadOptions,
  ) -> deno_graph::source::LoadFuture {
    let res = match specifier.scheme() {
      "file" => {
        let specifier_str = specifier.to_string();
        let specifier_str = specifier_str.trim_start_matches("file:///");
        let path = format!(
          "./tests/ecosystem/jsr_mirror/{}/{}/{}/{}",
          self.scope, self.name, self.version, specifier_str
        );
        match std::fs::read_to_string(path) {
          Ok(source_code) => Ok(Some(LoadResponse::Module {
            content: source_code.into_bytes().into(),
            maybe_headers: None,
            specifier: specifier.clone(),
          })),
          Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(None),
          Err(err) => Err(anyhow::Error::from(err)),
        }
      }
      "data" => deno_graph::source::load_data_url(specifier),
      "jsr" | "npm" | "node" => Ok(Some(LoadResponse::External {
        specifier: specifier.clone(),
      })),
      _ => Err(anyhow::anyhow!(
        "Unsupported scheme: {}",
        specifier.scheme()
      )),
    };
    async move { res }.boxed()
  }
}

struct PassthroughJsrUrlProvider;

impl deno_graph::source::JsrUrlProvider for PassthroughJsrUrlProvider {
  fn url(&self) -> &Url {
    unreachable!(
      "BuildOptions::passthrough_jsr_specifiers should be set to true"
    )
  }

  fn package_url(&self, _nv: &PackageNv) -> Url {
    unreachable!(
      "BuildOptions::passthrough_jsr_specifiers should be set to true"
    )
  }

  fn package_url_to_nv(&self, _url: &Url) -> Option<PackageNv> {
    None
  }
}

use std::path::Path;
use std::{fs, io};

fn copy_dir_all(
  src: impl AsRef<Path>,
  dst: impl AsRef<Path>,
) -> io::Result<()> {
  fs::create_dir_all(&dst)?;
  for entry in fs::read_dir(src)? {
    let entry = entry?;
    let ty = entry.file_type()?;
    if ty.is_dir() {
      copy_dir_all(entry.path(), dst.as_ref().join(entry.file_name()))?;
    } else {
      fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
    }
  }
  Ok(())
}

#[tokio::main(flavor = "current_thread")]
async fn test_version(
  scope: &str,
  name: &str,
  version: &str,
  spec_path: &Path,
  expected: &str,
) {
  let version_meta_path =
    format!("./tests/ecosystem/jsr_mirror/{scope}/{name}/{version}_meta.json");
  let version_meta_str = std::fs::read_to_string(version_meta_path).unwrap();
  let version_meta: VersionMeta =
    serde_json::from_str(&version_meta_str).unwrap();

  let module_analyzer = CapturingModuleAnalyzer::default();

  let mut graph = ModuleGraph::new(GraphKind::All);
  let workspace_members = vec![WorkspaceMember {
    base: Url::parse("file:///").unwrap(),
    exports: version_meta.exports.clone(),
    nv: PackageNv {
      name: format!("@{scope}/{name}"),
      version: deno_semver::Version::parse_standard(version).unwrap(),
    },
  }];

  let mut roots = vec![];
  for (_, specifier) in &version_meta.exports {
    let url = Url::parse(&format!("file:///{specifier}")).unwrap();
    roots.push(url);
  }

  let loader = Loader {
    scope,
    name,
    version,
  };
  graph
    .build(
      roots.clone(),
      &loader,
      BuildOptions {
        is_dynamic: false,
        module_analyzer: &module_analyzer,
        workspace_members: &workspace_members,
        file_system: &NullFileSystem,
        resolver: None,
        npm_resolver: None,
        reporter: None,
        jsr_url_provider: &PassthroughJsrUrlProvider,
        passthrough_jsr_specifiers: true,
        executor: Default::default(),
        imports: vec![],
      },
    )
    .await;
  graph.valid().unwrap();
  graph.build_fast_check_type_graph(BuildFastCheckTypeGraphOptions {
    fast_check_cache: Default::default(),
    fast_check_dts: true,
    jsr_url_provider: &PassthroughJsrUrlProvider,
    module_parser: Some(&module_analyzer),
    resolver: None,
    npm_resolver: None,
    workspace_fast_check: WorkspaceFastCheckOption::Enabled(&workspace_members),
  });

  let mut fast_check_diagnostics = String::new();
  for root in &roots {
    let module = graph.get(root).unwrap();
    if let Some(module) = module.js() {
      if let Some(diagnostics) = module.fast_check_diagnostics() {
        for diagnostic in diagnostics {
          writeln!(&mut fast_check_diagnostics, "{}", diagnostic.display())
            .unwrap();
        }
      }
    }
  }

  let mut output = if fast_check_diagnostics.is_empty() {
    "== FAST CHECK EMIT PASSED ==\n".to_owned()
  } else {
    format!("== FAST CHECK EMIT FAILED ==\n{}", fast_check_diagnostics)
  };

  if fast_check_diagnostics.is_empty() {
    let tmpdir = tempdir().unwrap();
    let tmpdir_path = tmpdir.path().canonicalize().unwrap();
    let base_path =
      format!("./tests/ecosystem/jsr_mirror/{scope}/{name}/{version}");
    copy_dir_all(base_path, &tmpdir_path).unwrap();

    for module in graph.modules() {
      if module.specifier().scheme() != "file" {
        continue;
      }
      if let Some(module) = module.js() {
        if let Some(fcm) = module.fast_check_module() {
          let path =
            format!("{}{}", tmpdir_path.display(), module.specifier.path());
          std::fs::write(&path, fcm.source.as_bytes()).unwrap();
        }
      }
    }

    let deno_out = std::process::Command::new("deno")
      .arg("check")
      .arg("--no-lock")
      .arg("--no-config")
      .env("DENO_NO_PACKAGE_JSON", "true")
      .env("NO_COLOR", "true")
      .env("RUST_LIB_BACKTRACE", "0")
      .args(roots.iter().map(|root| format!(".{}", root.path())))
      .current_dir(&tmpdir_path)
      .output()
      .unwrap();
    if deno_out.status.success() {
      writeln!(&mut output, "\n== TYPE CHECK PASSED ==").unwrap();
    } else {
      writeln!(&mut output, "\n== TYPE CHECK FAILED ==").unwrap();
      let initialize_regexp =
        regex::Regex::new(r"(:?Initialize|Download|Check) [^\n]*\n").unwrap();
      let node_modules_dir_regexp =
        regex::Regex::new(r"\/[^\s\n]*/registry.npmjs.org").unwrap();
      let stdout = String::from_utf8_lossy(&deno_out.stdout)
        .replace(tmpdir_path.to_str().unwrap(), "<tmpdir>");
      let stdout = initialize_regexp.replace_all(&stdout, "");
      let stdout =
        node_modules_dir_regexp.replace_all(&stdout, "<global_npm_dir>");
      let stderr = String::from_utf8_lossy(&deno_out.stderr)
        .replace(tmpdir_path.to_str().unwrap(), "<tmpdir>");
      let stderr = initialize_regexp.replace_all(&stderr, "");
      let stderr =
        node_modules_dir_regexp.replace_all(&stderr, "<global_npm_dir>");
      writeln!(&mut output, "-- stdout --\n{}", stdout).unwrap();
      writeln!(&mut output, "-- stderr --\n{}", stderr).unwrap();
    }

    if std::env::var("DONT_CLEAN").is_ok() {
      println!("leaving tempdir: {}", tmpdir_path.display());
      Box::leak(Box::new(tmpdir));
    }
  }

  if std::env::var("UPDATE").is_ok() {
    std::fs::write(
      spec_path,
      format!("{scope}/{name}/{version}\n===\n\n{}", output),
    )
    .unwrap();
  } else {
    let expected = expected.trim_end();
    let output = output.trim_end();
    if output != expected {
      panic!("expected:\n{}\n\nactual:\n{}", expected, output);
    }
  }
}
