// Copyright 2018-2024 the Deno authors. MIT license.

#![allow(clippy::disallowed_methods)]

// todo(dsherret): move the integration-like tests to this file because it
// helps ensure we're testing the public API and ensures we export types
// out of deno_graph that should be public

use std::cell::RefCell;

use deno_ast::ModuleSpecifier;
use deno_graph::packages::JsrPackageInfo;
use deno_graph::packages::JsrPackageInfoVersion;
use deno_graph::packages::JsrPackageVersionInfo;
use deno_graph::source::CacheSetting;
use deno_graph::source::ChecksumIntegrityError;
use deno_graph::source::LoadError;
use deno_graph::source::LoadFuture;
use deno_graph::source::LoadOptions;
use deno_graph::source::LoadResponse;
use deno_graph::source::MemoryLoader;
use deno_graph::BuildOptions;
use deno_graph::FillFromLockfileOptions;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_semver::jsr::JsrDepPackageReq;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use pretty_assertions::assert_eq;
use sys_traits::impls::InMemorySys;
use sys_traits::FsCreateDirAll;
use sys_traits::FsWrite;
use url::Url;

use crate::helpers::TestBuilder;

mod helpers;

#[cfg(feature = "symbols")]
#[tokio::test]
async fn test_symbols_dep_definition() {
  use deno_graph::symbols::ResolvedSymbolDepEntry;

  let result = TestBuilder::new()
    .with_loader(|loader| {
      loader.remote.add_source_with_text(
        "file:///mod.ts",
        r#"
export type MyType = typeof MyClass;
export type MyTypeProp = typeof MyClass.staticProp;
export type MyTypeIndexAccess = typeof MyClass["staticProp"];
export type PrototypeAccess = typeof MyClass.prototype.instanceProp;

export class MyClass {
  instanceProp: string = "";
  static staticProp: string = "";
}
"#,
      );
    })
    .build()
    .await;

  let root_symbol = result.root_symbol();
  let module = root_symbol
    .module_from_specifier(&ModuleSpecifier::parse("file:///mod.ts").unwrap())
    .unwrap();
  let exports = module.exports(&root_symbol);

  let resolve_single_definition_text = |name: &str| -> String {
    let resolved_type = exports.resolved.get(name).unwrap();
    let resolved_type = resolved_type.as_resolved_export();
    let type_symbol = resolved_type.symbol();
    let deps = type_symbol
      .decls()
      .iter()
      .filter_map(|d| d.maybe_node())
      .flat_map(|s| {
        s.deps(deno_graph::symbols::ResolveDepsMode::TypesAndExpressions)
      })
      .collect::<Vec<_>>();
    assert_eq!(deps.len(), 1);
    let mut resolved_deps =
      root_symbol.resolve_symbol_dep(resolved_type.module, &deps[0]);
    assert_eq!(resolved_deps.len(), 1);
    let resolved_dep = resolved_deps.remove(0);
    let path = match resolved_dep {
      ResolvedSymbolDepEntry::Path(p) => p,
      ResolvedSymbolDepEntry::ImportType(_) => unreachable!(),
    };
    let definitions = path.into_definitions().collect::<Vec<_>>();
    assert_eq!(definitions.len(), 1);
    let definition = &definitions[0];
    definition.text().to_string()
  };

  let class_text =
    "export class MyClass {\n  instanceProp: string = \"\";\n  static staticProp: string = \"\";\n}";
  assert_eq!(resolve_single_definition_text("MyType"), class_text);
  assert_eq!(
    resolve_single_definition_text("MyTypeProp"),
    "static staticProp: string = \"\";"
  );
  assert_eq!(
    resolve_single_definition_text("MyTypeIndexAccess"),
    // good enough for now
    class_text
  );
  assert_eq!(
    resolve_single_definition_text("PrototypeAccess"),
    // good enough for now
    class_text
  );
}

#[cfg(feature = "symbols")]
#[tokio::test]
async fn test_symbols_re_export_external_and_npm() {
  let result = TestBuilder::new()
    .with_loader(|loader| {
      loader.remote.add_source_with_text(
        "file:///mod.ts",
        r#"export * from 'npm:example@1.0.0'; export * from 'external:other"#,
      );
      loader.remote.add_external_source("external:other");
    })
    .build()
    .await;

  let root_symbol = result.root_symbol();
  let module = root_symbol
    .module_from_specifier(&ModuleSpecifier::parse("file:///mod.ts").unwrap())
    .unwrap();
  let exports = module.exports(&root_symbol);
  assert_eq!(
    exports
      .unresolved_specifiers
      .into_iter()
      .map(|s| s.specifier)
      .collect::<Vec<_>>(),
    vec!["npm:example@1.0.0", "external:other"]
  );
}

#[tokio::test]
async fn test_jsr_version_not_found_then_found() {
  #[derive(Default)]
  struct TestLoader {
    requests: RefCell<Vec<(String, CacheSetting)>>,
  }

  impl deno_graph::source::Loader for TestLoader {
    fn load(
      &self,
      specifier: &ModuleSpecifier,
      options: LoadOptions,
    ) -> LoadFuture {
      assert!(!options.is_dynamic);
      self
        .requests
        .borrow_mut()
        .push((specifier.to_string(), options.cache_setting));
      let specifier = specifier.clone();
      match specifier.as_str() {
        "file:///main.ts" => Box::pin(async move {
          Ok(Some(LoadResponse::Module {
            specifier: specifier.clone(),
            maybe_headers: None,
            content: b"import 'jsr:@scope/a@1.2".to_vec().into(),
          }))
        }),
        "file:///empty.ts" => Box::pin(async move {
          Ok(Some(LoadResponse::Module {
            specifier: specifier.clone(),
            maybe_headers: None,
            content: Default::default(),
          }))
        }),
        "https://jsr.io/@scope/a/meta.json" => {
          Box::pin(async move {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: match options.cache_setting {
                CacheSetting::Only | CacheSetting::Use => {
                  // first time it won't have the version
                  br#"{ "versions": { "1.0.0": {} } }"#.to_vec().into()
                }
                CacheSetting::Reload => {
                  // then on reload it will
                  br#"{ "versions": { "1.0.0": {}, "1.2.0": {} } }"#.to_vec().into()
                }
              },
            }))
          })
        }
        "https://jsr.io/@scope/a/1.2.0_meta.json" => Box::pin(async move {
          Ok(Some(LoadResponse::Module {
            specifier: specifier.clone(),
            maybe_headers: None,
            content: br#"{
                "exports": { ".": "./mod.ts" },
                "manifest": {
                  "/mod.ts": {
                    "size": 123,
                    "checksum": "sha256-b8059cfb1ea623e79efbf432db31595df213c99c6534c58bec9d5f5e069344df"
                  }
                }
              }"#
              .to_vec()
              .into(),
          }))
        }),
        "https://jsr.io/@scope/a/1.2.0/mod.ts" => Box::pin(async move {
          Ok(Some(LoadResponse::Module {
            specifier: specifier.clone(),
            maybe_headers: None,
            content: b"console.log('Hello, world!')".to_vec().into(),
          }))
        }),
        _ => unreachable!(),
      }
    }
  }

  {
    let loader = TestLoader::default();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///main.ts").unwrap()],
        &loader,
        Default::default(),
      )
      .await;
    graph.valid().unwrap();
    assert_eq!(
      *loader.requests.borrow(),
      vec![
        ("file:///main.ts".to_string(), CacheSetting::Use),
        (
          "https://jsr.io/@scope/a/meta.json".to_string(),
          CacheSetting::Use
        ),
        ("file:///main.ts".to_string(), CacheSetting::Use),
        (
          "https://jsr.io/@scope/a/meta.json".to_string(),
          CacheSetting::Reload
        ),
        (
          "https://jsr.io/@scope/a/1.2.0_meta.json".to_string(),
          CacheSetting::Reload
        ),
        (
          "https://jsr.io/@scope/a/1.2.0/mod.ts".to_string(),
          CacheSetting::Use
        ),
      ]
    );
  }

  {
    let loader = TestLoader::default();
    let mut graph = ModuleGraph::new(GraphKind::All);
    // do an initial build
    graph
      .build(
        vec![Url::parse("file:///empty.ts").unwrap()],
        &loader,
        Default::default(),
      )
      .await;
    graph.valid().unwrap();

    // full restart won't be supported at this point because
    // a build previously happened, so it will only reload
    // specific meta files
    graph
      .build(
        vec![Url::parse("file:///main.ts").unwrap()],
        &loader,
        Default::default(),
      )
      .await;
    graph.valid().unwrap();
    assert_eq!(
      *loader.requests.borrow(),
      vec![
        ("file:///empty.ts".to_string(), CacheSetting::Use),
        ("file:///main.ts".to_string(), CacheSetting::Use),
        (
          "https://jsr.io/@scope/a/meta.json".to_string(),
          CacheSetting::Use
        ),
        (
          "https://jsr.io/@scope/a/meta.json".to_string(),
          CacheSetting::Reload
        ),
        (
          "https://jsr.io/@scope/a/1.2.0_meta.json".to_string(),
          CacheSetting::Use
        ),
        (
          "https://jsr.io/@scope/a/1.2.0/mod.ts".to_string(),
          CacheSetting::Use
        ),
      ]
    );
  }
}

#[tokio::test]
async fn test_jsr_wasm_module() {
  struct TestLoader;

  impl deno_graph::source::Loader for TestLoader {
    fn load(
      &self,
      specifier: &ModuleSpecifier,
      options: LoadOptions,
    ) -> LoadFuture {
      assert!(!options.is_dynamic);
      let specifier = specifier.clone();
      match specifier.as_str() {
        "file:///main.ts" => Box::pin(async move {
          Ok(Some(LoadResponse::Module {
            specifier: specifier.clone(),
            maybe_headers: None,
            content: b"import 'jsr:@scope/a@1".to_vec().into(),
          }))
        }),
        "https://jsr.io/@scope/a/meta.json" => Box::pin(async move {
          Ok(Some(LoadResponse::Module {
            specifier: specifier.clone(),
            maybe_headers: None,
            content: br#"{ "versions": { "1.0.0": {} } }"#.to_vec().into(),
          }))
        }),
        "https://jsr.io/@scope/a/1.0.0_meta.json" => Box::pin(async move {
          Ok(Some(LoadResponse::Module {
            specifier: specifier.clone(),
            maybe_headers: None,
            content: br#"{
                "exports": { ".": "./math.wasm" },
                "manifest": {
                  "/math.wasm": {
                    "size": 123,
                    "checksum": "sha256-b8059cfb1ea623e79efbf432db31595df213c99c6534c58bec9d5f5e069344df"
                  }
                },
                "moduleGraph2": {
                  "/math.wasm": {
                    "dependencies": []
                  }
                }
              }"#
              .to_vec()
              .into(),
          }))
        }),
        "https://jsr.io/@scope/a/1.0.0/math.wasm" => Box::pin(async move {
          if options.cache_setting == CacheSetting::Only {
            Ok(None)
          } else {
            Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: std::fs::read("./tests/testdata/math.wasm")
                .unwrap()
                .into(),
            }))
          }
        }),
        _ => unreachable!(),
      }
    }
  }

  {
    let loader = TestLoader;
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse("file:///main.ts").unwrap()],
        &loader,
        Default::default(),
      )
      .await;
    graph.valid().unwrap();
  }
}

#[tokio::test]
async fn test_checksum_error_force_refresh() {
  #[derive(Default)]
  struct TestLoader {
    requests: RefCell<Vec<(String, CacheSetting)>>,
  }

  impl deno_graph::source::Loader for TestLoader {
    fn load(
      &self,
      specifier: &ModuleSpecifier,
      options: LoadOptions,
    ) -> LoadFuture {
      self
        .requests
        .borrow_mut()
        .push((specifier.to_string(), options.cache_setting));
      let specifier = specifier.clone();
      match specifier.as_str() {
        "https://deno.land/mod.ts" => Box::pin(async move {
          match options.cache_setting {
            CacheSetting::Only => unreachable!(),
            CacheSetting::Use => {
              Err(LoadError::ChecksumIntegrity(ChecksumIntegrityError {
                actual: "actual".to_string(),
                expected: "expected".to_string(),
              }))
            }
            CacheSetting::Reload => Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: b"import './other.js';".to_vec().into(),
            })),
          }
        }),
        "https://deno.land/other.js" => Box::pin(async move {
          match options.cache_setting {
            CacheSetting::Only => unreachable!(),
            CacheSetting::Use => {
              Err(LoadError::ChecksumIntegrity(ChecksumIntegrityError {
                actual: "actual".to_string(),
                expected: "expected".to_string(),
              }))
            }
            CacheSetting::Reload => Ok(Some(LoadResponse::Module {
              specifier: specifier.clone(),
              maybe_headers: None,
              content: b"console.log(1);".to_vec().into(),
            })),
          }
        }),
        _ => unreachable!(),
      }
    }
  }

  let loader = TestLoader::default();
  let mut graph = ModuleGraph::new(GraphKind::All);
  graph
    .build(
      vec![Url::parse("https://deno.land/mod.ts").unwrap()],
      &loader,
      Default::default(),
    )
    .await;
  graph.valid().unwrap();
  assert_eq!(
    *loader.requests.borrow(),
    vec![
      ("https://deno.land/mod.ts".to_string(), CacheSetting::Use),
      ("https://deno.land/mod.ts".to_string(), CacheSetting::Reload),
      ("https://deno.land/other.js".to_string(), CacheSetting::Use),
      (
        "https://deno.land/other.js".to_string(),
        CacheSetting::Reload
      ),
    ]
  );
}

#[tokio::test]
async fn test_dynamic_imports_with_template_arg() {
  async fn run_test(
    code: &str,
    files: Vec<(&str, &str)>,
    expected_specifiers: Vec<&str>,
  ) {
    let mut loader = MemoryLoader::default();
    let sys = InMemorySys::default();
    for (specifier, text) in &files {
      let specifier = if cfg!(windows) {
        specifier.replace("file:///", "file:///C:/")
      } else {
        specifier.to_string()
      };
      let specifier = ModuleSpecifier::parse(&specifier).unwrap();
      let path = deno_path_util::url_to_file_path(&specifier).unwrap();
      sys.fs_create_dir_all(path.parent().unwrap()).unwrap();
      sys.fs_write(&path, text).unwrap();
      loader.add_source_with_text(specifier, text);
    }

    let entrypoint = if cfg!(windows) {
      "file:///C:/dev/main.ts"
    } else {
      "file:///dev/main.ts"
    };
    loader.add_source_with_text(entrypoint, code);
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![Url::parse(entrypoint).unwrap()],
        &loader,
        BuildOptions {
          file_system: &sys,
          ..Default::default()
        },
      )
      .await;
    graph.valid().unwrap();

    let specifiers = graph
      .specifiers()
      .map(|s| {
        if cfg!(windows) {
          s.0.as_str().replace("file:///C:/", "file:///")
        } else {
          s.0.to_string()
        }
      })
      .filter(|s| s != "file:///dev/main.ts")
      .collect::<Vec<_>>();
    assert_eq!(specifiers, expected_specifiers);
  }

  // relative with ./
  run_test(
    "
await import(`./${test}`);
",
    vec![
      ("file:///dev/a/mod.ts", ""),
      ("file:///dev/a/sub_dir/a.ts", ""),
      ("file:///dev/b.ts", ""),
    ],
    vec![
      "file:///dev/a/mod.ts",
      "file:///dev/a/sub_dir/a.ts",
      "file:///dev/b.ts",
    ],
  )
  .await;

  // relative with sub dir
  run_test(
    "
await import(`./a/${test}`);
",
    vec![
      ("file:///dev/a/mod.ts", ""),
      ("file:///dev/a/sub_dir/a.ts", ""),
      ("file:///dev/b.ts", ""),
    ],
    vec!["file:///dev/a/mod.ts", "file:///dev/a/sub_dir/a.ts"],
  )
  .await;

  run_test(
    "
  // should not match these two because it does not end in a slash
  await import(`./b${test}`);
  await import(`./c/a${test}`);
  ",
    vec![
      ("file:///dev/a/mod.ts", ""),
      ("file:///dev/b.ts", ""),
      ("file:///dev/c/a.ts", ""),
      ("file:///dev/c/a/a.ts", ""),
    ],
    vec![],
  )
  .await;

  run_test(
    "
  await import(`./d/other/${test}/main.json`, {
    with: {
      type: 'json',
    },
  });
  await import(`./d/sub/${test}`);
  ",
    vec![
      ("file:///dev/d/a.ts", ""),
      ("file:///dev/d/sub/main.json", ""),
      ("file:///dev/d/sub/a.ts", ""),
      ("file:///dev/d/sub/a.js", ""),
      ("file:///dev/d/sub/a.mjs", ""),
      ("file:///dev/d/sub/a.mts", ""),
      // should not match because it's a declaration file
      ("file:///dev/d/sub/a.d.ts", ""),
      ("file:///dev/d/other/json/main.json", ""),
      ("file:///dev/d/other/json/main2.json", ""),
    ],
    vec![
      "file:///dev/d/other/json/main.json",
      "file:///dev/d/sub/a.js",
      "file:///dev/d/sub/a.mjs",
      "file:///dev/d/sub/a.mts",
      "file:///dev/d/sub/a.ts",
    ],
  )
  .await;

  // only matching one extension
  run_test(
    "
  await import(`./d/sub2/${test}.mjs`);
  ",
    vec![
      ("file:///dev/d/sub2/a.ts", ""),
      ("file:///dev/d/sub2/a.js", ""),
      ("file:///dev/d/sub2/a.mjs", ""),
      ("file:///dev/d/sub2/a.mts", ""),
    ],
    vec!["file:///dev/d/sub2/a.mjs"],
  )
  .await;

  // file specifiers
  run_test(
    if cfg!(windows) {
      "await import(`file:///C:/dev/other/${test}`);"
    } else {
      "await import(`file:///dev/other/${test}`);"
    },
    vec![("file:///dev/other/mod.ts", ""), ("file:///dev/b.ts", "")],
    vec!["file:///dev/other/mod.ts"],
  )
  .await;

  // multiple exprs with same string between
  run_test(
    "await import(`./other/${test}/other/${test}/mod.ts`);",
    vec![
      ("file:///dev/other/mod.ts", ""),
      ("file:///dev/other/other/mod.ts", ""),
      ("file:///dev/other/test/other/mod.ts", ""),
      ("file:///dev/other/test/other/test/mod.ts", ""),
      ("file:///dev/other/test/other/test/other/mod.ts", ""),
      ("file:///dev/b.ts", ""),
    ],
    vec![
      "file:///dev/other/test/other/test/mod.ts",
      "file:///dev/other/test/other/test/other/mod.ts",
    ],
  )
  .await;

  // finding itself
  run_test(
    "await import(`./${expr}`);",
    vec![
      ("file:///dev/main.ts", ""), // self
      ("file:///dev/other.ts", ""),
    ],
    // should not have "file:///dev/" here
    vec!["file:///dev/other.ts"],
  )
  .await;

  // root directory should be ignored because this is likely
  // not wanted because it would include the entire file system
  run_test(
    "await import(`file:///${expr}`);",
    vec![("file:///main.ts", ""), ("file:///dev/other.ts", "")],
    vec![],
  )
  .await;

  // won't search node_modules, vendor, or hidden folders
  run_test(
    "await import(`./${test}/mod.ts`);",
    vec![
      ("file:///dev/other/.git/mod.ts", ""),
      ("file:///dev/other/node_modules/mod.ts", ""),
      ("file:///dev/other/sub_dir/mod.ts", ""),
      ("file:///dev/other/vendor/mod.ts", ""),
      ("file:///dev/other/mod.ts", ""),
    ],
    vec![
      "file:///dev/other/mod.ts",
      "file:///dev/other/sub_dir/mod.ts",
    ],
  )
  .await;
}

#[tokio::test]
async fn test_fill_from_lockfile() {
  let mut graph = ModuleGraph::new(GraphKind::All);
  let redirects = [("https://example.com", "https://example.com/final")];
  let specifiers = [
    (
      JsrDepPackageReq::from_str("jsr:@scope/example").unwrap(),
      "1.0.0",
    ),
    (
      JsrDepPackageReq::from_str("jsr:@scope/example@1.0.1").unwrap(),
      "1.0.1",
    ),
  ];
  graph.fill_from_lockfile(FillFromLockfileOptions {
    redirects: redirects.iter().copied(),
    package_specifiers: specifiers.iter().map(|(k, v)| (k, *v)),
  });

  assert_eq!(
    graph
      .redirects
      .get(&Url::parse("https://example.com").unwrap())
      .unwrap()
      .as_str(),
    "https://example.com/final"
  );
  assert_eq!(
    *graph
      .packages
      .mappings()
      .get(&PackageReq::from_str("@scope/example").unwrap())
      .unwrap(),
    PackageNv::from_str("@scope/example@1.0.0").unwrap(),
  );

  let mut loader = MemoryLoader::default();
  loader.add_jsr_package_info(
    "@scope/example",
    &JsrPackageInfo {
      versions: vec![
        (
          deno_semver::Version::parse_standard("1.0.0").unwrap(),
          JsrPackageInfoVersion::default(),
        ),
        (
          deno_semver::Version::parse_standard("1.0.1").unwrap(),
          JsrPackageInfoVersion::default(),
        ),
      ]
      .into_iter()
      .collect(),
    },
  );
  loader.add_jsr_version_info(
    "@scope/example",
    "1.0.0",
    &JsrPackageVersionInfo {
      exports: serde_json::json!({
        ".": "./mod.ts"
      }),
      ..Default::default()
    },
  );
  loader.add_jsr_version_info(
    "@scope/example",
    "1.0.1",
    &JsrPackageVersionInfo {
      exports: serde_json::json!({
        ".": "./mod.ts"
      }),
      ..Default::default()
    },
  );
  loader.add_source_with_text(
    "https://jsr.io/@scope/example/1.0.0/mod.ts",
    "// This is version 1.0.0 of this package.",
  );
  loader.add_source_with_text(
    "https://jsr.io/@scope/example/1.0.1/mod.ts",
    "// This is version 1.0.1 of this package.",
  );
  graph
    .build(
      // This should match 1.0.0 due to the first entry in the lockfile.
      vec![Url::parse("jsr:/@scope/example").unwrap()],
      &loader,
      Default::default(),
    )
    .await;
  graph.valid().unwrap();
  let modules = graph.modules().collect::<Vec<_>>();
  assert_eq!(modules.len(), 1);
  let module = modules.into_iter().next().unwrap().js().unwrap();
  assert_eq!(
    module.source.as_ref(),
    "// This is version 1.0.0 of this package."
  );
}

#[tokio::test]
async fn test_json_root() {
  let mut graph = ModuleGraph::new(GraphKind::All);
  let mut loader = MemoryLoader::default();

  loader.add_source_with_text(
    "https://jsr.io/@scope/example/1.0.0/data.json",
    "{ \"a\": 1 }",
  );
  loader.add_source(
    "https://deno.land/x/redirect",
    deno_graph::source::Source::Redirect(
      "https://jsr.io/@scope/example/1.0.0/data.json",
    ),
  );
  loader.add_source(
    "https://deno.land/x/redirect2",
    deno_graph::source::Source::Redirect("https://deno.land/x/redirect"),
  );
  loader.add_source(
    "https://deno.land/x/redirect3",
    deno_graph::source::Source::Redirect("https://deno.land/x/redirect2"),
  );
  loader.add_jsr_package_info(
    "@scope/example",
    &JsrPackageInfo {
      versions: vec![(
        deno_semver::Version::parse_standard("1.0.0").unwrap(),
        JsrPackageInfoVersion::default(),
      )]
      .into_iter()
      .collect(),
    },
  );
  loader.add_jsr_version_info(
    "@scope/example",
    "1.0.0",
    &JsrPackageVersionInfo {
      exports: serde_json::json!({
        "./json-export": "./data.json"
      }),
      ..Default::default()
    },
  );
  graph
    .build(
      vec![Url::parse("jsr:/@scope/example@^1.0.0/json-export").unwrap()],
      &loader,
      Default::default(),
    )
    .await;
  graph.valid().unwrap();
  graph
    .build(
      vec![Url::parse("https://deno.land/x/redirect").unwrap()],
      &loader,
      Default::default(),
    )
    .await;
  graph.valid().unwrap();
  graph
    .build(
      vec![Url::parse("https://deno.land/x/redirect3").unwrap()],
      &loader,
      Default::default(),
    )
    .await;
  graph.valid().unwrap();
  assert_eq!(
    graph.roots.iter().map(|s| s.as_str()).collect::<Vec<_>>(),
    vec![
      "jsr:/@scope/example@^1.0.0/json-export",
      "https://deno.land/x/redirect",
      "https://deno.land/x/redirect3", // not 2
    ]
  );
}

#[tokio::test]
async fn test_wasm_math() {
  let mut graph = ModuleGraph::new(GraphKind::All);
  let mut loader = MemoryLoader::default();

  loader.add_bytes_source(
    "file:///project/math.wasm",
    std::fs::read("tests/testdata/math.wasm").unwrap(),
  );

  loader.add_source_with_text(
    "file:///project/main.ts",
    "import { add } from './math.wasm'; console.log(add(1, 2));",
  );

  graph
    .build(
      vec![Url::parse("file:///project/main.ts").unwrap()],
      &loader,
      Default::default(),
    )
    .await;
  graph.valid().unwrap();
  let wasm_module = graph
    .get(&Url::parse("file:///project/math.wasm").unwrap())
    .unwrap();
  match wasm_module {
    deno_graph::Module::Wasm(wasm_module) => {
      assert_eq!(
        wasm_module.source_dts.to_string(),
        "export declare const memory: WebAssembly.Memory;
export declare function add(arg0: number, arg1: number): number;
export declare function subtract(arg0: number, arg1: number): number;
export declare const __data_end: number;
export declare const __heap_base: number;
"
      );
    }
    _ => unreachable!(),
  }
}

#[tokio::test]
async fn test_wasm_math_with_import() {
  let mut graph = ModuleGraph::new(GraphKind::All);
  let mut loader = MemoryLoader::default();

  loader.add_bytes_source(
    "file:///project/math.wasm",
    std::fs::read("tests/testdata/math_with_import.wasm").unwrap(),
  );

  loader.add_source_with_text(
    "file:///project/main.ts",
    "import { add } from './math.wasm'; console.log(add(1, 2));",
  );

  loader.add_source_with_text(
    "file:///project/math.ts",
    "export function add(a: number, b: number): number { return a + b; }\n export function subtract(a: number, b: number): number { return a - b; }",
  );

  graph
    .build(
      vec![Url::parse("file:///project/main.ts").unwrap()],
      &loader,
      Default::default(),
    )
    .await;
  graph.valid().unwrap();
  let wasm_module = graph
    .get(&Url::parse("file:///project/math.wasm").unwrap())
    .unwrap();
  match wasm_module {
    deno_graph::Module::Wasm(wasm_module) => {
      assert_eq!(
        wasm_module.source_dts.to_string(),
        "import { \"js_add\" as __deno_wasm_import_0__, \"js_subtract\" as __deno_wasm_import_1__ } from \"./math.ts\";
export declare const memory: WebAssembly.Memory;
export declare function add(arg0: number, arg1: number): number;
export declare function subtract(arg0: number, arg1: number): number;
export declare const __data_end: number;
export declare const __heap_base: number;
"
      );
    }
    _ => unreachable!(),
  }

  // now ensure the imports are in the graph
  assert!(graph
    .get(&Url::parse("file:///project/math.ts").unwrap())
    .is_some());
}
