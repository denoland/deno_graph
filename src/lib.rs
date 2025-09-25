// Copyright 2018-2024 the Deno authors. MIT license.

#![deny(clippy::print_stderr)]
#![deny(clippy::print_stdout)]
#![deny(clippy::unused_async)]
#![deny(clippy::unnecessary_wraps)]

pub mod analysis;
#[cfg(feature = "swc")]
pub mod ast;
mod collections;
mod graph;
mod jsr;
mod module_specifier;
mod rt;

#[cfg(feature = "symbols")]
pub mod symbols;

#[cfg(feature = "fast_check")]
pub mod fast_check;
pub mod packages;
pub mod source;

use source::FileSystem;
use source::JsrUrlProvider;
use source::Resolver;

use std::collections::HashMap;
use std::sync::Arc;
use std::time::SystemTime;

pub use deno_media_type::MediaType;
#[cfg(feature = "fast_check")]
pub use graph::BuildFastCheckTypeGraphOptions;
pub use graph::BuildOptions;
pub use graph::CheckJsOption;
pub use graph::CheckJsResolver;
pub use graph::Dependency;
pub use graph::ExternalModule;
#[cfg(feature = "fast_check")]
pub use graph::FastCheckTypeModule;
#[cfg(feature = "fast_check")]
pub use graph::FastCheckTypeModuleSlot;
pub use graph::FillFromLockfileOptions;
pub use graph::GraphImport;
pub use graph::GraphKind;
pub use graph::Import;
pub use graph::ImportKind;
pub use graph::JsModule;
pub use graph::JsonModule;
pub use graph::JsrLoadError;
pub use graph::JsrPackageReqNotFoundError;
pub use graph::Module;
pub use graph::ModuleEntryRef;
pub use graph::ModuleError;
pub use graph::ModuleErrorKind;
pub use graph::ModuleGraph;
pub use graph::ModuleGraphError;
pub use graph::ModuleLoadError;
pub use graph::ModuleTextSource;
pub use graph::NpmLoadError;
pub use graph::NpmModule;
pub use graph::Position;
pub use graph::PositionRange;
pub use graph::Range;
pub use graph::Resolution;
pub use graph::ResolutionError;
pub use graph::ResolutionResolved;
pub use graph::TypesDependency;
pub use graph::WalkOptions;
pub use graph::WasmModule;
#[cfg(feature = "fast_check")]
pub use graph::WorkspaceFastCheckOption;
pub use graph::WorkspaceMember;
pub use jsr::JsrMetadataStore;
pub use module_specifier::resolve_import;
pub use module_specifier::ModuleSpecifier;
pub use module_specifier::SpecifierError;
pub use rt::Executor;
pub use source::NpmResolvePkgReqsResult;

use self::analysis::ModuleAnalyzer;

/// Additional import that should be brought into the scope of
/// the module graph to add to the graph's "imports". This may
/// be extra modules such as TypeScript's "types" option or JSX
/// runtime types.
#[derive(Debug, Clone)]
pub struct ReferrerImports {
  /// The referrer to resolve the imports from.
  pub referrer: ModuleSpecifier,
  /// Specifiers relative to the referrer to resolve.
  pub imports: Vec<String>,
}

pub struct ParseModuleOptions<'a> {
  pub graph_kind: GraphKind,
  pub specifier: ModuleSpecifier,
  pub maybe_headers: Option<HashMap<String, String>>,
  pub mtime: Option<SystemTime>,
  pub content: Arc<[u8]>,
  pub file_system: &'a FileSystem,
  pub jsr_url_provider: &'a dyn JsrUrlProvider,
  pub maybe_resolver: Option<&'a dyn Resolver>,
  pub module_analyzer: &'a dyn ModuleAnalyzer,
}

/// Parse an individual module, returning the module as a result, otherwise
/// erroring with a module graph error.
#[allow(clippy::result_large_err)]
pub async fn parse_module(
  options: ParseModuleOptions<'_>,
) -> Result<Module, ModuleError> {
  let module_source_and_info = graph::parse_module_source_and_info(
    options.module_analyzer,
    graph::ParseModuleAndSourceInfoOptions {
      specifier: options.specifier,
      maybe_headers: options.maybe_headers,
      mtime: options.mtime,
      content: options.content,
      maybe_attribute_type: None,
      maybe_referrer: None,
      is_root: true,
      is_dynamic_branch: false,
    },
  )
  .await?;
  let module = graph::parse_module(
    options.file_system,
    options.jsr_url_provider,
    options.maybe_resolver,
    graph::ParseModuleOptions {
      graph_kind: options.graph_kind,
      module_source_and_info,
    },
  );

  Ok(module)
}

#[cfg(feature = "swc")]
pub struct ParseModuleFromAstOptions<'a> {
  pub graph_kind: GraphKind,
  pub specifier: ModuleSpecifier,
  pub maybe_headers: Option<&'a HashMap<String, String>>,
  pub mtime: Option<SystemTime>,
  pub parsed_source: &'a deno_ast::ParsedSource,
  pub file_system: &'a FileSystem,
  pub jsr_url_provider: &'a dyn JsrUrlProvider,
  pub maybe_resolver: Option<&'a dyn Resolver>,
}

/// Parse an individual module from an AST, returning the module.
#[cfg(feature = "swc")]
pub fn parse_module_from_ast(options: ParseModuleFromAstOptions) -> JsModule {
  graph::parse_js_module_from_module_info(
    options.graph_kind,
    options.specifier,
    options.parsed_source.media_type(),
    options.maybe_headers,
    ast::ParserModuleAnalyzer::module_info(options.parsed_source),
    options.mtime,
    ModuleTextSource::new_unknown(options.parsed_source.text().clone()),
    options.file_system,
    options.jsr_url_provider,
    options.maybe_resolver,
  )
}

#[cfg(test)]
mod tests {
  use crate::graph::Import;
  use crate::graph::ImportKind;
  use crate::graph::PositionRange;
  use crate::graph::ResolutionResolved;
  use crate::source::NullFileSystem;
  use crate::source::ResolutionKind;

  use self::graph::CheckJsOption;

  use super::*;
  use deno_error::JsErrorBox;
  use deno_semver::package::PackageNv;
  use deno_semver::package::PackageReq;
  use indexmap::IndexMap;
  use indexmap::IndexSet;
  use parking_lot::Mutex;
  use pretty_assertions::assert_eq;
  use serde_json::json;
  use source::tests::MockResolver;
  use source::CacheInfo;
  use source::MemoryLoader;
  use source::ResolutionMode;
  use source::Source;
  use source::DEFAULT_JSX_IMPORT_SOURCE_MODULE;
  use std::collections::BTreeMap;

  type Sources<'a> = Vec<(&'a str, Source<&'a str>)>;

  fn setup(
    sources: Sources,
    cache_info: Vec<(&str, CacheInfo)>,
  ) -> MemoryLoader {
    MemoryLoader::new(sources, cache_info)
  }

  #[tokio::test]
  async fn test_build_graph() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"import * as b from "./test02.ts";"#,
          },
        ),
        (
          "file:///a/test02.ts",
          Source::Module {
            specifier: "file:///a/test02.ts",
            maybe_headers: None,
            content: r#"export const b = "b";"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(graph.module_slots.len(), 2);
    assert_eq!(graph.roots, IndexSet::from([root_specifier.clone()]));
    assert!(graph.contains(&root_specifier));
    assert!(
      !graph.contains(&ModuleSpecifier::parse("file:///a/test03.ts").unwrap())
    );
    let module = graph
      .module_slots
      .get(&root_specifier)
      .unwrap()
      .module()
      .unwrap()
      .js()
      .unwrap();
    assert_eq!(module.dependencies.len(), 1);
    let maybe_dependency = module.dependencies.get("./test02.ts");
    assert!(maybe_dependency.is_some());
    let dependency_specifier =
      ModuleSpecifier::parse("file:///a/test02.ts").unwrap();
    let dependency = maybe_dependency.unwrap();
    assert!(!dependency.is_dynamic);
    assert_eq!(
      dependency.maybe_code.ok().unwrap().specifier,
      dependency_specifier
    );
    assert_eq!(dependency.maybe_type, Resolution::None);
    let maybe_dep_module_slot = graph.get(&dependency_specifier);
    assert!(maybe_dep_module_slot.is_some());
  }

  #[tokio::test]
  async fn test_build_graph_multiple_roots() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"import * as b from "./test02.ts";"#,
          },
        ),
        (
          "file:///a/test02.ts",
          Source::Module {
            specifier: "file:///a/test02.ts",
            maybe_headers: None,
            content: r#"export const b = "b";"#,
          },
        ),
        (
          "https://example.com/a.ts",
          Source::Module {
            specifier: "https://example.com/a.ts",
            maybe_headers: None,
            content: r#"import * as c from "./c.ts";"#,
          },
        ),
        (
          "https://example.com/c.ts",
          Source::Module {
            specifier: "https://example.com/c.ts",
            maybe_headers: None,
            content: r#"export const c = "c";"#,
          },
        ),
      ],
      vec![],
    );
    let roots = IndexSet::from([
      ModuleSpecifier::parse("file:///a/test01.ts").unwrap(),
      ModuleSpecifier::parse("https://example.com/a.ts").unwrap(),
    ]);
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        roots.iter().cloned().collect(),
        Vec::new(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(graph.module_slots.len(), 4);
    assert_eq!(graph.roots, roots);
    assert!(
      graph.contains(&ModuleSpecifier::parse("file:///a/test01.ts").unwrap())
    );
    assert!(
      graph.contains(&ModuleSpecifier::parse("file:///a/test02.ts").unwrap())
    );
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/a.ts").unwrap()));
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/c.ts").unwrap()));
  }

  #[tokio::test]
  async fn test_build_graph_multiple_times() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"import * as b from "./test02.ts";"#,
          },
        ),
        (
          "file:///a/test02.ts",
          Source::Module {
            specifier: "file:///a/test02.ts",
            maybe_headers: None,
            content: r#"import "https://example.com/c.ts"; export const b = "b";"#,
          },
        ),
        (
          "https://example.com/a.ts",
          Source::Module {
            specifier: "https://example.com/a.ts",
            maybe_headers: None,
            content: r#"import * as c from "./c.ts";"#,
          },
        ),
        (
          "https://example.com/c.ts",
          Source::Module {
            specifier: "https://example.com/c.ts",
            maybe_headers: None,
            content: r#"import "./d.ts"; export const c = "c";"#,
          },
        ),
        (
          "https://example.com/d.ts",
          Source::Module {
            specifier: "https://example.com/d.ts",
            maybe_headers: None,
            content: r#"export const d = "d";"#,
          },
        ),
      ],
      vec![],
    );
    let first_root = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let second_root =
      ModuleSpecifier::parse("https://example.com/a.ts").unwrap();
    let third_root =
      ModuleSpecifier::parse("https://example.com/d.ts").unwrap();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![first_root.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(graph.module_slots.len(), 4);
    assert_eq!(graph.roots, IndexSet::from([first_root.clone()]));

    // now build with the second root
    graph
      .build(
        vec![second_root.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    let mut roots = IndexSet::from([first_root, second_root]);
    assert_eq!(graph.module_slots.len(), 5);
    assert_eq!(graph.roots, roots);
    assert!(
      graph.contains(&ModuleSpecifier::parse("file:///a/test01.ts").unwrap())
    );
    assert!(
      graph.contains(&ModuleSpecifier::parse("file:///a/test02.ts").unwrap())
    );
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/a.ts").unwrap()));
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/c.ts").unwrap()));
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/d.ts").unwrap()));

    // now try making one of the already existing modules a root
    graph
      .build(
        vec![third_root.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    roots.insert(third_root);
    assert_eq!(graph.module_slots.len(), 5);
    assert_eq!(graph.roots, roots);
  }

  #[tokio::test]
  async fn test_build_graph_json_module_root() {
    let loader = setup(
      vec![(
        "file:///a/test.json",
        Source::Module {
          specifier: "file:///a/test.json",
          maybe_headers: None,
          content: r#"{"a": 1, "b": "c"}"#,
        },
      )],
      vec![],
    );
    let roots = vec![ModuleSpecifier::parse("file:///a/test.json").unwrap()];
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        roots.clone(),
        Default::default(),
        &loader,
        BuildOptions {
          is_dynamic: true,
          ..Default::default()
        },
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test.json"
        ],
        "modules": [
          {
            "size": 18,
            "kind": "asserted",
            "mediaType": "Json",
            "specifier": "file:///a/test.json"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_valid_type_missing() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"// @deno-types=./test02.d.ts
import * as a from "./test02.js";

console.log(a);
"#,
          },
        ),
        (
          "file:///a/test02.js",
          Source::Module {
            specifier: "file:///a/test02.js",
            maybe_headers: None,
            content: r#"export const b = "b";"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert!(graph.valid().is_ok());
  }

  #[tokio::test]
  async fn test_valid_code_missing() {
    let loader = setup(
      vec![(
        "file:///a/test01.ts",
        Source::Module {
          specifier: "file:///a/test01.ts",
          maybe_headers: None,
          content: r#"import * as a from "./test02.js";

console.log(a);
"#,
        },
      )],
      vec![],
    );
    let root_specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert!(graph.valid().is_err());
    assert_eq!(
      graph.valid().err().unwrap().to_string(),
      "Module not found \"file:///a/test02.js\"."
    );
  }

  #[tokio::test]
  async fn test_remote_import_data_url() {
    let loader = setup(
      vec![(
        "https://deno.land/main.ts",
        Source::Module {
          specifier: "https://deno.land/main.ts",
          maybe_headers: None,
          content: r#"import * as a from "data:application/typescript;base64,ZXhwb3J0IGNvbnN0IGEgPSAiYSI7CgpleHBvcnQgZW51bSBBIHsKICBBLAogIEIsCiAgQywKfQo=";

console.log(a);
"#,
        },
      )],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("https://deno.land/main.ts").unwrap();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert!(graph.valid().is_ok());
  }

  #[tokio::test]
  async fn test_remote_import_local_url() {
    for scheme in &["http", "https"] {
      let root_specifier =
        ModuleSpecifier::parse(&format!("{scheme}://deno.land/main.ts"))
          .unwrap();
      let loader = setup(
        vec![
          (
            root_specifier.as_str(),
            Source::Module {
              specifier: root_specifier.as_str(),
              maybe_headers: None,
              content: r#"import * as a from "file:///local.ts";

console.log(a);
"#,
            },
          ),
          (
            "file:///local.ts",
            Source::Module {
              specifier: "file:///local.ts",
              maybe_headers: None,
              content: r#"console.log(1);"#,
            },
          ),
        ],
        vec![],
      );
      let mut graph = ModuleGraph::new(GraphKind::All);
      graph
        .build(
          vec![root_specifier],
          Default::default(),
          &loader,
          Default::default(),
        )
        .await;
      assert!(matches!(
        graph.valid().err().unwrap(),
        ModuleGraphError::ResolutionError(
          ResolutionError::InvalidLocalImport { .. },
        )
      ));
    }
  }

  #[tokio::test]
  async fn test_remote_import_local_url_remapped() {
    for scheme in &["http", "https"] {
      let root_specifier_str = format!("{scheme}://deno.land/main.ts");
      let root_specifier = ModuleSpecifier::parse(&root_specifier_str).unwrap();
      let loader = setup(
        vec![
          (
            root_specifier.as_str(),
            Source::Module {
              specifier: root_specifier.as_str(),
              maybe_headers: None,
              content: r#"import * as a from "remapped";

console.log(a);
"#,
            },
          ),
          (
            "file:///local.ts",
            Source::Module {
              specifier: "file:///local.ts",
              maybe_headers: None,
              content: r#"console.log(1);"#,
            },
          ),
        ],
        vec![],
      );
      let resolver = MockResolver::new(
        vec![(
          root_specifier_str.as_str(),
          vec![("remapped", "file:///local.ts")],
        )],
        vec![],
      );
      let maybe_resolver: Option<&dyn Resolver> = Some(&resolver);
      let mut graph = ModuleGraph::new(GraphKind::All);
      graph
        .build(
          vec![root_specifier.clone()],
          Default::default(),
          &loader,
          BuildOptions {
            resolver: maybe_resolver,
            ..Default::default()
          },
        )
        .await;
      assert!(graph.valid().is_ok());
    }
  }

  #[tokio::test]
  async fn test_build_graph_imports() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"console.log("a");"#,
          },
        ),
        (
          "file:///a/types.d.ts",
          Source::Module {
            specifier: "file:///a/types.d.ts",
            maybe_headers: None,
            content: r#"export type { A } from "./types_01.d.ts";"#,
          },
        ),
        (
          "file:///a/types_01.d.ts",
          Source::Module {
            specifier: "file:///a/types_01.d.ts",
            maybe_headers: None,
            content: r#"export class A {};"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let config_specifier =
      ModuleSpecifier::parse("file:///a/tsconfig.json").unwrap();
    let imports = vec![ReferrerImports {
      referrer: config_specifier,
      imports: vec!["./types.d.ts".to_string()],
    }];
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier],
        imports,
        &loader,
        BuildOptions::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": ["file:///a/test01.ts"],
        "modules": [
          {
            "kind": "esm",
            "mediaType": "TypeScript",
            "size": 17,
            "specifier": "file:///a/test01.ts"
          },
          {
            "dependencies": [
              {
                "specifier": "./types_01.d.ts",
                "type": {
                  "specifier": "file:///a/types_01.d.ts",
                  "span": {
                    "start": {
                      "line":0,
                      "character":23
                    },
                    "end": {
                      "line":0,
                      "character":40
                    }
                  }
                }
              }
            ],
            "kind": "esm",
            "mediaType": "Dts",
            "size": 41,
            "specifier": "file:///a/types.d.ts"
          },
          {
            "kind": "esm",
            "mediaType": "Dts",
            "size": 18,
            "specifier": "file:///a/types_01.d.ts"
          }
        ],
        "imports": [
          {
            "referrer": "file:///a/tsconfig.json",
            "dependencies": [
              {
                "specifier": "./types.d.ts",
                "type": {
                  "specifier": "file:///a/types.d.ts",
                  "span": {
                    "start": {
                      "line": 0,
                      "character": 0
                    },
                    "end": {
                      "line": 0,
                      "character": 0
                    }
                  }
                }
              }
            ]
          },
        ],
        "redirects":{},
      })
    );
  }

  #[tokio::test]
  async fn test_build_graph_imports_imported() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"import config from "./deno.json" assert { type: "json" };

            console.log(config);"#,
          },
        ),
        (
          "file:///a/deno.json",
          Source::Module {
            specifier: "file:///a/deno.json",
            maybe_headers: None,
            content: r#"{
              "compilerOptions": {
                "jsxImportSource": "https://esm.sh/preact"
              }
            }"#,
          },
        ),
        (
          "https://esm.sh/preact/runtime-jsx",
          Source::Module {
            specifier: "https://esm.sh/preact/runtime-jsx",
            maybe_headers: Some(vec![(
              "content-type",
              "application/javascript",
            )]),
            content: r#"export function jsx() {}"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let config_specifier =
      ModuleSpecifier::parse("file:///a/deno.json").unwrap();
    let imports = vec![ReferrerImports {
      referrer: config_specifier,
      imports: vec!["https://esm.sh/preact/runtime-jsx".to_string()],
    }];
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier],
        imports,
        &loader,
        BuildOptions::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": ["file:///a/test01.ts"],
        "modules": [
          {
            "kind": "asserted",
            "size": 125,
            "mediaType": "Json",
            "specifier": "file:///a/deno.json",
          },
          {
            "dependencies": [
              {
                "specifier": "./deno.json",
                "code": {
                  "specifier": "file:///a/deno.json",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 0,
                      "character": 19,
                    },
                    "end": {
                      "line": 0,
                      "character": 32,
                    }
                  }
                },
                "assertionType": "json"
              }
            ],
            "kind": "esm",
            "size": 91,
            "mediaType": "TypeScript",
            "specifier": "file:///a/test01.ts",
          },
          {
            "kind": "esm",
            "size": 24,
            "mediaType": "JavaScript",
            "specifier": "https://esm.sh/preact/runtime-jsx",
          },
        ],
        "imports": [
          {
            "referrer": "file:///a/deno.json",
            "dependencies": [
              {
                "specifier": "https://esm.sh/preact/runtime-jsx",
                "type": {
                  "specifier": "https://esm.sh/preact/runtime-jsx",
                  "span": {
                    "start": {
                      "line": 0,
                      "character": 0,
                    },
                    "end": {
                      "line": 0,
                      "character": 0,
                    }
                  }
                },
              }
            ],
          }
        ],
        "redirects":{},
      })
    );
  }

  #[tokio::test]
  async fn test_build_graph_imports_resolve_dependency() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"console.log("a");"#,
          },
        ),
        (
          "https://example.com/jsx-runtime",
          Source::Module {
            specifier: "https://example.com/jsx-runtime",
            maybe_headers: Some(vec![
              ("content-type", "application/javascript"),
              ("x-typescript-types", "./jsx-runtime.d.ts"),
            ]),
            content: r#"export const a = "a";"#,
          },
        ),
        (
          "https://example.com/jsx-runtime.d.ts",
          Source::Module {
            specifier: "https://example.com/jsx-runtime.d.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"export const a: "a";"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let config_specifier =
      ModuleSpecifier::parse("file:///a/tsconfig.json").unwrap();
    let imports = vec![ReferrerImports {
      referrer: config_specifier.clone(),
      imports: vec!["https://example.com/jsx-runtime".to_string()],
    }];
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier],
        imports,
        &loader,
        BuildOptions::default(),
      )
      .await;
    assert_eq!(
      graph.resolve_dependency(
        "https://example.com/jsx-runtime",
        &config_specifier,
        false
      ),
      Some(ModuleSpecifier::parse("https://example.com/jsx-runtime").unwrap())
        .as_ref()
    );
    assert_eq!(
      graph.resolve_dependency(
        "https://example.com/jsx-runtime",
        &config_specifier,
        true
      ),
      Some(
        ModuleSpecifier::parse("https://example.com/jsx-runtime.d.ts").unwrap()
      )
      .as_ref()
    );
    assert_eq!(
      graph
        .try_get(
          &ModuleSpecifier::parse("https://example.com/jsx-runtime").unwrap()
        )
        .unwrap()
        .unwrap()
        .specifier()
        .as_str(),
      "https://example.com/jsx-runtime"
    );
    assert_eq!(
      graph
        .try_get_prefer_types(
          &ModuleSpecifier::parse("https://example.com/jsx-runtime").unwrap()
        )
        .unwrap()
        .unwrap()
        .specifier()
        .as_str(),
      // should end up at the declaration file
      "https://example.com/jsx-runtime.d.ts"
    );
  }

  #[tokio::test]
  async fn test_build_graph_with_headers() {
    let loader = setup(
      vec![(
        "https://example.com/a",
        Source::Module {
          specifier: "https://example.com/a",
          maybe_headers: Some(vec![(
            "content-type",
            "application/typescript; charset=utf-8",
          )]),
          content: r#"declare interface A { a: string; }"#,
        },
      )],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("https://example.com/a").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(graph.module_slots.len(), 1);
    assert_eq!(graph.roots, IndexSet::from([root_specifier.clone()]));
    let module = graph
      .module_slots
      .get(&root_specifier)
      .unwrap()
      .module()
      .unwrap()
      .js()
      .unwrap();
    assert_eq!(module.media_type, MediaType::TypeScript);
  }

  #[tokio::test]
  async fn test_build_graph_jsx_import_source() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.tsx",
          Source::Module {
            specifier: "file:///a/test01.tsx",
            maybe_headers: None,
            content: r#"/* @jsxImportSource https://example.com/preact */

            export function A() {
              <div>Hello Deno</div>
            }
            "#,
          },
        ),
        (
          "https://example.com/preact/jsx-runtime",
          Source::Module {
            specifier: "https://example.com/preact/jsx-runtime/index.js",
            maybe_headers: Some(vec![(
              "content-type",
              "application/javascript",
            )]),
            content: r#"export function jsx() {}"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.tsx").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test01.tsx"
        ],
        "modules": [
          {
            "dependencies": [
              {
                "specifier": "https://example.com/preact/jsx-runtime",
                "code": {
                  "specifier": "https://example.com/preact/jsx-runtime",
                  "span": {
                    "start": {
                      "line": 0,
                      "character": 20
                    },
                    "end": {
                      "line": 0,
                      "character": 46
                    }
                  }
                }
              }
            ],
            "kind": "esm",
            "mediaType": "TSX",
            "size": 147,
            "specifier": "file:///a/test01.tsx"
          },
          {
            "kind": "esm",
            "mediaType": "JavaScript",
            "size": 24,
            "specifier": "https://example.com/preact/jsx-runtime/index.js"
          }
        ],
        "redirects": {
          "https://example.com/preact/jsx-runtime": "https://example.com/preact/jsx-runtime/index.js"
        }
      })
    );
  }

  #[tokio::test]
  async fn test_build_graph_jsx_import_source_types() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.tsx",
          Source::Module {
            specifier: "file:///a/test01.tsx",
            maybe_headers: None,
            content: r#"/* @jsxImportSource https://example.com/preact */
            /* @jsxImportSourceTypes https://example.com/preact-types */

            export function A() {
              <div>Hello Deno</div>
            }
            "#,
          },
        ),
        (
          "https://example.com/preact/jsx-runtime",
          Source::Module {
            specifier: "https://example.com/preact/jsx-runtime/index.js",
            maybe_headers: Some(vec![(
              "content-type",
              "application/javascript",
            )]),
            content: r#"export function jsx() {}"#,
          },
        ),
        (
          "https://example.com/preact-types/jsx-runtime",
          Source::Module {
            specifier:
              "https://example.com/preact-types/jsx-runtime/index.d.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"export declare function jsx();"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.tsx").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test01.tsx"
        ],
        "modules": [
          {
            "dependencies": [
              {
                "specifier": "https://example.com/preact/jsx-runtime",
                "code": {
                  "specifier": "https://example.com/preact/jsx-runtime",
                  "span": {
                    "start": {
                      "line": 0,
                      "character": 20
                    },
                    "end": {
                      "line": 0,
                      "character": 46
                    }
                  }
                },
                "type": {
                  "specifier": "https://example.com/preact-types/jsx-runtime",
                  "span": {
                    "start": {
                      "line": 1,
                      "character": 37
                    },
                    "end": {
                      "line": 1,
                      "character": 69
                    }
                  }
                }
              }
            ],
            "kind": "esm",
            "mediaType": "TSX",
            "size": 220,
            "specifier": "file:///a/test01.tsx"
          },
          {
            "kind": "esm",
            "mediaType": "Dts",
            "size": 30,
            "specifier": "https://example.com/preact-types/jsx-runtime/index.d.ts"
          },
          {
            "kind": "esm",
            "mediaType": "JavaScript",
            "size": 24,
            "specifier": "https://example.com/preact/jsx-runtime/index.js"
          }
        ],
        "redirects": {
          "https://example.com/preact-types/jsx-runtime": "https://example.com/preact-types/jsx-runtime/index.d.ts",
          "https://example.com/preact/jsx-runtime": "https://example.com/preact/jsx-runtime/index.js"
        }
      })
    );
  }

  #[tokio::test]
  async fn test_bare_specifier_error() {
    let loader = setup(
      vec![(
        "file:///a/test.ts",
        Source::Module {
          specifier: "file:///a/test.ts",
          maybe_headers: None,
          content: r#"import "foo";"#,
        },
      )],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    let result = graph.valid();
    assert!(result.is_err());
    let err = result.unwrap_err();
    match err {
      ModuleGraphError::ResolutionError(err) => {
        assert_eq!(err.range().specifier, root_specifier)
      }
      _ => unreachable!(),
    }
  }

  #[tokio::test]
  async fn test_builtin_node_module() {
    let expectation = json!({
      "roots": [
        "file:///a/test.ts"
      ],
      "modules": [
        {
          "kind": "esm",
          "dependencies": [
            {
              "specifier": "node:path",
              "code": {
                "specifier": "node:path",
                "resolutionMode": "import",
                "span": {
                  "start": {
                    "line": 0,
                    "character": 7
                  },
                  "end": {
                    "line": 0,
                    "character": 18
                  }
                }
              },
            }
          ],
          "size": 19,
          "mediaType": "TypeScript",
          "specifier": "file:///a/test.ts"
        },
        {
          "kind": "node",
          "specifier": "node:path",
          "moduleName": "path",
        }
      ],
      "redirects": {}
    });

    let loader = setup(
      vec![(
        "file:///a/test.ts",
        Source::Module {
          specifier: "file:///a/test.ts",
          maybe_headers: None,
          content: r#"import "node:path";"#,
        },
      )],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    graph.valid().unwrap();
    assert_eq!(json!(graph), expectation);
  }

  #[tokio::test]
  async fn test_unsupported_media_type() {
    let loader = setup(
      vec![
        (
          "file:///a/test.ts",
          Source::Module {
            specifier: "file:///a/test.ts",
            maybe_headers: None,
            content: r#"import "./test.json";"#,
          },
        ),
        (
          "file:///a/test.json",
          Source::Module {
            specifier: "file:///a/test.json",
            maybe_headers: None,
            content: r#"{"hello":"world"}"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    let result = graph.valid();
    assert!(result.is_err());
    let err = result.unwrap_err();
    let err = match err {
      ModuleGraphError::ModuleError(err) => err,
      _ => unreachable!(),
    };
    assert!(matches!(
      err.as_kind(),
      ModuleErrorKind::UnsupportedMediaType {
        media_type: MediaType::Json,
        ..
      },
    ));
  }

  #[tokio::test]
  async fn test_root_is_extensionless() {
    let loader = setup(
      vec![
        (
          "file:///a/test01",
          Source::Module {
            specifier: "file:///a/test01",
            maybe_headers: None,
            content: r#"import * as b from "./test02.ts";"#,
          },
        ),
        (
          "file:///a/test02.ts",
          Source::Module {
            specifier: "file:///a/test02.ts",
            maybe_headers: None,
            content: r#"export const b = "b";"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert!(graph.valid().is_ok());
  }

  #[tokio::test]
  async fn test_crate_graph_with_dynamic_imports() {
    let loader = setup(
      vec![
        (
          "file:///a.ts",
          Source::Module {
            specifier: "file:///a.ts",
            maybe_headers: None,
            content: r#"const b = await import("./b.ts");"#,
          },
        ),
        (
          "file:///b.ts",
          Source::Module {
            specifier: "file:///b.ts",
            maybe_headers: None,
            content: r#"export const b = "b";"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a.ts"
        ],
        "modules": [
          {
            "dependencies": [
              {
                "specifier": "./b.ts",
                "code": {
                  "specifier": "file:///b.ts",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 0,
                      "character": 23
                    },
                    "end": {
                      "line": 0,
                      "character": 31
                    }
                  }
                },
                "isDynamic": true
              }
            ],
            "kind": "esm",
            "mediaType": "TypeScript",
            "size": 33,
            "specifier": "file:///a.ts"
          },
          {
            "kind": "esm",
            "mediaType": "TypeScript",
            "size": 21,
            "specifier": "file:///b.ts"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_build_graph_with_jsdoc_imports() {
    let loader = setup(
      vec![
        (
          "file:///a/test.js",
          Source::Module {
            specifier: "file:///a/test.js",
            maybe_headers: None,
            content: r#"
/**
 * Some js doc
 *
 * @param {import("./types.d.ts").A} a
 * @return {import("./other.ts").B}
 */
export function a(a) {
  return;
}
"#,
          },
        ),
        (
          "file:///a/types.d.ts",
          Source::Module {
            specifier: "file:///a/types.d.ts",
            maybe_headers: None,
            content: r#"export type A = string;"#,
          },
        ),
        (
          "file:///a/other.ts",
          Source::Module {
            specifier: "file:///a/other.ts",
            maybe_headers: None,
            content: r#"export type B = string | undefined;"#,
          },
        ),
      ],
      vec![],
    );
    let root = ModuleSpecifier::parse("file:///a/test.js").unwrap();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test.js"
        ],
        "modules": [
          {
            "kind": "esm",
            "mediaType": "TypeScript",
            "size": 35,
            "specifier": "file:///a/other.ts"
          },
          {
            "dependencies": [
              {
                "specifier": "./types.d.ts",
                "type": {
                  "specifier": "file:///a/types.d.ts",
                  "span": {
                    "start": {
                      "line": 4,
                      "character": 18
                    },
                    "end": {
                      "line": 4,
                      "character": 32
                    }
                  }
                }
              },
              {
                "specifier": "./other.ts",
                "type": {
                  "specifier": "file:///a/other.ts",
                  "span": {
                    "start": {
                      "line": 5,
                      "character": 19
                    },
                    "end": {
                      "line": 5,
                      "character": 31
                    }
                  }
                }
              },
            ],
            "kind": "esm",
            "mediaType": "JavaScript",
            "size": 137,
            "specifier": "file:///a/test.js"
          },
          {
            "kind": "esm",
            "mediaType": "Dts",
            "size": 23,
            "specifier": "file:///a/types.d.ts"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_build_graph_with_redirects() {
    let loader = setup(
      vec![
        (
          "https://example.com/a",
          Source::Module {
            specifier: "https://example.com/a.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"import * as b from "./b";"#,
          },
        ),
        (
          "https://example.com/b",
          Source::Module {
            specifier: "https://example.com/b.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"export const b = "b";"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("https://example.com/a").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      graph.roots,
      IndexSet::from(
        [ModuleSpecifier::parse("https://example.com/a").unwrap()]
      ),
    );
    assert_eq!(graph.module_slots.len(), 2);
    assert!(
      graph.contains(&ModuleSpecifier::parse("https://example.com/a").unwrap())
    );
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/a.ts").unwrap()));
    assert!(
      graph.contains(&ModuleSpecifier::parse("https://example.com/b").unwrap())
    );
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/b.ts").unwrap()));
    assert_eq!(
      json!(graph.redirects),
      json!({
        "https://example.com/a": "https://example.com/a.ts",
        "https://example.com/b": "https://example.com/b.ts",
      })
    );
    let graph_json = json!(graph).to_string();
    assert!(graph_json.contains("https://example.com/a.ts"));
    assert!(graph_json.contains("https://example.com/b.ts"));
  }

  #[tokio::test]
  async fn test_build_graph_with_circular_redirects() {
    let loader = setup(
      vec![
        (
          "https://example.com/a",
          Source::Module {
            specifier: "https://example.com/a.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"import * as b from "./b";"#,
          },
        ),
        (
          "https://example.com/b",
          Source::Module {
            specifier: "https://example.com/b.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"import * as a from "./a";
            export const b = "b";"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("https://example.com/a").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      graph.roots,
      IndexSet::from(
        [ModuleSpecifier::parse("https://example.com/a").unwrap()]
      ),
    );
    assert_eq!(graph.module_slots.len(), 2);
    assert!(
      graph.contains(&ModuleSpecifier::parse("https://example.com/a").unwrap())
    );
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/a.ts").unwrap()));
    assert!(
      graph.contains(&ModuleSpecifier::parse("https://example.com/b").unwrap())
    );
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/b.ts").unwrap()));
    assert_eq!(
      json!(graph.redirects),
      json!({
        "https://example.com/a": "https://example.com/a.ts",
        "https://example.com/b": "https://example.com/b.ts",
      })
    );
    let graph_json = json!(graph).to_string();
    assert!(graph_json.contains("https://example.com/a.ts"));
    assert!(graph_json.contains("https://example.com/b.ts"));
  }

  #[tokio::test]
  async fn test_build_graph_with_data_url() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"import * as b from "data:application/typescript,export%20*%20from%20%22https://example.com/c.ts%22;";"#,
          },
        ),
        (
          "https://example.com/c.ts",
          Source::Module {
            specifier: "https://example.com/c.ts",
            maybe_headers: None,
            content: r#"export const c = """#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(graph.module_slots.len(), 3);
    let data_specifier = ModuleSpecifier::parse("data:application/typescript,export%20*%20from%20%22https://example.com/c.ts%22;").unwrap();
    let module = graph.get(&data_specifier).unwrap().js().unwrap();
    assert_eq!(
      module.source.text.as_ref(),
      r#"export * from "https://example.com/c.ts";"#,
    );
  }

  #[tokio::test]
  async fn test_build_graph_with_reference_types_in_js() {
    let loader = setup(
      vec![
        (
          "file:///test01.js",
          Source::Module {
            specifier: "file:///test01.js",
            maybe_headers: None,
            content: r#"/// <reference types="./test01.d.ts" />
export const foo = 'bar';"#,
          },
        ),
        (
          "file:///test01.d.ts",
          Source::Module {
            specifier: "file:///test01.d.ts",
            maybe_headers: None,
            content: r#"export declare const foo: string;"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///test01.js").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(graph.module_slots.len(), 2);
    let module = graph.get(&root_specifier).unwrap().js().unwrap();
    assert_eq!(module.dependencies.len(), 0);
    let types_dep = module.maybe_types_dependency.as_ref().unwrap();
    assert_eq!(types_dep.specifier, "./test01.d.ts");
    assert_eq!(
      *types_dep.dependency.ok().unwrap(),
      ResolutionResolved {
        specifier: ModuleSpecifier::parse("file:///test01.d.ts").unwrap(),
        range: Range {
          specifier: ModuleSpecifier::parse("file:///test01.js").unwrap(),
          range: PositionRange {
            start: Position {
              line: 0,
              character: 21
            },
            end: Position {
              line: 0,
              character: 36
            },
          },
          resolution_mode: None,
        }
      }
    );
  }

  #[tokio::test]
  async fn test_build_graph_with_self_types_in_js() {
    let loader = setup(
      vec![
        (
          "file:///test01.js",
          Source::Module {
            specifier: "file:///test01.js",
            maybe_headers: None,
            content: r#"// @ts-self-types="./test01.d.ts"
export const foo = 'bar';"#,
          },
        ),
        (
          "file:///test01.d.ts",
          Source::Module {
            specifier: "file:///test01.d.ts",
            maybe_headers: None,
            content: r#"export declare const foo: string;"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///test01.js").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(graph.module_slots.len(), 2);
    let module = graph.get(&root_specifier).unwrap().js().unwrap();
    assert_eq!(module.dependencies.len(), 0);
    let types_dep = module.maybe_types_dependency.as_ref().unwrap();
    assert_eq!(types_dep.specifier, "./test01.d.ts");
    assert_eq!(
      *types_dep.dependency.ok().unwrap(),
      ResolutionResolved {
        specifier: ModuleSpecifier::parse("file:///test01.d.ts").unwrap(),
        range: Range {
          specifier: ModuleSpecifier::parse("file:///test01.js").unwrap(),
          range: PositionRange {
            start: Position {
              line: 0,
              character: 18
            },
            end: Position {
              line: 0,
              character: 33
            },
          },
          resolution_mode: None,
        }
      }
    );
  }

  #[tokio::test]
  async fn test_build_graph_with_self_types_in_ts() {
    let loader = setup(
      vec![
        (
          "file:///test01.ts",
          Source::Module {
            specifier: "file:///test01.ts",
            maybe_headers: None,
            content: r#"// @ts-self-types="./test01.d.ts"
export const foo = 'bar';"#,
          },
        ),
        (
          "file:///test01.d.ts",
          Source::Module {
            specifier: "file:///test01.d.ts",
            maybe_headers: None,
            content: r#"export declare const foo: string;"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(graph.module_slots.len(), 1);
    let module = graph.get(&root_specifier).unwrap().js().unwrap();
    assert_eq!(module.dependencies.len(), 0);
    assert!(module.maybe_types_dependency.is_none());
  }

  #[tokio::test]
  async fn test_build_graph_with_resolver() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"import * as b from "b";"#,
          },
        ),
        (
          "file:///a/test02.ts",
          Source::Module {
            specifier: "file:///a/test02.ts",
            maybe_headers: None,
            content: r#"export const b = "b";"#,
          },
        ),
      ],
      vec![],
    );
    let resolver = MockResolver::new(
      vec![("file:///a/test01.ts", vec![("b", "file:///a/test02.ts")])],
      vec![],
    );
    let maybe_resolver: Option<&dyn Resolver> = Some(&resolver);
    let root_specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        BuildOptions {
          resolver: maybe_resolver,
          ..Default::default()
        },
      )
      .await;
    let module = graph.get(&root_specifier).unwrap().js().unwrap();
    let maybe_dep = module.dependencies.get("b");
    assert!(maybe_dep.is_some());
    let dep = maybe_dep.unwrap();
    assert_eq!(
      dep.maybe_code.maybe_specifier().unwrap(),
      &ModuleSpecifier::parse("file:///a/test02.ts").unwrap()
    );
  }

  #[tokio::test]
  async fn test_build_graph_with_resolve_types() {
    let loader = setup(
      vec![
        (
          "file:///a.js",
          Source::Module {
            specifier: "file:///a.js",
            maybe_headers: None,
            content: r#"export const a = "a";"#,
          },
        ),
        (
          "file:///a.d.ts",
          Source::Module {
            specifier: "file:///a.d.ts",
            maybe_headers: None,
            content: r#"export const a: "a";"#,
          },
        ),
      ],
      vec![],
    );
    let resolver = MockResolver::new(
      vec![],
      vec![(
        "file:///a.js",
        (
          "file:///a.d.ts",
          Some(Range {
            specifier: ModuleSpecifier::parse("file:///package.json").unwrap(),
            range: PositionRange::zeroed(),
            resolution_mode: None,
          }),
        ),
      )],
    );
    let maybe_resolver: Option<&dyn Resolver> = Some(&resolver);
    let root_specifier = ModuleSpecifier::parse("file:///a.js").unwrap();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        BuildOptions {
          resolver: maybe_resolver,
          ..Default::default()
        },
      )
      .await;
    let module = graph.get(&root_specifier).unwrap().js().unwrap();
    let types_dep = module.maybe_types_dependency.as_ref().unwrap();
    assert_eq!(types_dep.specifier, "file:///a.js");
    assert_eq!(
      *types_dep.dependency.ok().unwrap(),
      ResolutionResolved {
        specifier: ModuleSpecifier::parse("file:///a.d.ts").unwrap(),
        range: Range {
          specifier: ModuleSpecifier::parse("file:///package.json").unwrap(),
          range: PositionRange::zeroed(),
          resolution_mode: None,
        }
      }
    );
  }

  #[tokio::test]
  async fn test_build_graph_import_attributes() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            import a from "./a.json" with { type: "json" };
            const b = await import("./b.json", { with: { type: "json" } });
            export * as c from "./c.json" with { type: "json" };
            const json = "json";
            const d = await import("./d.json", { with: { type: json } });
            "#,
          },
        ),
        (
          "file:///a/a.json",
          Source::Module {
            specifier: "file:///a/a.json",
            maybe_headers: None,
            content: r#"{"a":"b"}"#,
          },
        ),
        (
          "file:///a/b.json",
          Source::Module {
            specifier: "file:///a/b.json",
            maybe_headers: None,
            content: r#"{"b":1}"#,
          },
        ),
        (
          "file:///a/c.json",
          Source::Module {
            specifier: "file:///a/c.json",
            maybe_headers: None,
            content: r#"{"c":"d"}"#,
          },
        ),
        (
          "file:///a/d.json",
          Source::Module {
            specifier: "file:///a/d.json",
            maybe_headers: None,
            content: r#"{"d":4}"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test01.ts"
        ],
        "modules": [
          {
            "kind": "asserted",
            "size": 9,
            "mediaType": "Json",
            "specifier": "file:///a/a.json"
          },
          {
            "kind": "asserted",
            "size": 7,
            "mediaType": "Json",
            "specifier": "file:///a/b.json"
          },
          {
            "kind": "asserted",
            "size": 9,
            "mediaType": "Json",
            "specifier": "file:///a/c.json"
          },
          {
            "kind": "asserted",
            "size": 7,
            "mediaType": "Json",
            "specifier": "file:///a/d.json"
          },
          {
            "dependencies": [
              {
                "specifier": "./a.json",
                "code": {
                  "specifier": "file:///a/a.json",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 1,
                      "character": 26
                    },
                    "end": {
                      "line": 1,
                      "character": 36
                    }
                  }
                },
                "assertionType": "json"
              },
              {
                "specifier": "./b.json",
                "code": {
                  "specifier": "file:///a/b.json",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 2,
                      "character": 35
                    },
                    "end": {
                      "line": 2,
                      "character": 45
                    }
                  }
                },
                "isDynamic": true,
                "assertionType": "json"
              },
              {
                "specifier": "./c.json",
                "code": {
                  "specifier": "file:///a/c.json",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 3,
                      "character": 31
                    },
                    "end": {
                      "line": 3,
                      "character": 41
                    }
                  }
                },
                "assertionType": "json"
              },
              {
                "specifier": "./d.json",
                "code": {
                  "specifier": "file:///a/d.json",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 5,
                      "character": 35
                    },
                    "end": {
                      "line": 5,
                      "character": 45
                    }
                  }
                },
                "isDynamic": true
              }
            ],
            "kind": "esm",
            "mediaType": "TypeScript",
            "size": 321,
            "specifier": "file:///a/test01.ts"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_build_graph_mixed_assertions() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            import a from "./a.json";
            import b from "./a.json" assert { type: "json" };
            "#,
          },
        ),
        (
          "file:///a/a.json",
          Source::Module {
            specifier: "file:///a/a.json",
            maybe_headers: None,
            content: r#"{"a":"b"}"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test01.ts"
        ],
        "modules": [
          {
            "kind": "asserted",
            "size": 9,
            "mediaType": "Json",
            "specifier": "file:///a/a.json"
          },
          {
            "dependencies": [
              {
                "specifier": "./a.json",
                "code": {
                  "specifier": "file:///a/a.json",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 1,
                      "character": 26
                    },
                    "end": {
                      "line": 1,
                      "character": 36
                    }
                  }
                },
                "assertionType": "json"
              }
            ],
            "kind": "esm",
            "size": 113,
            "mediaType": "TypeScript",
            "specifier": "file:///a/test01.ts"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_build_graph_import_assertion_errors() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            import a from "./a.json";
            import b from "./b.json" assert { type: "json" };
            import c from "./c.js" assert { type: "json" };
            import d from "./d.json" assert { type: "css" };
            import e from "./e.wasm";
            "#,
          },
        ),
        (
          "file:///a/a.json",
          Source::Module {
            specifier: "file:///a/a.json",
            maybe_headers: None,
            content: r#"{"a":"b"}"#,
          },
        ),
        (
          "file:///a/b.json",
          Source::Module {
            specifier: "file:///a/b.json",
            maybe_headers: None,
            content: r#"{"a":"b"}"#,
          },
        ),
        (
          "file:///a/c.js",
          Source::Module {
            specifier: "file:///a/c.js",
            maybe_headers: None,
            content: r#"export const c = "c";"#,
          },
        ),
        (
          "file:///a/d.json",
          Source::Module {
            specifier: "file:///a/d.json",
            maybe_headers: None,
            content: r#"{"a":"b"}"#,
          },
        ),
        (
          "file:///a/e.wasm",
          Source::Module {
            specifier: "file:///a/e.wasm",
            maybe_headers: None,
            content: "",
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test01.ts"
        ],
        "modules": [
          {
            "specifier": "file:///a/a.json",
            "error": "Expected a JavaScript or TypeScript module, but identified a Json module. Consider importing Json modules with an import attribute with the type of \"json\".\n  Specifier: file:///a/a.json"
          },
          {
            "kind": "asserted",
            "size": 9,
            "mediaType": "Json",
            "specifier": "file:///a/b.json"
          },
          {
            "specifier": "file:///a/c.js",
            "error": "Expected a Json module, but identified a JavaScript module.\n  Specifier: file:///a/c.js"
          },
          {
            "specifier": "file:///a/d.json",
            "error": "The import attribute type of \"css\" is unsupported.\n  Specifier: file:///a/d.json"
          },
          {
            "specifier": "file:///a/e.wasm",
            "error": "The Wasm module could not be parsed: not a Wasm module\n  Specifier: file:///a/e.wasm"
          },
          {
            "dependencies": [
              {
                "specifier": "./a.json",
                "code": {
                  "specifier": "file:///a/a.json",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 1,
                      "character": 26
                    },
                    "end": {
                      "line": 1,
                      "character": 36
                    }
                  }
                }
              },
              {
                "specifier": "./b.json",
                "code": {
                  "specifier": "file:///a/b.json",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 2,
                      "character": 26
                    },
                    "end": {
                      "line": 2,
                      "character": 36
                    }
                  }
                },
                "assertionType": "json"
              },
              {
                "specifier": "./c.js",
                "code": {
                  "specifier": "file:///a/c.js",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 3,
                      "character": 26
                    },
                    "end": {
                      "line": 3,
                      "character": 34
                    }
                  }
                },
                "assertionType": "json"
              },
              {
                "specifier": "./d.json",
                "code": {
                  "specifier": "file:///a/d.json",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 4,
                      "character": 26
                    },
                    "end": {
                      "line": 4,
                      "character": 36
                    }
                  }
                },
                "assertionType": "css"
              },
              {
                "specifier": "./e.wasm",
                "code": {
                  "specifier": "file:///a/e.wasm",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 5,
                      "character": 26
                    },
                    "end": {
                      "line": 5,
                      "character": 36
                    }
                  }
                }
              }
            ],
            "mediaType": "TypeScript",
            "kind": "esm",
            "size": 272,
            "specifier": "file:///a/test01.ts"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[derive(Debug, Default)]
  struct CollectingReporter {
    on_loads: Mutex<Vec<(ModuleSpecifier, usize, usize)>>,
    on_resolves: Mutex<Vec<(PackageReq, PackageNv)>>,
  }

  impl source::Reporter for CollectingReporter {
    fn on_load(
      &self,
      specifier: &ModuleSpecifier,
      modules_done: usize,
      modules_total: usize,
    ) {
      self.on_loads.lock().push((
        specifier.clone(),
        modules_done,
        modules_total,
      ));
    }

    fn on_resolve(&self, req: &PackageReq, package_nv: &PackageNv) {
      self
        .on_resolves
        .lock()
        .push((req.clone(), package_nv.clone()));
    }
  }

  #[tokio::test]
  async fn test_build_graph_with_reporter() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            import "./a.js";
            "#,
          },
        ),
        (
          "file:///a/a.js",
          Source::Module {
            specifier: "file:///a/a.js",
            maybe_headers: None,
            content: r#"import "./b.js";"#,
          },
        ),
        (
          "file:///a/b.js",
          Source::Module {
            specifier: "file:///a/b.js",
            maybe_headers: None,
            content: r#"import "./c.js"; import "./d.js""#,
          },
        ),
        (
          "file:///a/c.js",
          Source::Module {
            specifier: "file:///a/c.js",
            maybe_headers: None,
            content: r#""#,
          },
        ),
        (
          "file:///a/d.js",
          Source::Module {
            specifier: "file:///a/d.js",
            maybe_headers: None,
            content: r#""#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let reporter = CollectingReporter::default();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        BuildOptions {
          reporter: Some(&reporter),
          ..Default::default()
        },
      )
      .await;
    assert_eq!(graph.modules().count(), 5);

    let on_loads = reporter.on_loads.into_inner();
    assert_eq!(on_loads.len(), 5);

    assert_eq!(
      on_loads[0],
      (ModuleSpecifier::parse("file:///a/test01.ts").unwrap(), 1, 2)
    );
    assert_eq!(
      on_loads[1],
      (ModuleSpecifier::parse("file:///a/a.js").unwrap(), 2, 3)
    );
    assert_eq!(
      on_loads[2],
      (ModuleSpecifier::parse("file:///a/b.js").unwrap(), 3, 5)
    );
    let c = ModuleSpecifier::parse("file:///a/c.js").unwrap();
    if on_loads[3].0 == c {
      assert_eq!(
        on_loads[3],
        (ModuleSpecifier::parse("file:///a/c.js").unwrap(), 4, 5)
      );
      assert_eq!(
        on_loads[4],
        (ModuleSpecifier::parse("file:///a/d.js").unwrap(), 5, 5)
      );
    } else {
      assert_eq!(
        on_loads[3],
        (ModuleSpecifier::parse("file:///a/d.js").unwrap(), 4, 5)
      );
      assert_eq!(
        on_loads[4],
        (ModuleSpecifier::parse("file:///a/c.js").unwrap(), 5, 5)
      );
    }
  }

  #[tokio::test]
  async fn test_build_graph_types_only() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            // @deno-types="./a.d.ts"
            import * as a from "./a.js";
            import type { B } from "./b.d.ts";
            import * as c from "https://example.com/c";
            import * as d from "./d.js";
            "#,
          },
        ),
        (
          "file:///a/a.js",
          Source::Module {
            specifier: "file:///a/a.js",
            maybe_headers: None,
            content: r#"export const a = "a""#,
          },
        ),
        (
          "file:///a/a.d.ts",
          Source::Module {
            specifier: "file:///a/a.d.ts",
            maybe_headers: None,
            content: r#"export const a: "a";"#,
          },
        ),
        (
          "file:///a/b.d.ts",
          Source::Module {
            specifier: "file:///a/b.d.ts",
            maybe_headers: None,
            content: r#"export interface B {}"#,
          },
        ),
        (
          "https://example.com/c",
          Source::Module {
            specifier: "https://example.com/c",
            maybe_headers: Some(vec![
              ("x-typescript-types", "./c.d.ts"),
              ("content-type", "application/javascript"),
            ]),
            content: r#"export { c } from "./c.js";"#,
          },
        ),
        (
          "https://example.com/c.d.ts",
          Source::Module {
            specifier: "https://example.com/c.d.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"export const c: "c";"#,
          },
        ),
        (
          "https://example.com/c.js",
          Source::Module {
            specifier: "https://example.com/c.js",
            maybe_headers: Some(vec![(
              "content-type",
              "application/javascript",
            )]),
            content: r#"export const c = "c";"#,
          },
        ),
        (
          "file:///a/d.js",
          Source::Module {
            specifier: "file:///a/d.js",
            maybe_headers: None,
            content: r#"export const d = "d";"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test01.ts"
        ],
        "modules": [
          {
            "kind": "esm",
            "mediaType": "Dts",
            "size": 20,
            "specifier": "file:///a/a.d.ts"
          },
          {
            "kind": "esm",
            "mediaType": "Dts",
            "size": 21,
            "specifier": "file:///a/b.d.ts"
          },
          {
            "kind": "esm",
            "mediaType": "JavaScript",
            "size": 21,
            "specifier": "file:///a/d.js"
          },
          {
            "dependencies": [
              {
                "specifier": "./a.js",
                "type": {
                  "specifier": "file:///a/a.d.ts",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 1,
                      "character": 27
                    },
                    "end": {
                      "line": 1,
                      "character": 37
                    }
                  }
                }
              },
              {
                "specifier": "./b.d.ts",
                "type": {
                  "specifier": "file:///a/b.d.ts",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 3,
                      "character": 35
                    },
                    "end": {
                      "line": 3,
                      "character": 45
                    }
                  }
                }
              },
              {
                "specifier": "https://example.com/c",
                "code": {
                  "specifier": "https://example.com/c",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 4,
                      "character": 31
                    },
                    "end": {
                      "line": 4,
                      "character": 54
                    }
                  }
                }
              },
              {
                "specifier": "./d.js",
                "code": {
                  "specifier": "file:///a/d.js",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 5,
                      "character": 31
                    },
                    "end": {
                      "line": 5,
                      "character": 39
                    }
                  }
                }
              },
            ],
            "kind": "esm",
            "mediaType": "TypeScript",
            "size": 236,
            "specifier": "file:///a/test01.ts"
          },
          {
            "typesDependency": {
              "specifier": "./c.d.ts",
              "dependency": {
                "specifier": "https://example.com/c.d.ts",
                "span": {
                  "start": {
                    "line": 0,
                    "character": 0
                  },
                  "end": {
                    "line": 0,
                    "character": 0
                  }
                }
              }
            },
            "kind": "esm",
            "mediaType": "JavaScript",
            "size": 27,
            "specifier": "https://example.com/c"
          },
          {
            "kind": "esm",
            "mediaType": "Dts",
            "size": 20,
            "specifier": "https://example.com/c.d.ts"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_build_graph_code_only() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            // @deno-types="./a.d.ts"
            import * as a from "./a.js";
            import type { B } from "./b.d.ts";
            import * as c from "https://example.com/c";
            import * as d from "./d.js";
            "#,
          },
        ),
        (
          "file:///a/a.js",
          Source::Module {
            specifier: "file:///a/a.js",
            maybe_headers: None,
            content: r#"export const a = "a""#,
          },
        ),
        (
          "file:///a/a.d.ts",
          Source::Module {
            specifier: "file:///a/a.d.ts",
            maybe_headers: None,
            content: r#"export const a: "a";"#,
          },
        ),
        (
          "file:///a/b.d.ts",
          Source::Module {
            specifier: "file:///a/b.d.ts",
            maybe_headers: None,
            content: r#"export interface B {}"#,
          },
        ),
        (
          "https://example.com/c",
          Source::Module {
            specifier: "https://example.com/c",
            maybe_headers: Some(vec![
              ("x-typescript-types", "./c.d.ts"),
              ("content-type", "application/javascript"),
            ]),
            content: r#"export { c } from "./c.js";"#,
          },
        ),
        (
          "https://example.com/c.d.ts",
          Source::Module {
            specifier: "https://example.com/c.d.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"export const c: "c";"#,
          },
        ),
        (
          "https://example.com/c.js",
          Source::Module {
            specifier: "https://example.com/c.js",
            maybe_headers: Some(vec![(
              "content-type",
              "application/javascript",
            )]),
            content: r#"export const c = "c";"#,
          },
        ),
        (
          "file:///a/d.js",
          Source::Module {
            specifier: "file:///a/d.js",
            maybe_headers: None,
            content: r#"export const d = "d";"#,
          },
        ),
        (
          "file:///a/test04.js",
          Source::Module {
            specifier: "file:///a/test04.js",
            maybe_headers: None,
            content: r#"/// <reference types="./test04.d.ts" />\nexport const d = "d";"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::CodeOnly);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test01.ts"
        ],
        "modules": [
          {
            "mediaType": "JavaScript",
            "kind": "esm",
            "size": 20,
            "specifier": "file:///a/a.js"
          },
          {
            "mediaType": "JavaScript",
            "kind": "esm",
            "size": 21,
            "specifier": "file:///a/d.js"
          },
          {
            "dependencies": [
              {
                "specifier": "./a.js",
                "code": {
                  "specifier": "file:///a/a.js",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 2,
                      "character": 31
                    },
                    "end": {
                      "line": 2,
                      "character": 39
                    }
                  }
                }
              },
              {
                "specifier": "https://example.com/c",
                "code": {
                  "specifier": "https://example.com/c",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 4,
                      "character": 31
                    },
                    "end": {
                      "line": 4,
                      "character": 54
                    }
                  }
                }
              },
              {
                "specifier": "./d.js",
                "code": {
                  "specifier": "file:///a/d.js",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 5,
                      "character": 31
                    },
                    "end": {
                      "line": 5,
                      "character": 39
                    }
                  }
                }
              },
            ],
            "mediaType": "TypeScript",
            "kind": "esm",
            "size": 236,
            "specifier": "file:///a/test01.ts"
          },
          {
            "dependencies": [
              {
                "specifier": "./c.js",
                "code": {
                  "specifier": "https://example.com/c.js",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 0,
                      "character": 18
                    },
                    "end": {
                      "line": 0,
                      "character": 26
                    }
                  }
                }
              }
            ],
            "mediaType": "JavaScript",
            "kind": "esm",
            "size": 27,
            "specifier": "https://example.com/c"
          },
          {
            "mediaType": "JavaScript",
            "kind": "esm",
            "size": 21,
            "specifier": "https://example.com/c.js"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_build_graph_with_builtin_external() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            import * as fs from "builtin:fs";
            import * as bundle from "https://example.com/bundle";
            "#,
          },
        ),
        ("builtin:fs", Source::External("builtin:fs")),
        (
          "https://example.com/bundle",
          Source::External("https://example.com/bundle"),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        Default::default(),
      )
      .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test01.ts"
        ],
        "modules": [
          {
            "kind": "external",
            "specifier": "builtin:fs"
          },
          {
            "dependencies": [
              {
                "specifier": "builtin:fs",
                "code": {
                  "specifier": "builtin:fs",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 1,
                      "character": 32
                    },
                    "end": {
                      "line": 1,
                      "character": 44
                    }
                  }
                }
              },
              {
                "specifier": "https://example.com/bundle",
                "code": {
                  "specifier": "https://example.com/bundle",
                  "resolutionMode": "import",
                  "span": {
                    "start": {
                      "line": 2,
                      "character": 36
                    },
                    "end": {
                      "line": 2,
                      "character": 64
                    }
                  }
                }
              }
            ],
            "kind": "esm",
            "size": 125,
            "mediaType": "TypeScript",
            "specifier": "file:///a/test01.ts"
          },
          {
            "kind": "external",
            "specifier": "https://example.com/bundle"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_parse_module() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let code = br#"
    /// <reference types="./a.d.ts" />
    import { a } from "./a.ts";
    import * as b from "./b.ts";
    export { c } from "./c.ts";
    const d = await import("./d.ts");
    import type { e } from "./e.ts";
    export type { f } from "./f.ts";
    "#;
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: code.to_vec().into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    let actual = actual.js().unwrap();
    assert_eq!(actual.dependencies.len(), 7);
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::TypeScript);

    // now try code only
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::CodeOnly,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: code.to_vec().into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    let actual = actual.js().unwrap();
    assert_eq!(actual.dependencies.len(), 4);
  }

  #[tokio::test]
  async fn test_parse_module_deno_types() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let code = br#"
    // @deno-types="./a.d.ts"
    import { a } from "./a.js";
    "#;
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: code.to_vec().into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    let actual = actual.js().unwrap();
    assert_eq!(
      &actual.dependencies,
      &[(
        "./a.js".to_string(),
        Dependency {
          maybe_code: Resolution::Ok(Box::new(ResolutionResolved {
            specifier: ModuleSpecifier::parse("file:///a/a.js").unwrap(),
            range: Range {
              specifier: specifier.clone(),
              range: PositionRange {
                start: Position::new(2, 22),
                end: Position::new(2, 30),
              },
              resolution_mode: Some(ResolutionMode::Import),
            },
          })),
          maybe_type: Resolution::Ok(Box::new(ResolutionResolved {
            specifier: ModuleSpecifier::parse("file:///a/a.d.ts").unwrap(),
            range: Range {
              specifier: specifier.clone(),
              range: PositionRange {
                start: Position::new(1, 19),
                end: Position::new(1, 29),
              },
              resolution_mode: Some(ResolutionMode::Import),
            },
          })),
          maybe_deno_types_specifier: Some("./a.d.ts".to_string()),
          imports: vec![Import {
            specifier: "./a.js".to_string(),
            kind: ImportKind::Es,
            specifier_range: Range {
              specifier: specifier.clone(),
              range: PositionRange {
                start: Position::new(2, 22),
                end: Position::new(2, 30),
              },
              resolution_mode: Some(ResolutionMode::Import),
            },
            is_dynamic: false,
            attributes: Default::default(),
            is_side_effect: false,
          }],
          ..Default::default()
        },
      )]
      .into_iter()
      .collect::<IndexMap<_, _>>()
    )
  }

  #[tokio::test]
  async fn test_parse_module_import_assertions() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier,
      maybe_headers: None,
      mtime: None,
      content: br#"
    import a from "./a.json" assert { type: "json" };
    await import("./b.json", { assert: { type: "json" } });
    "#
      .to_vec()
      .into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    assert_eq!(
      json!(actual),
      json!({
        "kind": "esm",
        "dependencies": [
          {
            "specifier": "./a.json",
            "code": {
              "specifier": "file:///a/a.json",
              "resolutionMode": "import",
              "span": {
                "start": {
                  "line": 1,
                  "character": 18
                },
                "end": {
                  "line": 1,
                  "character": 28
                }
              }
            },
            "assertionType": "json"
          },
          {
            "specifier": "./b.json",
            "code": {
              "specifier": "file:///a/b.json",
              "resolutionMode": "import",
              "span": {
                "start": {
                  "line": 2,
                  "character": 17
                },
                "end": {
                  "line": 2,
                  "character": 27
                }
              }
            },
            "isDynamic": true,
            "assertionType": "json"
          }
        ],
        "mediaType": "TypeScript",
        "size": 119,
        "specifier": "file:///a/test01.ts"
      })
    );
  }

  #[tokio::test]
  async fn test_parse_module_jsx_import_source() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.tsx").unwrap();
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: br#"
    /** @jsxImportSource https://example.com/preact */

    export function A() {
      return <div>Hello Deno</div>;
    }
    "#
      .to_vec()
      .into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    let actual = actual.js().unwrap();
    assert_eq!(actual.dependencies.len(), 1);
    let dep = actual
      .dependencies
      .get("https://example.com/preact/jsx-runtime")
      .unwrap();
    assert_eq!(
      dep.maybe_code.ok().unwrap().specifier,
      ModuleSpecifier::parse("https://example.com/preact/jsx-runtime").unwrap()
    );
    assert!(dep.maybe_type.is_none());
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::Tsx);
  }

  #[tokio::test]
  async fn test_parse_module_jsx_import_source_types() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.tsx").unwrap();
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: br#"
    /** @jsxImportSource https://example.com/preact */
    /** @jsxImportSourceTypes https://example.com/preact-types */

    export function A() {
      return <div>Hello Deno</div>;
    }
    "#
      .to_vec()
      .into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    let actual = actual.js().unwrap();
    assert_eq!(actual.dependencies.len(), 1);
    let dep = actual
      .dependencies
      .get("https://example.com/preact/jsx-runtime")
      .unwrap();
    assert_eq!(
      dep.maybe_code.ok().unwrap().specifier,
      ModuleSpecifier::parse("https://example.com/preact/jsx-runtime").unwrap()
    );
    assert_eq!(
      dep.maybe_type.ok().unwrap().specifier,
      ModuleSpecifier::parse("https://example.com/preact-types/jsx-runtime")
        .unwrap()
    );
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::Tsx);
  }

  #[tokio::test]
  async fn test_parse_module_jsx_import_source_types_pragma() {
    #[derive(Debug)]
    struct R;
    impl Resolver for R {
      fn default_jsx_import_source(
        &self,
        _referrer: &ModuleSpecifier,
      ) -> Option<String> {
        Some("https://example.com/preact".into())
      }
    }

    let specifier = ModuleSpecifier::parse("file:///a/test01.tsx").unwrap();
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: br#"
    /** @jsxImportSourceTypes https://example.com/preact-types */

    export function A() {
      return <div>Hello Deno</div>;
    }
    "#
      .to_vec()
      .into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: Some(&R),
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    let actual = actual.js().unwrap();
    assert_eq!(actual.dependencies.len(), 1);
    let dep = actual
      .dependencies
      .get("https://example.com/preact/jsx-runtime")
      .unwrap();
    assert_eq!(
      dep.maybe_code.ok().unwrap().specifier,
      ModuleSpecifier::parse("https://example.com/preact/jsx-runtime").unwrap()
    );
    assert_eq!(
      dep.maybe_type.ok().unwrap().specifier,
      ModuleSpecifier::parse("https://example.com/preact-types/jsx-runtime")
        .unwrap()
    );
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::Tsx);
  }

  #[tokio::test]
  async fn test_parse_module_jsx_import_source_pragma() {
    #[derive(Debug)]
    struct R;
    impl Resolver for R {
      fn default_jsx_import_source_types(
        &self,
        _referrer: &ModuleSpecifier,
      ) -> Option<String> {
        Some("https://example.com/preact-types".into())
      }
    }

    let specifier = ModuleSpecifier::parse("file:///a/test01.tsx").unwrap();
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: br#"
    /** @jsxImportSource https://example.com/preact */

    export function A() {
      return <div>Hello Deno</div>;
    }
    "#
      .to_vec()
      .into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: Some(&R),
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    let actual = actual.js().unwrap();
    assert_eq!(actual.dependencies.len(), 1);
    let dep = actual
      .dependencies
      .get("https://example.com/preact/jsx-runtime")
      .unwrap();
    assert_eq!(
      dep.maybe_code.ok().unwrap().specifier,
      ModuleSpecifier::parse("https://example.com/preact/jsx-runtime").unwrap()
    );
    assert!(dep.maybe_type.is_none());
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::Tsx);
  }

  #[tokio::test]
  async fn test_default_jsx_import_source() {
    #[derive(Debug)]
    struct R;
    impl Resolver for R {
      fn default_jsx_import_source(
        &self,
        referrer: &ModuleSpecifier,
      ) -> Option<String> {
        assert_eq!(
          referrer,
          &ModuleSpecifier::parse("file:///a/test01.tsx").unwrap()
        );
        Some("https://example.com/preact".into())
      }
    }

    let specifier = ModuleSpecifier::parse("file:///a/test01.tsx").unwrap();
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: br#"
    export function A() {
      return <div>Hello Deno</div>;
    }
    "#
      .to_vec()
      .into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: Some(&R),
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    let actual = actual.js().unwrap();
    assert_eq!(actual.dependencies.len(), 1);
    let dep = actual
      .dependencies
      .get("https://example.com/preact/jsx-runtime")
      .unwrap();
    assert_eq!(
      dep.maybe_code.ok().unwrap().specifier,
      ModuleSpecifier::parse("https://example.com/preact/jsx-runtime").unwrap()
    );
    assert!(dep.maybe_type.is_none());
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::Tsx);
  }

  #[tokio::test]
  async fn test_default_jsx_import_source_types() {
    #[derive(Debug)]
    struct R;
    impl Resolver for R {
      fn default_jsx_import_source(
        &self,
        referrer: &ModuleSpecifier,
      ) -> Option<String> {
        assert_eq!(
          referrer,
          &ModuleSpecifier::parse("file:///a/test01.tsx").unwrap()
        );
        Some("https://example.com/preact".into())
      }

      fn default_jsx_import_source_types(
        &self,
        referrer: &ModuleSpecifier,
      ) -> Option<String> {
        assert_eq!(
          referrer,
          &ModuleSpecifier::parse("file:///a/test01.tsx").unwrap()
        );
        Some("https://example.com/preact-types".into())
      }

      fn jsx_import_source_module(&self, referrer: &ModuleSpecifier) -> &str {
        assert_eq!(
          referrer,
          &ModuleSpecifier::parse("file:///a/test01.tsx").unwrap()
        );
        DEFAULT_JSX_IMPORT_SOURCE_MODULE
      }
    }

    let specifier = ModuleSpecifier::parse("file:///a/test01.tsx").unwrap();
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: br#"
    export function A() {
      return <div>Hello Deno</div>;
    }
    "#
      .to_vec()
      .into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: Some(&R),
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    let actual = actual.js().unwrap();
    assert_eq!(actual.dependencies.len(), 1);
    let dep = actual
      .dependencies
      .get("https://example.com/preact/jsx-runtime")
      .unwrap();
    assert_eq!(
      dep.maybe_code.ok().unwrap().specifier,
      ModuleSpecifier::parse("https://example.com/preact/jsx-runtime").unwrap()
    );
    assert_eq!(
      dep.maybe_type.ok().unwrap().specifier,
      ModuleSpecifier::parse("https://example.com/preact-types/jsx-runtime")
        .unwrap()
    );
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::Tsx);
  }

  #[tokio::test]
  async fn test_parse_module_with_headers() {
    let specifier = ModuleSpecifier::parse("https://localhost/file").unwrap();
    let mut headers = HashMap::new();
    headers.insert(
      "content-type".to_string(),
      "application/typescript; charset=utf-8".to_string(),
    );
    let result = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: Some(headers),
      mtime: None,
      content: br#"declare interface A {
  a: string;
}"#
        .to_vec()
        .into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await;
    assert!(result.is_ok());
  }

  #[tokio::test]
  async fn test_parse_module_with_jsdoc_imports() {
    let specifier = ModuleSpecifier::parse("file:///a/test.js").unwrap();
    let code = br#"
/**
 * Some js doc
 *
 * @param {import("./types.d.ts").A} a
 * @return {import("./other.ts").B}
 */
export function a(a) {
  return;
}
"#;
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: code.to_vec().into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    assert_eq!(
      json!(actual),
      json!({
        "kind": "esm",
        "dependencies": [
          {
            "specifier": "./types.d.ts",
            "type": {
              "specifier": "file:///a/types.d.ts",
              "span": {
                "start": {
                  "line": 4,
                  "character": 18,
                },
                "end": {
                  "line": 4,
                  "character": 32,
                }
              }
            }
          },
          {
            "specifier": "./other.ts",
            "type": {
              "specifier": "file:///a/other.ts",
              "span": {
                "start": {
                  "line": 5,
                  "character": 19,
                },
                "end": {
                  "line": 5,
                  "character": 31
                }
              }
            }
          }
        ],
        "mediaType": "JavaScript",
        "size": 137,
        "specifier": "file:///a/test.js"
      })
    );

    // GraphKind::CodeOnly should not include them
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::CodeOnly,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: code.to_vec().into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    assert_eq!(
      json!(actual),
      json!({
        "kind": "esm",
        "mediaType": "JavaScript",
        "size": 137,
        "specifier": "file:///a/test.js"
      })
    );
  }

  #[tokio::test]
  async fn test_parse_ts_jsdoc_imports_ignored() {
    let specifier = ModuleSpecifier::parse("file:///a/test.ts").unwrap();
    let actual = parse_module(ParseModuleOptions {
      graph_kind: GraphKind::All,
      specifier: specifier.clone(),
      maybe_headers: None,
      mtime: None,
      content: br#"
/**
 * Some js doc
 *
 * @param {import("./types.d.ts").A} a
 * @return {import("./other.ts").B}
 */
export function a(a: A): B {
  return;
}
"#
      .to_vec()
      .into(),
      file_system: &NullFileSystem,
      jsr_url_provider: Default::default(),
      maybe_resolver: None,
      module_analyzer: Default::default(),
    })
    .await
    .unwrap();
    assert_eq!(
      json!(actual),
      json!({
        "kind": "esm",
        "mediaType": "TypeScript",
        "size": 143,
        "specifier": "file:///a/test.ts"
      })
    );
  }

  #[tokio::test]
  async fn test_segment_graph() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"import * as b from "./test02.ts"; import "https://example.com/a.ts";"#,
          },
        ),
        (
          "file:///a/test02.ts",
          Source::Module {
            specifier: "file:///a/test02.ts",
            maybe_headers: None,
            content: r#"export const b = "b";"#,
          },
        ),
        (
          "https://example.com/a.ts",
          Source::Module {
            specifier: "https://example.com/a.ts",
            maybe_headers: None,
            content: r#"import * as c from "./c";"#,
          },
        ),
        (
          "https://example.com/c",
          Source::Module {
            specifier: "https://example.com/c.ts",
            maybe_headers: None,
            content: r#"export const c = "c";"#,
          },
        ),
        (
          "https://example.com/jsx-runtime",
          Source::Module {
            specifier: "https://example.com/jsx-runtime",
            maybe_headers: Some(vec![
              ("content-type", "application/javascript"),
              ("x-typescript-types", "./jsx-runtime.d.ts"),
            ]),
            content: r#"export const a = "a";"#,
          },
        ),
        (
          "https://example.com/jsx-runtime.d.ts",
          Source::Module {
            specifier: "https://example.com/jsx-runtime.d.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"export const a: "a";"#,
          },
        ),
      ],
      vec![],
    );
    let roots = vec![ModuleSpecifier::parse("file:///a/test01.ts").unwrap()];
    let mut graph = ModuleGraph::new(GraphKind::All);
    let config_specifier =
      ModuleSpecifier::parse("file:///a/tsconfig.json").unwrap();
    let imports = vec![ReferrerImports {
      referrer: config_specifier.clone(),
      imports: vec!["https://example.com/jsx-runtime".to_string()],
    }];
    graph
      .build(
        roots.clone(),
        imports.clone(),
        &loader,
        BuildOptions::default(),
      )
      .await;
    assert!(graph.valid().is_ok());
    assert_eq!(graph.module_slots.len(), 6);
    assert_eq!(graph.redirects.len(), 1);

    let example_a_url =
      ModuleSpecifier::parse("https://example.com/a.ts").unwrap();
    let graph = graph.segment(std::slice::from_ref(&example_a_url));
    assert_eq!(graph.roots, IndexSet::from([example_a_url]));
    // should get the redirect
    assert_eq!(
      graph.redirects,
      BTreeMap::from([(
        ModuleSpecifier::parse("https://example.com/c").unwrap(),
        ModuleSpecifier::parse("https://example.com/c.ts").unwrap(),
      )])
    );

    // should copy over the imports
    assert_eq!(graph.imports.len(), 1);

    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/a.ts").unwrap()));
    assert!(graph
      .contains(&ModuleSpecifier::parse("https://example.com/c.ts").unwrap()));

    assert_eq!(
      graph.resolve_dependency(
        "https://example.com/jsx-runtime",
        &config_specifier,
        false
      ),
      Some(ModuleSpecifier::parse("https://example.com/jsx-runtime").unwrap())
        .as_ref()
    );
    assert_eq!(
      graph.resolve_dependency(
        "https://example.com/jsx-runtime",
        &config_specifier,
        true
      ),
      Some(
        ModuleSpecifier::parse("https://example.com/jsx-runtime.d.ts").unwrap()
      )
      .as_ref()
    );
  }

  #[tokio::test]
  async fn test_walk() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"import * as b from "./test02.ts"; import "https://example.com/a.ts"; import "./test04.js"; import "./test_no_dts.js"; await import("./test03.ts");"#,
          },
        ),
        (
          "file:///a/test02.ts",
          Source::Module {
            specifier: "file:///a/test02.ts",
            maybe_headers: None,
            content: r#"export const b = "b";"#,
          },
        ),
        (
          "file:///a/test03.ts",
          Source::Module {
            specifier: "file:///a/test03.ts",
            maybe_headers: None,
            content: r#"export const c = "c";"#,
          },
        ),
        (
          "file:///a/test04.js",
          Source::Module {
            specifier: "file:///a/test04.js",
            maybe_headers: None,
            content: r#"/// <reference types="./test04.d.ts" />\nexport const d = "d";"#,
          },
        ),
        (
          "file:///a/test04.d.ts",
          Source::Module {
            specifier: "file:///a/test04.d.ts",
            maybe_headers: None,
            content: r#"export const d: "d";"#,
          },
        ),
        (
          "file:///a/test_no_dts.js",
          Source::Module {
            specifier: "file:///a/test_no_dts.js",
            maybe_headers: None,
            content: r#"export const e = "e";"#,
          },
        ),
        (
          "https://example.com/a.ts",
          Source::Module {
            specifier: "https://example.com/a.ts",
            maybe_headers: None,
            content: r#"import * as c from "./c";"#,
          },
        ),
        (
          "https://example.com/c",
          Source::Module {
            specifier: "https://example.com/c.ts",
            maybe_headers: None,
            content: r#"export const c = "c";"#,
          },
        ),
        (
          "https://example.com/jsx-runtime",
          Source::Module {
            specifier: "https://example.com/jsx-runtime",
            maybe_headers: Some(vec![
              ("content-type", "application/javascript"),
              ("x-typescript-types", "./jsx-runtime.d.ts"),
            ]),
            content: r#"export const a = "a";"#,
          },
        ),
        (
          "https://example.com/jsx-runtime.d.ts",
          Source::Module {
            specifier: "https://example.com/jsx-runtime.d.ts",
            maybe_headers: Some(vec![(
              "content-type",
              "application/typescript",
            )]),
            content: r#"export const a: "a";"#,
          },
        ),
      ],
      vec![],
    );
    let root = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let mut graph = ModuleGraph::new(GraphKind::All);
    let config_specifier =
      ModuleSpecifier::parse("file:///a/tsconfig.json").unwrap();
    let imports = vec![ReferrerImports {
      referrer: config_specifier.clone(),
      imports: vec!["https://example.com/jsx-runtime".to_string()],
    }];
    graph
      .build(
        vec![root.clone()],
        imports.clone(),
        &loader,
        BuildOptions::default(),
      )
      .await;
    assert!(graph.valid().is_ok());

    // all true
    let roots = [root.clone()];
    let result = graph.walk(
      roots.iter(),
      WalkOptions {
        check_js: CheckJsOption::True,
        follow_dynamic: true,
        kind: GraphKind::All,
        prefer_fast_check_graph: true,
      },
    );
    assert_eq!(
      result
        .map(|(specifier, _)| specifier.to_string())
        .collect::<Vec<_>>(),
      vec![
        "https://example.com/jsx-runtime",
        "https://example.com/jsx-runtime.d.ts",
        "file:///a/test01.ts",
        "file:///a/test02.ts",
        "https://example.com/a.ts",
        "https://example.com/c",
        "https://example.com/c.ts",
        "file:///a/test04.js",
        "file:///a/test04.d.ts",
        "file:///a/test_no_dts.js",
        "file:///a/test03.ts",
      ]
    );

    // all false
    let result = graph.walk(
      roots.iter(),
      WalkOptions {
        check_js: CheckJsOption::False,
        follow_dynamic: false,
        kind: GraphKind::CodeOnly,
        prefer_fast_check_graph: true,
      },
    );
    assert_eq!(
      result
        .map(|(specifier, _)| specifier.to_string())
        .collect::<Vec<_>>(),
      vec![
        "file:///a/test01.ts",
        "file:///a/test02.ts",
        "https://example.com/a.ts",
        "https://example.com/c",
        "https://example.com/c.ts",
        "file:///a/test04.js", // no types
        "file:///a/test_no_dts.js",
      ]
    );
    // dynamic true
    let result = graph.walk(
      roots.iter(),
      WalkOptions {
        check_js: CheckJsOption::False,
        follow_dynamic: true,
        kind: GraphKind::CodeOnly,
        prefer_fast_check_graph: true,
      },
    );
    assert_eq!(
      result
        .map(|(specifier, _)| specifier.to_string())
        .collect::<Vec<_>>(),
      vec![
        "file:///a/test01.ts",
        "file:///a/test02.ts",
        "https://example.com/a.ts",
        "https://example.com/c",
        "https://example.com/c.ts",
        "file:///a/test04.js",
        "file:///a/test_no_dts.js",
        "file:///a/test03.ts",
      ]
    );

    // check_js true (won't have any effect since GraphKind is CodeOnly)
    let result = graph.walk(
      roots.iter(),
      WalkOptions {
        check_js: CheckJsOption::True,
        follow_dynamic: false,
        kind: GraphKind::CodeOnly,
        prefer_fast_check_graph: true,
      },
    );
    assert_eq!(
      result
        .map(|(specifier, _)| specifier.to_string())
        .collect::<Vec<_>>(),
      vec![
        "file:///a/test01.ts",
        "file:///a/test02.ts",
        "https://example.com/a.ts",
        "https://example.com/c",
        "https://example.com/c.ts",
        "file:///a/test04.js",
        "file:///a/test_no_dts.js",
      ]
    );

    // check_js false, GraphKind All
    let result = graph.walk(
      roots.iter(),
      WalkOptions {
        check_js: CheckJsOption::False,
        follow_dynamic: false,
        kind: GraphKind::All,
        prefer_fast_check_graph: true,
      },
    );
    assert_eq!(
      result
        .map(|(specifier, _)| specifier.to_string())
        .collect::<Vec<_>>(),
      vec![
        "https://example.com/jsx-runtime",
        "https://example.com/jsx-runtime.d.ts",
        "file:///a/test01.ts",
        "file:///a/test02.ts",
        "https://example.com/a.ts",
        "https://example.com/c",
        "https://example.com/c.ts",
        "file:///a/test04.js",
        "file:///a/test04.d.ts",
        "file:///a/test_no_dts.js",
      ]
    );

    // check_js false, GraphKind TypesOnly
    let result = graph.walk(
      roots.iter(),
      WalkOptions {
        check_js: CheckJsOption::False,
        follow_dynamic: false,
        kind: GraphKind::TypesOnly,
        prefer_fast_check_graph: true,
      },
    );
    assert_eq!(
      result
        .map(|(specifier, _)| specifier.to_string())
        .collect::<Vec<_>>(),
      vec![
        "https://example.com/jsx-runtime.d.ts",
        "file:///a/test01.ts",
        "file:///a/test02.ts",
        "https://example.com/a.ts",
        "https://example.com/c",
        "https://example.com/c.ts",
        "file:///a/test04.d.ts",
      ]
    );

    // check_js true, GraphKind::TypesOnly
    let result = graph.walk(
      roots.iter(),
      WalkOptions {
        check_js: CheckJsOption::True,
        follow_dynamic: false,
        kind: GraphKind::TypesOnly,
        prefer_fast_check_graph: true,
      },
    );
    assert_eq!(
      result
        .map(|(specifier, _)| specifier.to_string())
        .collect::<Vec<_>>(),
      vec![
        "https://example.com/jsx-runtime.d.ts",
        "file:///a/test01.ts",
        "file:///a/test02.ts",
        "https://example.com/a.ts",
        "https://example.com/c",
        "https://example.com/c.ts",
        "file:///a/test04.d.ts",
        // will include this now
        "file:///a/test_no_dts.js",
      ]
    );

    // try skip analyzing the dependencies after getting the first module
    {
      let mut iterator = graph.walk(
        roots.iter(),
        WalkOptions {
          check_js: CheckJsOption::True,
          follow_dynamic: false,
          kind: GraphKind::All,
          prefer_fast_check_graph: false,
        },
      );
      assert_eq!(
        iterator.next().unwrap().0.as_str(),
        "https://example.com/jsx-runtime"
      );
      assert_eq!(
        iterator.next().unwrap().0.as_str(),
        "https://example.com/jsx-runtime.d.ts"
      );
      assert_eq!(iterator.next().unwrap().0.as_str(), "file:///a/test01.ts");
      iterator.skip_previous_dependencies();
      assert!(iterator.next().is_none());
    }

    // try skipping after first remote
    {
      let mut iterator = graph.walk(
        roots.iter(),
        WalkOptions {
          check_js: CheckJsOption::True,
          follow_dynamic: false,
          kind: GraphKind::All,
          prefer_fast_check_graph: false,
        },
      );
      assert_eq!(
        iterator.next().unwrap().0.as_str(),
        "https://example.com/jsx-runtime"
      );
      assert_eq!(
        iterator.next().unwrap().0.as_str(),
        "https://example.com/jsx-runtime.d.ts"
      );
      assert_eq!(iterator.next().unwrap().0.as_str(), "file:///a/test01.ts");
      assert_eq!(iterator.next().unwrap().0.as_str(), "file:///a/test02.ts");
      assert_eq!(
        iterator.next().unwrap().0.as_str(),
        "https://example.com/a.ts"
      );
      iterator.skip_previous_dependencies(); // now won't analyze the remote's dependencies
      assert_eq!(iterator.next().unwrap().0.as_str(), "file:///a/test04.js");
      assert_eq!(iterator.next().unwrap().0.as_str(), "file:///a/test04.d.ts");
      assert_eq!(
        iterator.next().unwrap().0.as_str(),
        "file:///a/test_no_dts.js"
      );
      assert!(iterator.next().is_none());
    }
  }

  #[tokio::test]
  async fn test_resolver_execution_and_types_resolution() {
    #[derive(Debug)]
    struct ExtResolver;

    impl Resolver for ExtResolver {
      fn resolve(
        &self,
        specifier_text: &str,
        referrer_range: &Range,
        resolution_kind: ResolutionKind,
      ) -> Result<ModuleSpecifier, source::ResolveError> {
        let specifier_text = match resolution_kind {
          ResolutionKind::Types => format!("{}.d.ts", specifier_text),
          ResolutionKind::Execution => format!("{}.js", specifier_text),
        };
        Ok(resolve_import(&specifier_text, &referrer_range.specifier)?)
      }
    }

    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            import a from "./a";
            "#,
          },
        ),
        (
          "file:///a/a.js",
          Source::Module {
            specifier: "file:///a/a.js",
            maybe_headers: None,
            content: r#"export default 5;"#,
          },
        ),
        (
          "file:///a/a.d.ts",
          Source::Module {
            specifier: "file:///a/a.d.ts",
            maybe_headers: None,
            content: r#"export default 5;"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let resolver = ExtResolver;

    // test GraphKind::All
    {
      let mut graph = ModuleGraph::new(GraphKind::All);
      graph
        .build(
          vec![root_specifier.clone()],
          Default::default(),
          &loader,
          BuildOptions {
            resolver: Some(&resolver),
            ..Default::default()
          },
        )
        .await;
      assert_eq!(
        json!(graph),
        json!({
          "roots": [
            "file:///a/test01.ts"
          ],
          "modules": [
            {
              "kind": "esm",
              "size": 17,
              "mediaType": "Dts",
              "specifier": "file:///a/a.d.ts"
            },
            {
              "kind": "esm",
              "size": 17,
              "mediaType": "JavaScript",
              "specifier": "file:///a/a.js"
            },
            {
              "dependencies": [
                {
                  "specifier": "./a",
                  "code": {
                    "specifier": "file:///a/a.js",
                    "resolutionMode": "import",
                    "span": {
                      "start": {
                        "line": 1,
                        "character": 26
                      },
                      "end": {
                        "line": 1,
                        "character": 31
                      }
                    }
                  },
                  "type": {
                    "specifier": "file:///a/a.d.ts",
                    "resolutionMode": "import",
                    "span": {
                      "start": {
                        "line": 1,
                        "character": 26
                      },
                      "end": {
                        "line": 1,
                        "character": 31
                      }
                    }
                  },
                }
              ],
              "kind": "esm",
              "size": 46,
              "mediaType": "TypeScript",
              "specifier": "file:///a/test01.ts"
            }
          ],
          "redirects": {}
        })
      );
    }

    // test GraphKind::CodeOnly
    {
      let mut graph = ModuleGraph::new(GraphKind::CodeOnly);
      graph
        .build(
          vec![root_specifier.clone()],
          Default::default(),
          &loader,
          BuildOptions {
            resolver: Some(&resolver),
            ..Default::default()
          },
        )
        .await;
      assert_eq!(
        json!(graph),
        json!({
          "roots": [
            "file:///a/test01.ts"
          ],
          "modules": [
            {
              "kind": "esm",
              "size": 17,
              "mediaType": "JavaScript",
              "specifier": "file:///a/a.js"
            },
            {
              "dependencies": [
                {
                  "specifier": "./a",
                  "code": {
                    "specifier": "file:///a/a.js",
                    "resolutionMode": "import",
                    "span": {
                      "start": {
                        "line": 1,
                        "character": 26
                      },
                      "end": {
                        "line": 1,
                        "character": 31
                      }
                    }
                  },
                }
              ],
              "kind": "esm",
              "size": 46,
              "mediaType": "TypeScript",
              "specifier": "file:///a/test01.ts"
            }
          ],
          "redirects": {}
        })
      );
    }
  }

  #[tokio::test]
  async fn test_resolver_failed_types_only() {
    #[derive(Debug)]
    struct FailForTypesResolver;

    #[derive(Debug, thiserror::Error, deno_error::JsError)]
    #[class(generic)]
    #[error("Failed.")]
    struct FailedError;

    impl Resolver for FailForTypesResolver {
      fn resolve(
        &self,
        specifier_text: &str,
        referrer_range: &Range,
        resolution_kind: ResolutionKind,
      ) -> Result<ModuleSpecifier, source::ResolveError> {
        match resolution_kind {
          ResolutionKind::Execution => {
            Ok(resolve_import(specifier_text, &referrer_range.specifier)?)
          }
          ResolutionKind::Types => Err(source::ResolveError::Other(
            JsErrorBox::from_err(FailedError),
          )),
        }
      }
    }

    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            import a from "./a.js";
            "#,
          },
        ),
        (
          "file:///a/a.js",
          Source::Module {
            specifier: "file:///a/a.js",
            maybe_headers: None,
            content: r#"export default 5;"#,
          },
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let resolver = FailForTypesResolver;

    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        BuildOptions {
          resolver: Some(&resolver),
          ..Default::default()
        },
      )
      .await;
    let errors = graph
      .walk(
        graph.roots.iter(),
        WalkOptions {
          check_js: CheckJsOption::True,
          kind: GraphKind::TypesOnly,
          follow_dynamic: false,
          prefer_fast_check_graph: true,
        },
      )
      .errors()
      .collect::<Vec<_>>();
    assert_eq!(errors.len(), 1);
    assert!(matches!(
      &errors[0],
      ModuleGraphError::TypesResolutionError(_)
    ));
  }

  #[tokio::test]
  async fn test_passthrough_jsr_specifiers() {
    let loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"import "jsr:@foo/bar@1";"#,
          },
        ),
        ("jsr:@foo/bar@1", Source::External("jsr:@foo/bar@1")),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![root_specifier.clone()],
        Default::default(),
        &loader,
        BuildOptions {
          passthrough_jsr_specifiers: true,
          ..Default::default()
        },
      )
      .await;
    assert_eq!(graph.module_slots.len(), 2);
    assert_eq!(graph.roots, IndexSet::from([root_specifier.clone()]));
    assert!(graph.contains(&root_specifier));
    let module = graph
      .module_slots
      .get(&root_specifier)
      .unwrap()
      .module()
      .unwrap()
      .js()
      .unwrap();
    assert_eq!(module.dependencies.len(), 1);
    let maybe_dependency = module.dependencies.get("jsr:@foo/bar@1");
    assert!(maybe_dependency.is_some());
    let dependency_specifier =
      ModuleSpecifier::parse("jsr:@foo/bar@1").unwrap();
    let maybe_dep_module_slot = graph.get(&dependency_specifier);
    let dep_module_slot = maybe_dep_module_slot.unwrap();
    assert!(dep_module_slot.external().is_some());
  }
}
