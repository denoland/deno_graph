// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

// remove this after https://github.com/rustwasm/wasm-bindgen/issues/2774 is released
#![allow(clippy::unused_unit)]
#![deny(clippy::disallowed_methods)]
#![deny(clippy::disallowed_types)]

#[macro_use]
extern crate cfg_if;

mod analyzer;
mod ast;
mod colors;
mod graph;
mod info;
mod module_specifier;
pub mod source;
mod source_map;
mod text_encoding;

use graph::BuildKind;
use graph::Builder;
pub use graph::ModuleKind;
use graph::ModuleSlot;
use source::Locker;
use source::Resolver;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

cfg_if! {
  if #[cfg(feature = "rust")] {
    use std::sync::Arc;

    pub use analyzer::ByteRange;
    pub use analyzer::Comment;
    pub use analyzer::DependencyDescriptor;
    pub use analyzer::DependencyKind;
    pub use analyzer::ImportAssertion;
    pub use analyzer::ImportAssertions;
    pub use analyzer::ModuleAnalyzer;
    pub use analyzer::ModuleAnalyzerProvider;
    pub use analyzer::SpecifierWithRange;
    pub use analyzer::TypeScriptReference;
    pub use analyzer::ModuleInfo;
    pub use ast::analyze_deno_types;
    pub use ast::ParsedSourceAnalyzerProvider;
    pub use ast::ParsedSourceAnalyzer;
    pub use graph::Dependency;
    pub use graph::GraphImport;
    pub use graph::Module;
    pub use graph::ModuleGraph;
    pub use graph::ModuleGraphError;
    pub use graph::Position;
    pub use graph::Range;
    pub use graph::ResolutionError;
    pub use graph::Resolved;
    pub use deno_ast::MediaType;
    pub use module_specifier::ModuleSpecifier;
    pub use module_specifier::SpecifierError;
    pub use module_specifier::resolve_import;

    use source::Loader;
    use source::Reporter;

    /// Create a module graph, based on loading and recursively analyzing the
    /// dependencies of the module, returning the resulting graph.
    #[allow(clippy::too_many_arguments)]
    pub async fn create_graph(
      roots: Vec<(ModuleSpecifier, ModuleKind)>,
      is_dynamic: bool,
      maybe_imports: Option<Vec<(ModuleSpecifier, Vec<String>)>>,
      loader: &mut dyn Loader,
      maybe_resolver: Option<&dyn Resolver>,
      maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
      maybe_analyzer_provider: Option<&dyn ModuleAnalyzerProvider>,
      maybe_reporter: Option<&dyn Reporter>,
    ) -> ModuleGraph {
      let default_analyzer_provider = ast::ParsedSourceAnalyzerProvider::default();
      let analyzer_provider = maybe_analyzer_provider.unwrap_or(&default_analyzer_provider);
      let builder = Builder::new(
        roots,
        is_dynamic,
        loader,
        maybe_resolver,
        maybe_locker,
        analyzer_provider,
        maybe_reporter,
      );
      builder.build(BuildKind::All, maybe_imports).await
    }

    /// Create a module graph, including only dependencies of the roots that
    /// would contain code that would be executed, skipping any type only
    /// dependencies. This is useful when wanting to build a graph of code for
    /// loading in runtime that doesn't care about type only dependencies.
    #[allow(clippy::too_many_arguments)]
    pub async fn create_code_graph(
      roots: Vec<(ModuleSpecifier, ModuleKind)>,
      is_dynamic: bool,
      maybe_imports: Option<Vec<(ModuleSpecifier, Vec<String>)>>,
      loader: &mut dyn Loader,
      maybe_resolver: Option<&dyn Resolver>,
      maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
      maybe_analyzer_provider: Option<&dyn ModuleAnalyzerProvider>,
      maybe_reporter: Option<&dyn Reporter>,
    ) -> ModuleGraph {
      let default_analyzer_provider = ast::ParsedSourceAnalyzerProvider::default();
      let analyzer_provider = maybe_analyzer_provider.unwrap_or(&default_analyzer_provider);
      let builder = Builder::new(
        roots,
        is_dynamic,
        loader,
        maybe_resolver,
        maybe_locker,
        analyzer_provider,
        maybe_reporter,
      );
      builder.build(BuildKind::CodeOnly, maybe_imports).await
    }

    /// Create a module graph, including only dependencies that might affect
    /// the types of the graph, skipping any "code only" imports. This is
    /// useful in situations where only the types are being used, like when
    /// type checking code or generating documentation.
    ///
    /// Note that code which is overloaded with types upon access (like the
    /// `X-TypeScript-Types` header or types defined in the code itself) will
    /// still be loaded into the graph, but further code only dependencies will
    /// not be followed.
    #[allow(clippy::too_many_arguments)]
    pub async fn create_type_graph(
      roots: Vec<ModuleSpecifier>,
      is_dynamic: bool,
      maybe_imports: Option<Vec<(ModuleSpecifier, Vec<String>)>>,
      loader: &mut dyn Loader,
      maybe_resolver: Option<&dyn Resolver>,
      maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
      maybe_analyzer_provider: Option<&dyn ModuleAnalyzerProvider>,
      maybe_reporter: Option<&dyn Reporter>,
    ) -> ModuleGraph {
      let default_analyzer_provider = ast::ParsedSourceAnalyzerProvider::default();
      let analyzer_provider = maybe_analyzer_provider.unwrap_or(&default_analyzer_provider);
      let roots = roots.into_iter().map(|s| (s, ModuleKind::Esm)).collect();
      let builder = Builder::new(
        roots,
        is_dynamic,
        loader,
        maybe_resolver,
        maybe_locker,
        analyzer_provider,
        maybe_reporter,
      );
      builder.build(BuildKind::TypesOnly, maybe_imports).await
    }

    /// Parse an individual module, returning the module as a result, otherwise
    /// erroring with a module graph error.
    pub fn parse_module(
      specifier: &ModuleSpecifier,
      maybe_headers: Option<&HashMap<String, String>>,
      content: Arc<str>,
      maybe_kind: Option<&ModuleKind>,
      maybe_resolver: Option<&dyn Resolver>,
      maybe_analyzer_provider: Option<&dyn ModuleAnalyzerProvider>,
    ) -> Result<Module, ModuleGraphError> {
      let default_analyzer_provider = ast::ParsedSourceAnalyzerProvider::default();
      let analyzer_provider = maybe_analyzer_provider.unwrap_or(&default_analyzer_provider);
      match graph::parse_module(
        specifier,
        maybe_headers,
        content,
        None,
        maybe_kind,
        maybe_resolver,
        analyzer_provider,
        true,
        false,
      ) {
        ModuleSlot::Module(module) => Ok(module),
        ModuleSlot::Err(err) => Err(err),
        _ => unreachable!("unreachable ModuleSlot variant"),
      }
    }

    /// Parse an individual module from an AST, returning the module.
    pub fn parse_module_from_ast(
      specifier: &ModuleSpecifier,
      kind: &ModuleKind,
      maybe_headers: Option<&HashMap<String, String>>,
      parsed_source: &deno_ast::ParsedSource,
      maybe_resolver: Option<&dyn Resolver>,
    ) -> Module {
      graph::parse_module_from_analyzer(
        specifier,
        kind,
        parsed_source.media_type(),
        maybe_headers,
        &ParsedSourceAnalyzer::new(parsed_source.clone()),
        parsed_source.text_info().text(),
        maybe_resolver,
      )
    }
  }
}

cfg_if! {
  if #[cfg(feature = "wasm")] {
    mod checksum;
    mod js_graph;

    pub use js_graph::JsLoader;
    pub use js_graph::JsLocker;
    pub use js_graph::JsResolver;
    use js_graph::StringOrTuple;

    use wasm_bindgen::prelude::*;

    #[wasm_bindgen(js_name = createGraph)]
    #[allow(clippy::too_many_arguments)]
    pub async fn js_create_graph(
      roots: JsValue,
      load: js_sys::Function,
      maybe_jsx_import_source_module: Option<String>,
      maybe_cache_info: Option<js_sys::Function>,
      maybe_resolve: Option<js_sys::Function>,
      maybe_resolve_types: Option<js_sys::Function>,
      maybe_check: Option<js_sys::Function>,
      maybe_get_checksum: Option<js_sys::Function>,
      maybe_lockfile_name: Option<String>,
      maybe_build_kind: Option<String>,
      maybe_imports: JsValue,
    ) -> Result<js_graph::ModuleGraph, JsValue> {
      let roots_vec: Vec<StringOrTuple> = roots.into_serde().map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
      let maybe_imports_map: Option<HashMap<String, Vec<String>>> = maybe_imports.into_serde().map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
      let mut loader = js_graph::JsLoader::new(load, maybe_cache_info);
      let maybe_resolver = if maybe_jsx_import_source_module.is_some() || maybe_resolve.is_some() || maybe_resolve_types.is_some() {
        Some(js_graph::JsResolver::new(maybe_jsx_import_source_module, maybe_resolve, maybe_resolve_types))
      } else {
        None
      };
      let maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>> =
        if maybe_check.is_some() || maybe_get_checksum.is_some() {
          let locker = js_graph::JsLocker::new(maybe_check, maybe_get_checksum, maybe_lockfile_name);
          Some(Rc::new(RefCell::new(Box::new(locker))))
        } else {
          None
        };
      let mut roots = Vec::new();
      for root in roots_vec.into_iter() {
        let root = root.to_tuple_result().map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
        roots.push(root);
      }
      let build_kind = match maybe_build_kind.as_deref() {
        Some("typesOnly") => BuildKind::TypesOnly,
        Some("codeOnly") => BuildKind::CodeOnly,
        _ => BuildKind::All,
      };
      let mut maybe_imports = None;
      if let Some(imports_map) = maybe_imports_map {
        let mut imports = Vec::new();
        for (referrer_str, specifier_vec) in imports_map.into_iter() {
          let referrer = module_specifier::ModuleSpecifier::parse(&referrer_str)
            .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
          imports.push((referrer, specifier_vec));
        }
        maybe_imports = Some(imports);
      }

      let analyzer_provider = ast::ParsedSourceAnalyzerProvider::default();
      let builder = Builder::new(
        roots,
        false,
        &mut loader,
        maybe_resolver.as_ref().map(|r| r as &dyn Resolver),
        maybe_locker,
        &analyzer_provider,
        None,
      );
      let graph = builder.build(build_kind, maybe_imports).await;
      Ok(js_graph::ModuleGraph(graph))
    }

    #[wasm_bindgen(js_name = parseModule)]
    pub fn js_parse_module(
      specifier: String,
      maybe_headers: JsValue,
      maybe_jsx_import_source_module: Option<String>,
      content: String,
      maybe_kind: JsValue,
      maybe_resolve: Option<js_sys::Function>,
      maybe_resolve_types: Option<js_sys::Function>,
    ) -> Result<js_graph::Module, JsValue> {
      let maybe_headers: Option<HashMap<String, String>> = maybe_headers
        .into_serde()
        .map_err(|err| js_sys::Error::new(&err.to_string()))?;
      let specifier = module_specifier::ModuleSpecifier::parse(&specifier)
        .map_err(|err| js_sys::Error::new(&err.to_string()))?;
      let maybe_resolver = if maybe_jsx_import_source_module.is_some() || maybe_resolve.is_some() || maybe_resolve_types.is_some() {
        Some(js_graph::JsResolver::new(maybe_jsx_import_source_module, maybe_resolve, maybe_resolve_types))
      } else {
        None
      };
      let maybe_kind: Option<ModuleKind> = maybe_kind.into_serde().map_err(|err| js_sys::Error::new(&err.to_string()))?;
      let analyzer_provider = ast::ParsedSourceAnalyzerProvider::default();
      match graph::parse_module(
        &specifier,
        maybe_headers.as_ref(),
        content.into(),
        None,
        maybe_kind.as_ref(),
        maybe_resolver.as_ref().map(|r| r as &dyn Resolver),
        &analyzer_provider,
        true,
        false,
      ) {
        ModuleSlot::Module(module) => Ok(js_graph::Module(module)),
        ModuleSlot::Err(err) => Err(js_sys::Error::new(&err.to_string()).into()),
        _ => unreachable!("unreachable ModuleSlot variant"),
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::graph::Resolved;
  use pretty_assertions::assert_eq;
  use serde_json::json;
  use source::tests::MockResolver;
  use source::CacheInfo;
  use source::MemoryLoader;
  use source::Source;

  type Sources<'a> = Vec<(&'a str, Source<&'a str>)>;

  fn setup(
    sources: Sources,
    cache_info: Vec<(&str, CacheInfo)>,
  ) -> MemoryLoader {
    MemoryLoader::new(sources, cache_info)
  }

  #[tokio::test]
  async fn test_create_graph() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert_eq!(graph.module_slots.len(), 2);
    assert_eq!(graph.roots, vec![(root_specifier.clone(), ModuleKind::Esm)]);
    assert!(graph.contains(&root_specifier));
    assert!(
      !graph.contains(&ModuleSpecifier::parse("file:///a/test03.ts").unwrap())
    );
    let maybe_root_module = graph.module_slots.get(&root_specifier);
    assert!(maybe_root_module.is_some());
    let root_module_slot = maybe_root_module.unwrap();
    if let ModuleSlot::Module(module) = root_module_slot {
      assert_eq!(module.dependencies.len(), 1);
      let maybe_dependency = module.dependencies.get("./test02.ts");
      assert!(maybe_dependency.is_some());
      let dependency_specifier =
        ModuleSpecifier::parse("file:///a/test02.ts").unwrap();
      let dependency = maybe_dependency.unwrap();
      assert!(!dependency.is_dynamic);
      if let Resolved::Ok { specifier, .. } = &dependency.maybe_code {
        assert_eq!(specifier, &dependency_specifier);
      } else {
        panic!("unexpected resolved slot");
      }
      assert_eq!(dependency.maybe_type, Resolved::None);
      let maybe_dep_module_slot = graph.get(&dependency_specifier);
      assert!(maybe_dep_module_slot.is_some());
    } else {
      panic!("unexpected module slot");
    }
  }

  #[tokio::test]
  async fn test_create_graph_multiple_roots() {
    let mut loader = setup(
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
    let roots = vec![
      (
        ModuleSpecifier::parse("file:///a/test01.ts").unwrap(),
        ModuleKind::Esm,
      ),
      (
        ModuleSpecifier::parse("https://example.com/a.ts").unwrap(),
        ModuleKind::Esm,
      ),
    ];
    let graph = create_graph(
      roots.clone(),
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
  async fn test_create_graph_json_module_root() {
    let mut loader = setup(
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
    let roots = vec![(
      ModuleSpecifier::parse("file:///a/test.json").unwrap(),
      ModuleKind::Asserted,
    )];
    let graph = create_graph(
      roots.clone(),
      true,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
  async fn test_create_graph_dynamic_json_ignores_assert() {
    let mut loader = setup(
      vec![
        (
          "file:///a/test.js",
          Source::Module {
            specifier: "file:///a/test.js",
            maybe_headers: None,
            content: r#"
        const a = await import("./a.json");
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
    let roots = vec![(
      ModuleSpecifier::parse("file:///a/test.js").unwrap(),
      ModuleKind::Esm,
    )];
    let graph = create_graph(
      roots.clone(),
      true,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test.js",
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
                  "span": {
                    "start": {
                      "line": 1,
                      "character": 31
                    },
                    "end": {
                      "line": 1,
                      "character": 41
                    }
                  }
                },
                "isDynamic": true
              }
            ],
            "kind": "esm",
            "mediaType": "JavaScript",
            "size": 53,
            "specifier": "file:///a/test.js"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_valid_type_missing() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert!(graph.valid().is_ok());
    assert!(graph.valid_types_only().is_err());
  }

  #[tokio::test]
  async fn test_valid_code_missing() {
    let mut loader = setup(
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
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert!(graph.valid().is_err());
    assert_eq!(
      graph.valid().err().unwrap().to_string(),
      r#"Module not found "file:///a/test02.js"."#
    );
    assert!(graph.valid_types_only().is_ok());
  }

  #[tokio::test]
  async fn test_create_graph_imports() {
    let mut loader = setup(
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
    let maybe_imports =
      Some(vec![(config_specifier, vec!["./types.d.ts".to_string()])]);
    let graph = create_graph(
      vec![(root_specifier, ModuleKind::Esm)],
      false,
      maybe_imports,
      &mut loader,
      None,
      None,
      None,
      None,
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
  async fn test_create_graph_imports_imported() {
    let mut loader = setup(
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
    let maybe_imports = Some(vec![(
      config_specifier,
      vec!["https://esm.sh/preact/runtime-jsx".to_string()],
    )]);
    let graph = create_graph(
      vec![(root_specifier, ModuleKind::Esm)],
      false,
      maybe_imports,
      &mut loader,
      None,
      None,
      None,
      None,
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
  async fn test_create_graph_imports_resolve_dependency() {
    let mut loader = setup(
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
    let maybe_imports = Some(vec![(
      config_specifier.clone(),
      vec!["https://example.com/jsx-runtime".to_string()],
    )]);
    let graph = create_graph(
      vec![(root_specifier, ModuleKind::Esm)],
      false,
      maybe_imports,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert_eq!(
      graph.resolve_dependency(
        "https://example.com/jsx-runtime",
        &config_specifier,
        false
      ),
      Some(&ModuleSpecifier::parse("https://example.com/jsx-runtime").unwrap())
    );
    assert_eq!(
      graph.resolve_dependency(
        "https://example.com/jsx-runtime",
        &config_specifier,
        true
      ),
      Some(
        &ModuleSpecifier::parse("https://example.com/jsx-runtime.d.ts")
          .unwrap()
      )
    );
  }

  #[tokio::test]
  async fn test_create_graph_with_headers() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert_eq!(graph.module_slots.len(), 1);
    assert_eq!(graph.roots, vec![(root_specifier.clone(), ModuleKind::Esm)]);
    let maybe_root_module = graph.module_slots.get(&root_specifier);
    assert!(maybe_root_module.is_some());
    let root_module_slot = maybe_root_module.unwrap();
    if let ModuleSlot::Module(module) = root_module_slot {
      assert_eq!(module.media_type, MediaType::TypeScript);
    } else {
      panic!("unspected module slot");
    }
  }

  #[tokio::test]
  async fn test_create_graph_jsx_import_source() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
  async fn test_bare_specifier_error() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    let result = graph.valid();
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.specifier(), &root_specifier);
    assert!(matches!(err, ModuleGraphError::ResolutionError(_)));
  }

  #[tokio::test]
  async fn test_unsupported_media_type() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    let result = graph.valid();
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(
      err,
      ModuleGraphError::UnsupportedMediaType(_, MediaType::Json)
    ));
  }

  #[tokio::test]
  async fn test_root_is_extensionless() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert!(graph.valid().is_ok());
  }

  #[tokio::test]
  async fn test_crate_graph_with_dynamic_imports() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
  async fn test_create_graph_with_jsdoc_imports() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
                "specifier": "./other.ts",
                "type": {
                  "specifier": "file:///a/other.ts",
                  "span": {
                    "start": {
                      "line": 5,
                      "character": 20
                    },
                    "end": {
                      "line": 5,
                      "character": 30
                    }
                  }
                }
              },
              {
                "specifier": "./types.d.ts",
                "type": {
                  "specifier": "file:///a/types.d.ts",
                  "span": {
                    "start": {
                      "line": 4,
                      "character": 19
                    },
                    "end": {
                      "line": 4,
                      "character": 31
                    }
                  }
                }
              }
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
  async fn test_create_graph_with_redirects() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert_eq!(
      graph.roots,
      vec![(
        ModuleSpecifier::parse("https://example.com/a").unwrap(),
        ModuleKind::Esm
      )]
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
    assert!(graph.to_string().contains("https://example.com/a.ts "));
    assert!(graph.to_string().contains("https://example.com/b.ts"));
  }

  #[tokio::test]
  async fn test_create_graph_with_circular_redirects() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert_eq!(
      graph.roots,
      vec![(
        ModuleSpecifier::parse("https://example.com/a").unwrap(),
        ModuleKind::Esm
      )]
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
    assert!(graph.to_string().contains("https://example.com/a.ts "));
    assert!(graph.to_string().contains("https://example.com/b.ts"));
  }

  #[tokio::test]
  async fn test_create_graph_with_data_url() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
    )
    .await;
    assert_eq!(graph.module_slots.len(), 3);
    let data_specifier = ModuleSpecifier::parse("data:application/typescript,export%20*%20from%20%22https://example.com/c.ts%22;").unwrap();
    let maybe_module = graph.get(&data_specifier);
    assert!(maybe_module.is_some());
    let module = maybe_module.unwrap();
    let source: &str = module.maybe_source.as_ref().unwrap();
    assert_eq!(source, r#"export * from "https://example.com/c.ts";"#,);
  }

  #[tokio::test]
  async fn test_create_graph_with_resolver() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier, ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      maybe_resolver,
      None,
      None,
      None,
    )
    .await;
    let maybe_module = graph.get(&graph.roots[0].0);
    assert!(maybe_module.is_some());
    let module = maybe_module.unwrap();
    let maybe_dep = module.dependencies.get("b");
    assert!(maybe_dep.is_some());
    let dep = maybe_dep.unwrap();
    if let Some(dep_specifier) = dep.maybe_code.maybe_specifier() {
      assert_eq!(
        dep_specifier,
        &ModuleSpecifier::parse("file:///a/test02.ts").unwrap()
      );
    } else {
      panic!("unexpected resolved type");
    }
  }

  #[tokio::test]
  async fn test_create_graph_with_resolve_types() {
    let mut loader = setup(
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
            start: Position::zeroed(),
            end: Position::zeroed(),
          }),
        ),
      )],
    );
    let maybe_resolver: Option<&dyn Resolver> = Some(&resolver);
    let root_specifier = ModuleSpecifier::parse("file:///a.js").unwrap();
    let graph = create_graph(
      vec![(root_specifier, ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      maybe_resolver,
      None,
      None,
      None,
    )
    .await;
    let maybe_module = graph.get(&graph.roots[0].0);
    assert!(maybe_module.is_some());
    let module = maybe_module.unwrap();
    assert_eq!(
      module.maybe_types_dependency,
      Some((
        "file:///a.js".to_string(),
        Resolved::Ok {
          specifier: ModuleSpecifier::parse("file:///a.d.ts").unwrap(),
          kind: ModuleKind::Esm,
          range: Range {
            specifier: ModuleSpecifier::parse("file:///package.json").unwrap(),
            start: Position::zeroed(),
            end: Position::zeroed(),
          }
        }
      ))
    );
  }

  #[tokio::test]
  async fn test_create_graph_import_assertions() {
    let mut loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Source::Module {
            specifier: "file:///a/test01.ts",
            maybe_headers: None,
            content: r#"
            import a from "./a.json" assert { type: "json" };
            const b = await import("./b.json", { assert: { type: "json" } });
            export * as c from "./c.json" assert { type: "json" };
            const json = "json";
            const d = await import("./d.json", { assert: { type: json } });
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
            "size": 329,
            "specifier": "file:///a/test01.ts"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_create_graph_mixed_assertions() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
              }
            ],
            "kind": "esm",
            "mediaType": "TypeScript",
            "size": 113,
            "specifier": "file:///a/test01.ts"
          }
        ],
        "redirects": {}
      })
    );
  }

  #[tokio::test]
  async fn test_create_graph_import_assertion_errors() {
    let mut loader = setup(
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
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
            "error": "Expected a JavaScript or TypeScript module, but identified a Json module. Consider importing Json modules with an import assertion with the type of \"json\".\n  Specifier: file:///a/a.json"
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
            "error": "The import assertion type of \"css\" is unsupported."
          },
          {
            "specifier": "file:///a/e.wasm",
            "error": "Expected a JavaScript or TypeScript module, but identified a Wasm module. Importing these types of modules is currently not supported.\n  Specifier: file:///a/e.wasm"
          },
          {
            "dependencies": [
              {
                "specifier": "./a.json",
                "code": {
                  "specifier": "file:///a/a.json",
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

  #[derive(Debug)]
  struct CollectingReporter {
    on_loads: RefCell<Vec<(ModuleSpecifier, usize, usize)>>,
  }

  impl source::Reporter for CollectingReporter {
    fn on_load(
      &self,
      specifier: &ModuleSpecifier,
      modules_done: usize,
      modules_total: usize,
    ) {
      self.on_loads.borrow_mut().push((
        specifier.clone(),
        modules_done,
        modules_total,
      ));
    }
  }

  #[tokio::test]
  async fn test_create_graph_with_reporter() {
    let mut loader = setup(
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
    let reporter = CollectingReporter {
      on_loads: RefCell::new(vec![]),
    };
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      Some(&reporter),
    )
    .await;
    assert_eq!(graph.modules().len(), 5);

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
  async fn test_create_type_graph() {
    let mut loader = setup(
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
    let graph = create_type_graph(
      vec![root_specifier.clone()],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
                  "span": {
                    "start": {
                      "line": 1,
                      "character": 28
                    },
                    "end": {
                      "line": 1,
                      "character": 36
                    }
                  }
                }
              },
              {
                "specifier": "./b.d.ts",
                "type": {
                  "specifier": "file:///a/b.d.ts",
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
                "specifier": "./d.js",
                "code": {
                  "specifier": "file:///a/d.js",
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
              {
                "specifier": "https://example.com/c",
                "code": {
                  "specifier": "https://example.com/c",
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
              }
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
  async fn test_create_code_graph() {
    let mut loader = setup(
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
    let graph = create_code_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
                "specifier": "./b.d.ts",
              },
              {
                "specifier": "./d.js",
                "code": {
                  "specifier": "file:///a/d.js",
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
              {
                "specifier": "https://example.com/c",
                "code": {
                  "specifier": "https://example.com/c",
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
              }
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
  async fn test_create_graph_with_builtin_external() {
    let mut loader = setup(
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
        ("builtin:fs", Source::BuiltIn("builtin:fs")),
        (
          "https://example.com/bundle",
          Source::External("https://example.com/bundle"),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(
      vec![(root_specifier.clone(), ModuleKind::Esm)],
      false,
      None,
      &mut loader,
      None,
      None,
      None,
      None,
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
            "kind": "builtIn",
            "specifier": "builtin:fs"
          },
          {
            "dependencies": [
              {
                "specifier": "builtin:fs",
                "code": {
                  "specifier": "builtin:fs",
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

  #[test]
  fn test_parse_module() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let result = parse_module(
      &specifier,
      None,
      r#"
    import { a } from "./a.ts";
    import * as b from "./b.ts";
    export { c } from "./c.ts";
    const d = await import("./d.ts");
    "#
      .into(),
      None,
      None,
      None,
    );
    assert!(result.is_ok());
    let actual = result.unwrap();
    assert_eq!(actual.dependencies.len(), 4);
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::TypeScript);
  }

  #[test]
  fn test_parse_module_import_assertions() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let result = parse_module(
      &specifier,
      None,
      r#"
    import a from "./a.json" assert { type: "json" };
    await import("./b.json", { assert: { type: "json" } });
    "#
      .into(),
      Some(&ModuleKind::Esm),
      None,
      None,
    );
    assert!(result.is_ok());
    let actual = result.unwrap();
    assert_eq!(
      json!(actual),
      json!({
        "dependencies": [
          {
            "specifier": "./a.json",
            "code": {
              "specifier": "file:///a/a.json",
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
        "kind": "esm",
        "mediaType": "TypeScript",
        "size": 119,
        "specifier": "file:///a/test01.ts"
      })
    );
  }

  #[test]
  fn test_parse_module_jsx_import_source() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.tsx").unwrap();
    let result = parse_module(
      &specifier,
      None,
      r#"
    /** @jsxImportSource https://example.com/preact */

    export function A() {
      return <div>Hello Deno</div>;
    }
    "#
      .into(),
      Some(&ModuleKind::Esm),
      None,
      None,
    );
    assert!(result.is_ok());
    let actual = result.unwrap();
    assert_eq!(actual.dependencies.len(), 1);
    let dep = actual
      .dependencies
      .get("https://example.com/preact/jsx-runtime")
      .unwrap();
    assert!(!dep.maybe_code.is_none());
    if let Resolved::Ok { specifier, .. } = &dep.maybe_code {
      assert_eq!(
        specifier,
        &ModuleSpecifier::parse("https://example.com/preact/jsx-runtime")
          .unwrap()
      );
    }
    assert!(dep.maybe_type.is_none());
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::Tsx);
  }

  #[test]
  fn test_parse_module_with_headers() {
    let specifier = ModuleSpecifier::parse("https://localhost/file").unwrap();
    let mut headers = HashMap::new();
    headers.insert(
      "content-type".to_string(),
      "application/typescript; charset=utf-8".to_string(),
    );
    let maybe_headers = Some(&headers);
    let result = parse_module(
      &specifier,
      maybe_headers,
      r#"declare interface A {
  a: string;
}"#
        .into(),
      Some(&ModuleKind::Esm),
      None,
      None,
    );
    assert!(result.is_ok());
  }

  #[test]
  fn test_parse_module_with_jsdoc_imports() {
    let specifier = ModuleSpecifier::parse("file:///a/test.js").unwrap();
    let result = parse_module(
      &specifier,
      None,
      r#"
/**
 * Some js doc
 *
 * @param {import("./types.d.ts").A} a
 * @return {import("./other.ts").B}
 */
export function a(a) {
  return;
}
"#
      .into(),
      Some(&ModuleKind::Esm),
      None,
      None,
    );
    assert!(result.is_ok());
    let actual = result.unwrap();
    assert_eq!(
      json!(actual),
      json!({
        "dependencies": [
          {
            "specifier": "./other.ts",
            "type": {
              "specifier": "file:///a/other.ts",
              "span": {
                "start": {
                  "line": 5,
                  "character": 20,
                },
                "end": {
                  "line": 5,
                  "character": 30
                }
              }
            }
          },
          {
            "specifier": "./types.d.ts",
            "type": {
              "specifier": "file:///a/types.d.ts",
              "span": {
                "start": {
                  "line": 4,
                  "character": 19,
                },
                "end": {
                  "line": 4,
                  "character": 31,
                }
              }
            }
          }
        ],
        "kind": "esm",
        "mediaType": "JavaScript",
        "size": 137,
        "specifier": "file:///a/test.js"
      })
    );
  }

  #[test]
  fn test_parse_ts_jsdoc_imports_ignored() {
    let specifier = ModuleSpecifier::parse("file:///a/test.ts").unwrap();
    let result = parse_module(
      &specifier,
      None,
      r#"
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
      .into(),
      Some(&ModuleKind::Esm),
      None,
      None,
    );
    assert!(result.is_ok());
    let actual = result.unwrap();
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
}
