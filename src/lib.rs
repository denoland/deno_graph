// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

#[macro_use]
extern crate cfg_if;

mod ast;
mod colors;
mod graph;
mod info;
mod module_specifier;
pub mod source;
mod text_encoding;

use graph::BuildKind;
use graph::Builder;
use graph::ModuleSlot;
use source::Locker;
use source::Resolver;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

cfg_if! {
  if #[cfg(feature = "rust")] {
    pub use ast::analyze_dependencies;
    pub use ast::analyze_deno_types;
    pub use ast::analyze_ts_references;
    pub use ast::SourceParser;
    pub use ast::CapturingSourceParser;
    pub use ast::DefaultSourceParser;
    pub use ast::DependencyDescriptor;
    pub use ast::DependencyKind;
    pub use graph::Dependency;
    pub use graph::EsModule;
    pub use graph::Module;
    pub use graph::ModuleGraph;
    pub use graph::ModuleGraphError;
    pub use graph::Position;
    pub use graph::Range;
    pub use graph::ResolutionError;
    pub use graph::Resolved;
    pub use graph::SyntheticModule;
    pub use deno_ast::MediaType;
    pub use module_specifier::ModuleSpecifier;
    pub use module_specifier::SpecifierError;

    use source::Loader;
    use source::Reporter;

    /// Create a module graph, based on loading and recursively analyzing the
    /// dependencies of the module, returning the resulting graph.
    #[allow(clippy::too_many_arguments)]
    pub async fn create_graph(
      roots: Vec<ModuleSpecifier>,
      is_dynamic: bool,
      maybe_imports: Option<Vec<(ModuleSpecifier, Vec<String>)>>,
      loader: &mut dyn Loader,
      maybe_resolver: Option<&dyn Resolver>,
      maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
      maybe_parser: Option<&dyn SourceParser>,
      maybe_reporter: Option<&dyn Reporter>,
    ) -> ModuleGraph {
      let default_parser = ast::DefaultSourceParser::new();
      let source_parser = maybe_parser.unwrap_or(&default_parser);
      let builder = Builder::new(
        roots,
        is_dynamic,
        loader,
        maybe_resolver,
        maybe_locker,
        source_parser,
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
      roots: Vec<ModuleSpecifier>,
      is_dynamic: bool,
      maybe_imports: Option<Vec<(ModuleSpecifier, Vec<String>)>>,
      loader: &mut dyn Loader,
      maybe_resolver: Option<&dyn Resolver>,
      maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
      maybe_parser: Option<&dyn SourceParser>,
      maybe_reporter: Option<&dyn Reporter>,
    ) -> ModuleGraph {
      let default_parser = ast::DefaultSourceParser::new();
      let source_parser = maybe_parser.unwrap_or(&default_parser);
      let builder = Builder::new(
        roots,
        is_dynamic,
        loader,
        maybe_resolver,
        maybe_locker,
        source_parser,
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
      maybe_parser: Option<&dyn SourceParser>,
      maybe_reporter: Option<&dyn Reporter>,
    ) -> ModuleGraph {
      let default_parser = ast::DefaultSourceParser::new();
      let source_parser = maybe_parser.unwrap_or(&default_parser);
      let builder = Builder::new(
        roots,
        is_dynamic,
        loader,
        maybe_resolver,
        maybe_locker,
        source_parser,
        maybe_reporter,
      );
      builder.build(BuildKind::TypesOnly, maybe_imports).await
    }

    /// Parse an individual module, returning the module as a result, otherwise
    /// erroring with a module graph error.
    pub fn parse_module(
      specifier: &ModuleSpecifier,
      maybe_headers: Option<&HashMap<String, String>>,
      content: Arc<String>,
      maybe_resolver: Option<&dyn Resolver>,
      maybe_parser: Option<&dyn SourceParser>,
    ) -> Result<Module, ModuleGraphError> {
      let default_parser = ast::DefaultSourceParser::new();
      let source_parser = maybe_parser.unwrap_or(&default_parser);
      match graph::parse_module(
        specifier,
        maybe_headers,
        content,
        None,
        maybe_resolver,
        source_parser,
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
      maybe_headers: Option<&HashMap<String, String>>,
      parsed_ast: &deno_ast::ParsedSource,
      maybe_resolver: Option<&dyn Resolver>,
    ) -> Module {
      graph::parse_module_from_ast(
        specifier,
        maybe_headers,
        parsed_ast,
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
      let roots_vec: Vec<String> = roots.into_serde().map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
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
      for root_str in &roots_vec {
        let root = module_specifier::ModuleSpecifier::parse(root_str)
          .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
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

      let source_parser = ast::DefaultSourceParser::new();
      let builder = Builder::new(
        roots,
        false,
        &mut loader,
        maybe_resolver.as_ref().map(|r| r as &dyn Resolver),
        maybe_locker,
        &source_parser,
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
      let source_parser = ast::DefaultSourceParser::new();
      match graph::parse_module(
        &specifier,
        maybe_headers.as_ref(),
        Arc::new(content),
        None,
        maybe_resolver.as_ref().map(|r| r as &dyn Resolver),
        &source_parser,
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
  use anyhow::Error;
  use pretty_assertions::assert_eq;
  use serde_json::json;
  use source::tests::MockResolver;
  use source::CacheInfo;
  use source::MemoryLoader;

  type Sources<'a> = Vec<(
    &'a str,
    Result<(&'a str, Option<Vec<(&'a str, &'a str)>>, &'a str), Error>,
  )>;

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
          Ok((
            "file:///a/test01.ts",
            None,
            r#"import * as b from "./test02.ts";"#,
          )),
        ),
        (
          "file:///a/test02.ts",
          Ok(("file:///a/test02.ts", None, r#"export const b = "b";"#)),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(
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
    assert_eq!(graph.module_slots.len(), 2);
    assert_eq!(graph.roots, vec![root_specifier.clone()]);
    assert!(graph.contains(&root_specifier));
    assert!(
      !graph.contains(&ModuleSpecifier::parse("file:///a/test03.ts").unwrap())
    );
    let maybe_root_module = graph.module_slots.get(&root_specifier);
    assert!(maybe_root_module.is_some());
    let root_module_slot = maybe_root_module.unwrap();
    if let ModuleSlot::Module(Module::Es(module)) = root_module_slot {
      assert_eq!(module.dependencies.len(), 1);
      let maybe_dependency = module.dependencies.get("./test02.ts");
      assert!(maybe_dependency.is_some());
      let dependency_specifier =
        ModuleSpecifier::parse("file:///a/test02.ts").unwrap();
      let dependency = maybe_dependency.unwrap();
      assert!(!dependency.is_dynamic);
      if let Some(Ok((resolved_specifier, _))) = &dependency.maybe_code {
        assert_eq!(resolved_specifier, &dependency_specifier);
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
          Ok((
            "file:///a/test01.ts",
            None,
            r#"import * as b from "./test02.ts";"#,
          )),
        ),
        (
          "file:///a/test02.ts",
          Ok(("file:///a/test02.ts", None, r#"export const b = "b";"#)),
        ),
        (
          "https://example.com/a.ts",
          Ok((
            "https://example.com/a.ts",
            None,
            r#"import * as c from "./c.ts";"#,
          )),
        ),
        (
          "https://example.com/c.ts",
          Ok(("https://example.com/c.ts", None, r#"export const c = "c";"#)),
        ),
      ],
      vec![],
    );
    let roots = vec![
      ModuleSpecifier::parse("file:///a/test01.ts").unwrap(),
      ModuleSpecifier::parse("https://example.com/a.ts").unwrap(),
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
        Ok(("file:///a/test.json", None, r#"{"a": 1, "b": "c"}"#)),
      )],
      vec![],
    );
    let roots = vec![ModuleSpecifier::parse("file:///a/test.json").unwrap()];
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
          Ok((
            "file:///a/test.js",
            None,
            r#"
        const a = await import("./a.json");
        "#,
          )),
        ),
        (
          "file:///a/a.json",
          Ok(("file:///a/a.json", None, r#"{"a":"b"}"#)),
        ),
      ],
      vec![],
    );
    let roots = vec![ModuleSpecifier::parse("file:///a/test.js").unwrap()];
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
          "file:///a/test.js"
        ],
        "modules": [
          {
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
          Ok((
            "file:///a/test01.ts",
            None,
            r#"// @deno-types=./test02.d.ts
import * as a from "./test02.js";

console.log(a);
"#,
          )),
        ),
        (
          "file:///a/test02.js",
          Ok(("file:///a/test02.js", None, r#"export const b = "b";"#)),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(
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
    assert!(graph.valid().is_ok());
    assert!(graph.valid_types_only().is_err());
  }

  #[tokio::test]
  async fn test_valid_code_missing() {
    let mut loader = setup(
      vec![(
        "file:///a/test01.ts",
        Ok((
          "file:///a/test01.ts",
          None,
          r#"import * as a from "./test02.js";

console.log(a);
"#,
        )),
      )],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(
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
          Ok(("file:///a/test01.ts", None, r#"console.log("a");"#)),
        ),
        (
          "file:///a/types.d.ts",
          Ok((
            "file:///a/types.d.ts",
            None,
            r#"export type { A } from "./types_01.d.ts";"#,
          )),
        ),
        (
          "file:///a/types_01.d.ts",
          Ok(("file:///a/types_01.d.ts", None, r#"export class A {};"#)),
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
      vec![root_specifier],
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
            "dependencies": [],
            "mediaType": "TypeScript",
            "size": 17,
            "specifier": "file:///a/test01.ts"
          },
          {
            "specifier": "file:///a/tsconfig.json",
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
            "mediaType": "Dts",
            "size": 41,
            "specifier": "file:///a/types.d.ts"
          },
          {
            "dependencies": [],
            "mediaType": "Dts",
            "size": 18,
            "specifier": "file:///a/types_01.d.ts"
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
          Ok(("file:///a/test01.ts", None, r#"console.log("a");"#)),
        ),
        (
          "https://example.com/jsx-runtime",
          Ok((
            "https://example.com/jsx-runtime",
            Some(vec![
              ("content-type", "application/javascript"),
              ("x-typescript-types", "./jsx-runtime.d.ts"),
            ]),
            r#"export const a = "a";"#,
          )),
        ),
        (
          "https://example.com/jsx-runtime.d.ts",
          Ok((
            "https://example.com/jsx-runtime.d.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"export const a: "a";"#,
          )),
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
      vec![root_specifier],
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
        Ok((
          "https://example.com/a",
          Some(vec![(
            "content-type",
            "application/typescript; charset=utf-8",
          )]),
          r#"declare interface A { a: string; }"#,
        )),
      )],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("https://example.com/a").expect("bad url");
    let graph = create_graph(
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
    assert_eq!(graph.module_slots.len(), 1);
    assert_eq!(graph.roots, vec![root_specifier.clone()]);
    let maybe_root_module = graph.module_slots.get(&root_specifier);
    assert!(maybe_root_module.is_some());
    let root_module_slot = maybe_root_module.unwrap();
    if let ModuleSlot::Module(Module::Es(module)) = root_module_slot {
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
          Ok((
            "file:///a/test01.tsx",
            None,
            r#"/* @jsxImportSource https://example.com/preact */

            export function A() {
              <div>Hello Deno</div>
            }
            "#,
          )),
        ),
        (
          "https://example.com/preact/jsx-runtime",
          Ok((
            "https://example.com/preact/jsx-runtime/index.js",
            Some(vec![("content-type", "application/javascript")]),
            r#"export function jsx() {}"#,
          )),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.tsx").expect("bad url");
    let graph = create_graph(
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
            "mediaType": "TSX",
            "size": 147,
            "specifier": "file:///a/test01.tsx"
          },
          {
            "dependencies": [],
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
        Ok(("file:///a/test.ts", None, r#"import "foo";"#)),
      )],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad url");
    let graph = create_graph(
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
          Ok(("file:///a/test.ts", None, r#"import "./test.json";"#)),
        ),
        (
          "file:///a/test.json",
          Ok(("file:///a/test.json", None, r#"{"hello":"world"}"#)),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad url");
    let graph = create_graph(
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
          Ok((
            "file:///a/test01",
            None,
            r#"import * as b from "./test02.ts";"#,
          )),
        ),
        (
          "file:///a/test02.ts",
          Ok(("file:///a/test02.ts", None, r#"export const b = "b";"#)),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01").expect("bad url");
    let graph = create_graph(
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
    assert!(graph.valid().is_ok());
  }

  #[tokio::test]
  async fn test_crate_graph_with_dynamic_imports() {
    let mut loader = setup(
      vec![
        (
          "file:///a.ts",
          Ok(("file:///a.ts", None, r#"const b = await import("./b.ts");"#)),
        ),
        (
          "file:///b.ts",
          Ok(("file:///b.ts", None, r#"export const b = "b";"#)),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a.ts").expect("bad url");
    let graph = create_graph(
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
            "mediaType": "TypeScript",
            "size": 33,
            "specifier": "file:///a.ts"
          },
          {
            "dependencies": [],
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
          Ok((
            "file:///a/test.js",
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
"#,
          )),
        ),
        (
          "file:///a/types.d.ts",
          Ok(("file:///a/types.d.ts", None, r#"export type A = string;"#)),
        ),
        (
          "file:///a/other.ts",
          Ok((
            "file:///a/other.ts",
            None,
            r#"export type B = string | undefined;"#,
          )),
        ),
      ],
      vec![],
    );
    let root = ModuleSpecifier::parse("file:///a/test.js").unwrap();
    let graph = create_graph(
      vec![root.clone()],
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
            "dependencies": [],
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
            "mediaType": "JavaScript",
            "size": 137,
            "specifier": "file:///a/test.js"
          },
          {
            "dependencies": [],
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
          Ok((
            "https://example.com/a.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"import * as b from "./b";"#,
          )),
        ),
        (
          "https://example.com/b",
          Ok((
            "https://example.com/b.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"export const b = "b";"#,
          )),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("https://example.com/a").expect("bad url");
    let graph = create_graph(
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
      graph.roots,
      vec![ModuleSpecifier::parse("https://example.com/a").unwrap()]
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
          Ok((
            "https://example.com/a.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"import * as b from "./b";"#,
          )),
        ),
        (
          "https://example.com/b",
          Ok((
            "https://example.com/b.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"import * as a from "./a";
            export const b = "b";"#,
          )),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("https://example.com/a").expect("bad url");
    let graph = create_graph(
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
      graph.roots,
      vec![ModuleSpecifier::parse("https://example.com/a").unwrap()]
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
          Ok((
            "file:///a/test01.ts",
            None,
            r#"import * as b from "data:application/typescript,export%20*%20from%20%22https://example.com/c.ts%22;";"#,
          )),
        ),
        (
          "https://example.com/c.ts",
          Ok(("https://example.com/c.ts", None, r#"export const c = """#)),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(
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
    assert_eq!(graph.module_slots.len(), 3);
    let data_specifier = ModuleSpecifier::parse("data:application/typescript,export%20*%20from%20%22https://example.com/c.ts%22;").unwrap();
    let maybe_module = graph.get(&data_specifier);
    assert!(maybe_module.is_some());
    if let Module::Es(module) = maybe_module.unwrap() {
      assert_eq!(
        module.source.as_str(),
        r#"export * from "https://example.com/c.ts";"#
      );
    } else {
      panic!("unexpected module type");
    }
  }

  #[tokio::test]
  async fn test_create_graph_with_resolver() {
    let mut loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Ok(("file:///a/test01.ts", None, r#"import * as b from "b";"#)),
        ),
        (
          "file:///a/test02.ts",
          Ok(("file:///a/test02.ts", None, r#"export const b = "b";"#)),
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
      vec![root_specifier],
      false,
      None,
      &mut loader,
      maybe_resolver,
      None,
      None,
      None,
    )
    .await;
    let maybe_module = graph.get(&graph.roots[0]);
    assert!(maybe_module.is_some());
    if let Module::Es(module) = maybe_module.unwrap() {
      let maybe_dep = module.dependencies.get("b");
      assert!(maybe_dep.is_some());
      let dep = maybe_dep.unwrap();
      if let Some(Ok((dep_sepcifier, _))) = &dep.maybe_code {
        assert_eq!(
          dep_sepcifier,
          &ModuleSpecifier::parse("file:///a/test02.ts").unwrap()
        );
      } else {
        panic!("unexpected resolved type");
      }
    } else {
      panic!("unexpected module type");
    }
  }

  #[tokio::test]
  async fn test_create_graph_with_resolve_types() {
    let mut loader = setup(
      vec![
        (
          "file:///a.js",
          Ok(("file:///a.js", None, r#"export const a = "a";"#)),
        ),
        (
          "file:///a.d.ts",
          Ok(("file:///a.d.ts", None, r#"export const a: "a";"#)),
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
      vec![root_specifier],
      false,
      None,
      &mut loader,
      maybe_resolver,
      None,
      None,
      None,
    )
    .await;
    let maybe_module = graph.get(&graph.roots[0]);
    assert!(maybe_module.is_some());
    if let Module::Es(module) = maybe_module.unwrap() {
      assert_eq!(
        module.maybe_types_dependency,
        Some((
          "file:///a.js".to_string(),
          Some(Ok((
            ModuleSpecifier::parse("file:///a.d.ts").unwrap(),
            Range {
              specifier: ModuleSpecifier::parse("file:///package.json")
                .unwrap(),
              start: Position::zeroed(),
              end: Position::zeroed(),
            }
          )))
        ))
      );
    } else {
      panic!("unexpected module type");
    }
  }

  #[tokio::test]
  async fn test_create_graph_with_parser() {
    let mut loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Ok((
            "file:///a/test01.ts",
            None,
            r#"import * as b from "./test02.ts";"#,
          )),
        ),
        (
          "file:///a/test02.ts",
          Ok((
            "file:///a/test02.ts",
            None,
            r#"export const b = "b"; let t;"#,
          )),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let test02_specifier =
      ModuleSpecifier::parse("file:///a/test02.ts").expect("bad url");
    let parser = crate::ast::CapturingSourceParser::new();
    create_graph(
      vec![root_specifier.clone()],
      false,
      None,
      &mut loader,
      None,
      None,
      Some(&parser),
      None,
    )
    .await;
    let root_ast = parser.get_parsed_source(&root_specifier).unwrap();
    let test02_ast = parser.get_parsed_source(&test02_specifier).unwrap();
    assert_eq!(root_ast.module().body.len(), 1);
    assert_eq!(test02_ast.module().body.len(), 2);

    let non_existent =
      ModuleSpecifier::parse("file:///a/test03.ts").expect("bad url");
    assert!(parser.get_parsed_source(&non_existent).is_none());
  }

  #[tokio::test]
  async fn test_create_graph_import_assertions() {
    let mut loader = setup(
      vec![
        (
          "file:///a/test01.ts",
          Ok((
            "file:///a/test01.ts",
            None,
            r#"
            import a from "./a.json" assert { type: "json" };
            const b = await import("./b.json", { assert: { type: "json" } });
            export * as c from "./c.json" assert { type: "json" };
            const json = "json";
            const d = await import("./d.json", { assert: { type: json } });
            "#,
          )),
        ),
        (
          "file:///a/a.json",
          Ok(("file:///a/a.json", None, r#"{"a":"b"}"#)),
        ),
        (
          "file:///a/b.json",
          Ok(("file:///a/b.json", None, r#"{"b":1}"#)),
        ),
        (
          "file:///a/c.json",
          Ok(("file:///a/c.json", None, r#"{"c":"d"}"#)),
        ),
        (
          "file:///a/d.json",
          Ok(("file:///a/d.json", None, r#"{"d":4}"#)),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(
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
            "size": 9,
            "mediaType": "Json",
            "specifier": "file:///a/a.json"
          },
          {
            "size": 7,
            "mediaType": "Json",
            "specifier": "file:///a/b.json"
          },
          {
            "size": 9,
            "mediaType": "Json",
            "specifier": "file:///a/c.json"
          },
          {
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
          Ok((
            "file:///a/test01.ts",
            None,
            r#"
            import a from "./a.json";
            import b from "./a.json" assert { type: "json" };
            "#,
          )),
        ),
        (
          "file:///a/a.json",
          Ok(("file:///a/a.json", None, r#"{"a":"b"}"#)),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(
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
          Ok((
            "file:///a/test01.ts",
            None,
            r#"
            import a from "./a.json";
            import b from "./b.json" assert { type: "json" };
            import c from "./c.js" assert { type: "json" };
            import d from "./d.json" assert { type: "css" };
            import e from "./e.wasm";
            "#,
          )),
        ),
        (
          "file:///a/a.json",
          Ok(("file:///a/a.json", None, r#"{"a":"b"}"#)),
        ),
        (
          "file:///a/b.json",
          Ok(("file:///a/b.json", None, r#"{"a":"b"}"#)),
        ),
        (
          "file:///a/c.js",
          Ok(("file:///a/c.js", None, r#"export const c = "c";"#)),
        ),
        (
          "file:///a/d.json",
          Ok(("file:///a/d.json", None, r#"{"a":"b"}"#)),
        ),
        ("file:///a/e.wasm", Ok(("file:///a/e.wasm", None, ""))),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(
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
            "specifier": "file:///a/a.json",
            "error": "Expected a JavaScript or TypeScript module, but identified a Json module. Consider importing Json modules with an import assertion with the type of \"json\".\n  Specifier: file:///a/a.json"
          },
          {
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
          Ok((
            "file:///a/test01.ts",
            None,
            r#"
            import "./a.js";
            "#,
          )),
        ),
        (
          "file:///a/a.js",
          Ok(("file:///a/a.js", None, r#"import "./b.js";"#)),
        ),
        (
          "file:///a/b.js",
          Ok((
            "file:///a/b.js",
            None,
            r#"import "./c.js"; import "./d.js""#,
          )),
        ),
        ("file:///a/c.js", Ok(("file:///a/c.js", None, r#""#))),
        ("file:///a/d.js", Ok(("file:///a/d.js", None, r#""#))),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let reporter = CollectingReporter {
      on_loads: RefCell::new(vec![]),
    };
    let graph = create_graph(
      vec![root_specifier.clone()],
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
          Ok((
            "file:///a/test01.ts",
            None,
            r#"
            // @deno-types="./a.d.ts"
            import * as a from "./a.js";
            import type { B } from "./b.d.ts";
            import * as c from "https://example.com/c";
            import * as d from "./d.js";
            "#,
          )),
        ),
        (
          "file:///a/a.js",
          Ok(("file:///a/a.js", None, r#"export const a = "a""#)),
        ),
        (
          "file:///a/a.d.ts",
          Ok(("file:///a/a.d.ts", None, r#"export const a: "a";"#)),
        ),
        (
          "file:///a/b.d.ts",
          Ok(("file:///a/b.d.ts", None, r#"export interface B {}"#)),
        ),
        (
          "https://example.com/c",
          Ok((
            "https://example.com/c",
            Some(vec![
              ("x-typescript-types", "./c.d.ts"),
              ("content-type", "application/javascript"),
            ]),
            r#"export { c } from "./c.js";"#,
          )),
        ),
        (
          "https://example.com/c.d.ts",
          Ok((
            "https://example.com/c.d.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"export const c: "c";"#,
          )),
        ),
        (
          "https://example.com/c.js",
          Ok((
            "https://example.com/c.js",
            Some(vec![("content-type", "application/javascript")]),
            r#"export const c = "c";"#,
          )),
        ),
        (
          "file:///a/d.js",
          Ok(("file:///a/d.js", None, r#"export const d = "d";"#)),
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
            "dependencies": [],
            "mediaType": "Dts",
            "size": 20,
            "specifier": "file:///a/a.d.ts"
          },
          {
            "dependencies": [],
            "mediaType": "Dts",
            "size": 21,
            "specifier": "file:///a/b.d.ts"
          },
          {
            "dependencies": [],
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
            "mediaType": "TypeScript",
            "size": 236,
            "specifier": "file:///a/test01.ts"
          },
          {
            "dependencies": [],
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
            "mediaType": "JavaScript",
            "size": 27,
            "specifier": "https://example.com/c"
          },
          {
            "dependencies": [],
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
          Ok((
            "file:///a/test01.ts",
            None,
            r#"
            // @deno-types="./a.d.ts"
            import * as a from "./a.js";
            import type { B } from "./b.d.ts";
            import * as c from "https://example.com/c";
            import * as d from "./d.js";
            "#,
          )),
        ),
        (
          "file:///a/a.js",
          Ok(("file:///a/a.js", None, r#"export const a = "a""#)),
        ),
        (
          "file:///a/a.d.ts",
          Ok(("file:///a/a.d.ts", None, r#"export const a: "a";"#)),
        ),
        (
          "file:///a/b.d.ts",
          Ok(("file:///a/b.d.ts", None, r#"export interface B {}"#)),
        ),
        (
          "https://example.com/c",
          Ok((
            "https://example.com/c",
            Some(vec![
              ("x-typescript-types", "./c.d.ts"),
              ("content-type", "application/javascript"),
            ]),
            r#"export { c } from "./c.js";"#,
          )),
        ),
        (
          "https://example.com/c.d.ts",
          Ok((
            "https://example.com/c.d.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"export const c: "c";"#,
          )),
        ),
        (
          "https://example.com/c.js",
          Ok((
            "https://example.com/c.js",
            Some(vec![("content-type", "application/javascript")]),
            r#"export const c = "c";"#,
          )),
        ),
        (
          "file:///a/d.js",
          Ok(("file:///a/d.js", None, r#"export const d = "d";"#)),
        ),
      ],
      vec![],
    );
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_code_graph(
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
    // println!("{}", serde_json::to_string_pretty(&graph).unwrap());
    assert_eq!(
      json!(graph),
      json!({
        "roots": [
          "file:///a/test01.ts"
        ],
        "modules": [
          {
            "dependencies": [],
            "mediaType": "JavaScript",
            "size": 20,
            "specifier": "file:///a/a.js"
          },
          {
            "dependencies": [],
            "mediaType": "JavaScript",
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
            "size": 27,
            "specifier": "https://example.com/c"
          },
          {
            "dependencies": [],
            "mediaType": "JavaScript",
            "size": 21,
            "specifier": "https://example.com/c.js"
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
      Arc::new(
        r#"
    import { a } from "./a.ts";
    import * as b from "./b.ts";
    export { c } from "./c.ts";
    const d = await import("./d.ts");
    "#
        .to_string(),
      ),
      None,
      None,
    );
    assert!(result.is_ok());
    if let Module::Es(actual) = result.unwrap() {
      assert_eq!(actual.dependencies.len(), 4);
      assert_eq!(actual.specifier, specifier);
      assert_eq!(actual.media_type, MediaType::TypeScript);
    } else {
      panic!("unexpected module type");
    }
  }

  #[test]
  fn test_parse_module_import_assertions() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let result = parse_module(
      &specifier,
      None,
      Arc::new(
        r#"
    import a from "./a.json" assert { type: "json" };
    await import("./b.json", { assert: { type: "json" } });
    "#
        .to_string(),
      ),
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
      Arc::new(
        r#"
    /** @jsxImportSource https://example.com/preact */

    export function A() {
      return <div>Hello Deno</div>;
    }
    "#
        .to_string(),
      ),
      None,
      None,
    );
    assert!(result.is_ok());
    if let Module::Es(actual) = result.unwrap() {
      assert_eq!(actual.dependencies.len(), 1);
      let dep = actual
        .dependencies
        .get("https://example.com/preact/jsx-runtime")
        .unwrap();
      assert!(dep.maybe_code.is_some());
      let code_dep = dep.maybe_code.clone().unwrap();
      assert!(code_dep.is_ok());
      let (dep_specifier, _) = code_dep.unwrap();
      assert_eq!(
        dep_specifier,
        ModuleSpecifier::parse("https://example.com/preact/jsx-runtime")
          .unwrap()
      );
      assert!(dep.maybe_type.is_none());
      assert_eq!(actual.specifier, specifier);
      assert_eq!(actual.media_type, MediaType::Tsx);
    } else {
      panic!("unexpected module type");
    }
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
      Arc::new(
        r#"declare interface A {
  a: string;
}"#
          .to_string(),
      ),
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
      Arc::new(
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
        .to_string(),
      ),
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
        "mediaType": "JavaScript",
        "size": 137,
        "specifier": "file:///a/test.js"
      })
    );
  }
}
