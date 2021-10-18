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
    pub use ast::Location;
    pub use ast::Position;
    pub use ast::Span;
    pub use graph::Module;
    pub use graph::ModuleGraph;
    pub use graph::ModuleGraphError;
    pub use graph::ResolutionError;
    pub use graph::Resolved;
    pub use deno_ast::MediaType;
    pub use module_specifier::ModuleSpecifier;

    use source::Loader;

    /// Create a module graph, based on loading and recursively analyzing the
    /// dependencies of the module, returning the resulting graph.
    pub async fn create_graph(
      roots: Vec<ModuleSpecifier>,
      is_dynamic: bool,
      maybe_imports: Option<Vec<(ModuleSpecifier, Vec<String>)>>,
      loader: &mut dyn Loader,
      maybe_resolver: Option<&dyn Resolver>,
      maybe_locker: Option<Rc<RefCell<Box<dyn Locker>>>>,
      maybe_parser: Option<&dyn SourceParser>,
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
      );
      builder.build(maybe_imports).await
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
        maybe_resolver,
        source_parser,
        true,
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
      maybe_cache_info: Option<js_sys::Function>,
      maybe_resolve: Option<js_sys::Function>,
      maybe_resolve_types: Option<js_sys::Function>,
      maybe_check: Option<js_sys::Function>,
      maybe_get_checksum: Option<js_sys::Function>,
      maybe_lockfile_name: Option<String>,
      maybe_imports: JsValue,
    ) -> Result<js_graph::ModuleGraph, JsValue> {
      let roots_vec: Vec<String> = roots.into_serde().map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
      let maybe_imports_map: Option<HashMap<String, Vec<String>>> = maybe_imports.into_serde().map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
      let mut loader = js_graph::JsLoader::new(load, maybe_cache_info);
      let maybe_resolver = if maybe_resolve.is_some() || maybe_resolve_types.is_some() {
        Some(js_graph::JsResolver::new(maybe_resolve, maybe_resolve_types))
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
      );
      let graph = builder.build(maybe_imports).await;
      Ok(js_graph::ModuleGraph(graph))
    }

    #[wasm_bindgen(js_name = parseModule)]
    pub fn js_parse_module(
      specifier: String,
      maybe_headers: JsValue,
      content: String,
      maybe_resolve: Option<js_sys::Function>,
      maybe_resolve_types: Option<js_sys::Function>,
    ) -> Result<js_graph::Module, JsValue> {
      let maybe_headers: Option<HashMap<String, String>> = maybe_headers
        .into_serde()
        .map_err(|err| js_sys::Error::new(&err.to_string()))?;
      let specifier = module_specifier::ModuleSpecifier::parse(&specifier)
        .map_err(|err| js_sys::Error::new(&err.to_string()))?;
      let maybe_resolver = if maybe_resolve.is_some() || maybe_resolve_types.is_some() {
        Some(js_graph::JsResolver::new(maybe_resolve, maybe_resolve_types))
      } else {
        None
      };
      let source_parser = ast::DefaultSourceParser::new();
      match graph::parse_module(
        &specifier,
        maybe_headers.as_ref(),
        Arc::new(content),
        maybe_resolver.as_ref().map(|r| r as &dyn Resolver),
        &source_parser,
        true,
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
    if let ModuleSlot::Module(module) = root_module_slot {
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
      panic!("unspected module slot");
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
    let graph =
      create_graph(roots.clone(), false, None, &mut loader, None, None, None)
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
    )
    .await;
    assert!(graph.valid().is_err());
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
                "code": {
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
    )
    .await;
    assert_eq!(graph.module_slots.len(), 1);
    assert_eq!(graph.roots, vec![root_specifier.clone()]);
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
    )
    .await;
    assert_eq!(graph.module_slots.len(), 3);
    let data_specifier = ModuleSpecifier::parse("data:application/typescript,export%20*%20from%20%22https://example.com/c.ts%22;").unwrap();
    let maybe_module = graph.get(&data_specifier);
    assert!(maybe_module.is_some());
    let module = maybe_module.unwrap();
    assert_eq!(
      module.source.as_str(),
      r#"export * from "https://example.com/c.ts";"#
    );
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
    )
    .await;
    let maybe_module = graph.get(&graph.roots[0]);
    assert!(maybe_module.is_some());
    let module = maybe_module.unwrap();
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
          Some(ast::Span {
            specifier: ModuleSpecifier::parse("file:///package.json").unwrap(),
            range: ast::Range::default(),
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
    )
    .await;
    let maybe_module = graph.get(&graph.roots[0]);
    assert!(maybe_module.is_some());
    let module = maybe_module.unwrap();
    assert_eq!(
      module.maybe_types_dependency,
      Some((
        "file:///a.js".to_string(),
        Some(Ok((
          ModuleSpecifier::parse("file:///a.d.ts").unwrap(),
          ast::Span {
            specifier: ModuleSpecifier::parse("file:///package.json").unwrap(),
            range: ast::Range::default(),
          }
        )))
      ))
    );
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
    let actual = result.unwrap();
    assert_eq!(actual.dependencies.len(), 4);
    assert_eq!(actual.specifier, specifier);
    assert_eq!(actual.media_type, MediaType::TypeScript);
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
}
