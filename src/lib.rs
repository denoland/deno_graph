// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

mod ast;
#[cfg(feature = "wasm")]
mod checksum;
mod colors;
mod graph;
mod info;
#[cfg(feature = "wasm")]
mod js_graph;
mod media_type;
mod module_specifier;
pub mod source;

use graph::Builder;
#[cfg(feature = "rust")]
pub use graph::Module;
#[cfg(feature = "rust")]
pub use graph::ModuleGraph;
#[cfg(feature = "rust")]
pub use graph::ModuleGraphError;
use graph::ModuleSlot;
#[cfg(feature = "wasm")]
pub use js_graph::JsLoader;
#[cfg(feature = "wasm")]
pub use js_graph::JsLocker;
#[cfg(feature = "wasm")]
pub use js_graph::JsResolver;
#[cfg(feature = "rust")]
pub use media_type::MediaType;
#[cfg(feature = "rust")]
pub use module_specifier::ModuleSpecifier;
#[cfg(feature = "rust")]
use source::Loader;
use source::Locker;
use source::Resolver;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

/// Create a module graph, based on loanding and recursively analyzing the
/// dependencies of the module, returning the resulting graph.
#[cfg(feature = "rust")]
pub async fn create_graph(
  root_specifier: ModuleSpecifier,
  loader: Box<dyn Loader>,
  maybe_resolver: Option<Box<dyn Resolver>>,
  maybe_locker: Option<Rc<RefCell<dyn Locker>>>,
) -> ModuleGraph {
  let builder =
    Builder::new(root_specifier, false, loader, maybe_resolver, maybe_locker);
  builder.build().await
}

/// Parse an individual module, returning the module as a result, otherwise
/// erroring with a module graph error.
#[cfg(feature = "rust")]
pub fn parse_module(
  specifier: &ModuleSpecifier,
  maybe_headers: &Option<HashMap<String, String>>,
  content: &str,
  maybe_resolver: &Option<Box<dyn Resolver>>,
) -> Result<Module, ModuleGraphError> {
  match graph::parse_module(specifier, maybe_headers, content, maybe_resolver) {
    ModuleSlot::Module(module) => Ok(module),
    ModuleSlot::Err(err) => Err(err),
    _ => unreachable!("unreachable ModuleSlot variant"),
  }
}

#[cfg(feature = "wasm")]
#[wasm_bindgen(js_name = createGraph)]
pub async fn js_create_graph(
  root_specifier: String,
  load: js_sys::Function,
  maybe_cache_info: Option<js_sys::Function>,
  maybe_resolve: Option<js_sys::Function>,
  maybe_check: Option<js_sys::Function>,
  maybe_get_checksum: Option<js_sys::Function>,
) -> Result<js_graph::ModuleGraph, JsValue> {
  let loader = Box::new(js_graph::JsLoader::new(load, maybe_cache_info));
  let maybe_resolver: Option<Box<dyn Resolver>> =
    if let Some(resolve) = maybe_resolve {
      Some(Box::new(js_graph::JsResolver::new(resolve)))
    } else {
      None
    };
  let maybe_locker: Option<Rc<RefCell<dyn Locker>>> =
    if maybe_check.is_some() || maybe_get_checksum.is_some() {
      let locker = js_graph::JsLocker::new(maybe_check, maybe_get_checksum);
      Some(Rc::new(RefCell::new(locker)))
    } else {
      None
    };
  let root_specifier =
    module_specifier::ModuleSpecifier::parse(&root_specifier)
      .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
  let builder =
    Builder::new(root_specifier, false, loader, maybe_resolver, maybe_locker);
  let graph = builder.build().await;
  Ok(js_graph::ModuleGraph(graph))
}

#[cfg(feature = "wasm")]
#[wasm_bindgen(js_name = parseModule)]
pub fn js_parse_module(
  specifier: String,
  maybe_headers: JsValue,
  content: String,
  maybe_resolve: Option<js_sys::Function>,
) -> Result<js_graph::Module, JsValue> {
  let maybe_headers: Option<HashMap<String, String>> = maybe_headers
    .into_serde()
    .map_err(|err| js_sys::Error::new(&err.to_string()))?;
  let specifier = module_specifier::ModuleSpecifier::parse(&specifier)
    .map_err(|err| js_sys::Error::new(&err.to_string()))?;
  let maybe_resolver: Option<Box<dyn Resolver>> =
    if let Some(resolve) = maybe_resolve {
      Some(Box::new(js_graph::JsResolver::new(resolve)))
    } else {
      None
    };
  match graph::parse_module(
    &specifier,
    &maybe_headers,
    &content,
    &maybe_resolver,
  ) {
    ModuleSlot::Module(module) => Ok(js_graph::Module(module)),
    ModuleSlot::Err(err) => Err(js_sys::Error::new(&err.to_string()).into()),
    _ => unreachable!("unreachable ModuleSlot variant"),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::graph::Resolved;
  use anyhow::Error;
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
  ) -> Box<dyn Loader> {
    Box::new(MemoryLoader::new(sources, cache_info))
  }

  #[tokio::test]
  async fn test_create_graph() {
    let loader = setup(
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
    let graph = create_graph(root_specifier.clone(), loader, None, None).await;
    assert_eq!(graph.modules.len(), 2);
    assert_eq!(graph.root, root_specifier);
    let maybe_root_module = graph.modules.get(&root_specifier);
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
      if let Resolved::Specifier(resolved_specifier, _) = &dependency.maybe_code
      {
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
  async fn test_create_graph_with_headers() {
    let loader = setup(
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
    let graph = create_graph(root_specifier.clone(), loader, None, None).await;
    assert_eq!(graph.modules.len(), 1);
    assert_eq!(graph.root, root_specifier);
    let maybe_root_module = graph.modules.get(&root_specifier);
    assert!(maybe_root_module.is_some());
    let root_module_slot = maybe_root_module.unwrap();
    if let ModuleSlot::Module(module) = root_module_slot {
      assert_eq!(module.media_type, MediaType::TypeScript);
    } else {
      panic!("unspected module slot");
    }
  }

  #[tokio::test]
  async fn test_create_graph_with_data_url() {
    let loader = setup(
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
    let graph = create_graph(root_specifier.clone(), loader, None, None).await;
    assert_eq!(graph.modules.len(), 3);
    let data_specifier = ModuleSpecifier::parse("data:application/typescript,export%20*%20from%20%22https://example.com/c.ts%22;").unwrap();
    let maybe_module = graph.get(&data_specifier);
    assert!(maybe_module.is_some());
    let module = maybe_module.unwrap();
    assert_eq!(
      module.source,
      r#"export * from "https://example.com/c.ts";"#
    );
  }

  #[tokio::test]
  async fn test_create_graph_with_resolver() {
    let loader = setup(
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
    let resolver = MockResolver::new(vec![(
      "file:///a/test01.ts",
      vec![("b", "file:///a/test02.ts")],
    )]);
    let maybe_resolver: Option<Box<dyn Resolver>> = Some(Box::new(resolver));
    let root_specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let graph =
      create_graph(root_specifier, loader, maybe_resolver, None).await;
    let maybe_module = graph.get(&graph.root);
    assert!(maybe_module.is_some());
    let module = maybe_module.unwrap();
    let maybe_dep = module.dependencies.get("b");
    assert!(maybe_dep.is_some());
    let dep = maybe_dep.unwrap();
    if let Resolved::Specifier(dep_sepcifier, _) = &dep.maybe_code {
      assert_eq!(
        dep_sepcifier,
        &ModuleSpecifier::parse("file:///a/test02.ts").unwrap()
      );
    } else {
      panic!("unspected resolved type");
    }
  }

  #[test]
  fn test_parse_module() {
    let specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let result = parse_module(
      &specifier,
      &None,
      r#"
    import { a } from "./a.ts";
    import * as b from "./b.ts";
    export { c } from "./c.ts";
    const d = await import("./d.ts");
    "#,
      &None,
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
    let maybe_headers = Some(headers);
    let result = parse_module(
      &specifier,
      &maybe_headers,
      r#"declare interface A {
  a: string;
}"#,
      &None,
    );
    assert!(result.is_ok());
  }
}
