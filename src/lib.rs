// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

mod ast;
mod checksum;
mod colors;
mod graph;
mod info;
mod js_graph;
mod media_type;
mod module_specifier;
pub mod source;

use graph::Builder;
pub use graph::Module;
pub use graph::ModuleGraph;
pub use graph::ModuleGraphError;
use graph::ModuleSlot;
pub use module_specifier::ModuleSpecifier;
use source::Loader;
use source::Locker;
use source::Resolver;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

/// Create a module graph, based on loanding and recursively analyzing the
/// dependencies of the module, returning the resulting graph.
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

#[wasm_bindgen(js_name = createGraph)]
pub async fn js_create_graph(
  root_specifier: String,
  load: js_sys::Function,
  maybe_cache_info: Option<js_sys::Function>,
  maybe_resolve: Option<js_sys::Function>,
  maybe_check: Option<js_sys::Function>,
  maybe_get_checksum: Option<js_sys::Function>,
) -> js_graph::ModuleGraph {
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
  let root_specifier = ModuleSpecifier::parse(&root_specifier).unwrap();
  let builder =
    Builder::new(root_specifier, false, loader, maybe_resolver, maybe_locker);
  let graph = builder.build().await;
  js_graph::ModuleGraph(graph)
}

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
  let specifier = ModuleSpecifier::parse(&specifier)
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
  use anyhow::Error;
  use serde_json::to_string_pretty;
  use source::tests::*;
  use source::CacheInfo;

  type Sources<'a> = Vec<(
    &'a str,
    Result<(&'a str, Option<Vec<(&'a str, &'a str)>>, &'a str), Error>,
  )>;

  fn setup(
    sources: Sources,
    cache_info: Vec<(&str, CacheInfo)>,
  ) -> Box<dyn Loader> {
    Box::new(MockLoader::new(sources, cache_info))
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
    let graph = create_graph(root_specifier, loader, None, None).await;
    println!("{:?}", graph);
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
    println!("{}", to_string_pretty(&graph).unwrap());
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
    println!("{}", to_string_pretty(&actual).unwrap());
  }
}
