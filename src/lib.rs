// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

mod ast;
mod checksum;
mod graph;
mod js_graph;
mod media_type;
mod module_specifier;
pub mod source;

use graph::Builder;
pub use graph::ModuleGraph;
use js_graph::*;
pub use module_specifier::ModuleSpecifier;
use source::Loader;
use source::Locker;
use source::Resolver;

use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

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

#[wasm_bindgen]
pub async fn js_create_graph(
  root_specifier: String,
  load: js_sys::Function,
  maybe_cache_info: Option<js_sys::Function>,
  maybe_resolve: Option<js_sys::Function>,
  maybe_check: Option<js_sys::Function>,
  maybe_get_checksum: Option<js_sys::Function>,
) -> String {
  let loader = Box::new(JsLoader::new(load, maybe_cache_info));
  let maybe_resolver: Option<Box<dyn Resolver>> =
    if let Some(resolve) = maybe_resolve {
      Some(Box::new(JsResolver::new(resolve)))
    } else {
      None
    };
  let maybe_locker: Option<Rc<RefCell<dyn Locker>>> =
    if maybe_check.is_some() || maybe_get_checksum.is_some() {
      let locker = JsLocker::new(maybe_check, maybe_get_checksum);
      Some(Rc::new(RefCell::new(locker)))
    } else {
      None
    };
  let root_specifier = ModuleSpecifier::parse(&root_specifier).unwrap();
  let builder =
    Builder::new(root_specifier, false, loader, maybe_resolver, maybe_locker);
  let graph = builder.build().await;
  format!("{:?}", graph)
}

#[cfg(test)]
mod tests {
  use super::*;
  use anyhow::Error;
  use source::tests::*;

  fn setup(
    sources: Vec<(
      &str,
      Result<(&str, Option<Vec<(&str, &str)>>, &str), Error>,
    )>,
  ) -> Box<dyn Loader> {
    Box::new(MockLoader::new(sources))
  }

  #[tokio::test]
  async fn test_create_graph() {
    let loader = setup(vec![
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
    ]);
    let root_specifier =
      ModuleSpecifier::parse("file:///a/test01.ts").expect("bad url");
    let graph = create_graph(root_specifier, loader, None, None).await;
    println!("{:?}", graph);
  }

  #[tokio::test]
  async fn test_create_graph_with_resolver() {
    let loader = setup(vec![
      (
        "file:///a/test01.ts",
        Ok(("file:///a/test01.ts", None, r#"import * as b from "b";"#)),
      ),
      (
        "file:///a/test02.ts",
        Ok(("file:///a/test02.ts", None, r#"export const b = "b";"#)),
      ),
    ]);
    let resolver = MockResolver::new(vec![(
      "file:///a/test01.ts",
      vec![("b", "file:///a/test02.ts")],
    )]);
    let maybe_resolver: Option<Box<dyn Resolver>> = Some(Box::new(resolver));
    let root_specifier = ModuleSpecifier::parse("file:///a/test01.ts").unwrap();
    let graph =
      create_graph(root_specifier, loader, maybe_resolver, None).await;
    println!("{:?}", graph);
  }
}
