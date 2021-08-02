// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

mod ast;
mod graph;
mod media_type;
mod module_specifier;
pub mod source;

use graph::Builder;
pub use graph::ModuleGraph;
pub use module_specifier::ModuleSpecifier;
use source::Loader;
use source::Locker;
use source::Resolver;

use parking_lot::Mutex;
use std::sync::Arc;

pub async fn create_graph(
  root_specifier: ModuleSpecifier,
  loader: Arc<Mutex<dyn Loader>>,
  maybe_resolver: Option<Arc<Mutex<dyn Resolver>>>,
  maybe_locker: Option<Arc<Mutex<dyn Locker>>>,
) -> ModuleGraph {
  let builder =
    Builder::new(root_specifier, false, loader, maybe_resolver, maybe_locker);
  builder.build().await
}

#[cfg(test)]
mod tests {
  use super::*;
  use anyhow::Error;
  use source::tests::MockLoader;

  fn setup(
    sources: Vec<(
      &str,
      Result<(&str, Option<Vec<(&str, &str)>>, &str), Error>,
    )>,
  ) -> Arc<Mutex<dyn Loader>> {
    Arc::new(Mutex::new(MockLoader::new(sources)))
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
}
