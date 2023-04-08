// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

// todo(dsherret): move the integration-like tests to this file because it
// helps ensure we're testing the public API and ensures we export types
// out of deno_graph that should be public

use std::cell::RefCell;
use std::rc::Rc;

use anyhow::anyhow;
use deno_ast::ModuleSpecifier;
use deno_graph::source::MemoryLoader;
use deno_graph::source::NpmResolver;
use deno_graph::source::UnknownBuiltInNodeModuleError;
use deno_graph::BuildOptions;
use deno_graph::ModuleGraph;
use deno_graph::NpmPackageReqResolution;
use deno_semver::npm::NpmPackageNv;
use deno_semver::npm::NpmPackageReq;
use deno_semver::Version;
use futures::future::LocalBoxFuture;

#[tokio::test]
async fn test_npm_version_not_found_then_found() {
  #[derive(Debug)]
  struct TestNpmResolver {
    made_first_request: Rc<RefCell<bool>>,
    should_never_succeed: bool,
    number_times_load_called: Rc<RefCell<u32>>,
  }

  impl NpmResolver for TestNpmResolver {
    fn resolve_builtin_node_module(
      &self,
      _specifier: &ModuleSpecifier,
    ) -> Result<Option<String>, UnknownBuiltInNodeModuleError> {
      Ok(None)
    }

    fn load_and_cache_npm_package_info(
      &self,
      _package_name: &str,
    ) -> LocalBoxFuture<'static, Result<(), anyhow::Error>> {
      *self.number_times_load_called.borrow_mut() += 1;
      Box::pin(futures::future::ready(Ok(())))
    }

    fn resolve_npm(
      &self,
      package_req: &NpmPackageReq,
    ) -> NpmPackageReqResolution {
      let mut value = self.made_first_request.borrow_mut();
      if *value && !self.should_never_succeed {
        assert_eq!(*self.number_times_load_called.borrow(), 2);
        NpmPackageReqResolution::Ok(NpmPackageNv {
          name: package_req.name.clone(),
          version: Version::parse_from_npm("1.0.0").unwrap(),
        })
      } else {
        *value = true;
        NpmPackageReqResolution::ReloadRegistryInfo(anyhow!(
          "failed to resolve"
        ))
      }
    }
  }

  let mut loader = MemoryLoader::default();
  loader.add_source_with_text("file:///main.ts", "import 'npm:foo@1.0';");
  let root = ModuleSpecifier::parse("file:///main.ts").unwrap();

  {
    let npm_resolver = TestNpmResolver {
      made_first_request: Rc::new(RefCell::new(false)),
      number_times_load_called: Rc::new(RefCell::new(0)),
      should_never_succeed: false,
    };

    let mut graph = ModuleGraph::default();
    graph
      .build(
        vec![root.clone()],
        &mut loader,
        BuildOptions {
          npm_resolver: Some(&npm_resolver),
          ..Default::default()
        },
      )
      .await;
    assert!(graph.valid().is_ok());
    assert_eq!(
      graph
        .modules()
        .map(|m| m.specifier().to_string())
        .collect::<Vec<_>>(),
      vec![root.as_str(), "npm:foo@1.0.0"]
    );
  }

  // now try never succeeding
  {
    let npm_resolver = TestNpmResolver {
      made_first_request: Rc::new(RefCell::new(false)),
      number_times_load_called: Rc::new(RefCell::new(0)),
      should_never_succeed: true,
    };

    let mut graph = ModuleGraph::default();
    graph
      .build(
        vec![root.clone()],
        &mut loader,
        BuildOptions {
          npm_resolver: Some(&npm_resolver),
          ..Default::default()
        },
      )
      .await;
    assert_eq!(
      graph.valid().err().unwrap().to_string(),
      "failed to resolve"
    );
  }
}
