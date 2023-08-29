// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

// todo(dsherret): move the integration-like tests to this file because it
// helps ensure we're testing the public API and ensures we export types
// out of deno_graph that should be public

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use anyhow::anyhow;
use deno_ast::ModuleSpecifier;
use deno_graph::source::MemoryLoader;
use deno_graph::source::NpmResolver;
use deno_graph::source::UnknownBuiltInNodeModuleError;
use deno_graph::BuildOptions;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::NpmPackageReqResolution;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use deno_semver::Version;
use futures::future::LocalBoxFuture;
use pretty_assertions::assert_eq;

use crate::helpers::get_specs_in_dir;
use crate::helpers::TestBuilder;

mod helpers;

#[tokio::test]
async fn test_graph_specs() {
  for (test_file_path, spec) in
    get_specs_in_dir(&PathBuf::from("./tests/specs/graph"))
  {
    eprintln!("Running {}", test_file_path.display());
    let mut builder = TestBuilder::new();
    builder.with_loader(|loader| {
      for file in &spec.files {
        loader.add_source_with_text(file.url(), &file.text);
      }
    });
    builder.workspace_members(spec.workspace_members.clone());

    let result = builder.build().await;
    let update_var = std::env::var("UPDATE");
    let mut output_text = serde_json::to_string_pretty(&result.graph).unwrap();
    output_text.push('\n');
    let diagnostics = result
      .diagnostics
      .iter()
      .map(|d| serde_json::to_value(d.to_string()).unwrap())
      .collect::<Vec<_>>();
    let spec = if update_var.as_ref().map(|v| v.as_str()) == Ok("1") {
      let mut spec = spec;
      spec.output_file.text = output_text.clone();
      spec.diagnostics = diagnostics.clone();
      std::fs::write(&test_file_path, spec.emit()).unwrap();
      spec
    } else {
      spec
    };
    assert_eq!(
      output_text,
      spec.output_file.text,
      "Should be same for {}",
      test_file_path.display()
    );
    assert_eq!(
      diagnostics,
      spec.diagnostics,
      "Should be same for {}",
      test_file_path.display()
    );
  }
}

#[cfg(feature = "type_tracing")]
#[tokio::test]
async fn test_type_tracing_specs() {
  for (test_file_path, spec) in
    get_specs_in_dir(&PathBuf::from("./tests/specs/type_tracing"))
  {
    eprintln!("Running {}", test_file_path.display());
    let mut builder = TestBuilder::new();
    builder.with_loader(|loader| {
      for file in &spec.files {
        loader.add_source_with_text(file.url(), &file.text);
      }
    });

    let result = builder.trace().await.unwrap();
    let update_var = std::env::var("UPDATE");
    let diagnostics = result
      .diagnostics
      .iter()
      .map(|d| serde_json::to_value(d.clone()).unwrap())
      .collect::<Vec<_>>();
    let spec = if update_var.as_ref().map(|v| v.as_str()) == Ok("1") {
      let mut spec = spec;
      spec.output_file.text = result.output.clone();
      spec.diagnostics = diagnostics.clone();
      std::fs::write(&test_file_path, spec.emit()).unwrap();
      spec
    } else {
      spec
    };
    assert_eq!(
      result.output,
      spec.output_file.text,
      "Should be same for {}",
      test_file_path.display()
    );
    assert_eq!(
      diagnostics,
      spec.diagnostics,
      "Should be same for {}",
      test_file_path.display()
    );
  }
}

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

    fn resolve_npm(&self, package_req: &PackageReq) -> NpmPackageReqResolution {
      let mut value = self.made_first_request.borrow_mut();
      if *value && !self.should_never_succeed {
        assert_eq!(*self.number_times_load_called.borrow(), 2);
        NpmPackageReqResolution::Ok(PackageNv {
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

    let mut graph = ModuleGraph::new(GraphKind::All);
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
      vec![root.as_str(), "npm:/foo@1.0.0"]
    );
  }

  // now try never succeeding
  {
    let npm_resolver = TestNpmResolver {
      made_first_request: Rc::new(RefCell::new(false)),
      number_times_load_called: Rc::new(RefCell::new(0)),
      should_never_succeed: true,
    };

    let mut graph = ModuleGraph::new(GraphKind::All);
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
