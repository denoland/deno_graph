use std::cell::RefCell;
use std::collections::BTreeMap;

use anyhow::Result;
use deno_ast::ModuleSpecifier;
use deno_graph::type_tracer::trace_public_types;
use deno_graph::type_tracer::TypeTraceDiagnostic;
use deno_graph::type_tracer::TypeTraceHandler;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::DefaultModuleParser;
use deno_graph::GraphKind;

use super::InMemoryLoader;

#[derive(Default)]
struct TestTypeTraceHandler {
  diagnostics: RefCell<Vec<TypeTraceDiagnostic>>,
}

impl TestTypeTraceHandler {
  pub fn diagnostics(self) -> Vec<TypeTraceDiagnostic> {
    self.diagnostics.take()
  }
}

impl TypeTraceHandler for TestTypeTraceHandler {
  fn diagnostic(&self, diagnostic: TypeTraceDiagnostic) {
    self.diagnostics.borrow_mut().push(diagnostic);
  }
}

pub struct TypeTraceResult {
  pub output: String,
  pub diagnostics: Vec<TypeTraceDiagnostic>,
}

pub struct TestBuilder {
  loader: InMemoryLoader,
  entry_point: String,
}

impl TestBuilder {
  pub fn new() -> Self {
    let loader = InMemoryLoader::default();
    Self {
      loader,
      entry_point: "file:///mod.ts".to_string(),
    }
  }

  pub fn with_loader(
    &mut self,
    mut action: impl FnMut(&mut InMemoryLoader),
  ) -> &mut Self {
    action(&mut self.loader);
    self
  }

  pub fn entry_point(&mut self, value: impl AsRef<str>) -> &mut Self {
    self.entry_point = value.as_ref().to_string();
    self
  }

  pub async fn trace(&mut self) -> Result<TypeTraceResult> {
    let handler = TestTypeTraceHandler::default();
    let mut graph = deno_graph::ModuleGraph::new(GraphKind::All);
    let roots = vec![ModuleSpecifier::parse(&self.entry_point).unwrap()];
    graph
      .build(
        roots.clone(),
        &mut self.loader,
        deno_graph::BuildOptions::default(),
      )
      .await;
    let source_parser = DefaultModuleParser::new_for_analysis();
    let capturing_analyzer =
      CapturingModuleAnalyzer::new(Some(Box::new(source_parser)), None);
    let output = trace_public_types(
      &graph,
      &roots,
      &capturing_analyzer.as_capturing_parser(),
      &handler,
    )?;
    Ok(TypeTraceResult {
      output: {
        let output = output
          .into_inner()
          .into_iter()
          .map(|(k, v)| (k.to_string(), v))
          .collect::<BTreeMap<_, _>>();
        format!("{:#?}", output)
      },
      diagnostics: {
        let diagnostics = handler.diagnostics.borrow();
        diagnostics.clone()
      },
    })
  }
}
