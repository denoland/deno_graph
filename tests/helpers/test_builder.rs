use std::collections::BTreeMap;

use anyhow::Result;
use deno_ast::ModuleSpecifier;
use deno_ast::SourceRanged;
use deno_graph::source::MemoryLoader;
use deno_graph::BuildDiagnostic;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::DefaultModuleParser;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;

#[cfg(feature = "type_tracing")]
pub mod tracing {
  use std::cell::RefCell;

  use deno_graph::type_tracer::RootSymbol;
  use deno_graph::type_tracer::TypeTraceDiagnostic;
  use deno_graph::type_tracer::TypeTraceHandler;
  use deno_graph::ModuleGraph;

  #[derive(Default)]
  pub struct TestTypeTraceHandler {
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
    pub graph: ModuleGraph,
    pub root_symbol: RootSymbol,
    pub output: String,
    pub diagnostics: Vec<TypeTraceDiagnostic>,
  }
}

pub struct BuildResult {
  pub graph: ModuleGraph,
  pub diagnostics: Vec<BuildDiagnostic>,
}

pub struct TestBuilder {
  loader: MemoryLoader,
  entry_point: String,
}

impl TestBuilder {
  pub fn new() -> Self {
    Self {
      loader: Default::default(),
      entry_point: "file:///mod.ts".to_string(),
    }
  }

  pub fn with_loader(
    &mut self,
    mut action: impl FnMut(&mut MemoryLoader),
  ) -> &mut Self {
    action(&mut self.loader);
    self
  }

  #[allow(dead_code)]
  pub fn entry_point(&mut self, value: impl AsRef<str>) -> &mut Self {
    self.entry_point = value.as_ref().to_string();
    self
  }

  pub async fn build(&mut self) -> BuildResult {
    let mut graph = deno_graph::ModuleGraph::new(GraphKind::All);
    let entry_point_url = ModuleSpecifier::parse(&self.entry_point).unwrap();
    let roots = vec![entry_point_url.clone()];
    let diagnostics = graph
      .build(
        roots.clone(),
        &mut self.loader,
        deno_graph::BuildOptions::default(),
      )
      .await;
    BuildResult { graph, diagnostics }
  }

  #[cfg(feature = "type_tracing")]
  pub async fn trace(&mut self) -> Result<tracing::TypeTraceResult> {
    let handler = tracing::TestTypeTraceHandler::default();
    let mut graph = deno_graph::ModuleGraph::new(GraphKind::All);
    let entry_point_url = ModuleSpecifier::parse(&self.entry_point).unwrap();
    let roots = vec![entry_point_url.clone()];
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
    let root_symbol = deno_graph::type_tracer::trace_public_types(
      &graph,
      &roots,
      &capturing_analyzer.as_capturing_parser(),
      &handler,
    )?;
    Ok(tracing::TypeTraceResult {
      graph: graph.clone(),
      root_symbol: root_symbol.clone(),
      output: {
        let entrypoint_symbol = root_symbol
          .get_module_from_specifier(&entry_point_url)
          .unwrap();
        let mut output_text = String::new();
        for (k, v) in root_symbol.clone().into_specifier_map() {
          output_text.push_str(&format!("{}: {:#?}\n", k.as_str(), v));
        }
        let get_symbol_text =
          |module_symbol: &deno_graph::type_tracer::ModuleSymbol,
           symbol_id: deno_graph::type_tracer::SymbolId| {
            let symbol = module_symbol.symbol(symbol_id).unwrap();
            let definitions =
              root_symbol.go_to_definitions(&graph, module_symbol, symbol);
            if definitions.is_empty() {
              "NONE".to_string()
            } else {
              let mut results = Vec::new();
              for definition in definitions {
                let decl_text = {
                  let decl_text = definition
                    .range
                    .text_fast(definition.module.source().text_info());
                  let lines = decl_text.split('\n').collect::<Vec<_>>();
                  if lines.len() > 4 {
                    lines[0..2]
                      .iter()
                      .chain(std::iter::once(&"..."))
                      .chain(&lines[lines.len() - 2..])
                      .cloned()
                      .collect::<Vec<_>>()
                  } else {
                    lines
                  }
                  .into_iter()
                  .map(|line| format!("  {}", line).trim_end().to_string())
                  .collect::<Vec<_>>()
                  .join("\n")
                };
                let range = definition.range.as_byte_range(
                  definition.module.source().text_info().range().start,
                );
                results.push(format!(
                  "{}:{}..{}\n{}",
                  definition.module.specifier(),
                  range.start,
                  range.end,
                  decl_text
                ));
              }
              results.join("\n")
            }
          };
        let exports = entrypoint_symbol
          .exports(&graph, &root_symbol)
          .into_iter()
          .collect::<BTreeMap<_, _>>();
        if !exports.is_empty() {
          output_text.push_str("== export definitions ==\n");
          for (name, (module_symbol, symbol_id)) in exports {
            let position = get_symbol_text(module_symbol, symbol_id);
            output_text.push_str(&format!("[{}]: {}\n", name, position));
          }
        }
        output_text
      },
      diagnostics: handler.diagnostics(),
    })
  }
}
