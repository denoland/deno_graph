use std::cell::RefCell;
use std::collections::BTreeMap;

use anyhow::Result;
use deno_ast::ModuleSpecifier;
use deno_ast::SourceRanged;
use deno_graph::type_tracer::trace_public_types;
use deno_graph::type_tracer::RootsGraphSymbol;
use deno_graph::type_tracer::SymbolId;
use deno_graph::type_tracer::TypeTraceDiagnostic;
use deno_graph::type_tracer::TypeTraceHandler;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::DefaultModuleParser;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;

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
  pub graph: ModuleGraph,
  pub roots_graph_symbol: RootsGraphSymbol,
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
    let output = trace_public_types(
      &graph,
      &roots,
      &capturing_analyzer.as_capturing_parser(),
      &handler,
    )?;
    Ok(TypeTraceResult {
      graph: graph.clone(),
      roots_graph_symbol: output.clone(),
      output: {
        let entrypoint_symbol = output.get(&entry_point_url).unwrap();
        let sorted_symbols = output
          .clone()
          .into_inner()
          .into_iter()
          .map(|(k, v)| (k.to_string(), v))
          .collect::<BTreeMap<_, _>>();
        let mut output_text = format!("{:#?}\n", sorted_symbols);
        output_text.push_str("== export definitions ==\n");
        let get_symbol_text = |symbol_id: SymbolId| {
          let symbol = entrypoint_symbol.symbol(symbol_id).unwrap();
          let definitions =
            output.go_to_definitions(&graph, entrypoint_symbol, symbol);
          if definitions.is_empty() {
            format!("NONE")
          } else {
            let mut results = Vec::new();
            for definition in definitions {
              let decl_text = {
                let decl_text = definition
                  .decl
                  .range
                  .text_fast(definition.module.source().text_info());
                let lines = decl_text.split('\n').collect::<Vec<_>>();
                if lines.len() > 4 {
                  lines[0..2]
                    .into_iter()
                    .chain(std::iter::once(&"..."))
                    .chain(&lines[lines.len() - 2..])
                    .map(|l| *l)
                    .collect::<Vec<_>>()
                } else {
                  lines
                }
                .into_iter()
                .map(|line| format!("  {}", line).trim_end().to_string())
                .collect::<Vec<_>>()
                .join("\n")
              };
              let range = definition.decl.range.as_byte_range(
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
        for (name, symbol_id) in entrypoint_symbol.exports() {
          let position = get_symbol_text(*symbol_id);
          output_text.push_str(&format!("[{}]: {}\n", name, position));
        }
        output_text
      },
      diagnostics: {
        let diagnostics = handler.diagnostics.borrow();
        diagnostics.clone()
      },
    })
  }
}
