// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use deno_ast::ModuleSpecifier;
use deno_graph::source::CacheInfo;
use deno_graph::source::LoadFuture;
use deno_graph::source::Loader;
use deno_graph::source::LoaderCacheSetting;
use deno_graph::source::MemoryLoader;
use deno_graph::BuildDiagnostic;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::WorkspaceMember;

#[derive(Default)]
pub struct TestLoader {
  pub cache: MemoryLoader,
  pub remote: MemoryLoader,
}

impl Loader for TestLoader {
  fn get_cache_info(&self, specifier: &ModuleSpecifier) -> Option<CacheInfo> {
    self.cache.get_cache_info(specifier)
  }

  fn load_with_cache_setting(
    &mut self,
    specifier: &ModuleSpecifier,
    is_dynamic: bool,
    cache_setting: LoaderCacheSetting,
  ) -> LoadFuture {
    match cache_setting {
      // todo(dsherret): in the future, actually make this use the cache
      LoaderCacheSetting::Prefer => self.remote.load_with_cache_setting(
        specifier,
        is_dynamic,
        cache_setting,
      ),
      // todo(dsherret): in the future, make this update the cache
      LoaderCacheSetting::Reload => self.remote.load_with_cache_setting(
        specifier,
        is_dynamic,
        cache_setting,
      ),
      LoaderCacheSetting::Only => {
        self
          .cache
          .load_with_cache_setting(specifier, is_dynamic, cache_setting)
      }
    }
  }
}

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
  loader: TestLoader,
  entry_point: String,
  workspace_members: Vec<WorkspaceMember>,
}

impl TestBuilder {
  pub fn new() -> Self {
    Self {
      loader: Default::default(),
      entry_point: "file:///mod.ts".to_string(),
      workspace_members: Default::default(),
    }
  }

  pub fn with_loader(
    &mut self,
    mut action: impl FnMut(&mut TestLoader),
  ) -> &mut Self {
    action(&mut self.loader);
    self
  }

  #[allow(dead_code)]
  pub fn entry_point(&mut self, value: impl AsRef<str>) -> &mut Self {
    self.entry_point = value.as_ref().to_string();
    self
  }

  pub fn workspace_members(
    &mut self,
    members: Vec<WorkspaceMember>,
  ) -> &mut Self {
    self.workspace_members = members;
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
        deno_graph::BuildOptions {
          workspace_members: self.workspace_members.clone(),
          ..Default::default()
        },
      )
      .await;
    BuildResult { graph, diagnostics }
  }

  #[cfg(feature = "type_tracing")]
  pub async fn trace(&mut self) -> anyhow::Result<tracing::TypeTraceResult> {
    use deno_ast::SourceRanged;

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
    let source_parser = deno_graph::DefaultModuleParser::new_for_analysis();
    let capturing_analyzer = deno_graph::CapturingModuleAnalyzer::new(
      Some(Box::new(source_parser)),
      None,
    );
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
          .collect::<std::collections::BTreeMap<_, _>>();
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
