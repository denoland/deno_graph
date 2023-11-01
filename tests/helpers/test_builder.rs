// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use deno_ast::ModuleSpecifier;
use deno_graph::source::CacheInfo;
use deno_graph::source::CacheSetting;
use deno_graph::source::LoadFuture;
use deno_graph::source::Loader;
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

  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    is_dynamic: bool,
    cache_setting: CacheSetting,
  ) -> LoadFuture {
    match cache_setting {
      // todo(dsherret): in the future, actually make this use the cache
      CacheSetting::Use => {
        self.remote.load(specifier, is_dynamic, cache_setting)
      }
      // todo(dsherret): in the future, make this update the cache
      CacheSetting::Reload => {
        self.remote.load(specifier, is_dynamic, cache_setting)
      }
      CacheSetting::Only => {
        self.cache.load(specifier, is_dynamic, cache_setting)
      }
    }
  }
}

#[cfg(feature = "symbols")]
pub mod symbols {
  use deno_graph::symbols::SymbolFillDiagnostic;

  pub struct SymbolsResult {
    pub output: String,
    pub diagnostics: Vec<SymbolFillDiagnostic>,
  }
}

pub struct BuildResult {
  pub graph: ModuleGraph,
  pub diagnostics: Vec<BuildDiagnostic>,
}

pub struct TestBuilder {
  loader: TestLoader,
  entry_point: String,
  entry_point_types: String,
  workspace_members: Vec<WorkspaceMember>,
}

impl TestBuilder {
  pub fn new() -> Self {
    Self {
      loader: Default::default(),
      entry_point: "file:///mod.ts".to_string(),
      entry_point_types: "file:///mod.ts".to_string(),
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

  pub fn entry_point(&mut self, value: impl AsRef<str>) -> &mut Self {
    self.entry_point = value.as_ref().to_string();
    self
  }

  pub fn entry_point_types(&mut self, value: impl AsRef<str>) -> &mut Self {
    self.entry_point_types = value.as_ref().to_string();
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

  #[cfg(feature = "symbols")]
  pub async fn symbols(&mut self) -> anyhow::Result<symbols::SymbolsResult> {
    use deno_graph::symbols::ModuleSymbolRef;

    let mut graph = deno_graph::ModuleGraph::new(GraphKind::All);
    let entry_point_url = ModuleSpecifier::parse(&self.entry_point).unwrap();
    let entry_point_types_url =
      ModuleSpecifier::parse(&self.entry_point_types).unwrap();
    let roots = vec![entry_point_url.clone()];
    graph
      .build(
        roots.clone(),
        &mut self.loader,
        deno_graph::BuildOptions::default(),
      )
      .await;
    graph.valid().unwrap(); // assert valid
    let source_parser = deno_graph::DefaultModuleParser::new_for_analysis();
    let capturing_analyzer = deno_graph::CapturingModuleAnalyzer::new(
      Some(Box::new(source_parser)),
      None,
    );
    let capturing_parser = capturing_analyzer.as_capturing_parser();
    let root_symbol =
      deno_graph::symbols::RootSymbol::new(&graph, &capturing_parser);
    Ok(symbols::SymbolsResult {
      output: {
        let entrypoint_symbol = root_symbol
          .get_module_from_specifier(&entry_point_types_url)
          .unwrap();
        let mut output_text = String::new();
        let mut specifiers =
          graph.specifiers().map(|(s, _)| s).collect::<Vec<_>>();
        specifiers.sort_unstable();
        for specifier in specifiers {
          let Some(module) = root_symbol.get_module_from_specifier(specifier)
          else {
            continue;
          };
          output_text.push_str(&format!(
            "{}: {}\n",
            specifier.as_str(),
            match module {
              ModuleSymbolRef::Esm(m) => format!("{:#?}", m),
              ModuleSymbolRef::Json(m) => format!("{:#?}", m),
            }
          ));
        }
        let get_symbol_text =
          |module_symbol: deno_graph::symbols::ModuleSymbolRef,
           symbol_id: deno_graph::symbols::SymbolId| {
            let symbol = module_symbol.symbol(symbol_id).unwrap();
            let definitions =
              root_symbol.go_to_definitions(module_symbol, symbol);
            if definitions.is_empty() {
              "NONE".to_string()
            } else {
              let mut results = Vec::new();
              for definition in definitions {
                let decl_text = {
                  let decl_text = definition.text();
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
                let range = definition.byte_range();
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
      diagnostics: root_symbol.take_diagnostics(),
    })
  }
}
