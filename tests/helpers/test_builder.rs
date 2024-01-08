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
  pub struct SymbolsResult {
    pub output: String,
  }
}

pub struct BuildResult {
  pub graph: ModuleGraph,
  pub diagnostics: Vec<BuildDiagnostic>,
  pub analyzer: deno_graph::CapturingModuleAnalyzer,
}

#[cfg(feature = "symbols")]
impl BuildResult {
  pub fn root_symbol(&self) -> deno_graph::symbols::RootSymbol {
    self.graph.valid().unwrap(); // assert valid
    deno_graph::symbols::RootSymbol::new(&self.graph, &self.analyzer)
  }
}

pub struct TestBuilder {
  loader: TestLoader,
  entry_point: String,
  entry_point_types: String,
  workspace_members: Vec<WorkspaceMember>,
  workspace_fast_check: bool,
}

impl TestBuilder {
  pub fn new() -> Self {
    Self {
      loader: Default::default(),
      entry_point: "file:///mod.ts".to_string(),
      entry_point_types: "file:///mod.ts".to_string(),
      workspace_members: Default::default(),
      workspace_fast_check: false,
    }
  }

  pub fn with_loader(
    &mut self,
    mut action: impl FnMut(&mut TestLoader),
  ) -> &mut Self {
    action(&mut self.loader);
    self
  }

  #[allow(unused)]
  pub fn entry_point(&mut self, value: impl AsRef<str>) -> &mut Self {
    self.entry_point = value.as_ref().to_string();
    self
  }

  #[allow(unused)]
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

  pub fn workspace_fast_check(&mut self, value: bool) -> &mut Self {
    self.workspace_fast_check = value;
    self
  }

  pub async fn build(&mut self) -> BuildResult {
    let mut graph = deno_graph::ModuleGraph::new(GraphKind::All);
    let entry_point_url = ModuleSpecifier::parse(&self.entry_point).unwrap();
    let roots = vec![entry_point_url.clone()];
    let capturing_analyzer = deno_graph::CapturingModuleAnalyzer::default();
    let diagnostics = graph
      .build(
        roots.clone(),
        &mut self.loader,
        deno_graph::BuildOptions {
          workspace_members: self.workspace_members.clone(),
          module_analyzer: Some(&capturing_analyzer),
          module_parser: Some(&capturing_analyzer),
          workspace_fast_check: self.workspace_fast_check,
          ..Default::default()
        },
      )
      .await;
    BuildResult {
      graph,
      diagnostics,
      analyzer: capturing_analyzer,
    }
  }

  #[cfg(feature = "symbols")]
  pub async fn symbols(&mut self) -> anyhow::Result<symbols::SymbolsResult> {
    fn check_fatal_diagnostics(
      module: deno_graph::symbols::ModuleInfoRef,
    ) -> Vec<String> {
      let mut results = Vec::new();
      for symbol in module.symbols() {
        // ensure all decls have the same name as their symbol
        {
          let maybe_name = symbol.maybe_name();
          for decl in symbol.decls() {
            if decl.maybe_name() != maybe_name {
              results.push(format!(
                "Symbol {:?} with name {:?} had a decl with a different name: {:?}",
                symbol.symbol_id(),
                maybe_name,
                decl.maybe_name(),
              ));
            }
          }
        }

        if let Some(parent_id) = symbol.parent_id() {
          let parent_symbol = module.symbol(parent_id).unwrap();
          let has_child =
            parent_symbol.child_ids().any(|id| id == symbol.symbol_id());
          let has_member = parent_symbol
            .members()
            .iter()
            .any(|id| *id == symbol.symbol_id());
          let is_definition_decl =
            symbol.decls().iter().all(|d| d.kind.is_definition());
          if is_definition_decl {
            // ensure it's possible to go from a parent to its child
            if !has_child && !has_member {
              results.push(format!(
                "Parent {:#?} does not have child {:#?}",
                parent_symbol.symbol_id(),
                symbol.symbol_id()
              ));
            }
          } else if has_child || has_member {
            results.push(format!(
              "Parent {:#?} should not have the child or member {:#?}",
              parent_symbol.symbol_id(),
              symbol.symbol_id()
            ));
          }

          if has_child && has_member {
            results.push(format!(
              "Parent {:?} should not have both a child and a member {:?}",
              parent_symbol.symbol_id(),
              symbol.symbol_id()
            ));
          }
        }

        // ensure it's possible to get the module symbol id
        {
          let mut parent = symbol;
          let mut i = 0;
          while let Some(parent_id) = parent.parent_id() {
            parent = module.symbol(parent_id).unwrap();
            if i == 1000 {
              results.push(format!(
                "Could not find root from symbol: {:?}",
                symbol.symbol_id()
              ));
              break;
            }
            i += 1;
          }
        }
      }

      // from the root module, ensure everything is a tree
      fn ensure_no_multiple_paths(
        module: deno_graph::symbols::ModuleInfoRef,
        symbol: &deno_graph::symbols::Symbol,
        visited: &mut HashSet<deno_graph::symbols::SymbolId>,
      ) -> Vec<String> {
        let mut results = Vec::new();
        if !visited.insert(symbol.symbol_id()) {
          results.push(format!(
            "Found symbol in multiple paths: {:?}",
            symbol.symbol_id()
          ));
        } else {
          for id in symbol.child_ids().chain(symbol.members().iter().copied()) {
            let symbol = module.symbol(id).unwrap();
            results.extend(ensure_no_multiple_paths(module, symbol, visited));
          }
        }
        results
      }

      results.extend(ensure_no_multiple_paths(
        module,
        module.module_symbol(),
        &mut HashSet::new(),
      ));

      results
    }

    use std::collections::HashSet;

    use deno_graph::symbols::DefinitionOrUnresolved;
    use deno_graph::symbols::ModuleInfoRef;
    use deno_graph::symbols::ResolveDepsMode;

    let build_result = self.build().await;
    let graph = &build_result.graph;
    let entry_point_types_url =
      ModuleSpecifier::parse(&self.entry_point_types).unwrap();
    let root_symbol = build_result.root_symbol();
    Ok(symbols::SymbolsResult {
      output: {
        let entrypoint_symbol = root_symbol
          .module_from_specifier(&entry_point_types_url)
          .unwrap();
        let mut output_text = String::new();
        let mut specifiers =
          graph.specifiers().map(|(s, _)| s).collect::<Vec<_>>();
        specifiers.sort_unstable();
        for specifier in specifiers {
          let Some(module) = root_symbol.module_from_specifier(specifier)
          else {
            continue;
          };
          let module_output_text = format!(
            "{}: {}\n",
            specifier.as_str(),
            match module {
              ModuleInfoRef::Esm(m) => format!("{:#?}", m),
              ModuleInfoRef::Json(m) => format!("{:#?}", m),
            }
          );
          output_text.push_str(&module_output_text);

          fn get_symbol_deps_text_for_mode(
            module: ModuleInfoRef<'_>,
            resolve_mode: ResolveDepsMode,
          ) -> String {
            let mut symbol_deps_text = String::new();
            for symbol in module.symbols() {
              for decl in symbol.decls() {
                if let Some((node, source)) = decl.maybe_node_and_source() {
                  let deps = node.deps(resolve_mode);
                  if !deps.is_empty() {
                    symbol_deps_text.push_str(&format!(
                      "{:?}:{:?} {:?}\n",
                      symbol.symbol_id(),
                      decl
                        .range
                        .as_byte_range(source.text_info().range().start),
                      deps
                    ));
                  }
                }
              }
            }
            symbol_deps_text
          }

          let symbol_deps_text = get_symbol_deps_text_for_mode(
            module,
            ResolveDepsMode::TypesAndExpressions,
          );
          if !symbol_deps_text.is_empty() {
            output_text.push_str(&format!(
              "== symbol deps (types and exprs) ==\n{}\n",
              symbol_deps_text
            ));
          }
          let symbol_deps_text =
            get_symbol_deps_text_for_mode(module, ResolveDepsMode::TypesOnly);
          if !symbol_deps_text.is_empty() {
            output_text.push_str(&format!(
              "== symbol deps (types only) ==\n{}\n",
              symbol_deps_text
            ));
          }

          // analyze the module graph for any problems
          let diagnostics = check_fatal_diagnostics(module);
          if !diagnostics.is_empty() {
            eprintln!("== Output ==");
            eprintln!("{}", module_output_text);
            eprintln!("== Source ==");
            eprintln!("{}", module.text());
            eprintln!("== {} == \n\n{}", specifier, diagnostics.join("\n"));
            panic!("FAILED");
          }
        }
        let get_symbol_text =
          |module_symbol: deno_graph::symbols::ModuleInfoRef,
           symbol_id: deno_graph::symbols::SymbolId| {
            let symbol = module_symbol.symbol(symbol_id).unwrap();
            let items = root_symbol
              .go_to_definitions_or_unresolveds(module_symbol, symbol)
              .collect::<Vec<_>>();
            if items.is_empty() {
              "NONE".to_string()
            } else {
              let mut results = Vec::new();
              for definition_or_unresolved in items {
                match definition_or_unresolved {
                  DefinitionOrUnresolved::Definition(definition) => {
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
                  DefinitionOrUnresolved::Unresolved(unresolved) => results
                    .push(format!(
                      "{}\n  Unresolved {:?} ({:?})",
                      unresolved.module.specifier(),
                      unresolved.kind,
                      unresolved.parts,
                    )),
                }
              }
              results.join("\n")
            }
          };
        let exports = entrypoint_symbol.exports(&root_symbol).resolved;
        if !exports.is_empty() {
          output_text.push_str("== export definitions ==\n");
          for (name, resolved) in exports {
            let resolved = resolved.as_resolved_export();
            let position = get_symbol_text(resolved.module, resolved.symbol_id);
            output_text.push_str(&format!("[{}]: {}\n", name, position));
          }
        }
        output_text
      },
    })
  }
}
