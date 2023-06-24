// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashSet;

use anyhow::Result;
use deno_ast::LineAndColumnDisplay;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;

use crate::CapturingModuleParser;
use crate::ModuleGraph;

use self::analyzer::SymbolDep;
use self::analyzer::TypeTraceModuleAnalyzer;

pub use self::analyzer::FileDep;
pub use self::analyzer::FileDepName;
pub use self::analyzer::ModuleId;
pub use self::analyzer::ModuleSymbol;
pub use self::analyzer::RootSymbol;
pub use self::analyzer::Symbol;
pub use self::analyzer::SymbolId;
pub use self::analyzer::UniqueSymbolId;

mod analyzer;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TypeTraceDiagnosticKind {
  UnsupportedDefaultExpr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TypeTraceDiagnostic {
  pub kind: TypeTraceDiagnosticKind,
  pub specifier: ModuleSpecifier,
  pub line_and_column: Option<LineAndColumnDisplay>,
}

pub trait TypeTraceHandler {
  fn diagnostic(&self, diagnostic: TypeTraceDiagnostic);
}

/// Analyzes the public types and all the private dependent types of
/// the specified modules throughout the entire graph.
pub fn trace_public_types<'a, THandler: TypeTraceHandler>(
  graph: &'a ModuleGraph,
  roots: &[ModuleSpecifier],
  parser: &'a CapturingModuleParser<'a>,
  handler: &'a THandler,
) -> Result<RootSymbol> {
  let mut context = Context {
    graph,
    analyzer: TypeTraceModuleAnalyzer::new(graph, parser, handler),
    pending_traces: roots
      .iter()
      .map(|r| (r.clone(), ExportsToTrace::AllWithDefault))
      .collect(),
  };
  while let Some((specifier, exports_to_trace)) = context.pending_traces.pop() {
    trace_module(&specifier, &mut context, &exports_to_trace)?;
  }

  Ok(context.analyzer.into_roots_graph_symbol())
}

#[derive(Debug)]
enum ExportsToTrace {
  AllWithDefault,
  Star,
  Named(Vec<String>),
}

impl ExportsToTrace {
  pub fn from_file_dep_name(dep_name: &FileDepName) -> Self {
    match dep_name {
      FileDepName::Star => Self::Star,
      FileDepName::Name(value) => Self::Named(vec![value.clone()]),
    }
  }

  pub fn add(&mut self, name: &FileDepName) {
    match name {
      FileDepName::Star => {
        if !matches!(self, Self::Star | Self::AllWithDefault) {
          *self = Self::Star;
        }
      }
      FileDepName::Name(name) => {
        if let Self::Named(names) = self {
          names.push(name.to_string());
        }
      }
    }
  }
}

struct Context<'a, TReporter: TypeTraceHandler> {
  graph: &'a ModuleGraph,
  analyzer: TypeTraceModuleAnalyzer<'a, TReporter>,
  pending_traces: IndexMap<ModuleSpecifier, ExportsToTrace>,
}

impl<'a, TReporter: TypeTraceHandler> Context<'a, TReporter> {
  pub fn get_module_symbol(
    &mut self,
    specifier: &ModuleSpecifier,
  ) -> Result<&ModuleSymbol> {
    self.analyzer.get_or_analyze(specifier)
  }

  pub fn trace_exports(
    &mut self,
    specifier: &ModuleSpecifier,
    exports_to_trace: &ExportsToTrace,
  ) -> Result<Vec<(ModuleSpecifier, SymbolId)>> {
    let exports =
      self.trace_exports_inner(specifier, exports_to_trace, HashSet::new())?;
    let module_symbol = self.analyzer.get_mut(specifier).unwrap();
    for (export_specifier, module_id, name, symbol_id) in &exports {
      if specifier != export_specifier {
        module_symbol.add_traced_re_export(
          name.clone(),
          UniqueSymbolId {
            module_id: *module_id,
            symbol_id: *symbol_id,
          },
        );
      }
    }
    Ok(
      exports
        .into_iter()
        .map(|(specifier, _module_id, _name, symbol_id)| (specifier, symbol_id))
        .collect(),
    )
  }

  fn trace_exports_inner(
    &mut self,
    specifier: &ModuleSpecifier,
    exports_to_trace: &ExportsToTrace,
    visited: HashSet<ModuleSpecifier>,
  ) -> Result<Vec<(ModuleSpecifier, ModuleId, String, SymbolId)>> {
    let mut result = Vec::new();
    let module_symbol = self.get_module_symbol(specifier)?;
    if matches!(exports_to_trace, ExportsToTrace::AllWithDefault) {
      let maybe_symbol_id = module_symbol.exports_map().get("default").copied();
      if let Some(symbol_id) = maybe_symbol_id {
        result.push((
          specifier.clone(),
          module_symbol.module_id(),
          "default".to_string(),
          symbol_id,
        ));
      }
    }
    match exports_to_trace {
      ExportsToTrace::Star | ExportsToTrace::AllWithDefault => {
        let mut found_names = HashSet::new();
        for (name, symbol_id) in module_symbol.exports_map() {
          if name != "default" {
            result.push((
              specifier.clone(),
              module_symbol.module_id(),
              name.clone(),
              *symbol_id,
            ));
            found_names.insert(name.clone());
          }
        }
        let re_exports = module_symbol.re_exports().clone();
        for re_export_specifier in &re_exports {
          let maybe_specifier = self.graph.resolve_dependency(
            re_export_specifier,
            specifier,
            /* prefer_types */ true,
          );
          if let Some(specifier) = maybe_specifier {
            let inner =
              self.trace_exports_inner(&specifier, &ExportsToTrace::Star, {
                let mut visited = visited.clone();
                visited.insert(specifier.clone());
                visited
              })?;
            for (specifier, module_id, name, symbol_id) in inner {
              if name != "default" && found_names.insert(name.clone()) {
                result.push((specifier, module_id, name, symbol_id));
              }
            }
          }
        }
      }
      ExportsToTrace::Named(names) => {
        let module_id = module_symbol.module_id();
        let exports = module_symbol.exports_map().clone();
        let re_exports = module_symbol.re_exports().clone();
        for name in names {
          if let Some(symbol_id) = exports.get(name) {
            result.push((
              specifier.clone(),
              module_id,
              name.clone(),
              *symbol_id,
            ));
          } else if name != "default" {
            for re_export_specifier in &re_exports {
              let maybe_specifier = self.graph.resolve_dependency(
                re_export_specifier,
                specifier,
                /* prefer_types */ true,
              );
              if let Some(specifier) = maybe_specifier {
                let mut found = self.trace_exports_inner(
                  &specifier,
                  &ExportsToTrace::Named(vec![name.clone()]),
                  {
                    let mut visited = visited.clone();
                    visited.insert(specifier.clone());
                    visited
                  },
                )?;
                if !found.is_empty() {
                  assert_eq!(found.len(), 1);
                  result.push(found.remove(0));
                  break;
                }
              }
            }
          }
        }
      }
    }
    Ok(result)
  }
}

fn trace_module<TReporter: TypeTraceHandler>(
  specifier: &ModuleSpecifier,
  context: &mut Context<TReporter>,
  exports_to_trace: &ExportsToTrace,
) -> Result<()> {
  let mut pending = context.trace_exports(specifier, exports_to_trace)?;

  while let Some((specifier, symbol_id)) = pending.pop() {
    let module_symbol = context.analyzer.get_mut(&specifier).unwrap();
    let symbol = module_symbol.symbol_mut(symbol_id).unwrap();
    if symbol.mark_public() {
      if let Some(file_dep) = symbol.file_dep() {
        let maybe_dep_specifier = context.graph.resolve_dependency(
          &file_dep.specifier,
          &specifier,
          /* prefer types */ true,
        );
        if let Some(dep_specifier) = maybe_dep_specifier {
          if let Some(exports_to_trace) =
            context.pending_traces.get_mut(&dep_specifier)
          {
            exports_to_trace.add(&file_dep.name);
          } else {
            context.pending_traces.insert(
              dep_specifier,
              ExportsToTrace::from_file_dep_name(&file_dep.name),
            );
          }
        }
      }
      let deps = symbol.deps().map(ToOwned::to_owned).collect::<Vec<_>>();
      pending.extend(deps.iter().filter_map(|dep| {
        let (specifier, id) = match dep {
          SymbolDep::Id(id) => (specifier.clone(), id),
          SymbolDep::QualifiedId(id, parts) => {
            // todo: resolve this
            todo!("resolve the parts to get the id")
          }
        };
        match module_symbol.symbol_id_from_swc(id) {
          Some(id) => Some((specifier, id)),
          None => {
            if cfg!(debug_assertions) {
              //panic!("Failed to find symbol id for swc id: {:?}", id);
            }
            None
          }
        }
      }));
    }
  }

  Ok(())
}
