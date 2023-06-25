// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashSet;

use anyhow::Result;
use deno_ast::LineAndColumnDisplay;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;

use crate::type_tracer::cross_module::resolve_qualified_export_name;
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
use self::cross_module::resolve_qualified_name;

mod analyzer;
mod collections;
mod cross_module;

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
    pending_traces: PendingTraces(
      roots
        .iter()
        .map(|r| (r.clone(), ExportsToTrace::AllWithDefault))
        .collect(),
    ),
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

  pub fn add(&mut self, exports_to_trace: ExportsToTrace) {
    match exports_to_trace {
      ExportsToTrace::AllWithDefault => {
        *self = Self::AllWithDefault;
      }
      ExportsToTrace::Star => {
        if !matches!(self, Self::Star | Self::AllWithDefault) {
          *self = Self::Star;
        }
      }
      ExportsToTrace::Named(new_names) => {
        if let Self::Named(names) = self {
          names.extend(new_names);
        }
      }
    }
  }
}

#[derive(Default)]
struct PendingTraces(IndexMap<ModuleSpecifier, ExportsToTrace>);

impl PendingTraces {
  pub fn add(
    &mut self,
    dep_specifier: ModuleSpecifier,
    exports_to_trace: ExportsToTrace,
  ) {
    if let Some(current_exports_to_trace) = self.0.get_mut(&dep_specifier) {
      current_exports_to_trace.add(exports_to_trace);
    } else {
      self.0.insert(dep_specifier, exports_to_trace);
    }
  }

  pub fn pop(&mut self) -> Option<(ModuleSpecifier, ExportsToTrace)> {
    self.0.pop()
  }
}

struct Context<'a, TReporter: TypeTraceHandler> {
  graph: &'a ModuleGraph,
  analyzer: TypeTraceModuleAnalyzer<'a, TReporter>,
  pending_traces: PendingTraces,
}

impl<'a, TReporter: TypeTraceHandler> Context<'a, TReporter> {
  pub fn trace_exports(
    &mut self,
    specifier: &ModuleSpecifier,
    exports_to_trace: &ExportsToTrace,
  ) -> Result<Vec<(ModuleSpecifier, SymbolId)>> {
    let exports =
      self.trace_exports_inner(specifier, exports_to_trace, HashSet::new())?;
    let module_symbol = self.analyzer.get_or_analyze(specifier)?;
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
    let module_symbol = self.analyzer.get_or_analyze(specifier)?;
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

fn trace_module<THandler: TypeTraceHandler>(
  specifier: &ModuleSpecifier,
  context: &mut Context<THandler>,
  exports_to_trace: &ExportsToTrace,
) -> Result<()> {
  let mut pending = context.trace_exports(specifier, exports_to_trace)?;

  while let Some((specifier, symbol_id)) = pending.pop() {
    let module_symbol = context.analyzer.get_or_analyze(&specifier)?;
    let symbol = module_symbol.symbol(symbol_id).unwrap();
    if symbol.mark_public() {
      if let Some(file_dep) = symbol.file_dep() {
        let maybe_dep_specifier = context.graph.resolve_dependency(
          &file_dep.specifier,
          &specifier,
          /* prefer types */ true,
        );
        if let Some(dep_specifier) = maybe_dep_specifier {
          context.pending_traces.add(
            dep_specifier,
            ExportsToTrace::from_file_dep_name(&file_dep.name),
          );
        }
      }
      let symbol_deps =
        symbol.deps().map(ToOwned::to_owned).collect::<Vec<_>>();
      for dep in symbol_deps {
        match &dep {
          SymbolDep::Id(id) => {
            match module_symbol.symbol_id_from_swc(id) {
              Some(id) => pending.push((module_symbol.specifier().clone(), id)),
              None => {
                if cfg!(debug_assertions) {
                  // todo: remove
                  eprintln!("Failed to find symbol id for swc id: {:?}", id);
                }
              }
            }
          }
          SymbolDep::QualifiedId(id, parts) => {
            if let Some(symbol_id) = module_symbol.symbol_id_from_swc(id) {
              pending.extend(resolve_qualified_name(
                context.graph,
                &module_symbol,
                symbol_id,
                parts,
                &|specifier| context.analyzer.get_or_analyze(specifier).ok(),
              )?);
            }
          }
          SymbolDep::ImportType(import_specifier, parts) => {
            let maybe_dep_specifier = context.graph.resolve_dependency(
              &import_specifier,
              &specifier,
              /* prefer types */ true,
            );
            if let Some(dep_specifier) = maybe_dep_specifier {
              let module_symbol =
                context.analyzer.get_or_analyze(&dep_specifier)?;
              if parts.is_empty() {
                // an ImportType includes default exports
                context
                  .pending_traces
                  .add(dep_specifier, ExportsToTrace::AllWithDefault);
              } else {
                pending.extend(resolve_qualified_export_name(
                  context.graph,
                  module_symbol,
                  &parts[0],
                  &parts[1..],
                  &|specifier| context.analyzer.get_or_analyze(specifier).ok(),
                )?);
              }
            }
          }
        }
      }
    }
  }

  Ok(())
}

// fn find_export<TReporter: TypeTraceHandler>(
//   specifier: &ModuleSpecifier,
//   export_name: &str,
//   context: &mut Context<'_, TReporter>,
// ) -> Result<Option<UniqueSymbolId>> {
//   let module_symbol = context.analyzer.get_or_analyze(&specifier)?;
//   if let Some(symbol_id) = module_symbol.exports_map().get(export_name) {
//     return Ok(Some(UniqueSymbolId {
//       module_id: module_symbol.module_id(),
//       symbol_id: *symbol_id,
//     }));
//   }
//   for re_export in module_symbol.re_exports().clone() {}
//   Ok(())
// }
