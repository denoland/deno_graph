// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashSet;

use anyhow::Result;
use deno_ast::LineAndColumnDisplay;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use indexmap::IndexSet;
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
        .map(|r| {
          (
            r.clone(),
            (ImportedExports::AllWithDefault, Default::default()),
          )
        })
        .collect(),
    ),
  };
  while let Some(pending_trace) = context.pending_traces.pop() {
    trace_module(pending_trace, &mut context)?;
  }

  Ok(context.analyzer.into_roots_graph_symbol())
}

#[derive(Debug, Clone)]
pub enum ImportedExports {
  AllWithDefault,
  Star,
  Named(IndexSet<String>),
}

impl ImportedExports {
  pub(crate) fn from_file_dep_name(dep_name: &FileDepName) -> Self {
    match dep_name {
      FileDepName::Star => Self::Star,
      FileDepName::Name(value) => Self::Named(IndexSet::from([value.clone()])),
    }
  }

  pub(crate) fn add(&mut self, exports_to_trace: ImportedExports) {
    match exports_to_trace {
      ImportedExports::AllWithDefault => {
        *self = Self::AllWithDefault;
      }
      ImportedExports::Star => {
        if !matches!(self, Self::Star | Self::AllWithDefault) {
          *self = Self::Star;
        }
      }
      ImportedExports::Named(new_names) => {
        if let Self::Named(names) = self {
          names.extend(new_names);
        }
      }
    }
  }
}

type ReferrerTracesMap = IndexMap<ModuleId, ImportedExports>;

#[derive(Default)]
struct PendingTraces(
  IndexMap<ModuleSpecifier, (ImportedExports, ReferrerTracesMap)>,
);

impl PendingTraces {
  pub fn add(
    &mut self,
    dep_specifier: ModuleSpecifier,
    exports_to_trace: ImportedExports,
    referrer_module_id: ModuleId,
  ) {
    if let Some((current_exports_to_trace, referrer_traces)) =
      self.0.get_mut(&dep_specifier)
    {
      current_exports_to_trace.add(exports_to_trace.clone());
      if let Some(referrer_traces) =
        referrer_traces.get_mut(&referrer_module_id)
      {
        referrer_traces.add(exports_to_trace);
      } else {
        referrer_traces.insert(referrer_module_id, exports_to_trace);
      }
    } else {
      self.0.insert(
        dep_specifier,
        (
          exports_to_trace.clone(),
          IndexMap::from([(referrer_module_id, exports_to_trace)]),
        ),
      );
    }
  }

  pub fn pop(&mut self) -> Option<PendingTrace> {
    self
      .0
      .pop()
      .map(
        |(specifier, (exports_to_trace, referrer_traces))| PendingTrace {
          specifier,
          exports_to_trace,
          referrer_traces,
        },
      )
  }
}

struct PendingTrace {
  pub specifier: ModuleSpecifier,
  pub exports_to_trace: ImportedExports,
  pub referrer_traces: ReferrerTracesMap,
}

struct Context<'a, TReporter: TypeTraceHandler> {
  graph: &'a ModuleGraph,
  analyzer: TypeTraceModuleAnalyzer<'a, TReporter>,
  pending_traces: PendingTraces,
}

impl<'a, TReporter: TypeTraceHandler> Context<'a, TReporter> {
  pub fn trace_exports(
    &mut self,
    pending_trace: PendingTrace,
  ) -> Result<Vec<(ModuleSpecifier, SymbolId)>> {
    let specifier = &pending_trace.specifier;
    let exports = self.trace_exports_inner(
      specifier,
      &pending_trace.exports_to_trace,
      HashSet::new(),
    )?;
    if let Some(module_symbol) = self.analyzer.get_or_analyze(specifier)? {
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

      // add the traced referrers
      for (module_id, imported_exports) in pending_trace.referrer_traces {
        module_symbol.add_traced_referrer(module_id, imported_exports);
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
    exports_to_trace: &ImportedExports,
    visited: HashSet<ModuleSpecifier>,
  ) -> Result<Vec<(ModuleSpecifier, ModuleId, String, SymbolId)>> {
    let mut result = Vec::new();
    let Some(module_symbol) = self.analyzer.get_or_analyze(specifier)? else {
      return Ok(Vec::new());
    };
    if matches!(exports_to_trace, ImportedExports::AllWithDefault) {
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
      ImportedExports::Star | ImportedExports::AllWithDefault => {
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
            let inner = self.trace_exports_inner(
              &specifier,
              &ImportedExports::Star,
              {
                let mut visited = visited.clone();
                visited.insert(specifier.clone());
                visited
              },
            )?;
            for (specifier, module_id, name, symbol_id) in inner {
              if name != "default" && found_names.insert(name.clone()) {
                result.push((specifier, module_id, name, symbol_id));
              }
            }
          }
        }
      }
      ImportedExports::Named(names) => {
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
                  &ImportedExports::Named(IndexSet::from([name.clone()])),
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
  pending_trace: PendingTrace,
  context: &mut Context<THandler>,
) -> Result<()> {
  let mut pending = context.trace_exports(pending_trace)?;

  while let Some((specifier, symbol_id)) = pending.pop() {
    let Some(module_symbol) = context.analyzer.get_or_analyze(&specifier)? else {
      continue;
    };
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
            ImportedExports::from_file_dep_name(&file_dep.name),
            /* referrer */ module_symbol.module_id(),
          );
        }
      }
      let symbol_deps =
        symbol.deps().map(ToOwned::to_owned).collect::<Vec<_>>();
      for dep in symbol_deps {
        match &dep {
          SymbolDep::Id(id) => {
            if let Some(id) = module_symbol.symbol_id_from_swc(id) {
              pending.push((module_symbol.specifier().clone(), id))
            }
          }
          SymbolDep::QualifiedId(id, parts) => {
            if let Some(symbol_id) = module_symbol.symbol_id_from_swc(id) {
              pending.extend(resolve_qualified_name(
                context.graph,
                module_symbol,
                symbol_id,
                parts,
                &|specifier| {
                  context.analyzer.get_or_analyze(specifier).ok().flatten()
                },
              )?);
            }
          }
          SymbolDep::ImportType(import_specifier, parts) => {
            let maybe_dep_specifier = context.graph.resolve_dependency(
              import_specifier,
              &specifier,
              /* prefer types */ true,
            );
            if let Some(dep_specifier) = maybe_dep_specifier {
              if let Some(module_symbol) =
                context.analyzer.get_or_analyze(&dep_specifier)?
              {
                if parts.is_empty() {
                  // an ImportType includes default exports
                  context.pending_traces.add(
                    dep_specifier,
                    ImportedExports::AllWithDefault,
                    /* referrer */ module_symbol.module_id(),
                  );
                } else {
                  pending.extend(resolve_qualified_export_name(
                    context.graph,
                    module_symbol,
                    &parts[0],
                    &parts[1..],
                    &|specifier| {
                      context.analyzer.get_or_analyze(specifier).ok().flatten()
                    },
                  )?);
                }
              }
            }
          }
        }
      }
    }
  }

  Ok(())
}
