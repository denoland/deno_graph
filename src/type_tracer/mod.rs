use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::Result;
use deno_ast::LineAndColumnDisplay;
use deno_ast::ModuleSpecifier;
use deno_ast::ParsedSource;
use indexmap::IndexMap;

use crate::CapturingModuleParser;
use crate::ModuleGraph;
use crate::ModuleParser;

use self::analyzer::ModuleAnalyzer;

pub use self::analyzer::FileDep;
pub use self::analyzer::FileDepName;
pub use self::analyzer::ModuleId;
pub use self::analyzer::ModuleSymbol;
pub use self::analyzer::Symbol;
pub use self::analyzer::SymbolId;
pub use self::analyzer::UniqueSymbol;

mod analyzer;

pub enum TypeTraceDiagnosticKind {
  UnsupportedDefaultExpr,
}

pub struct TypeTraceDiagnostic {
  pub kind: TypeTraceDiagnosticKind,
  pub specifier: ModuleSpecifier,
  pub line_and_column: Option<LineAndColumnDisplay>,
}

pub trait TypeTraceHandler {
  fn diagnostic(&self, diagnostic: TypeTraceDiagnostic);
}

/// Traces the public types of the specified modules.
pub fn trace<'a, THandler: TypeTraceHandler>(
  roots: &[ModuleSpecifier],
  graph: &'a ModuleGraph,
  parser: &'a CapturingModuleParser<'a>,
  handler: &'a THandler,
) -> Result<HashMap<ModuleSpecifier, ModuleSymbol>> {
  let mut context = Context {
    graph,
    parser,
    analyzer: ModuleAnalyzer::new(handler),
    pending_traces: roots
      .iter()
      .map(|r| (r.clone(), ExportsToTrace::AllWithDefault))
      .collect(),
  };
  while let Some((specifier, exports_to_trace)) = context.pending_traces.pop() {
    trace_module(&specifier, &mut context, &exports_to_trace)?;
  }

  Ok(context.analyzer.into_module_symbols())
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
  parser: &'a CapturingModuleParser<'a>,
  analyzer: ModuleAnalyzer<'a, TReporter>,
  pending_traces: IndexMap<ModuleSpecifier, ExportsToTrace>,
}

impl<'a, TReporter: TypeTraceHandler> Context<'a, TReporter> {
  pub fn parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<ParsedSource, deno_ast::Diagnostic> {
    let graph_module = self.graph.get(specifier).unwrap();
    let graph_module = graph_module.esm().unwrap();
    self.parser.parse_module(
      &graph_module.specifier,
      graph_module.source.clone(),
      graph_module.media_type,
    )
  }

  pub fn get_module_symbol(
    &mut self,
    specifier: &ModuleSpecifier,
  ) -> Result<&ModuleSymbol> {
    self.ensure_analyze(specifier)?;
    Ok(self.analyzer.get(specifier).unwrap())
  }

  pub fn ensure_analyze(&mut self, specifier: &ModuleSpecifier) -> Result<()> {
    if self.analyzer.get(specifier).is_none() {
      let parsed_source = self.parsed_source(specifier)?;
      self.analyzer.analyze(specifier.clone(), &parsed_source);
    }
    Ok(())
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
          UniqueSymbol {
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
      let maybe_symbol_id = module_symbol
        .default_export_symbol_id()
        .or_else(|| module_symbol.exports().get("default").copied());
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
        for (name, symbol_id) in module_symbol.exports() {
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
        let exports = module_symbol.exports().clone();
        let re_exports = module_symbol.re_exports().clone();
        let default_export_symbol_id = module_symbol.default_export_symbol_id();
        for name in names {
          #[allow(clippy::unnecessary_unwrap)]
          if name == "default" && default_export_symbol_id.is_some() {
            result.push((
              specifier.clone(),
              module_id,
              name.clone(),
              default_export_symbol_id.unwrap(),
            ));
          } else if let Some(symbol_id) = exports.get(name) {
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
      let ids = symbol
        .swc_dep_ids()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();
      pending.extend(ids.iter().filter_map(|id| {
        match module_symbol.symbol_id_from_swc(id) {
          Some(id) => Some((specifier.clone(), id)),
          None => {
            if cfg!(debug_assertions) {
              panic!("Failed to find symbol id for swc id: {:?}", id);
            }
            None
          }
        }
      }));
    }
  }

  Ok(())
}
