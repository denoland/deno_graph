// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::Arc;

use deno_semver::package::PackageNv;

use crate::source::Loader;
use crate::symbols::RootSymbol;
use crate::symbols::SymbolFillDiagnostic;
use crate::symbols::SymbolFillDiagnosticKind;
use crate::ModuleGraph;
use crate::ModuleSpecifier;
use crate::Range;

mod range_finder;
mod swc_helpers;
mod transform;

pub use transform::LowResModule;
pub use transform::TransformOptions;

fn format_diagnostics(diagnostics: &[LowResDiagnostic]) -> String {
  diagnostics
    .iter()
    .map(|diag| format!("{}", diag))
    .collect::<Vec<_>>()
    .join("\n")
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum LowResDiagnostic {
  #[error("Could not resolve '{name}' referenced from '{referrer}'. This may indicate a bug in Deno. Please open an issue to help us improve if so.")]
  NotFoundReference {
    range: Range,
    name: String,
    referrer: String,
  },
  #[error("Missing explicit type in the public API.")]
  MissingExplicitType { range: Range },
  #[error("Missing explicit return type in the public API.")]
  MissingExplicitReturnType { range: Range },
  #[error("Global augmentations such as ambient modules are not supported.")]
  UnsupportedAmbientModule { range: Range },
  #[error("The reference '{name}' from '{referrer}' was too complex. Extract out the shared type to a type alias.")]
  UnsupportedComplexReference {
    range: Range,
    name: String,
    referrer: String,
  },
  #[error(
    "Default export expression was too complex. Extract it out to a variable and add an explicit type."
  )]
  UnsupportedDefaultExportExpr { range: Range },
  #[error("Destructuring is not supported in the public API.")]
  UnsupportedDestructuring { range: Range },
  #[error("Global augmentations are not supported.")]
  UnsupportedGlobalModule { range: Range },
  #[error("Require is not supported in ES modules.")]
  UnsupportedRequire { range: Range },
  #[error("Public API members ({referrer}) referencing or transitively referencing a class private member ({name}) are not supported. Extract out the shared type to a type alias.")]
  UnsupportedPrivateMemberReference {
    range: Range,
    name: String,
    referrer: String,
  },
  #[error(
    "Super class expression was too complex. Extract it out to a variable and add an explicit type."
  )]
  UnsupportedSuperClassExpr { range: Range },
  #[error(
    "CommonJS export assignments (`export =`) are not supported in ES modules."
  )]
  UnsupportedTsExportAssignment { range: Range },
  #[error("Global augmentations such as namespace exports are not supported.")]
  UnsupportedTsNamespaceExport { range: Range },
  #[error("Using declarations are not supported in the public API.")]
  UnsupportedUsing { range: Range },
  #[error("Failed to emit low res module: {0:#}")]
  Emit(Arc<anyhow::Error>),
  #[error("{}", format_diagnostics(.0))]
  Multiple(Vec<LowResDiagnostic>),
}

impl LowResDiagnostic {
  pub fn count(&self) -> usize {
    match self {
      LowResDiagnostic::Multiple(diagnostics) => diagnostics.iter().map(|d| d.count()).sum(),
      _ => 1,
    }
  }

  pub fn from_vec(mut diagnostics: Vec<LowResDiagnostic>) -> Option<Self> {
    match diagnostics.len() {
      0 => None,
      1 => diagnostics.pop(),
      _ => Some(LowResDiagnostic::Multiple(diagnostics)),
    }
  }

  pub fn line_and_column_display(&self) -> Option<&Range> {
    use LowResDiagnostic::*;
    match self {
      NotFoundReference { range, .. } => Some(range),
      MissingExplicitType { range } => Some(range),
      MissingExplicitReturnType { range } => Some(range),
      UnsupportedAmbientModule { range } => Some(range),
      UnsupportedComplexReference { range, .. } => Some(range),
      UnsupportedDefaultExportExpr { range } => Some(range),
      UnsupportedDestructuring { range } => Some(range),
      UnsupportedGlobalModule { range } => Some(range),
      UnsupportedPrivateMemberReference { range, .. } => Some(range),
      UnsupportedRequire { range } => Some(range),
      UnsupportedSuperClassExpr { range } => Some(range),
      UnsupportedTsExportAssignment { range } => Some(range),
      UnsupportedTsNamespaceExport { range } => Some(range),
      UnsupportedUsing { range } => Some(range),
      Emit(_) => None,
      Multiple(_) => None,
    }
  }

  pub fn message_with_range(&self) -> String {
    match self.line_and_column_display() {
      Some(range) => format!("{}\n    at {}", self, range),
      None => format!("{}", self),
    }
  }
}

pub fn build_low_res_type_graph<'a>(
  loader: &'a dyn Loader,
  graph: &'a ModuleGraph,
  root_symbol: &'a RootSymbol<'a>,
  pending_nvs: VecDeque<PackageNv>,
  options: &TransformOptions,
) -> Vec<(ModuleSpecifier, Result<LowResModule, Box<LowResDiagnostic>>)> {
  let public_modules = range_finder::find_public_ranges(
    loader,
    graph,
    root_symbol,
    options.workspace_members,
    pending_nvs,
  );
  let symbol_fill_diagnostics =
    root_symbol.take_diagnostics().into_iter().fold(
      HashMap::<ModuleSpecifier, Vec<SymbolFillDiagnostic>>::new(),
      |mut acc, d| {
        acc.entry(d.range.specifier.clone()).or_default().push(d);
        acc
      },
    );

  let mut result = Vec::new();
  for (_nv, modules) in public_modules {
    for (specifier, mut ranges) in modules {
      let module_info = root_symbol.module_from_specifier(&specifier).unwrap();
      if let Some(module_info) = module_info.esm() {
        let transform_result =
          match LowResDiagnostic::from_vec(ranges.take_diagnostics()) {
            Some(diagnostic) => Err(Box::new(diagnostic)),
            None => {
              let maybe_symbol_fill_diagnostic = symbol_fill_diagnostics
                .get(&specifier)
                .and_then(|diagnostics| {
                  let diagnostics = diagnostics
                    .iter()
                    .map(|d| match d.kind {
                      SymbolFillDiagnosticKind::UnsupportedDefaultExpr => {
                        LowResDiagnostic::UnsupportedDefaultExportExpr {
                          range: d.range.clone(),
                        }
                      }
                    })
                    .collect::<Vec<_>>();
                  LowResDiagnostic::from_vec(diagnostics).map(Box::new)
                });
              match maybe_symbol_fill_diagnostic {
                Some(diagnostic) => Err(diagnostic),
                None => transform::transform(
                  &specifier,
                  &ranges,
                  module_info.source(),
                  options,
                ),
              }
            }
          };
        result.push((specifier, transform_result));
      }
    }
  }
  result
}
