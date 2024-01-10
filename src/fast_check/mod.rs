// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::sync::Arc;

use crate::Range;

#[cfg(feature = "fast_check")]
mod range_finder;
#[cfg(feature = "fast_check")]
mod swc_helpers;
#[cfg(feature = "fast_check")]
mod transform;

#[cfg(feature = "fast_check")]
pub use transform::FastCheckModule;
#[cfg(feature = "fast_check")]
pub use transform::TransformOptions;

fn format_diagnostics(diagnostics: &[FastCheckDiagnostic]) -> String {
  diagnostics
    .iter()
    .map(|diag| format!("{}", diag))
    .collect::<Vec<_>>()
    .join("\n")
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum FastCheckDiagnostic {
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
  #[error("Referencing a JavaScript file from a TypeScript file is not supported. Add a declaration file or convert to TypeScript.")]
  UnsupportedNestedJavaScript { range: Range },
  #[error("Failed to emit fast check module: {0:#}")]
  Emit(Arc<anyhow::Error>),
  #[error("{}", format_diagnostics(.0))]
  Multiple(Vec<FastCheckDiagnostic>),
}

impl FastCheckDiagnostic {
  pub fn flatten_multiple<'a>(
    &'a self,
  ) -> Box<dyn Iterator<Item = &FastCheckDiagnostic> + 'a> {
    match self {
      FastCheckDiagnostic::Multiple(diagnostics) => {
        Box::new(diagnostics.iter().flat_map(|d| d.flatten_multiple()))
      }
      _ => Box::new(std::iter::once(self)),
    }
  }

  pub fn from_vec(mut diagnostics: Vec<FastCheckDiagnostic>) -> Option<Self> {
    match diagnostics.len() {
      0 => None,
      1 => diagnostics.pop(),
      _ => Some(FastCheckDiagnostic::Multiple(diagnostics)),
    }
  }

  pub fn line_and_column_display(&self) -> Option<&Range> {
    use FastCheckDiagnostic::*;
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
      UnsupportedNestedJavaScript { range } => Some(range),
      Emit(_) => None,
      Multiple(_) => None,
    }
  }

  pub fn message_with_range(&self) -> String {
    match self {
      FastCheckDiagnostic::Multiple(errors) => errors
        .iter()
        .map(|e| e.message_with_range())
        .collect::<Vec<_>>()
        .join("\n"),
      _ => match self.line_and_column_display() {
        Some(range) => format!("{}\n    at {}", self, range),
        None => format!("{}", self),
      },
    }
  }
}

#[cfg(feature = "fast_check")]
pub fn build_fast_check_type_graph<'a>(
  loader: &'a dyn crate::source::Loader,
  graph: &'a crate::ModuleGraph,
  root_symbol: &'a crate::symbols::RootSymbol<'a>,
  pending_nvs: std::collections::VecDeque<deno_semver::package::PackageNv>,
  options: &TransformOptions,
) -> Vec<(
  crate::ModuleSpecifier,
  Result<FastCheckModule, Box<FastCheckDiagnostic>>,
)> {
  let public_modules = range_finder::find_public_ranges(
    loader,
    graph,
    root_symbol,
    options.workspace_members,
    pending_nvs,
  );

  let mut final_result = Vec::new();
  for (nv, package) in public_modules {
    log::debug!("Analyzing '{}' for fast check", nv);
    let mut errors = Vec::with_capacity(package.module_ranges.len());
    let mut fast_check_modules =
      Vec::with_capacity(package.module_ranges.len());
    for (specifier, mut ranges) in package.module_ranges {
      let module_info = root_symbol.module_from_specifier(&specifier).unwrap();
      if let Some(module_info) = module_info.esm() {
        let transform_result =
          match FastCheckDiagnostic::from_vec(ranges.take_diagnostics()) {
            Some(diagnostic) => Err(Box::new(diagnostic)),
            None => transform::transform(
              graph,
              &specifier,
              &ranges,
              module_info.source(),
              options,
            ),
          };
        match transform_result {
          Ok(modules) => {
            if errors.is_empty() {
              fast_check_modules.push((specifier.clone(), Ok(modules)));
            }
          }
          Err(d) => {
            errors.push(*d);
          }
        }
      }
    }

    if errors.is_empty() {
      final_result.extend(fast_check_modules);
    } else {
      // If there are errors, insert a copy into each entrypoint.
      //
      // If one entrypoint can't be analyzed then we consider all
      // entrypoints are non-analyzable because it's very difficult
      // to determine the overlap of internal types between entrypoints.
      let combined_errors = FastCheckDiagnostic::Multiple(errors);
      for entrypoint in package.entrypoints {
        final_result.push((entrypoint, Err(Box::new(combined_errors.clone()))));
      }
    }
  }

  final_result
}
