// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::sync::Arc;

use crate::graph::DiagnosticRange;
use crate::ModuleSpecifier;

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
  #[error("could not resolve '{name}' referenced from '{referrer}'")]
  NotFoundReference {
    range: DiagnosticRange,
    name: String,
    referrer: String,
  },
  #[error("missing explicit type in the public API")]
  MissingExplicitType { range: DiagnosticRange },
  #[error("missing explicit return type in the public API")]
  MissingExplicitReturnType { range: DiagnosticRange },
  #[error(
    "found an ambient module, which is a global augmentation, which are not unsupported"
  )]
  UnsupportedAmbientModule { range: DiagnosticRange },
  #[error("the reference '{name}' from '{referrer}' was too complex")]
  UnsupportedComplexReference {
    range: DiagnosticRange,
    name: String,
    referrer: String,
  },
  #[error("default export expression was too complex")]
  UnsupportedDefaultExportExpr { range: DiagnosticRange },
  #[error("found destructuring, which is not supported in the public API")]
  UnsupportedDestructuring { range: DiagnosticRange },
  #[error("found global augmentations, which are not supported.")]
  UnsupportedGlobalModule { range: DiagnosticRange },
  #[error("require statements are a CommonJS feature, which are not supported in ES modules")]
  UnsupportedRequire { range: DiagnosticRange },
  #[error("public API member ({referrer}) is referencing or transitively referencing a class private member ({name})")]
  UnsupportedPrivateMemberReference {
    range: DiagnosticRange,
    name: String,
    referrer: String,
  },
  #[error("super class expression was too complex")]
  UnsupportedSuperClassExpr { range: DiagnosticRange },
  #[error(
    "export assignments are a Common JS feature, which are not supported in ES modules"
  )]
  UnsupportedTsExportAssignment { range: DiagnosticRange },
  #[error("found namespace export, which is a global augmentation, which are not unsupported")]
  UnsupportedTsNamespaceExport { range: DiagnosticRange },
  #[error("using declarations are not supproted in the public API")]
  UnsupportedUsing { range: DiagnosticRange },
  #[error("referenced a JavaScript module without type declarations from a TypeScript module")]
  UnsupportedNestedJavaScript { specifier: ModuleSpecifier },
  #[error(
    "used a JavaScript module without type declarations as an entrypoints"
  )]
  UnsupportedJavaScriptEntrypoint { specifier: ModuleSpecifier },
  #[error("failed to emit fast check module: {inner:#}")]
  Emit {
    specifier: ModuleSpecifier,
    inner: Arc<anyhow::Error>,
  },
  #[error("{}", format_diagnostics(.0))]
  Multiple(Vec<FastCheckDiagnostic>),
}

impl FastCheckDiagnostic {
  pub fn code(&self) -> &'static str {
    use FastCheckDiagnostic::*;
    match self {
      NotFoundReference { .. } => "zap-not-found-reference",
      MissingExplicitType { .. } => "zap-missing-explicit-type",
      MissingExplicitReturnType { .. } => "zap-missing-explicit-return-type",
      UnsupportedAmbientModule { .. } => "zap-unsupported-ambient-module",
      UnsupportedComplexReference { .. } => "zap-unsupported-complex-reference",
      UnsupportedDefaultExportExpr { .. } => {
        "zap-unsupported-default-export-expr"
      }
      UnsupportedDestructuring { .. } => "zap-unsupported-destructuring",
      UnsupportedGlobalModule { .. } => "zap-unsupported-global-module",
      UnsupportedRequire { .. } => "zap-unsupported-require",
      UnsupportedPrivateMemberReference { .. } => {
        "zap-unsupported-private-member-reference"
      }
      UnsupportedSuperClassExpr { .. } => "zap-unsupported-super-class-expr",
      UnsupportedTsExportAssignment { .. } => {
        "zap-unsupported-ts-export-assignment"
      }
      UnsupportedTsNamespaceExport { .. } => {
        "zap-unsupported-ts-namespace-export"
      }
      UnsupportedUsing { .. } => "zap-unsupported-using",
      UnsupportedNestedJavaScript { .. } => "zap-unsupported-nested-javascript",
      UnsupportedJavaScriptEntrypoint { .. } => {
        "zap-unsupported-javascript-entrypoint"
      }
      Emit { .. } => "zap-emit",
      Multiple(_) => unreachable!(),
    }
  }

  /// Return a human readable description of what the range of the diagnostic
  /// is.
  ///
  /// Panics if the diagnostic does not have a range.
  pub fn range_description(&self) -> Option<&'static str> {
    use FastCheckDiagnostic::*;
    match self {
      NotFoundReference { .. } => Some("this is the reference"),
      MissingExplicitType { .. } => {
        Some("this symbol is missing an explicit type")
      }
      MissingExplicitReturnType { .. } => {
        Some("this function is missing an explicit return type")
      }
      UnsupportedAmbientModule { .. } => None,
      UnsupportedComplexReference { .. } => Some("this is the reference"),
      UnsupportedDefaultExportExpr { .. } => None,
      UnsupportedDestructuring { .. } => None,
      UnsupportedGlobalModule { .. } => None,
      UnsupportedRequire { .. } => None,
      UnsupportedPrivateMemberReference { .. } => Some("this is the reference"),
      UnsupportedSuperClassExpr { .. } => {
        Some("this is the superclass expression")
      }
      UnsupportedTsExportAssignment { .. } => None,
      UnsupportedTsNamespaceExport { .. } => None,
      UnsupportedUsing { .. } => None,
      UnsupportedNestedJavaScript { .. } => unreachable!(),
      UnsupportedJavaScriptEntrypoint { .. } => unreachable!(),
      Emit { .. } => unreachable!(),
      Multiple(_) => unreachable!(),
    }
  }

  pub fn fix_hint(&self) -> &'static str {
    use FastCheckDiagnostic::*;
    match self {
      NotFoundReference { .. } => {
        "fix the reference to point to a symbol that exists"
      }
      MissingExplicitType { .. } => {
        "add an explit type annotation to the symbol"
      }
      MissingExplicitReturnType { .. } => {
        "add an explit return type to the function"
      }
      UnsupportedAmbientModule { .. } => {
        "remove the ambient module declaration"
      }
      UnsupportedComplexReference { .. } => {
        "extract the shared type to a type alias and reference the type alias instead"
      }
      UnsupportedDefaultExportExpr { .. } => "add an 'as' clause with an explicit type after the expression, or extract to a variable",
      UnsupportedDestructuring { .. } => "separate each destructured symbol into its own export statement",
      UnsupportedGlobalModule { .. } => "remove the 'global' augmentation",
      UnsupportedRequire { .. } => "use an import statement instead",
      UnsupportedPrivateMemberReference { .. } => "extract the type of the private member to a type alias and reference the type alias instead",
      UnsupportedSuperClassExpr { .. } => "extract the superclass expression into a variable",
      UnsupportedTsExportAssignment { .. } => "use an export statement instead",
      UnsupportedTsNamespaceExport { .. } => "remove the namespace export",
      UnsupportedUsing { .. } => "use 'const' instead of 'using'",
      UnsupportedNestedJavaScript { .. } => "add a type declaration (d.ts) for the JavaScript module, or rewrite it to TypeScript",
      UnsupportedJavaScriptEntrypoint { .. } => "add a type declaration (d.ts) for the JavaScript module, or rewrite it to TypeScript",
      Emit { .. } => "this error may be the result of a bug in Deno - if you think this is the case, please open an issue",
      Multiple(_) => unreachable!(),
    }
  }

  pub fn additional_info(&self) -> &[&'static str] {
    use FastCheckDiagnostic::*;
    match self {
      NotFoundReference { .. } => &[
        "this error may be the result of a bug in Deno - if you think this is the case, please open an issue",
      ],
      MissingExplicitType { .. } => &[
        "all symbols in the public API must have an explicit type",
      ],
      MissingExplicitReturnType { .. } => &[
        "all functions in the public API must have an explicit return type",
      ],
      UnsupportedAmbientModule { .. } => &[
        "ambient modules are not supported because they can modify the types of a module from outside of that module",
      ],
      UnsupportedComplexReference { .. } => &[
        "the reference was too complex to be resolved by fast check",
      ],
      UnsupportedDefaultExportExpr { .. } => &[
        "fast check was unable to infer the type of the default export expression",
      ],
      UnsupportedDestructuring { .. } => &[
        "destructuring can not be inferred by fast check",
      ],
      UnsupportedGlobalModule { .. } => &[
        "global augmentations are not supported because they can modify global types, which can affect other modules type checking",
      ],
      UnsupportedRequire { .. } => &[
        "CommonJS features such as require are not supported in ES modules",
      ],
      UnsupportedPrivateMemberReference {  .. } => &[
        "private members can not be referenced from public API members",
        "this is because fast check removes private members from the types",
      ],
      UnsupportedSuperClassExpr { .. } => &[
        "fast check was unable to infer the type of the superclass expression",
      ],
      UnsupportedTsExportAssignment { .. } => &[
        "CommonJS features such as export assignments are not supported in ES modules",
      ],
      UnsupportedTsNamespaceExport { .. } => &[
        "namespace exports are not supported because they can modify the types of a module from outside of that module",
      ],
      UnsupportedUsing { .. } => &[
        "using declarations have unclear semantics in the public API",
        "they are thus not supported in the public API",
      ],
      UnsupportedNestedJavaScript { .. } => &[
        "JavaScript files with no corresponding declaration require type inference to be type checked",
        "fast check avoids type inference, so referencing a JavaScript file with no type declarations is not supported",
      ],
      UnsupportedJavaScriptEntrypoint { .. } => &[
        "JavaScript files with no corresponding declaration require type inference to be type checked",
        "fast check avoids type inference, so JavaScript entrypoints should be avoided",
      ],
      Emit {  .. } => &[
        "this error may be the result of a bug in Deno - if you think this is the case, please open an issue",
      ],
      Multiple(_) => unreachable!(),
    }
  }
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

  pub fn specifier(&self) -> &ModuleSpecifier {
    use FastCheckDiagnostic::*;
    match self {
      NotFoundReference { range, .. } => &range.specifier,
      MissingExplicitType { range } => &range.specifier,
      MissingExplicitReturnType { range } => &range.specifier,
      UnsupportedAmbientModule { range } => &range.specifier,
      UnsupportedComplexReference { range, .. } => &range.specifier,
      UnsupportedDefaultExportExpr { range } => &range.specifier,
      UnsupportedDestructuring { range } => &range.specifier,
      UnsupportedGlobalModule { range } => &range.specifier,
      UnsupportedPrivateMemberReference { range, .. } => &range.specifier,
      UnsupportedRequire { range } => &range.specifier,
      UnsupportedSuperClassExpr { range } => &range.specifier,
      UnsupportedTsExportAssignment { range } => &range.specifier,
      UnsupportedTsNamespaceExport { range } => &range.specifier,
      UnsupportedUsing { range } => &range.specifier,
      UnsupportedJavaScriptEntrypoint { specifier } => specifier,
      UnsupportedNestedJavaScript { specifier } => specifier,
      Emit { specifier, .. } => specifier,
      Multiple(_) => unreachable!("Multiple should be flattened"),
    }
  }

  pub fn range(&self) -> Option<&DiagnosticRange> {
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
      UnsupportedJavaScriptEntrypoint { .. } => None,
      UnsupportedNestedJavaScript { .. } => None,
      Emit { .. } => None,
      Multiple(_) => unreachable!("Multiple should be flattened"),
    }
  }

  pub fn message_with_range_for_test(&self) -> String {
    match self {
      FastCheckDiagnostic::Multiple(errors) => errors
        .iter()
        .map(|e| e.message_with_range_for_test())
        .collect::<Vec<_>>()
        .join("\n"),
      _ => match self.range() {
        Some(range) => {
          format!("{}\n    at {}@{}", self, range.specifier, range.range.start)
        }
        None => format!("{}\n    at {}", self, self.specifier()),
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
    let mut errors =
      Vec::with_capacity(package.module_ranges.len() + package.errors.len());
    errors.extend(package.errors);
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
