// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::sync::Arc;

use crate::ModuleSpecifier;

use deno_ast::diagnostics::DiagnosticLevel;
use deno_ast::diagnostics::DiagnosticLocation;
use deno_ast::diagnostics::DiagnosticSnippet;
use deno_ast::diagnostics::DiagnosticSnippetHighlight;
use deno_ast::diagnostics::DiagnosticSnippetHighlightStyle;
use deno_ast::diagnostics::DiagnosticSourcePos;
use deno_ast::diagnostics::DiagnosticSourceRange;
use deno_ast::SourceRange;
use deno_ast::SourceTextInfo;

mod cache;
#[cfg(feature = "fast_check")]
mod range_finder;
#[cfg(feature = "fast_check")]
mod swc_helpers;
#[cfg(feature = "fast_check")]
mod transform;
#[cfg(feature = "fast_check")]
mod transform_dts;

pub use cache::FastCheckCache;
pub use cache::FastCheckCacheItem;
pub use cache::FastCheckCacheKey;
pub use cache::FastCheckCacheModuleItem;
pub use cache::FastCheckCacheModuleItemDiagnostic;
pub use cache::FastCheckCacheModuleItemInfo;
#[cfg(feature = "fast_check")]
pub use transform::FastCheckDtsModule;
#[cfg(feature = "fast_check")]
pub use transform::FastCheckModule;
#[cfg(feature = "fast_check")]
pub use transform::TransformOptions;

#[derive(Clone)]
pub struct FastCheckDiagnosticRange {
  pub specifier: ModuleSpecifier,
  pub range: SourceRange,
  pub text_info: SourceTextInfo,
}

impl std::fmt::Debug for FastCheckDiagnosticRange {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("FastCheckDiagnosticRange")
      .field("specifier", &self.specifier)
      .field("range", &self.range)
      .field("text_info", &"<omitted>")
      .finish()
  }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum FastCheckDiagnostic {
  #[error("could not resolve '{name}' referenced from '{referrer}'")]
  NotFoundReference {
    range: FastCheckDiagnosticRange,
    name: String,
    referrer: String,
  },
  #[error("missing explicit type in the public API")]
  MissingExplicitType { range: FastCheckDiagnosticRange },
  #[error("missing explicit return type in the public API")]
  MissingExplicitReturnType { range: FastCheckDiagnosticRange },
  #[error(
    "found an ambient module, which is a global augmentation, which are not unsupported"
  )]
  UnsupportedAmbientModule { range: FastCheckDiagnosticRange },
  #[error("the reference '{name}' from '{referrer}' was too complex")]
  UnsupportedComplexReference {
    range: FastCheckDiagnosticRange,
    name: String,
    referrer: String,
  },
  #[error("default export expression was too complex")]
  UnsupportedDefaultExportExpr { range: FastCheckDiagnosticRange },
  #[error("found destructuring, which is not supported in the public API")]
  UnsupportedDestructuring { range: FastCheckDiagnosticRange },
  #[error("found global augmentations, which are not supported")]
  UnsupportedGlobalModule { range: FastCheckDiagnosticRange },
  #[error("require statements are a CommonJS feature, which are not supported in ES modules")]
  UnsupportedRequire { range: FastCheckDiagnosticRange },
  #[error("public API member ({referrer}) is referencing or transitively referencing a class private member ({name})")]
  UnsupportedPrivateMemberReference {
    range: FastCheckDiagnosticRange,
    name: String,
    referrer: String,
  },
  #[error("super class expression was too complex")]
  UnsupportedSuperClassExpr { range: FastCheckDiagnosticRange },
  #[error(
    "export assignments are a Common JS feature, which are not supported in ES modules"
  )]
  UnsupportedTsExportAssignment { range: FastCheckDiagnosticRange },
  #[error("found namespace export, which is a global augmentation, which are not unsupported")]
  UnsupportedTsNamespaceExport { range: FastCheckDiagnosticRange },
  #[error("using declarations are not supproted in the public API")]
  UnsupportedUsing { range: FastCheckDiagnosticRange },
  #[error("referenced a JavaScript module without type declarations from a TypeScript module")]
  UnsupportedNestedJavaScript { specifier: ModuleSpecifier },
  #[error(
    "used a JavaScript module without type declarations as an entrypoint"
  )]
  UnsupportedJavaScriptEntrypoint { specifier: ModuleSpecifier },
  #[error("failed to emit fast check module: {inner:#}")]
  Emit {
    specifier: ModuleSpecifier,
    inner: Arc<anyhow::Error>,
  },
  /// This is a special diagnostic that appears when a module is loaded from the
  /// fast check cache that had a diagnostic. When we load a diagnostic from the
  /// cache, we're only really interested in if there was a fast check diagnostic
  /// and not what the diagnostic was because we just need to know if we can use
  /// fast check for the package or not.
  ///
  /// Note: This diagnostic will never (should never) be displayed to the user
  /// because the fast check cache should not be used in deno lint or when
  /// publishing.
  #[error("diagnostic was cached")]
  Cached { specifier: ModuleSpecifier },
}

impl FastCheckDiagnostic {
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
      UnsupportedNestedJavaScript { .. } => None,
      UnsupportedJavaScriptEntrypoint { .. } => None,
      Emit { .. } => None,
      Cached { .. } => None,
    }
  }
}

impl FastCheckDiagnostic {
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
      Cached { specifier, .. } => specifier,
    }
  }

  pub fn range(&self) -> Option<&FastCheckDiagnosticRange> {
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
      Cached { .. } => None,
    }
  }
}

impl deno_ast::diagnostics::Diagnostic for FastCheckDiagnostic {
  fn level(&self) -> DiagnosticLevel {
    use FastCheckDiagnostic::*;
    match self {
      NotFoundReference { .. }
      | MissingExplicitType { .. }
      | MissingExplicitReturnType { .. }
      | UnsupportedAmbientModule { .. }
      | UnsupportedComplexReference { .. }
      | UnsupportedDefaultExportExpr { .. }
      | UnsupportedDestructuring { .. }
      | UnsupportedGlobalModule { .. }
      | UnsupportedRequire { .. }
      | UnsupportedPrivateMemberReference { .. }
      | UnsupportedSuperClassExpr { .. }
      | UnsupportedTsExportAssignment { .. }
      | UnsupportedTsNamespaceExport { .. }
      | UnsupportedUsing { .. }
      | UnsupportedNestedJavaScript { .. }
      | Emit { .. }
      | Cached { .. } => DiagnosticLevel::Error,
      UnsupportedJavaScriptEntrypoint { .. } => DiagnosticLevel::Warning,
    }
  }

  fn code(&self) -> Cow<'_, str> {
    // WARNING: When adding a code, make sure to update jsr
    use FastCheckDiagnostic::*;
    Cow::Borrowed(match self {
      NotFoundReference { .. } => "not-found-reference",
      MissingExplicitType { .. } => "missing-explicit-type",
      MissingExplicitReturnType { .. } => "missing-explicit-return-type",
      UnsupportedAmbientModule { .. } => "unsupported-ambient-module",
      UnsupportedComplexReference { .. } => "unsupported-complex-reference",
      UnsupportedDefaultExportExpr { .. } => "unsupported-default-export-expr",
      UnsupportedDestructuring { .. } => "unsupported-destructuring",
      UnsupportedGlobalModule { .. } => "unsupported-global-module",
      UnsupportedRequire { .. } => "unsupported-require",
      UnsupportedPrivateMemberReference { .. } => {
        "unsupported-private-member-reference"
      }
      UnsupportedSuperClassExpr { .. } => "unsupported-super-class-expr",
      UnsupportedTsExportAssignment { .. } => {
        "unsupported-ts-export-assignment"
      }
      UnsupportedTsNamespaceExport { .. } => "unsupported-ts-namespace-export",
      UnsupportedUsing { .. } => "unsupported-using",
      UnsupportedNestedJavaScript { .. } => "unsupported-nested-javascript",
      UnsupportedJavaScriptEntrypoint { .. } => {
        "unsupported-javascript-entrypoint"
      }
      Emit { .. } => "emit",
      Cached { .. } => "cached",
    })
  }

  fn message(&self) -> Cow<'_, str> {
    Cow::Owned(self.to_string())
  }

  fn location(&self) -> deno_ast::diagnostics::DiagnosticLocation {
    match self.range() {
      Some(range) => DiagnosticLocation::ModulePosition {
        specifier: Cow::Borrowed(self.specifier()),
        text_info: Cow::Borrowed(&range.text_info),
        source_pos: DiagnosticSourcePos::SourcePos(range.range.start),
      },
      None => DiagnosticLocation::Module {
        specifier: Cow::Borrowed(self.specifier()),
      },
    }
  }

  fn snippet(&self) -> Option<deno_ast::diagnostics::DiagnosticSnippet<'_>> {
    self.range().map(|range| DiagnosticSnippet {
      source: Cow::Borrowed(&range.text_info),
      highlight: DiagnosticSnippetHighlight {
        style: DiagnosticSnippetHighlightStyle::Error,
        range: DiagnosticSourceRange {
          start: DiagnosticSourcePos::SourcePos(range.range.start),
          end: DiagnosticSourcePos::SourcePos(range.range.end),
        },
        description: self.range_description().map(Cow::Borrowed),
      },
    })
  }

  fn hint(&self) -> Option<Cow<'_, str>> {
    use FastCheckDiagnostic::*;
    Some(Cow::Borrowed(match self {
      NotFoundReference { .. } => {
        "fix the reference to point to a symbol that exists"
      }
      MissingExplicitType { .. } => {
        "add an explicit type annotation to the symbol"
      }
      MissingExplicitReturnType { .. } => {
        "add an explicit return type to the function"
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
      Cached { .. } => "this error is the result of a bug in Deno and you don't be seeing it - please open an issue if one doesn't exist",
    }))
  }

  fn snippet_fixed(
    &self,
  ) -> Option<deno_ast::diagnostics::DiagnosticSnippet<'_>> {
    None
  }

  fn info(&self) -> std::borrow::Cow<'_, [std::borrow::Cow<'_, str>]> {
    use FastCheckDiagnostic::*;
    match self {
      NotFoundReference { .. } => Cow::Borrowed(&[
        Cow::Borrowed("this error may be the result of a bug in Deno - if you think this is the case, please open an issue"),
      ]),
      MissingExplicitType { .. } => Cow::Borrowed(&[
        Cow::Borrowed("all symbols in the public API must have an explicit type")
      ]),
      MissingExplicitReturnType { .. } => Cow::Borrowed(&[
        Cow::Borrowed("all functions in the public API must have an explicit return type")
      ]),
      UnsupportedAmbientModule { .. } => Cow::Borrowed(&[
        Cow::Borrowed("ambient modules are not supported because they can modify the types of a module from outside of that module")
      ]),
      UnsupportedComplexReference { .. } => Cow::Borrowed(&[
        Cow::Borrowed("the reference was too complex to be resolved by fast check")
      ]),
      UnsupportedDefaultExportExpr { .. } => Cow::Borrowed(&[
        Cow::Borrowed("fast check was unable to infer the type of the default export expression")
      ]),
      UnsupportedDestructuring { .. } => Cow::Borrowed(&[
        Cow::Borrowed("destructuring can not be inferred by fast check")
      ]),
      UnsupportedGlobalModule { .. } => Cow::Borrowed(&[
        Cow::Borrowed("global augmentations are not supported because they can modify global types, which can affect other modules type checking")
      ]),
      UnsupportedRequire { .. } => Cow::Borrowed(&[
        Cow::Borrowed("CommonJS features such as require are not supported in ES modules")
      ]),
      UnsupportedPrivateMemberReference {  .. } => Cow::Borrowed(&[
        Cow::Borrowed("private members can not be referenced from public API members"),
        Cow::Borrowed("this is because fast check removes private members from the types"),
      ]),
      UnsupportedSuperClassExpr { .. } => Cow::Borrowed(&[
        Cow::Borrowed("fast check was unable to infer the type of the superclass expression")
      ]),
      UnsupportedTsExportAssignment { .. } => Cow::Borrowed(&[
        Cow::Borrowed("CommonJS features such as export assignments are not supported in ES modules")
      ]),
      UnsupportedTsNamespaceExport { .. } => Cow::Borrowed(&[
        Cow::Borrowed("namespace exports are not supported because they can modify the types of a module from outside of that module")
      ]),
      UnsupportedUsing { .. } => Cow::Borrowed(&[
        Cow::Borrowed("using declarations have unclear semantics in the public API"),
        Cow::Borrowed("they are thus not supported in the public API"),
      ]),
      UnsupportedNestedJavaScript { .. } => Cow::Borrowed(&[
        Cow::Borrowed("JavaScript files with no corresponding declaration require type inference to be type checked"),
        Cow::Borrowed("fast check avoids type inference, so referencing a JavaScript file with no type declarations is not supported"),
      ]),
      UnsupportedJavaScriptEntrypoint { .. } => Cow::Borrowed(&[
        Cow::Borrowed("JavaScript files with no corresponding declaration require type inference to be type checked"),
        Cow::Borrowed("fast check avoids type inference, so JavaScript entrypoints should be avoided"),
      ]),
      Emit {  .. } => Cow::Borrowed(&[
        Cow::Borrowed("this error may be the result of a bug in Deno - if you think this is the case, please open an issue")
      ]),
      Cached {  .. } => Cow::Borrowed(&[
        Cow::Borrowed("this error is the result of a bug in Deno and you don't be seeing it - please open an issue if one doesn't exist")
      ]),
    }
  }

  fn docs_url(&self) -> Option<Cow<'_, str>> {
    Some(Cow::Owned(format!(
      "https://jsr.io/go/slow-type-{}",
      self.code()
    )))
  }
}

#[cfg(feature = "fast_check")]
pub fn build_fast_check_type_graph<'a>(
  fast_check_cache: Option<&'a dyn FastCheckCache>,
  jsr_url_provider: &'a dyn crate::source::JsrUrlProvider,
  graph: &'a crate::ModuleGraph,
  root_symbol: &'a crate::symbols::RootSymbol<'a>,
  pending_nvs: std::collections::VecDeque<deno_semver::package::PackageNv>,
  options: &TransformOptions,
) -> Vec<(
  crate::ModuleSpecifier,
  Result<FastCheckModule, Vec<FastCheckDiagnostic>>,
)> {
  use crate::fast_check::cache::fast_insecure_hash;

  let fast_check_cache = if options.dts && fast_check_cache.is_some() {
    debug_assert!(false, "using fast check cache with dts is not supported");
    None
  } else {
    fast_check_cache
  };

  let public_modules = range_finder::find_public_ranges(
    fast_check_cache,
    jsr_url_provider,
    graph,
    root_symbol,
    options.workspace_members,
    pending_nvs,
  );

  let mut final_result = Vec::new();
  for (nv, package) in public_modules {
    log::debug!("Analyzing '{}' for fast check", nv);
    let mut errors = Vec::new();
    errors.extend(package.errors);

    let mut fast_check_modules =
      Vec::with_capacity(package.module_ranges.len());
    if package.sources.is_empty() {
      transform_package(
        package.module_ranges,
        root_symbol,
        graph,
        options,
        &mut errors,
        &mut fast_check_modules,
      );

      // fill the cache
      if let Some(fast_check_cache) = fast_check_cache {
        let mut package_cache_items =
          Vec::with_capacity(fast_check_modules.len() + errors.len());
        for (specifier, module_result) in &fast_check_modules {
          let source_hash = graph
            .get(specifier)
            .and_then(|m| m.source())
            .map(|s| fast_insecure_hash(s.as_bytes()))
            .unwrap_or(0);
          if errors.is_empty() {
            let module = module_result.as_ref().ok().unwrap();
            package_cache_items.push((
              specifier.clone(),
              FastCheckCacheModuleItem::Info(FastCheckCacheModuleItemInfo {
                source_hash,
                module_info: serde_json::to_string(&module.module_info)
                  .unwrap(),
                text: module.text.clone(),
                source_map: module.source_map.clone(),
              }),
            ));
          } else {
            package_cache_items.push((
              specifier.clone(),
              FastCheckCacheModuleItem::Diagnostic(
                FastCheckCacheModuleItemDiagnostic { source_hash },
              ),
            ));
          }
        }
        for error in &errors {
          let specifier = error.specifier();
          let source_hash = graph
            .get(specifier)
            .and_then(|m| m.source())
            .map(|s| fast_insecure_hash(s.as_bytes()))
            .unwrap_or(0);
          package_cache_items.push((
            specifier.clone(),
            FastCheckCacheModuleItem::Diagnostic(
              FastCheckCacheModuleItemDiagnostic { source_hash },
            ),
          ));
        }
        let cache_key = FastCheckCacheKey::build(&nv, &package.entrypoints);
        fast_check_cache.set(
          cache_key,
          FastCheckCacheItem {
            dependencies: package.dependencies,
            modules: package_cache_items,
          },
        );
      }

      if errors.is_empty() {
        final_result.extend(fast_check_modules);
      }
    } else {
      // these were sources in the cache, so use those
      final_result.extend(
        package
          .sources
          .into_iter()
          .map(|(url, module_item)| (url, Ok(module_item))),
      );
    }

    if !errors.is_empty() {
      // If there are errors, insert a copy into each entrypoint.
      //
      // If one entrypoint can't be analyzed then we consider all
      // entrypoints are non-analyzable because it's very difficult
      // to determine the overlap of internal types between entrypoints.
      for entrypoint in package.entrypoints {
        final_result.push((entrypoint, Err(errors.clone())));
      }
    }
  }

  final_result
}

#[cfg(feature = "fast_check")]
fn transform_package(
  package_module_ranges: indexmap::IndexMap<
    ModuleSpecifier,
    self::range_finder::ModulePublicRanges,
  >,
  root_symbol: &crate::symbols::RootSymbol<'_>,
  graph: &crate::ModuleGraph,
  options: &TransformOptions<'_>,
  errors: &mut Vec<FastCheckDiagnostic>,
  fast_check_modules: &mut Vec<(
    url::Url,
    Result<FastCheckModule, Vec<FastCheckDiagnostic>>,
  )>,
) {
  for (specifier, mut ranges) in package_module_ranges {
    let module_info = root_symbol
      .module_from_specifier(&specifier)
      .unwrap_or_else(|| panic!("module not found: {}", specifier));
    if let Some(module_info) = module_info.esm() {
      let diagnostics = ranges.take_diagnostics();
      let transform_result = if diagnostics.is_empty() {
        transform::transform(
          graph,
          &specifier,
          &ranges,
          module_info.source(),
          options,
        )
      } else {
        Err(diagnostics)
      };
      match transform_result {
        Ok(modules) => {
          if errors.is_empty() {
            fast_check_modules.push((specifier.clone(), Ok(modules)));
          }
        }
        Err(d) => {
          // don't clear the fast_check_modules here because we still
          // use that to construct the package's cache items
          errors.extend(d);
          if options.should_error_on_first_diagnostic {
            return; // no need to continue analyzing the package
          }
        }
      }
    }
  }
}
