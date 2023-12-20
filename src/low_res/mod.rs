use std::collections::HashMap;
use std::collections::VecDeque;

use deno_semver::package::PackageNv;

use crate::source::Loader;
use crate::symbols::RootSymbol;
use crate::symbols::SymbolFillDiagnostic;
use crate::symbols::SymbolFillDiagnosticKind;
use crate::ModuleGraph;
use crate::ModuleSpecifier;

mod range_finder;
mod swc_helpers;
mod transform;

pub use transform::LowResModule;
pub use transform::LowResTransformDiagnostic;
pub use transform::TransformOptions;

pub fn build_low_res_type_graph<'a>(
  loader: &'a dyn Loader,
  graph: &'a ModuleGraph,
  root_symbol: &'a RootSymbol<'a>,
  pending_nvs: VecDeque<PackageNv>,
  options: &TransformOptions,
) -> Vec<(
  ModuleSpecifier,
  Result<LowResModule, LowResTransformDiagnostic>,
)> {
  let public_modules =
    range_finder::find_public_ranges(loader, graph, root_symbol, pending_nvs);
  let symbol_fill_diagnostics =
    root_symbol.take_diagnostics().into_iter().fold(
      HashMap::<ModuleSpecifier, Vec<SymbolFillDiagnostic>>::new(),
      |mut acc, d| {
        acc.entry(d.range.specifier.clone()).or_default().push(d);
        acc
      },
    );

  let mut result = Vec::new();
  for (nv, modules) in public_modules {
    for (specifier, ranges) in modules {
      let module_info = root_symbol.module_from_specifier(&specifier).unwrap();
      if let Some(module_info) = module_info.esm() {
        let transform_result = match symbol_fill_diagnostics.get(&specifier) {
          Some(diagnostics) => {
            let diagnostics = diagnostics
              .iter()
              .map(|d| match d.kind {
                SymbolFillDiagnosticKind::UnsupportedDefaultExpr => {
                  LowResTransformDiagnostic::UnsupportedDefaultExportExpr {
                    range: d.range.clone(),
                  }
                }
              })
              .collect::<Vec<_>>();
            Err(LowResTransformDiagnostic::from_vec(diagnostics).unwrap())
          }
          None => transform::transform(
            &specifier,
            &ranges,
            module_info.source(),
            options,
          ),
        };
        result.push((specifier, transform_result));
      }
    }
  }
  result
}
