use std::collections::VecDeque;

use deno_semver::package::PackageNv;

use crate::source::Loader;
use crate::symbols::RootSymbol;
use crate::ModuleGraph;

mod range_finder;
mod swc_helpers;
mod transform;

pub use transform::LowResModule;

pub fn build_low_res_type_graph<'a>(
  loader: &'a dyn Loader,
  graph: &'a ModuleGraph,
  root_symbol: &'a RootSymbol<'a>,
  pending_nvs: VecDeque<PackageNv>,
) -> Vec<LowResModule> {
  let public_modules =
    range_finder::find_public_ranges(loader, graph, root_symbol, pending_nvs);

  let mut result = Vec::new();
  for (nv, modules) in public_modules {
    for (specifier, ranges) in modules {
      let module_info = root_symbol.module_from_specifier(&specifier).unwrap();
      if let Some(module_info) = module_info.esm() {
        match transform::transform(&specifier, &ranges, module_info.source()) {
          Ok(output) => result.push(output),
          Err(err) => panic!("{}", err),
        };
      }
    }
  }
  result
}
