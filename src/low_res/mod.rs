use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use deno_ast::SourceRange;
use deno_semver::package::PackageNv;

use crate::source::Loader;
use crate::symbols::ModuleInfoRef;
use crate::symbols::RootSymbol;
use crate::symbols::SymbolId;
use crate::symbols::SymbolNodeDep;
use crate::ModuleGraph;
use crate::ModuleSpecifier;

mod range_finder;

pub fn build_low_res_type_graph<'a>(
  loader: &'a dyn Loader,
  graph: &'a ModuleGraph,
  root_symbol: &'a RootSymbol<'a>,
  pending_nvs: VecDeque<PackageNv>,
) {
  let public_modules =
    range_finder::find_public_ranges(loader, graph, root_symbol, pending_nvs);

  for (nv, modules) in public_modules {
    for (specifier, ranges) in modules {
      if Some(module_info) = root_symbol.module_from_specifier(&specifier) {
        module_info
      }
    }
  }
}
