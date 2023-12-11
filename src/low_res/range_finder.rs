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

pub fn find_public_ranges<'a>(
  loader: &'a dyn Loader,
  graph: &'a ModuleGraph,
  root_symbol: &'a RootSymbol<'a>,
  pending_nvs: VecDeque<PackageNv>,
) -> HashMap<PackageNv, HashMap<ModuleSpecifier, HashSet<SourceRange>>> {
  PublicRangeFinder {
    seen_nvs: pending_nvs.iter().cloned().collect(),
    pending_nvs,
    pending_specifiers: Default::default(),
    seen_specifiers: Default::default(),
    public_ranges: Default::default(),
    loader,
    graph,
    root_symbol,
  }
  .find()
}

struct PublicRangeFinder<'a> {
  loader: &'a dyn Loader,
  graph: &'a ModuleGraph,
  root_symbol: &'a RootSymbol<'a>,
  pending_nvs: VecDeque<PackageNv>,
  pending_specifiers: VecDeque<(PackageNv, ModuleSpecifier)>,
  seen_nvs: HashSet<PackageNv>,
  seen_specifiers: HashSet<String>,
  public_ranges:
    HashMap<PackageNv, HashMap<ModuleSpecifier, HashSet<SourceRange>>>,
}

impl<'a> PublicRangeFinder<'a> {
  pub fn find(
    mut self,
  ) -> HashMap<PackageNv, HashMap<ModuleSpecifier, HashSet<SourceRange>>> {
    while let Some(nv) = self.pending_nvs.pop_front() {
      let Some(exports) = self.graph.packages.package_exports(&nv) else {
        continue; // should never happen
      };
      let base_url = self.loader.registry_package_url(&nv);
      for (key, value) in exports {
        let specifier = base_url.join(value).unwrap();
        if self.seen_specifiers.insert(specifier.to_string()) {
          self.pending_specifiers.push_back((nv.clone(), specifier));
        }
      }

      while let Some((nv, specifier)) = self.pending_specifiers.pop_front() {
        self.analyze_specifier(nv, &specifier);
      }
    }
    self.public_ranges
  }

  fn analyze_specifier(&mut self, nv: PackageNv, specifier: &ModuleSpecifier) {
    if let Some(module_info) = self.root_symbol.module_from_specifier(specifier)
    {
      self.analyze_module_info(nv, module_info);
    } else {
      // should never happen
      eprintln!("TEMP: NOT FOUND: {}", specifier);
    }
  }

  fn analyze_module_info(
    &mut self,
    nv: PackageNv,
    module_info: ModuleInfoRef<'a>,
  ) {
    // add all the specifiers to the list of pending specifiers
    if let Some(specifiers) = module_info.re_export_all_specifiers() {
      for specifier_text in specifiers {
        if let Some(dep_specifier) = self.graph.resolve_dependency(
          specifier_text,
          module_info.specifier(),
          /* prefer types */ true,
        ) {
          // only analyze registry specifiers
          if let Some(dep_nv) =
            self.loader.registry_package_url_to_nv(&dep_specifier)
          {
            if self.seen_nvs.insert(dep_nv.clone()) {
              self.pending_nvs.push_back(dep_nv.clone());
            }
            if self.seen_specifiers.insert(dep_specifier.to_string()) {
              self.pending_specifiers.push_back((dep_nv, dep_specifier));
            }
          }
        }
      }
    }

    enum PendingTrace {
      Id(SymbolId),
      QualifiedId(SymbolId, Vec<String>),
    }

    let mut pending_traces = VecDeque::new();
    let mut found_ranges = HashSet::new();

    for (_, export_symbol_id) in module_info.module_symbol().exports() {
      pending_traces.push_back(PendingTrace::Id(*export_symbol_id));
    }

    while let Some(trace) = pending_traces.pop_front() {
      match trace {
        PendingTrace::Id(symbol_id) => {
          let symbol = module_info.symbol(symbol_id).unwrap();
          for decl in symbol.decls() {
            found_ranges.insert(decl.range);
            if let Some(node) = decl.maybe_node() {
              for dep in node.deps() {
                match dep {
                  SymbolNodeDep::Id(id) => {
                    let module_info = module_info.esm().unwrap();
                    pending_traces.push_back(PendingTrace::Id(
                      module_info.symbol_id_from_swc(&id).unwrap(),
                    ));
                  }
                  SymbolNodeDep::QualifiedId(id, parts) => {
                    let module_info = module_info.esm().unwrap();
                    pending_traces.push_back(PendingTrace::QualifiedId(
                      module_info.symbol_id_from_swc(&id).unwrap(),
                      parts,
                    ));
                  }
                  SymbolNodeDep::ImportType(_, _) => {
                    // todo: this needs to resolve the specifier
                    // if it's in another package, then the entire package needs
                    // to be analyzed. If it's in the same package, then only
                    // the types used here need to be analyzed
                    todo!();
                  }
                }
              }
            }
          }
          pending_traces
            .extend(symbol.exports().values().map(|id| PendingTrace::Id(*id)));

          if let Some(file_dep) = symbol.file_dep() {
            // todo: this needs to resolve the specifier
            // if it's in another package, then the entire package needs
            // to be analyzed. If it's in the same package, then only
            // the types used here need to be analyzed
            todo!();
          }
        }
        PendingTrace::QualifiedId(_, _) => todo!(),
      }
    }

    self
      .public_ranges
      .entry(nv)
      .or_default()
      .insert(module_info.specifier().clone(), found_ranges);
  }
}
