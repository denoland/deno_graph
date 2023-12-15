use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_semver::package::PackageNv;
use indexmap::IndexMap;
use indexmap::IndexSet;

use crate::source::Loader;
use crate::symbols::FileDepName;
use crate::symbols::ModuleInfoRef;
use crate::symbols::RootSymbol;
use crate::symbols::SymbolId;
use crate::symbols::SymbolNodeDep;
use crate::ModuleGraph;
use crate::ModuleSpecifier;

#[derive(Default, Debug, Clone)]
struct NamedExports(IndexMap<String, NamedExports>);

impl NamedExports {
  pub fn add(&mut self, export: String) {
    // replace because it's for everything
    self.0.insert(export, NamedExports::default());
  }

  pub fn add_qualified(&mut self, export: &str, mut qualified: &[String]) {
    if let Some(entry) = self.0.get_mut(export) {
      if !entry.0.is_empty() && !qualified.is_empty() {
        entry.add_qualified(&qualified[0], &qualified[1..]);
      }
    } else {
      let mut named_exports = NamedExports::default();
      if !qualified.is_empty() {
        named_exports.add_qualified(&qualified[0], &qualified[1..]);
      }
      self.0.insert(export.to_string(), named_exports);
    }
  }

  pub fn contains(&self, arg: &str) -> bool {
    self.0.contains_key(arg)
  }

  pub fn extend(&mut self, new_named: NamedExports) {
    for (key, exports) in new_named.0 {
      if let Some(entry) = self.0.get_mut(&key) {
        entry.extend(exports);
      } else {
        self.0.insert(key, exports);
      }
    }
  }

  pub fn from_parts(parts: &[String]) -> NamedExports {
    let mut exports = NamedExports::default();
    if !parts.is_empty() {
      exports.add_qualified(&parts[0], &parts[1..]);
    }
    exports
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }
}

#[derive(Debug, Clone)]
enum ImportedExports {
  AllWithDefault,
  Star,
  Named(NamedExports),
}

impl ImportedExports {
  pub(crate) fn from_file_dep_name(dep_name: &FileDepName) -> Self {
    match dep_name {
      FileDepName::Star => Self::Star,
      FileDepName::Name(value) => {
        let mut named_exports = NamedExports::default();
        named_exports.add(value.clone());
        Self::Named(named_exports)
      }
    }
  }

  pub(crate) fn add(
    &mut self,
    exports_to_trace: ImportedExports,
  ) -> Option<ImportedExports> {
    match self {
      ImportedExports::AllWithDefault => return None,
      ImportedExports::Star => match exports_to_trace {
        Self::Star => return None,
        Self::AllWithDefault => {
          *self = Self::AllWithDefault;
          let mut named_exports = NamedExports::default();
          named_exports.add("default".to_string());
          Some(Self::Named(named_exports))
        }
        Self::Named(new_named) => {
          if new_named.contains("default") {
            *self = Self::AllWithDefault;
            let mut named = NamedExports::default();
            named.add("default".to_string());
            Some(ImportedExports::Named(named))
          } else {
            None
          }
        }
      },
      ImportedExports::Named(current_named) => match exports_to_trace {
        Self::AllWithDefault => {
          *self = exports_to_trace.clone();
          Some(exports_to_trace)
        }
        Self::Star => {
          let exports_to_trace = if current_named.contains("default") {
            ImportedExports::AllWithDefault
          } else {
            ImportedExports::Star
          };
          *self = exports_to_trace.clone();
          Some(exports_to_trace)
        }
        Self::Named(new_named) => {
          current_named.extend(new_named.clone());
          // todo(dsherret): instead return a difference between
          // the current_named and the new_named, which would be
          // more efficient
          Some(ImportedExports::Named(new_named))
        }
      },
    }
  }
}

#[derive(Default)]
struct HandledExports(HashMap<ModuleSpecifier, ImportedExports>);

impl HandledExports {
  pub fn add(
    &mut self,
    dep_specifier: &ModuleSpecifier,
    traced_exports: ImportedExports,
  ) -> Option<ImportedExports> {
    if let Some((handled_exports)) = self.0.get_mut(dep_specifier) {
      handled_exports.add(traced_exports)
    } else {
      self.0.insert(dep_specifier.clone(), traced_exports.clone());
      Some(traced_exports)
    }
  }
}

#[derive(Default)]
struct PendingTraces(IndexMap<ModuleSpecifier, (PackageNv, ImportedExports)>);

impl PendingTraces {
  pub fn add(
    &mut self,
    package_nv: PackageNv,
    dep_specifier: ModuleSpecifier,
    exports_to_trace: ImportedExports,
  ) {
    if let Some((_, current_exports_to_trace)) = self.0.get_mut(&dep_specifier)
    {
      current_exports_to_trace.add(exports_to_trace);
    } else {
      self.0.insert(dep_specifier, (package_nv, exports_to_trace));
    }
  }

  pub fn pop(&mut self) -> Option<PendingTrace> {
    self
      .0
      .pop()
      .map(|(specifier, (package_nv, exports_to_trace))| PendingTrace {
        package_nv,
        specifier,
        exports_to_trace,
      })
  }
}

struct PendingTrace {
  pub package_nv: PackageNv,
  pub specifier: ModuleSpecifier,
  pub exports_to_trace: ImportedExports,
}

pub fn find_public_ranges<'a>(
  loader: &'a dyn Loader,
  graph: &'a ModuleGraph,
  root_symbol: &'a RootSymbol<'a>,
  pending_nvs: VecDeque<PackageNv>,
) -> HashMap<PackageNv, HashMap<ModuleSpecifier, HashSet<SourceRange>>> {
  PublicRangeFinder {
    seen_nvs: pending_nvs.iter().cloned().collect(),
    traced_exports: Default::default(),
    pending_nvs,
    pending_traces: Default::default(),
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
  pending_traces: PendingTraces,
  traced_exports: HandledExports,
  seen_nvs: HashSet<PackageNv>,
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
        self.add_pending_trace(
          &nv,
          &specifier,
          ImportedExports::AllWithDefault,
        );
      }

      while let Some(trace) = self.pending_traces.pop() {
        self.analyze_trace(&trace);
      }
    }
    self.public_ranges
  }

  fn add_pending_trace(
    &mut self,
    nv: &PackageNv,
    specifier: &ModuleSpecifier,
    trace: ImportedExports,
  ) {
    if let Some(trace) = self.traced_exports.add(specifier, trace) {
      self
        .pending_traces
        .add(nv.clone(), specifier.clone(), trace);
    }
  }

  fn analyze_trace(&mut self, trace: &PendingTrace) {
    if let Some(module_info) =
      self.root_symbol.module_from_specifier(&trace.specifier)
    {
      self.analyze_module_info(trace, module_info);
    } else {
      // should never happen
      eprintln!("TEMP: NOT FOUND: {}", trace.specifier);
    }
  }

  fn analyze_module_info(
    &mut self,
    trace: &PendingTrace,
    module_info: ModuleInfoRef<'a>,
  ) -> bool {
    #[derive(Debug)]
    enum PendingIdTrace {
      Id(SymbolId),
      QualifiedId(SymbolId, NamedExports),
    }

    let pkg_nv = &trace.package_nv;
    let mut pending_traces = VecDeque::new();
    let mut found_ranges = HashSet::new();
    let mut found = false;

    match &trace.exports_to_trace {
      ImportedExports::AllWithDefault | ImportedExports::Star => {
        if matches!(trace.exports_to_trace, ImportedExports::AllWithDefault) {
          if let Some(default_symbol_id) =
            module_info.module_symbol().exports().get("default")
          {
            pending_traces.push_back(PendingIdTrace::Id(*default_symbol_id));
          }
        }

        for (name, export_symbol_id) in module_info.module_symbol().exports() {
          if name == "default" {
            continue;
          }

          pending_traces.push_back(PendingIdTrace::Id(*export_symbol_id));
        }

        // add all the specifiers to the list of pending specifiers
        if let Some(re_export_all_nodes) = module_info.re_export_all_nodes() {
          for re_export_all_node in re_export_all_nodes {
            found_ranges.insert(re_export_all_node.span.range());
            let specifier_text = re_export_all_node.src.value.as_str();
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

                self.add_pending_trace(
                  &dep_nv,
                  &dep_specifier,
                  ImportedExports::Star,
                );
              }
            }
          }
        }

        found = true;
      }
      ImportedExports::Named(named_exports) => {
        let mut named_exports = named_exports.0.clone();
        if !named_exports.is_empty() {
          let module_exports = module_info.module_symbol().exports();
          for i in (0..named_exports.len()).rev() {
            let (export_name, _) = named_exports.get_index(i).unwrap();
            if let Some(export_symbol_id) = module_exports.get(export_name) {
              let export_name = export_name.clone();
              let named_exports = named_exports.remove(&export_name).unwrap();
              if named_exports.is_empty() {
                pending_traces.push_back(PendingIdTrace::Id(*export_symbol_id));
              } else {
                pending_traces.push_back(PendingIdTrace::QualifiedId(
                  *export_symbol_id,
                  named_exports,
                ));
              }
            }
          }

          if !named_exports.is_empty() {
            if let Some(re_export_all_nodes) = module_info.re_export_all_nodes()
            {
              for re_export_all_node in re_export_all_nodes {
                if named_exports.is_empty() {
                  break; // all done
                }
                let specifier_text = re_export_all_node.src.value.as_str();
                if let Some(dep_specifier) = self.graph.resolve_dependency(
                  specifier_text,
                  module_info.specifier(),
                  /* prefer types */ true,
                ) {
                  if let Some(module_info) =
                    self.root_symbol.module_from_specifier(&dep_specifier)
                  {
                    let module_exports = module_info.exports(&self.root_symbol);

                    for i in (0..named_exports.len()).rev() {
                      let (export_name, _) =
                        named_exports.get_index(i).unwrap();
                      if let Some(export_path) =
                        module_exports.resolved.get(export_name)
                      {
                        found_ranges.insert(re_export_all_node.span.range());
                        let export_name = export_name.clone();
                        let named_exports =
                          named_exports.remove(&export_name).unwrap();
                        let module = match export_path {
                            crate::symbols::ResolvedExportOrReExportAllPath::Export(e) => e.module,
                            crate::symbols::ResolvedExportOrReExportAllPath::ReExportAllPath(p) => p.referrer_module,
                        };
                        if let Some(nv) = self
                          .loader
                          .registry_package_url_to_nv(module.specifier())
                        {
                          let mut new_named_exports = NamedExports::default();
                          new_named_exports
                            .0
                            .insert(export_name, named_exports);
                          self.add_pending_trace(
                            &nv,
                            module.specifier(),
                            ImportedExports::Named(new_named_exports),
                          );
                        }
                      }
                    }
                  }
                }
              }

              if !named_exports.is_empty() {
                // in this case, include all re_export all ranges because
                // we couldn't determine a named export
                if let Some(re_export_all_nodes) =
                  module_info.re_export_all_nodes()
                {
                  for re_export_all_node in re_export_all_nodes {
                    found_ranges.insert(re_export_all_node.span.range());
                  }
                }
              }
            }
          }
        }
      }
    }

    while let Some(trace) = pending_traces.pop_front() {
      match trace {
        PendingIdTrace::Id(symbol_id) => {
          let symbol = module_info.symbol(symbol_id).unwrap();
          if symbol.is_private_member() {
            continue;
          }

          for decl in symbol.decls() {
            found_ranges.insert(decl.range);

            if decl.has_overloads() {
              continue;
            }

            if let Some(node) = decl.maybe_node() {
              eprintln!("Node: {:#?}", node.maybe_name());
              for dep in node.deps() {
                eprintln!("Dep: {:#?}", dep);
                match dep {
                  SymbolNodeDep::Id(id) => {
                    let module_info = module_info.esm().unwrap();
                    if let Some(symbol_id) = module_info.symbol_id_from_swc(&id)
                    {
                      pending_traces.push_back(PendingIdTrace::Id(symbol_id));
                    }
                  }
                  SymbolNodeDep::QualifiedId(id, parts) => {
                    let module_info = module_info.esm().unwrap();
                    if let Some(symbol_id) = module_info.symbol_id_from_swc(&id)
                    {
                      pending_traces.push_back(PendingIdTrace::QualifiedId(
                        symbol_id,
                        NamedExports::from_parts(&parts),
                      ));
                    }
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

          pending_traces.extend(
            symbol
              .exports()
              .values()
              .map(|id| PendingIdTrace::Id(*id))
              .chain(symbol.members().iter().map(|id| PendingIdTrace::Id(*id))),
          );

          if let Some(file_dep) = symbol.file_dep() {
            // todo: this needs to resolve the specifier
            // if it's in another package, then the entire package needs
            // to be analyzed. If it's in the same package, then only
            // the types used here need to be analyzed
            if let Some(specifier) = self.graph.resolve_dependency(
              &file_dep.specifier,
              module_info.specifier(),
              /* prefer types */ true,
            ) {
              if let Some(dep_nv) =
                self.loader.registry_package_url_to_nv(&specifier)
              {
                if dep_nv == *pkg_nv {
                  // just add this specifier
                  self.add_pending_trace(
                    &dep_nv,
                    &specifier,
                    ImportedExports::from_file_dep_name(&file_dep.name),
                  );
                } else {
                  // need to analyze the whole package
                  if self.seen_nvs.insert(dep_nv.clone()) {
                    self.pending_nvs.push_back(dep_nv.clone());
                  }
                }
              }
            }
          }
        }
        PendingIdTrace::QualifiedId(_, _) => todo!(),
      }
    }

    self
      .public_ranges
      .entry(trace.package_nv.clone())
      .or_default()
      .entry(module_info.specifier().clone())
      .or_default()
      .extend(found_ranges);

    found
  }
}
