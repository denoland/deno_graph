// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use deno_ast::MediaType;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_semver::package::PackageNv;
use indexmap::IndexMap;
use url::Url;

use crate::fast_check::swc_helpers::is_expr_leavable;
use crate::source::Loader;
use crate::symbols::ExportDeclRef;
use crate::symbols::FileDepName;
use crate::symbols::ModuleInfoRef;
use crate::symbols::ResolveDepsMode;
use crate::symbols::RootSymbol;
use crate::symbols::SymbolDeclKind;
use crate::symbols::SymbolId;
use crate::symbols::SymbolNodeDep;
use crate::symbols::SymbolNodeRef;
use crate::DiagnosticRange;
use crate::ModuleGraph;
use crate::ModuleSpecifier;
use crate::WorkspaceMember;

use super::FastCheckDiagnostic;

#[derive(Default, Debug, Clone)]
struct NamedExports(IndexMap<String, NamedExports>);

impl NamedExports {
  pub fn retain_default_or_non_top_level(&mut self) {
    // retain non-top level and the default export
    self.0.retain(|k, v| k == "default" || !v.is_empty());
  }

  pub fn add(&mut self, export: String) {
    self.0.entry(export).or_default();
  }

  pub fn add_qualified(&mut self, export: &str, qualified: &[String]) {
    let entry = self.0.entry(export.to_string()).or_default();
    if !qualified.is_empty() {
      entry.add_qualified(&qualified[0], &qualified[1..]);
    }
  }

  pub fn contains(&self, arg: &str) -> bool {
    self.0.contains_key(arg)
  }

  pub fn extend(&mut self, new_named: NamedExports) -> NamedExports {
    let mut difference = NamedExports::default();
    for (key, exports) in new_named.0 {
      if let Some(entry) = self.0.get_mut(&key) {
        let sub_diff = entry.extend(exports);
        if !sub_diff.is_empty() {
          difference.add_named(key.clone(), sub_diff);
        }
      } else {
        difference.add_named(key.clone(), exports.clone());
        self.0.insert(key, exports);
      }
    }
    difference
  }

  pub fn from_parts(parts: &[String]) -> NamedExports {
    let mut exports = NamedExports::default();
    if !parts.is_empty() {
      exports.add_qualified(&parts[0], &parts[1..]);
    }
    exports
  }

  pub fn from_many_parts(many_parts: &[Vec<String>]) -> NamedExports {
    let mut exports = NamedExports::default();
    for parts in many_parts {
      if !parts.is_empty() {
        exports.add_qualified(&parts[0], &parts[1..]);
      }
    }
    exports
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn into_separate_parts(self) -> Vec<Vec<String>> {
    let mut parts = Vec::new();
    for (key, exports) in self.0 {
      if exports.is_empty() {
        parts.push(vec![key]);
      } else {
        let mut sub_parts = exports.into_separate_parts();
        for sub_part in &mut sub_parts {
          sub_part.insert(0, key.clone());
        }
        parts.extend(sub_parts);
      }
    }
    parts
  }

  pub fn add_named(&mut self, key: String, exports: NamedExports) {
    self.0.entry(key).or_default().extend(exports);
  }
}

#[derive(Debug, Clone)]
struct ImportedExports {
  star: bool,
  named: NamedExports,
}

impl ImportedExports {
  pub(crate) fn from_file_dep_name(dep_name: &FileDepName) -> Self {
    match dep_name {
      FileDepName::Star => ImportedExports {
        star: true,
        named: Default::default(),
      },
      FileDepName::Name(value) => {
        let mut named_exports = NamedExports::default();
        named_exports.add(value.clone());
        ImportedExports {
          star: false,
          named: named_exports,
        }
      }
    }
  }

  pub fn star_with_default() -> ImportedExports {
    Self {
      star: true,
      named: {
        let mut named_exports = NamedExports::default();
        named_exports.add("default".to_string());
        named_exports
      },
    }
  }

  pub fn star() -> ImportedExports {
    ImportedExports {
      star: true,
      named: Default::default(),
    }
  }

  pub fn named(named: NamedExports) -> ImportedExports {
    ImportedExports { star: false, named }
  }

  /// Adds the incoming exports to the existing exports and
  /// returns the newly added exports that have not previously
  /// been added.
  pub(crate) fn add(
    &mut self,
    mut exports_to_trace: ImportedExports,
  ) -> Option<ImportedExports> {
    let difference = if self.star {
      // retain named exports in the incoming that are not top level
      exports_to_trace.named.retain_default_or_non_top_level();
      let named_difference = self.named.extend(exports_to_trace.named);
      ImportedExports {
        star: false,
        named: named_difference,
      }
    } else if exports_to_trace.star {
      // retain named exports in the existing that are not top level
      self.named.retain_default_or_non_top_level();
      let named_difference = self.named.extend(exports_to_trace.named);
      self.star = true;
      ImportedExports {
        star: true,
        named: named_difference,
      }
    } else {
      let named_difference = self.named.extend(exports_to_trace.named);
      ImportedExports {
        star: false,
        named: named_difference,
      }
    };

    if difference.star || !difference.named.is_empty() {
      Some(difference)
    } else {
      None
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
    if let Some(handled_exports) = self.0.get_mut(dep_specifier) {
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

#[derive(Debug)]
struct PendingTrace {
  pub package_nv: PackageNv,
  pub specifier: ModuleSpecifier,
  pub exports_to_trace: ImportedExports,
}

pub fn find_public_ranges<'a>(
  loader: &'a dyn Loader,
  graph: &'a ModuleGraph,
  root_symbol: &'a RootSymbol<'a>,
  workspace_members: &'a [WorkspaceMember],
  pending_nvs: VecDeque<PackageNv>,
) -> HashMap<PackageNv, PackagePublicRanges> {
  PublicRangeFinder {
    seen_nvs: pending_nvs.iter().cloned().collect(),
    traced_exports: Default::default(),
    pending_nvs,
    pending_traces: Default::default(),
    public_ranges: Default::default(),
    graph,
    workspace_members,
    root_symbol,
    url_converter: RegistryUrlConverter {
      loader,
      workspace_members,
    },
  }
  .find()
}

#[derive(Debug, Default)]
pub struct ModulePublicRanges {
  ranges: HashSet<SourceRange>,
  impl_with_overload_ranges: HashSet<SourceRange>,
  diagnostics: Vec<FastCheckDiagnostic>,
}

impl ModulePublicRanges {
  pub fn contains(&self, range: &SourceRange) -> bool {
    self.ranges.contains(range)
  }

  pub fn is_impl_with_overloads(&self, range: &SourceRange) -> bool {
    self.impl_with_overload_ranges.contains(range)
  }

  pub fn take_diagnostics(&mut self) -> Vec<FastCheckDiagnostic> {
    std::mem::take(&mut self.diagnostics)
  }
}

struct RegistryUrlConverter<'a> {
  loader: &'a dyn Loader,
  workspace_members: &'a [WorkspaceMember],
}

impl<'a> RegistryUrlConverter<'a> {
  fn registry_package_url(&self, nv: &PackageNv) -> Url {
    if let Some(member) = self.workspace_members.iter().find(|m| m.nv == *nv) {
      member.base.clone()
    } else {
      self.loader.registry_package_url(nv)
    }
  }

  fn registry_package_url_to_nv(&self, url: &Url) -> Option<PackageNv> {
    if url.scheme() == "file" {
      for member in self.workspace_members.iter() {
        if url.as_str().starts_with(member.base.as_str()) {
          return Some(member.nv.clone());
        }
      }
      None
    } else {
      self.loader.registry_package_url_to_nv(url)
    }
  }
}

#[derive(Debug, Default)]
pub struct PackagePublicRanges {
  pub entrypoints: Vec<ModuleSpecifier>,
  pub module_ranges: HashMap<ModuleSpecifier, ModulePublicRanges>,
  pub errors: Vec<FastCheckDiagnostic>,
}

struct PublicRangeFinder<'a> {
  url_converter: RegistryUrlConverter<'a>,
  graph: &'a ModuleGraph,
  workspace_members: &'a [WorkspaceMember],
  root_symbol: &'a RootSymbol<'a>,
  pending_nvs: VecDeque<PackageNv>,
  pending_traces: PendingTraces,
  traced_exports: HandledExports,
  seen_nvs: HashSet<PackageNv>,
  public_ranges: HashMap<PackageNv, PackagePublicRanges>,
}

impl<'a> PublicRangeFinder<'a> {
  pub fn find(mut self) -> HashMap<PackageNv, PackagePublicRanges> {
    while let Some(nv) = self.pending_nvs.pop_front() {
      let Some(exports) =
        self.graph.packages.package_exports(&nv).or_else(|| {
          Some(&self.workspace_members.iter().find(|m| m.nv == nv)?.exports)
        })
      else {
        continue; // should never happen
      };
      let base_url = self.url_converter.registry_package_url(&nv);
      let mut entrypoints = Vec::with_capacity(exports.len());
      let mut errors = Vec::new();
      for value in exports.values() {
        // if we got this far, then the export must be valid, so we can unwrap
        let specifier = base_url.join(value).unwrap();
        if self.is_typed_specifier(&specifier) {
          self.add_pending_trace(
            &nv,
            &specifier,
            ImportedExports::star_with_default(),
          );
        } else {
          errors.push(FastCheckDiagnostic::UnsupportedJavaScriptEntrypoint {
            specifier: specifier.clone(),
          });
        }

        entrypoints.push(specifier);
      }

      while let Some(trace) = self.pending_traces.pop() {
        self.analyze_trace(&trace);
      }

      let public_ranges = self.public_ranges.entry(nv).or_default();
      public_ranges.entrypoints = entrypoints;
      public_ranges.errors.extend(errors);
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
    if !self.is_typed_specifier(&trace.specifier) {
      let ranges = self
        .public_ranges
        .entry(trace.package_nv.clone())
        .or_default()
        .module_ranges
        .entry(trace.specifier.clone())
        .or_default();
      // if there are any diagnostics present then that means
      // we already inserted this diagnostic, so we can ignore
      // doing it again
      if ranges.diagnostics.is_empty() {
        ranges.diagnostics.push(
          FastCheckDiagnostic::UnsupportedNestedJavaScript {
            specifier: trace.specifier.clone(),
          },
        );
      }
    } else if let Some(module_info) =
      self.root_symbol.module_from_specifier(&trace.specifier)
    {
      self.analyze_module_info(trace, module_info);
    } else {
      // should never happen except when the graph is not valid, so ignore
      log::debug!("Tracing did not find: {}", trace.specifier);
    }
  }

  fn analyze_module_info(
    &mut self,
    trace: &PendingTrace,
    module_info: ModuleInfoRef<'a>,
  ) -> bool {
    #[derive(Debug)]
    enum PendingIdTrace {
      Id {
        symbol_id: SymbolId,
        referrer_id: SymbolId,
      },
      QualifiedId {
        symbol_id: SymbolId,
        parts: NamedExports,
        referrer_id: SymbolId,
      },
    }

    #[derive(Default)]
    struct PendingTraces {
      traces: VecDeque<PendingIdTrace>,
      done_id_traces: HashSet<(SymbolId, SymbolId)>,
    }

    impl PendingTraces {
      fn maybe_add_id_trace(
        &mut self,
        symbol_id: SymbolId,
        referrer_id: SymbolId,
      ) {
        if self.done_id_traces.insert((symbol_id, referrer_id)) {
          self.traces.push_back(PendingIdTrace::Id {
            symbol_id,
            referrer_id,
          });
        }
      }
    }

    let pkg_nv = &trace.package_nv;
    let mut found_ranges = HashSet::new();
    let mut impl_with_overload_ranges = HashSet::new();
    let mut found = false;
    let mut diagnostics = Vec::new();
    let mut pending_traces = PendingTraces::default();
    let module_symbol = module_info.module_symbol();

    if trace.exports_to_trace.star {
      for (name, export_symbol_id) in module_info.module_symbol().exports() {
        if name == "default"
          && !trace.exports_to_trace.named.contains("default")
        {
          continue;
        }

        pending_traces
          .maybe_add_id_trace(*export_symbol_id, module_symbol.symbol_id());
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
            if let Some(dep_nv) = self
              .url_converter
              .registry_package_url_to_nv(&dep_specifier)
            {
              if self.seen_nvs.insert(dep_nv.clone()) {
                self.pending_nvs.push_back(dep_nv.clone());
              }

              self.add_pending_trace(
                &dep_nv,
                &dep_specifier,
                ImportedExports::star(),
              );
            }
          }
        }
      }

      found = true;
    }

    if !trace.exports_to_trace.named.is_empty() {
      let mut named_exports = trace.exports_to_trace.named.0.clone();
      let module_exports = module_info.module_symbol().exports();
      for i in (0..named_exports.len()).rev() {
        let (export_name, _) = named_exports.get_index(i).unwrap();
        if let Some(export_symbol_id) = module_exports.get(export_name) {
          let export_name = export_name.clone();
          let named_exports = named_exports.remove(&export_name).unwrap();
          if named_exports.is_empty() {
            pending_traces
              .maybe_add_id_trace(*export_symbol_id, module_symbol.symbol_id());
          } else {
            pending_traces
              .traces
              .push_back(PendingIdTrace::QualifiedId {
                symbol_id: *export_symbol_id,
                parts: named_exports,
                referrer_id: module_symbol.symbol_id(),
              });
          }
        }
      }

      if !named_exports.is_empty() {
        if let Some(re_export_all_nodes) = module_info.re_export_all_nodes() {
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
                let module_exports = module_info.exports(self.root_symbol);

                for i in (0..named_exports.len()).rev() {
                  let (export_name, _) = named_exports.get_index(i).unwrap();
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
                      .url_converter
                      .registry_package_url_to_nv(module.specifier())
                    {
                      let mut new_named_exports = NamedExports::default();
                      new_named_exports.0.insert(export_name, named_exports);
                      self.add_pending_trace(
                        &nv,
                        module.specifier(),
                        ImportedExports::named(new_named_exports),
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
            if let Some(re_export_all_nodes) = module_info.re_export_all_nodes()
            {
              for re_export_all_node in re_export_all_nodes {
                found_ranges.insert(re_export_all_node.span.range());
              }
            }
          }
        }
      }
    }

    while let Some(trace) = pending_traces.traces.pop_front() {
      match trace {
        PendingIdTrace::Id {
          symbol_id,
          referrer_id,
        } => {
          let symbol = module_info.symbol(symbol_id).unwrap();
          if symbol.is_private_member() {
            if Some(referrer_id) != symbol.parent_id() {
              diagnostics.push(
                FastCheckDiagnostic::UnsupportedPrivateMemberReference {
                  range: DiagnosticRange::new(
                    module_info.specifier().clone(),
                    symbol.decls()[0].range,
                  ),
                  name: module_info
                    .fully_qualified_symbol_name(symbol)
                    .unwrap_or_else(|| "<unknown>".to_string()),
                  referrer: module_info
                    .symbol(referrer_id)
                    .and_then(|symbol| {
                      module_info.fully_qualified_symbol_name(symbol)
                    })
                    .unwrap_or_else(|| "<unknown>".to_string()),
                },
              );
            }
            continue;
          }

          for decl in symbol.decls() {
            found_ranges.insert(decl.range);

            if decl.has_overloads() && decl.has_body() {
              impl_with_overload_ranges.insert(decl.range);
              continue;
            }
            let referrer_id = symbol_id;
            match &decl.kind {
              SymbolDeclKind::Target(id) => {
                found_ranges.insert(decl.range);
                if let Some(symbol_id) =
                  module_info.esm().and_then(|m| m.symbol_id_from_swc(id))
                {
                  pending_traces.maybe_add_id_trace(symbol_id, referrer_id);
                }
              }
              SymbolDeclKind::QualifiedTarget(id, parts) => {
                found_ranges.insert(decl.range);
                if let Some(symbol_id) =
                  module_info.esm().and_then(|m| m.symbol_id_from_swc(id))
                {
                  pending_traces.traces.push_back(
                    PendingIdTrace::QualifiedId {
                      symbol_id,
                      parts: NamedExports::from_parts(parts),
                      referrer_id,
                    },
                  );
                }
              }
              SymbolDeclKind::FileRef(file_dep) => {
                if let Some(specifier) = self.graph.resolve_dependency(
                  &file_dep.specifier,
                  module_info.specifier(),
                  /* prefer types */ true,
                ) {
                  if let Some(dep_nv) =
                    self.url_converter.registry_package_url_to_nv(&specifier)
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
              SymbolDeclKind::Definition(node) => {
                if let Some(node) = node.maybe_ref() {
                  for dep in node.deps(ResolveDepsMode::TypesAndExpressions) {
                    match dep {
                      SymbolNodeDep::Id(id) => {
                        let module_info = module_info.esm().unwrap();
                        if let Some(symbol_id) =
                          module_info.symbol_id_from_swc(&id)
                        {
                          pending_traces
                            .maybe_add_id_trace(symbol_id, referrer_id);
                        }
                      }
                      SymbolNodeDep::QualifiedId(id, parts) => {
                        let module_info = module_info.esm().unwrap();
                        if let Some(symbol_id) =
                          module_info.symbol_id_from_swc(&id)
                        {
                          pending_traces.traces.push_back(
                            PendingIdTrace::QualifiedId {
                              symbol_id,
                              parts: NamedExports::from_parts(&parts),
                              referrer_id,
                            },
                          );
                        }
                      }
                      SymbolNodeDep::ImportType(specifier, parts) => {
                        if let Some(specifier) = self.graph.resolve_dependency(
                          &specifier,
                          module_info.specifier(),
                          /* prefer types */ true,
                        ) {
                          if let Some(dep_nv) = self
                            .url_converter
                            .registry_package_url_to_nv(&specifier)
                          {
                            if dep_nv == *pkg_nv {
                              // just add this specifier
                              self.add_pending_trace(
                                &dep_nv,
                                &specifier,
                                if parts.is_empty() {
                                  ImportedExports::star_with_default()
                                } else {
                                  ImportedExports::named(
                                    NamedExports::from_parts(&parts),
                                  )
                                },
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
                  }
                }
              }
            }
          }

          pending_traces.traces.extend(
            symbol
              .exports()
              .values()
              .map(|id| (*id, symbol.symbol_id()))
              .chain(
                symbol.members().iter().map(|id| (*id, symbol.symbol_id())),
              )
              .filter(|(symbol_id, referrer_id)| {
                !pending_traces
                  .done_id_traces
                  .contains(&(*symbol_id, *referrer_id))
              })
              .map(|(symbol_id, referrer_id)| PendingIdTrace::Id {
                symbol_id,
                referrer_id,
              }),
          );
        }
        PendingIdTrace::QualifiedId {
          symbol_id,
          parts,
          referrer_id,
        } => {
          let symbol = module_info.symbol(symbol_id).unwrap();

          let mut handled = false;
          for decl in symbol.decls() {
            found_ranges.insert(decl.range);
            match &decl.kind {
              SymbolDeclKind::Target(id) => {
                handled = true;
                let symbol_id = module_info
                  .esm()
                  .and_then(|m| m.symbol_id_from_swc(id))
                  .unwrap();
                pending_traces
                  .traces
                  .push_back(PendingIdTrace::QualifiedId {
                    symbol_id,
                    parts: parts.clone(),
                    referrer_id,
                  });
              }
              SymbolDeclKind::QualifiedTarget(id, target_parts) => {
                handled = true;
                let symbol_id = module_info
                  .esm()
                  .and_then(|m| m.symbol_id_from_swc(id))
                  .unwrap();
                let mut new_parts = NamedExports::default();
                for parts in parts.clone().into_separate_parts() {
                  let combined_vec = target_parts
                    .iter()
                    .cloned()
                    .chain(parts.into_iter())
                    .collect::<Vec<_>>();
                  new_parts.add_qualified(&combined_vec[0], &combined_vec[1..]);
                }
                pending_traces
                  .traces
                  .push_back(PendingIdTrace::QualifiedId {
                    symbol_id,
                    parts: new_parts,
                    referrer_id,
                  });
              }
              SymbolDeclKind::FileRef(file_dep) => {
                handled = true;
                if let Some(specifier) = self.graph.resolve_dependency(
                  &file_dep.specifier,
                  module_info.specifier(),
                  /* prefer types */ true,
                ) {
                  if let Some(dep_nv) =
                    self.url_converter.registry_package_url_to_nv(&specifier)
                  {
                    if dep_nv == *pkg_nv {
                      let named_exports = match &file_dep.name {
                        FileDepName::Star => parts.clone(),
                        FileDepName::Name(first_part) => {
                          let mut separate_parts =
                            parts.clone().into_separate_parts();
                          for parts in &mut separate_parts {
                            parts.insert(0, first_part.clone());
                          }
                          NamedExports::from_many_parts(&separate_parts)
                        }
                      };
                      // just add this specifier
                      self.add_pending_trace(
                        &dep_nv,
                        &specifier,
                        ImportedExports::named(named_exports),
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
              SymbolDeclKind::Definition(_) => {}
            }
          }

          if !handled {
            for parts in parts.into_separate_parts() {
              if parts[0] == "prototype"
                && symbol.decls().iter().any(|d| d.is_class())
              {
                if parts.len() > 1 {
                  let mut member_symbols = symbol
                    .members()
                    .iter()
                    .filter_map(|id| module_info.symbol(*id));
                  let member_symbol = member_symbols.find(|s| {
                    let maybe_name = s.maybe_name();
                    maybe_name.as_deref() == Some(parts[1].as_str())
                  });
                  match member_symbol {
                    Some(member) => {
                      if parts.len() > 2 {
                        diagnostics.push(
                          FastCheckDiagnostic::UnsupportedComplexReference {
                            range: DiagnosticRange::new(
                              module_info.specifier().clone(),
                              symbol.decls()[0].range,
                            ),
                            name: format!(
                              "{}.prototype.{}",
                              module_info
                                .fully_qualified_symbol_name(symbol)
                                .unwrap_or_else(|| "<unknown>".to_string()),
                              parts[1..].join("."),
                            ),
                            referrer: module_info
                              .symbol(referrer_id)
                              .and_then(|symbol| {
                                module_info.fully_qualified_symbol_name(symbol)
                              })
                              .unwrap_or_else(|| "<unknown>".to_string()),
                          },
                        );
                      } else {
                        pending_traces
                          .maybe_add_id_trace(member.symbol_id(), referrer_id);
                      }
                    }
                    None => {
                      diagnostics.push(
                        FastCheckDiagnostic::NotFoundReference {
                          range: DiagnosticRange::new(
                            module_info.specifier().clone(),
                            symbol.decls()[0].range,
                          ),
                          name: format!(
                            "{}.prototype.{}",
                            module_info
                              .fully_qualified_symbol_name(symbol)
                              .unwrap_or_else(|| "<unknown>".to_string()),
                            parts[1],
                          ),
                          referrer: module_info
                            .symbol(referrer_id)
                            .and_then(|symbol| {
                              module_info.fully_qualified_symbol_name(symbol)
                            })
                            .unwrap_or_else(|| "<unknown>".to_string()),
                        },
                      );
                    }
                  }
                } else {
                  pending_traces.maybe_add_id_trace(symbol_id, referrer_id);
                }
              } else {
                match symbol.export(&parts[0]) {
                  Some(symbol_id) => {
                    if parts.len() > 1 {
                      pending_traces.traces.push_back(
                        PendingIdTrace::QualifiedId {
                          symbol_id,
                          parts: NamedExports::from_parts(&parts[1..]),
                          referrer_id,
                        },
                      );
                    } else {
                      pending_traces.maybe_add_id_trace(symbol_id, referrer_id);
                    }
                  }
                  None => {
                    // if the init expression of the variable is leavable, just ignore it
                    let ignore =
                      symbol.decls().iter().filter_map(|d| d.maybe_node()).all(
                        |n| match n {
                          SymbolNodeRef::ExportDecl(
                            _,
                            ExportDeclRef::Var(_, v, _),
                          )
                          | SymbolNodeRef::Var(_, v, _) => match &v.init {
                            Some(init) => is_expr_leavable(init),
                            None => false,
                          },
                          SymbolNodeRef::ExportDecl(
                            _,
                            ExportDeclRef::TsEnum(_),
                          )
                          | SymbolNodeRef::TsEnum(_) => true,
                          _ => false,
                        },
                      );
                    if !ignore {
                      diagnostics.push(
                        FastCheckDiagnostic::NotFoundReference {
                          range: DiagnosticRange::new(
                            module_info.specifier().clone(),
                            symbol.decls()[0].range,
                          ),
                          name: format!(
                            "{}.{}",
                            module_info
                              .fully_qualified_symbol_name(symbol)
                              .unwrap_or_else(|| "<unknown>".to_string()),
                            parts[0],
                          ),
                          referrer: module_info
                            .symbol(referrer_id)
                            .and_then(|symbol| {
                              module_info.fully_qualified_symbol_name(symbol)
                            })
                            .unwrap_or_else(|| "<unknown>".to_string()),
                        },
                      );
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    let ranges = self
      .public_ranges
      .entry(trace.package_nv.clone())
      .or_default()
      .module_ranges
      .entry(module_info.specifier().clone())
      .or_default();
    ranges.ranges.extend(found_ranges);
    ranges
      .impl_with_overload_ranges
      .extend(impl_with_overload_ranges);
    ranges.diagnostics.extend(diagnostics);

    found
  }

  fn is_typed_specifier(&mut self, specifier: &ModuleSpecifier) -> bool {
    let Some(module) = self.graph.get(specifier) else {
      return true; // just analyze it
    };
    match module {
      crate::Module::Esm(m) => is_typed_media_type(m.media_type),
      crate::Module::Json(_) => true,
      crate::Module::Npm(_)
      | crate::Module::Node(_)
      | crate::Module::External(_) => false,
    }
  }
}

fn is_typed_media_type(media_type: MediaType) -> bool {
  match media_type {
    MediaType::JavaScript
    | MediaType::Jsx
    | MediaType::Mjs
    | MediaType::Cjs
    | MediaType::TsBuildInfo
    | MediaType::SourceMap
    | MediaType::Unknown => false,
    MediaType::TypeScript
    | MediaType::Mts
    | MediaType::Cts
    | MediaType::Dts
    | MediaType::Dmts
    | MediaType::Dcts
    | MediaType::Tsx
    | MediaType::Json
    | MediaType::Wasm => true,
  }
}
