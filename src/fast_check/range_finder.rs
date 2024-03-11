// Copyright 2018-2024 the Deno authors. MIT license.

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::sync::Arc;

use deno_ast::swc::ast::Expr;
use deno_ast::MediaType;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_semver::package::PackageNv;
use indexmap::IndexMap;
use url::Url;

use crate::source::JsrUrlProvider;
use crate::symbols::FileDepName;
use crate::symbols::ModuleInfoRef;
use crate::symbols::ResolveDepsMode;
use crate::symbols::RootSymbol;
use crate::symbols::SymbolDeclKind;
use crate::symbols::SymbolId;
use crate::symbols::SymbolNodeDep;
use crate::symbols::SymbolNodeRef;
use crate::FastCheckCache;
use crate::FastCheckCacheItem;
use crate::FastCheckCacheKey;
use crate::FastCheckDiagnosticRange;
use crate::FastCheckModule;
use crate::ModuleGraph;
use crate::ModuleSpecifier;
use crate::WorkspaceMember;

use super::cache::fast_insecure_hash;
use super::FastCheckDiagnostic;

// todo(THIS PR): unit test
#[derive(Default, Debug, Clone)]
struct NamedSubset(IndexMap<String, Exports>);

impl NamedSubset {
  pub fn from_parts(parts: &[String]) -> Self {
    let mut exports = Self::default();
    if !parts.is_empty() {
      exports.add_qualified(parts[0].to_string(), &parts[1..]);
    }
    exports
  }

  pub fn add(&mut self, export: String) {
    match self.0.entry(export) {
      indexmap::map::Entry::Occupied(mut entry) => {
        *entry.get_mut() = Exports::All;
      }
      indexmap::map::Entry::Vacant(entry) => {
        entry.insert(Exports::All);
      }
    }
  }

  pub fn add_qualified(&mut self, export_name: String, qualified: &[String]) {
    if qualified.is_empty() {
      self.add(export_name);
    } else {
      let entry = self
        .0
        .entry(export_name)
        .or_insert_with(|| Exports::subset());
      if matches!(entry, Exports::All) {
        return;
      }
      entry.add_qualified(&qualified[0], &qualified[1..]);
    }
  }

  pub fn add_named(&mut self, export: String, exports: Exports) {
    match self.0.entry(export) {
      indexmap::map::Entry::Occupied(mut entry) => {
        let entry = entry.get_mut();
        entry.extend(exports);
      }
      indexmap::map::Entry::Vacant(entry) => {
        entry.insert(exports);
      }
    }
  }

  pub fn extend(&mut self, new_subset: NamedSubset) -> NamedSubset {
    let mut difference = NamedSubset::default();
    for (key, exports) in new_subset.0 {
      if let Some(entry) = self.0.get_mut(&key) {
        let sub_diff = entry.extend(exports);
        if let Some(sub_diff) = sub_diff {
          difference.add_named(key.clone(), sub_diff);
        }
      } else {
        difference.add_named(key.clone(), exports.clone());
        self.0.insert(key, exports);
      }
    }
    difference
  }
}

#[derive(Debug, Clone)]
enum Exports {
  All,
  Subset(NamedSubset),
}

impl Exports {
  pub fn subset() -> Self {
    Self::Subset(Default::default())
  }

  pub fn add_qualified(&mut self, export_name: &str, qualified: &[String]) {
    let Exports::Subset(inner) = self else {
      return;
    };
    inner.add_qualified(export_name.to_string(), qualified)
  }

  pub fn extend(&mut self, new_named: Exports) -> Option<Exports> {
    let current_subset = &mut match self {
      Exports::All => return None,
      Exports::Subset(inner) => inner,
    };

    match new_named {
      Exports::All => {
        *self = Exports::All;
        Some(Exports::All)
      }
      Exports::Subset(new_subset) => {
        Some(Exports::Subset(current_subset.extend(new_subset)))
      }
    }
  }
}

#[derive(Debug, Clone)]
enum ImportedExports {
  Star,
  StarWithDefault,
  Subset(NamedSubset),
}

impl ImportedExports {
  pub(crate) fn from_file_dep_name(dep_name: &FileDepName) -> Self {
    match dep_name {
      FileDepName::Star => ImportedExports::Star,
      FileDepName::Name(value) => {
        let mut named_exports = NamedSubset::default();
        named_exports.add(value.clone());
        ImportedExports::Subset(named_exports)
      }
    }
  }

  pub fn star_with_default() -> ImportedExports {
    ImportedExports::StarWithDefault
  }

  pub fn star() -> ImportedExports {
    ImportedExports::Star
  }

  pub fn subset(named: NamedSubset) -> ImportedExports {
    ImportedExports::Subset(named)
  }

  /// Adds the incoming exports to the existing exports and
  /// returns the newly added exports that have not previously
  /// been added.
  pub(crate) fn add(
    &mut self,
    exports_to_trace: ImportedExports,
  ) -> Option<ImportedExports> {
    match self {
      ImportedExports::Star => match exports_to_trace {
        ImportedExports::Star => None,
        ImportedExports::StarWithDefault => {
          *self = ImportedExports::StarWithDefault;
          let mut named_exports = NamedSubset::default();
          named_exports.add("default".to_string());
          Some(ImportedExports::Subset(named_exports))
        }
        ImportedExports::Subset(_) => None,
      },
      ImportedExports::StarWithDefault => None,
      ImportedExports::Subset(current_subset) => match exports_to_trace {
        ImportedExports::Star => {
          *self = ImportedExports::Star;
          Some(ImportedExports::Star)
        }
        ImportedExports::StarWithDefault => {
          *self = ImportedExports::StarWithDefault;
          Some(ImportedExports::StarWithDefault)
        }
        ImportedExports::Subset(new_subset) => {
          Some(ImportedExports::Subset(current_subset.extend(new_subset)))
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
  fast_check_cache: Option<&'a dyn FastCheckCache>,
  jsr_url_provider: &'a dyn JsrUrlProvider,
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
    fast_check_cache,
    graph,
    workspace_members,
    root_symbol,
    url_converter: RegistryUrlConverter {
      jsr_url_provider,
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
  jsr_url_provider: &'a dyn JsrUrlProvider,
  workspace_members: &'a [WorkspaceMember],
}

impl<'a> RegistryUrlConverter<'a> {
  fn registry_package_url(&self, nv: &PackageNv) -> Url {
    if let Some(member) = self.workspace_members.iter().find(|m| m.nv == *nv) {
      member.base.clone()
    } else {
      self.jsr_url_provider.package_url(nv)
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
      self.jsr_url_provider.package_url_to_nv(url)
    }
  }
}

#[derive(Debug, Default)]
pub struct PackagePublicRanges {
  pub entrypoints: BTreeSet<ModuleSpecifier>,
  // uses an IndexMap to maintain order so that when transforming
  // it goes over the modules in the exact same deterministic order
  pub module_ranges: IndexMap<ModuleSpecifier, ModulePublicRanges>,
  /// Items loaded from the cache. If set, these should be used over module_ranges.
  pub cache_items: Vec<(
    ModuleSpecifier,
    Result<FastCheckModule, Vec<FastCheckDiagnostic>>,
  )>,
  pub dependencies: BTreeSet<PackageNv>,
}

struct PublicRangeFinder<'a> {
  url_converter: RegistryUrlConverter<'a>,
  graph: &'a ModuleGraph,
  fast_check_cache: Option<&'a dyn FastCheckCache>,
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
      let Some(exports) = self
        .graph
        .packages
        .package_exports(&nv)
        .map(Cow::Borrowed)
        .or_else(|| {
          Some(Cow::Owned(
            self
              .workspace_members
              .iter()
              .find(|m| m.nv == nv)?
              .exports
              .iter()
              .map(|(k, v)| (k.clone(), v.clone()))
              .collect(),
          ))
        })
      else {
        // may happen in a segmented graph since graph
        // segmentation is not that smart at the moment
        continue;
      };
      let base_url = self.url_converter.registry_package_url(&nv);
      let entrypoints = exports
        .values()
        .map(|value| {
          // if we got this far, then the export must be valid, so we can unwrap
          base_url.join(value).unwrap()
        })
        .collect::<BTreeSet<_>>();

      if let Some(mut public_ranges) =
        self.try_get_cache_item(&nv, &entrypoints)
      {
        log::debug!("Using FastCheck cache for: {}", nv);
        public_ranges.entrypoints = entrypoints;
        self.public_ranges.insert(nv, public_ranges);
      } else {
        let mut had_diagnostic = false;
        for specifier in &entrypoints {
          if !self.is_typed_specifier(specifier) {
            self
              .public_ranges
              .entry(nv.clone())
              .or_default()
              .module_ranges
              .entry(specifier.clone())
              .or_default()
              .diagnostics
              .push(FastCheckDiagnostic::UnsupportedJavaScriptEntrypoint {
                specifier: specifier.clone(),
              });
            had_diagnostic = true;
          }
        }

        if !had_diagnostic {
          for specifier in &entrypoints {
            self.add_pending_trace(
              &nv,
              specifier,
              ImportedExports::star_with_default(),
            );
          }
        }

        while let Some(trace) = self.pending_traces.pop() {
          self.analyze_trace(&trace);
        }

        let public_ranges = self.public_ranges.entry(nv).or_default();
        public_ranges.entrypoints = entrypoints;
      }
    }

    self.public_ranges
  }

  fn try_get_cache_item(
    &mut self,
    nv: &PackageNv,
    entrypoints: &BTreeSet<ModuleSpecifier>,
  ) -> Option<PackagePublicRanges> {
    let Some(fast_check_cache) = &self.fast_check_cache else {
      return None;
    };
    let cache_key = FastCheckCacheKey::build(nv, entrypoints);
    let Some(cache_item) = fast_check_cache.get(cache_key) else {
      return None;
    };
    if !self.is_cache_item_valid(&cache_item) {
      return None;
    }
    // fill in the dependencies
    for dep in cache_item.dependencies {
      self.add_pending_nv_no_referrer(&dep)
    }
    // now fill in the entry
    let mut package = PackagePublicRanges::default();
    for (url, cache_item) in cache_item.modules {
      match cache_item {
        super::cache::FastCheckCacheModuleItem::Info(info) => {
          let Ok(module_info) = serde_json::from_str(&info.module_info) else {
            return None;
          };
          package.cache_items.push((
            url,
            Ok(FastCheckModule {
              module_info: Arc::new(module_info),
              text: info.text,
              source_map: info.source_map,
              dts: None,
            }),
          ));
        }
        super::cache::FastCheckCacheModuleItem::Diagnostic(_) => {
          package.cache_items.push((
            url.clone(),
            Err(vec![FastCheckDiagnostic::Cached { specifier: url }]),
          ));
        }
      }
    }
    Some(package)
  }

  fn is_cache_item_valid(&self, cache_item: &FastCheckCacheItem) -> bool {
    for (specifier, module_item) in &cache_item.modules {
      let hash = self
        .graph
        .get(specifier)
        .and_then(|m| m.source())
        .map(|s| fast_insecure_hash(s.as_bytes()))
        .unwrap_or(0);
      if hash != module_item.source_hash() {
        return false;
      }
    }

    true
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

  fn add_pending_nv(&mut self, dep: &PackageNv, referrer_nv: &PackageNv) {
    if dep == referrer_nv {
      return;
    }

    // when a package is referenced then we need to analyze
    // all the dependencies for it in the graph
    let is_new_dep = self
      .public_ranges
      .entry(referrer_nv.clone())
      .or_default()
      .dependencies
      .insert(dep.clone());
    // if it's not a new dep then we've been here before
    // so no reason to attempt this again
    if is_new_dep {
      self.add_pending_nv_no_referrer(dep);
    }
  }

  fn add_pending_nv_no_referrer(&mut self, nv: &PackageNv) {
    let never_seen = self.seen_nvs.insert(nv.clone());
    if never_seen {
      self.pending_nvs.push_back(nv.clone());
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
      let ranges = self
        .public_ranges
        .entry(trace.package_nv.clone())
        .or_default()
        .module_ranges
        .entry(trace.specifier.clone())
        .or_default();
      ranges.diagnostics.push(FastCheckDiagnostic::External {
        specifier: trace.specifier.clone(),
      });
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
        parts: NamedSubset,
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

    let include_default =
      matches!(trace.exports_to_trace, ImportedExports::StarWithDefault);
    match &trace.exports_to_trace {
      ImportedExports::Star | ImportedExports::StarWithDefault => {
        for (name, export_symbol_id) in module_info.module_symbol().exports() {
          if name == "default" && !include_default {
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
                self.add_pending_nv(&dep_nv, pkg_nv);

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
      ImportedExports::Subset(named_exports) => {
        let mut named_exports = named_exports.0.clone();
        let module_exports = module_info.module_symbol().exports();
        for i in (0..named_exports.len()).rev() {
          let (export_name, _) = named_exports.get_index(i).unwrap();
          if let Some(export_symbol_id) = module_exports.get(export_name) {
            let export_name = export_name.clone();
            let named_exports =
              named_exports.swap_remove(&export_name).unwrap();
            match named_exports {
              Exports::All => {
                pending_traces.maybe_add_id_trace(
                  *export_symbol_id,
                  module_symbol.symbol_id(),
                );
              }
              Exports::Subset(subset) => {
                pending_traces
                  .traces
                  .push_back(PendingIdTrace::QualifiedId {
                    symbol_id: *export_symbol_id,
                    parts: subset,
                    referrer_id: module_symbol.symbol_id(),
                  });
              }
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
                        named_exports.swap_remove(&export_name).unwrap();
                      let module = match export_path {
                                crate::symbols::ResolvedExportOrReExportAllPath::Export(e) => e.module,
                                crate::symbols::ResolvedExportOrReExportAllPath::ReExportAllPath(p) => p.referrer_module,
                            };
                      if let Some(nv) = self
                        .url_converter
                        .registry_package_url_to_nv(module.specifier())
                      {
                        let mut new_named_exports = NamedSubset::default();
                        new_named_exports.0.insert(export_name, named_exports);
                        self.add_pending_trace(
                          &nv,
                          module.specifier(),
                          ImportedExports::subset(new_named_exports),
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
                  range: FastCheckDiagnosticRange {
                    specifier: module_info.specifier().clone(),
                    range: symbol.decls()[0].range,
                    text_info: module_info.text_info().clone(),
                  },
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
                      parts: NamedSubset::from_parts(parts),
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
                      self.add_pending_nv(&dep_nv, pkg_nv);
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
                              parts: NamedSubset::from_parts(&parts),
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
                                  ImportedExports::subset(
                                    NamedSubset::from_parts(&parts),
                                  )
                                },
                              );
                            } else {
                              // need to analyze the whole package
                              self.add_pending_nv(&dep_nv, pkg_nv);
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
                let mut new_parts = parts.clone();
                for part in target_parts {
                  new_parts.add(part.clone());
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
                        FileDepName::Star => {
                          // pass along the names as-is
                          ImportedExports::Subset(parts.clone())
                        }
                        FileDepName::Name(first_part) => {
                          // todo(THIS PR): is this correct? Try changing this code to see if we have a test
                          // add this name to the parts
                          let mut new_subset = NamedSubset::default();
                          new_subset.add_named(
                            first_part.clone(),
                            Exports::Subset(parts.clone()),
                          );
                          ImportedExports::Subset(new_subset)
                        }
                      };
                      // just add this specifier
                      self.add_pending_trace(
                        &dep_nv,
                        &specifier,
                        named_exports,
                      );
                    } else {
                      // need to analyze the whole package
                      self.add_pending_nv(&dep_nv, pkg_nv);
                    }
                  }
                }
              }
              SymbolDeclKind::Definition(_) => {}
            }
          }

          if !handled {
            for (first_part, next_parts) in parts.0 {
              if first_part == "prototype"
                && symbol.decls().iter().any(|d| d.is_class())
              {
                match next_parts {
                  Exports::All => {
                    pending_traces.maybe_add_id_trace(symbol_id, referrer_id);
                  }
                  Exports::Subset(next_parts) => {
                    let mut member_symbols = symbol
                      .members()
                      .iter()
                      .filter_map(|id| module_info.symbol(*id));
                    for (second_part, next_parts) in next_parts.0 {
                      let member_symbol = member_symbols.find(|s| {
                        let maybe_name = s.maybe_name();
                        maybe_name.as_deref() == Some(second_part.as_str())
                      });
                      match member_symbol {
                        Some(member) => match next_parts {
                          Exports::All => {
                            pending_traces.maybe_add_id_trace(
                              member.symbol_id(),
                              referrer_id,
                            );
                          }
                          Exports::Subset(next_parts) => {
                            for third_part in next_parts.0.keys() {
                              diagnostics.push(
                                  FastCheckDiagnostic::UnsupportedComplexReference {
                                    range: FastCheckDiagnosticRange {
                                      specifier: module_info.specifier().clone(),
                                      range: symbol.decls()[0].range,
                                      text_info: module_info.text_info().clone(),
                                    },
                                    name: format!(
                                      "{}.prototype.{}.{}",
                                      module_info
                                        .fully_qualified_symbol_name(symbol)
                                        .unwrap_or_else(|| "<unknown>".to_string()),
                                      second_part,
                                      third_part,
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
                        },
                        None => {
                          diagnostics.push(
                            FastCheckDiagnostic::NotFoundReference {
                              range: FastCheckDiagnosticRange {
                                specifier: module_info.specifier().clone(),
                                range: symbol.decls()[0].range,
                                text_info: module_info.text_info().clone(),
                              },
                              name: format!(
                                "{}.prototype.{}",
                                module_info
                                  .fully_qualified_symbol_name(symbol)
                                  .unwrap_or_else(|| "<unknown>".to_string()),
                                second_part,
                              ),
                              referrer: module_info
                                .symbol(referrer_id)
                                .and_then(|symbol| {
                                  module_info
                                    .fully_qualified_symbol_name(symbol)
                                })
                                .unwrap_or_else(|| "<unknown>".to_string()),
                            },
                          );
                        }
                      }
                    }
                  }
                }
              } else {
                match symbol.export(&first_part) {
                  Some(symbol_id) => match next_parts {
                    Exports::All => {
                      pending_traces.maybe_add_id_trace(symbol_id, referrer_id);
                    }
                    Exports::Subset(subset) => {
                      pending_traces.traces.push_back(
                        PendingIdTrace::QualifiedId {
                          symbol_id,
                          parts: subset,
                          referrer_id,
                        },
                      );
                    }
                  },
                  None => {
                    // 1. For classes, we want to ensure the type is not referencing
                    // a private typescript member (ex. private myMethod() {})
                    // because those get removed from the output.
                    // 2. For namespaces, we could opt to include the entire namespace
                    // but that might cause more diagnostics and confusion down the line.
                    // This should never happen for namespaces, but if it does then someone
                    // could report a bug to us and we could look into how we can solve it.
                    let symbol_has_class_or_namespace_decl =
                      symbol.decls().iter().filter_map(|d| d.maybe_node()).any(
                        |n| {
                          if n.is_class() || n.is_ts_namespace() {
                            true
                          } else if let SymbolNodeRef::ExportDefaultExpr(
                            default_expr,
                          ) = n
                          {
                            matches!(&*default_expr.expr, Expr::Class(_))
                          } else {
                            false
                          }
                        },
                      );
                    if !symbol_has_class_or_namespace_decl {
                      // If we can't resolve the symbol member and it's not a namespace
                      // or a class (ex. if it's a variable with a type) then just add
                      // the symbol at this point because we'll want to include the entire
                      // type being referenced rather than just the member.
                      pending_traces.maybe_add_id_trace(symbol_id, referrer_id);
                    } else {
                      diagnostics.push(
                        FastCheckDiagnostic::NotFoundReference {
                          range: FastCheckDiagnosticRange {
                            specifier: module_info.specifier().clone(),
                            range: symbol.decls()[0].range,
                            text_info: module_info.text_info().clone(),
                          },
                          name: format!(
                            "{}.{}",
                            module_info
                              .fully_qualified_symbol_name(symbol)
                              .unwrap_or_else(|| "<unknown>".to_string()),
                            first_part,
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
      crate::Module::Js(m) => {
        is_typed_media_type(m.media_type) || m.maybe_types_dependency.is_some()
      }
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
