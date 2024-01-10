// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::collections::HashSet;
use std::collections::VecDeque;

use deno_ast::SourceRange;
use indexmap::IndexMap;

use crate::ModuleGraph;
use crate::ModuleSpecifier;

use super::analyzer::SymbolDeclKind;
use super::FileDep;
use super::FileDepName;
use super::ModuleInfoRef;
use super::Symbol;
use super::SymbolDecl;
use super::SymbolId;
use super::SymbolNodeDep;
use super::UniqueSymbolId;

#[derive(Debug, Clone)]
pub enum DefinitionOrUnresolved<'a> {
  Definition(Definition<'a>),
  Unresolved(DefinitionUnresolved<'a>),
}

impl<'a> DefinitionOrUnresolved<'a> {
  pub fn module(&self) -> ModuleInfoRef<'a> {
    match self {
      DefinitionOrUnresolved::Definition(def) => def.module,
      DefinitionOrUnresolved::Unresolved(unresolved) => unresolved.module,
    }
  }

  pub fn symbol(&self) -> &'a Symbol {
    match self {
      DefinitionOrUnresolved::Definition(def) => def.symbol,
      DefinitionOrUnresolved::Unresolved(unresolved) => unresolved.symbol,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefinitionKind<'a> {
  ExportStar(&'a FileDep),
  Definition,
}

#[derive(Debug, Clone)]
pub struct Definition<'a> {
  pub kind: DefinitionKind<'a>,
  pub module: ModuleInfoRef<'a>,
  pub symbol: &'a Symbol,
  pub symbol_decl: &'a SymbolDecl,
}

impl<'a> Definition<'a> {
  pub fn range(&self) -> &SourceRange {
    &self.symbol_decl.range
  }

  pub fn byte_range(&self) -> std::ops::Range<usize> {
    self
      .range()
      .as_byte_range(self.module.text_info().range().start)
  }

  pub fn text(&self) -> &str {
    self.module.text_info().range_text(self.range())
  }
}

#[derive(Debug, Clone)]
pub enum DefinitionUnresolvedKind {
  /// Could not resolve the swc Id.
  Id(deno_ast::swc::ast::Id),
  /// Could not resolve the specifier relative to this module via deno_graph.
  Specifier(String),
  /// Could not resolve the part on the symbol.
  Part(String),
}

/// The point at which a definition could not be resolved.
#[derive(Debug, Clone)]
pub struct DefinitionUnresolved<'a> {
  pub module: ModuleInfoRef<'a>,
  pub symbol: &'a Symbol,
  pub kind: DefinitionUnresolvedKind,
  pub parts: Vec<String>,
}

/// A graph path to a definition.
#[derive(Debug, Clone)]
pub enum DefinitionPath<'a> {
  Path {
    module: ModuleInfoRef<'a>,
    symbol: &'a Symbol,
    symbol_decl: &'a SymbolDecl,
    parts: Vec<String>,
    next: Vec<DefinitionPath<'a>>,
  },
  Definition(Definition<'a>),
  Unresolved(DefinitionUnresolved<'a>),
}

impl<'a> DefinitionPath<'a> {
  pub fn module(&self) -> ModuleInfoRef<'a> {
    match self {
      DefinitionPath::Path { module, .. } => *module,
      DefinitionPath::Definition(def) => def.module,
      DefinitionPath::Unresolved(unresolved) => unresolved.module,
    }
  }

  pub fn symbol(&self) -> &'a Symbol {
    match self {
      DefinitionPath::Path { symbol, .. } => symbol,
      DefinitionPath::Definition(def) => def.symbol,
      DefinitionPath::Unresolved(unresolved) => unresolved.symbol,
    }
  }

  pub fn into_definitions(self) -> impl Iterator<Item = Definition<'a>> {
    self
      .into_definitions_or_unresolveds()
      .filter_map(|d| match d {
        DefinitionOrUnresolved::Definition(d) => Some(d),
        DefinitionOrUnresolved::Unresolved(_) => None,
      })
  }

  pub fn into_definitions_or_unresolveds(
    self,
  ) -> impl Iterator<Item = DefinitionOrUnresolved<'a>> {
    struct IntoIterator<'a> {
      queue: VecDeque<DefinitionPath<'a>>,
    }

    impl<'a> Iterator for IntoIterator<'a> {
      type Item = DefinitionOrUnresolved<'a>;

      fn next(&mut self) -> Option<Self::Item> {
        while let Some(path) = self.queue.pop_front() {
          match path {
            DefinitionPath::Path { next, .. } => {
              for child_path in next.into_iter().rev() {
                self.queue.push_front(child_path);
              }
            }
            DefinitionPath::Definition(def) => {
              return Some(DefinitionOrUnresolved::Definition(def));
            }
            DefinitionPath::Unresolved(unresolved) => {
              return Some(DefinitionOrUnresolved::Unresolved(unresolved));
            }
          }
        }

        None
      }
    }

    IntoIterator {
      queue: VecDeque::from([self]),
    }
  }
}

/// Finds the path to a definition.
pub fn find_definition_paths<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  symbol: &'a Symbol,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  find_definition_paths_internal(
    module_graph,
    module,
    symbol,
    &mut Default::default(),
    specifier_to_module,
  )
}

fn find_definition_paths_internal<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  symbol: &'a Symbol,
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  debug_assert_eq!(module.module_id(), symbol.module_id());
  if !visited_symbols.insert(symbol.unique_id()) {
    return Vec::new();
  }
  let mut paths = Vec::with_capacity(symbol.decls().len());
  for decl in symbol.decls() {
    match &decl.kind {
      SymbolDeclKind::Definition(_) => {
        paths.push(DefinitionPath::Definition(Definition {
          module,
          symbol,
          symbol_decl: decl,
          kind: DefinitionKind::Definition,
        }));
      }
      SymbolDeclKind::Target(target_id) => {
        if let Some(symbol) = module
          .esm()
          .unwrap()
          .symbol_id_from_swc(target_id)
          .and_then(|id| module.symbol(id))
        {
          let inner_paths = find_definition_paths_internal(
            module_graph,
            module,
            symbol,
            visited_symbols,
            specifier_to_module,
          );
          if !inner_paths.is_empty() {
            paths.push(DefinitionPath::Path {
              module,
              symbol,
              symbol_decl: decl,
              parts: Vec::new(),
              next: inner_paths,
            });
          }
        }
      }
      SymbolDeclKind::QualifiedTarget(target_id, parts) => {
        let inner_paths = go_to_id_and_parts_definition_paths(
          module_graph,
          module,
          symbol,
          target_id,
          parts,
          specifier_to_module,
        );
        if !inner_paths.is_empty() {
          paths.push(DefinitionPath::Path {
            module,
            symbol,
            symbol_decl: decl,
            parts: parts.clone(),
            next: inner_paths,
          });
        }
      }
      SymbolDeclKind::FileRef(file_ref) => match &file_ref.name {
        FileDepName::Star => {
          paths.push(DefinitionPath::Definition(Definition {
            module,
            symbol,
            kind: DefinitionKind::ExportStar(file_ref),
            symbol_decl: decl,
          }));
        }
        FileDepName::Name(export_name) => {
          let inner_paths = go_to_file_export(
            module_graph,
            module,
            file_ref,
            export_name,
            specifier_to_module,
            visited_symbols,
          );
          if !inner_paths.is_empty() {
            paths.push(DefinitionPath::Path {
              module,
              symbol,
              symbol_decl: decl,
              parts: Vec::new(),
              next: inner_paths,
            });
          }
        }
      },
    }
  }
  paths
}

fn go_to_file_export<'a>(
  module_graph: &'a ModuleGraph,
  referrer_module: ModuleInfoRef<'a>,
  file_ref: &'a FileDep,
  export_name: &'a str,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
  visited_symbols: &mut HashSet<UniqueSymbolId>,
) -> Vec<DefinitionPath<'a>> {
  let maybe_dep_module = module_graph
    .resolve_dependency(
      &file_ref.specifier,
      referrer_module.specifier(),
      /* prefer types */ true,
    )
    .and_then(|dep| specifier_to_module(&dep));

  let Some(dep_module) = maybe_dep_module else {
    return vec![DefinitionPath::Unresolved(DefinitionUnresolved {
      module: referrer_module,
      symbol: referrer_module.module_symbol(),
      kind: DefinitionUnresolvedKind::Specifier(file_ref.specifier.clone()),
      parts: Vec::new(),
    })];
  };
  let maybe_export_symbol = dep_module
    .module_symbol()
    .exports()
    .get(export_name)
    .and_then(|symbol_id| dep_module.symbol(*symbol_id));
  match maybe_export_symbol {
    Some(export_symbol) => find_definition_paths_internal(
      module_graph,
      dep_module,
      export_symbol,
      visited_symbols,
      specifier_to_module,
    ),
    None => {
      // maybe it's in a re-export
      if let Some(re_export_all_specifiers) =
        dep_module.re_export_all_specifiers()
      {
        for re_export_specifier in re_export_all_specifiers {
          let maybe_specifier = module_graph.resolve_dependency(
            re_export_specifier,
            dep_module.specifier(),
            /* prefer_types */ true,
          );
          let maybe_module =
            maybe_specifier.and_then(|s| specifier_to_module(&s));
          let mut visited = HashSet::new();
          if let Some(module) = maybe_module {
            // todo(dsherret): this could be optimized to use an iterator
            let inner = exports_and_re_exports_inner(
              module_graph,
              module,
              specifier_to_module,
              &mut visited,
            );
            for (name, item) in inner.resolved {
              if name == export_name {
                let resolved_rexport = item.as_resolved_export();
                let paths = find_definition_paths_internal(
                  module_graph,
                  resolved_rexport.module,
                  resolved_rexport.symbol(),
                  visited_symbols,
                  specifier_to_module,
                );
                if !paths.is_empty() {
                  return paths;
                }
                break;
              }
            }
          }
        }
      }
      vec![DefinitionPath::Unresolved(DefinitionUnresolved {
        module: dep_module,
        symbol: dep_module.module_symbol(),
        kind: DefinitionUnresolvedKind::Part(export_name.to_string()),
        parts: Vec::new(),
      })]
    }
  }
}

/// A resolved `SymbolDep`.
#[derive(Debug)]
pub enum ResolvedSymbolDepEntry<'a> {
  /// The path to the definition of the symbol dep.
  Path(DefinitionPath<'a>),
  /// If the symbol dep was an import type with no property access.
  ///
  /// Ex. `type MyType = typeof import("./my_module.ts");`
  ImportType(ModuleInfoRef<'a>),
}

pub fn resolve_symbol_dep<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  symbol: &'a Symbol,
  dep: &SymbolNodeDep,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
) -> Vec<ResolvedSymbolDepEntry<'a>> {
  debug_assert_eq!(symbol.module_id(), module.module_id());
  match dep {
    SymbolNodeDep::Id(id) => {
      if let Some(dep_symbol) = module.esm().and_then(|m| m.symbol_from_swc(id))
      {
        find_definition_paths(
          module_graph,
          module,
          dep_symbol,
          specifier_to_module,
        )
        .into_iter()
        .map(ResolvedSymbolDepEntry::Path)
        .collect()
      } else {
        vec![ResolvedSymbolDepEntry::Path(DefinitionPath::Unresolved(
          DefinitionUnresolved {
            module,
            symbol,
            kind: DefinitionUnresolvedKind::Id(id.clone()),
            parts: Vec::new(),
          },
        ))]
      }
    }
    SymbolNodeDep::QualifiedId(id, parts) => {
      go_to_id_and_parts_definition_paths(
        module_graph,
        module,
        symbol,
        id,
        parts,
        specifier_to_module,
      )
      .into_iter()
      .map(ResolvedSymbolDepEntry::Path)
      .collect()
    }
    SymbolNodeDep::ImportType(import_specifier, parts) => {
      let maybe_dep_specifier = module_graph.resolve_dependency(
        import_specifier,
        module.specifier(),
        /* prefer types */ true,
      );
      let maybe_module =
        maybe_dep_specifier.as_ref().and_then(specifier_to_module);
      let Some(module) = maybe_module else {
        return vec![ResolvedSymbolDepEntry::Path(DefinitionPath::Unresolved(
          DefinitionUnresolved {
            module,
            symbol,
            kind: DefinitionUnresolvedKind::Specifier(import_specifier.clone()),
            parts: parts.clone(),
          },
        ))];
      };
      if parts.is_empty() {
        // an ImportType includes default exports
        vec![ResolvedSymbolDepEntry::ImportType(module)]
      } else {
        resolve_qualified_export_name(
          module_graph,
          module,
          parts,
          specifier_to_module,
        )
        .into_iter()
        .map(ResolvedSymbolDepEntry::Path)
        .collect()
      }
    }
  }
}

fn go_to_id_and_parts_definition_paths<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  symbol: &'a Symbol,
  target_id: &deno_ast::swc::ast::Id,
  parts: &[String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  debug_assert_eq!(symbol.module_id(), module.module_id());
  if let Some(symbol_id) =
    module.esm().and_then(|m| m.symbol_id_from_swc(target_id))
  {
    resolve_qualified_name(
      module_graph,
      module,
      module.symbol(symbol_id).unwrap(),
      parts,
      specifier_to_module,
    )
  } else {
    vec![DefinitionPath::Unresolved(DefinitionUnresolved {
      module,
      symbol,
      kind: DefinitionUnresolvedKind::Id(target_id.clone()),
      parts: parts.to_vec(),
    })]
  }
}

fn resolve_qualified_export_name<'a>(
  graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  parts: &[String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  debug_assert!(!parts.is_empty());
  resolve_qualified_export_name_internal(
    graph,
    module,
    parts,
    &mut HashSet::new(),
    specifier_to_module,
  )
}

fn resolve_qualified_export_name_internal<'a>(
  graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  parts: &[String],
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  debug_assert!(!parts.is_empty());
  let exports = exports_and_re_exports(graph, module, specifier_to_module);
  let export_name = &parts[0];
  if let Some(resolved) = exports.resolved.get(export_name) {
    let resolved = resolved.as_resolved_export();
    resolve_qualified_name_internal(
      graph,
      resolved.module,
      resolved.symbol(),
      &parts[1..],
      visited_symbols,
      specifier_to_module,
    )
  } else {
    vec![DefinitionPath::Unresolved(DefinitionUnresolved {
      module,
      symbol: module.module_symbol(),
      kind: DefinitionUnresolvedKind::Part(export_name.to_string()),
      parts: parts.to_vec(),
    })]
  }
}

pub fn resolve_qualified_name<'a>(
  graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  symbol: &'a Symbol,
  parts: &[String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  resolve_qualified_name_internal(
    graph,
    module,
    symbol,
    parts,
    &mut HashSet::new(),
    specifier_to_module,
  )
}

fn resolve_qualified_name_internal<'a>(
  graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  symbol: &'a Symbol,
  parts: &[String],
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  fn resolve_paths_with_parts<'a>(
    paths: Vec<DefinitionPath<'a>>,
    parts: &[String],
    graph: &'a ModuleGraph,
    visited_symbols: &mut HashSet<UniqueSymbolId>,
    specifier_to_module: &impl Fn(&url::Url) -> Option<ModuleInfoRef<'a>>,
  ) -> Vec<DefinitionPath<'a>> {
    debug_assert!(!parts.is_empty());
    paths
      .into_iter()
      .flat_map(|path| {
        resolve_path_with_parts(
          path,
          parts,
          graph,
          visited_symbols,
          specifier_to_module,
        )
      })
      .collect()
  }

  fn resolve_path_with_parts<'a>(
    path: DefinitionPath<'a>,
    parts: &[String],
    graph: &'a ModuleGraph,
    visited_symbols: &mut HashSet<UniqueSymbolId>,
    specifier_to_module: &impl Fn(&url::Url) -> Option<ModuleInfoRef<'a>>,
  ) -> Option<DefinitionPath<'a>> {
    match path {
      DefinitionPath::Path {
        module,
        symbol,
        symbol_decl,
        parts: _parts,
        next,
      } => {
        let next = resolve_paths_with_parts(
          next,
          parts,
          graph,
          visited_symbols,
          specifier_to_module,
        );
        if next.is_empty() {
          None
        } else {
          Some(DefinitionPath::Path {
            module,
            symbol,
            symbol_decl,
            parts: parts.to_vec(),
            next,
          })
        }
      }
      DefinitionPath::Definition(definition) => {
        let next_part = &parts[0];
        let mut next = Vec::new();
        match definition.kind {
          DefinitionKind::Definition => {
            if let Some(export_symbol_id) = definition.symbol.export(next_part)
            {
              next.extend(resolve_qualified_name_internal(
                graph,
                definition.module,
                definition.module.symbol(export_symbol_id).unwrap(),
                &parts[1..],
                visited_symbols,
                specifier_to_module,
              ));
            } else if next_part == "prototype"
              && definition.symbol_decl.is_class()
            {
              // for now, just resolve to this definition
              debug_assert!(next.is_empty());
              return Some(DefinitionPath::Definition(definition.clone()));
            } else {
              next.push(DefinitionPath::Unresolved(DefinitionUnresolved {
                module: definition.module,
                symbol: definition.symbol,
                kind: DefinitionUnresolvedKind::Part(next_part.to_string()),
                parts: parts.to_vec(),
              }))
            }
          }
          DefinitionKind::ExportStar(file_dep) => {
            let maybe_dep_specifier = graph.resolve_dependency(
              &file_dep.specifier,
              definition.module.specifier(),
              /* prefer types */ true,
            );
            let specifier_module =
              maybe_dep_specifier.and_then(|s| specifier_to_module(&s));
            if let Some(module) = specifier_module {
              next.extend(resolve_qualified_export_name_internal(
                graph,
                module,
                parts,
                visited_symbols,
                specifier_to_module,
              ));
            } else {
              next.push(DefinitionPath::Unresolved(DefinitionUnresolved {
                module: definition.module,
                symbol: definition.symbol,
                kind: DefinitionUnresolvedKind::Specifier(
                  file_dep.specifier.to_string(),
                ),
                parts: parts.to_vec(),
              }))
            }
          }
        }

        if next.is_empty() {
          None
        } else {
          // convert the definition into a path because the qualified name has yet to be resolved
          Some(DefinitionPath::Path {
            module: definition.module,
            symbol: definition.symbol,
            symbol_decl: definition.symbol_decl,
            parts: parts.to_vec(),
            next,
          })
        }
      }
      DefinitionPath::Unresolved(unresolved) => {
        Some(DefinitionPath::Unresolved(unresolved))
      }
    }
  }

  let paths = find_definition_paths_internal(
    graph,
    module,
    symbol,
    visited_symbols,
    specifier_to_module,
  );
  if !parts.is_empty() {
    resolve_paths_with_parts(
      paths,
      parts,
      graph,
      visited_symbols,
      specifier_to_module,
    )
  } else {
    paths
  }
}

#[derive(Debug, Default, Clone)]
pub struct ModuleExports<'a> {
  pub resolved: IndexMap<String, ResolvedExportOrReExportAllPath<'a>>,
  pub unresolved_specifiers: Vec<UnresolvedSpecifier<'a>>,
}

/// A resolved export. This lands at the first symbol it finds, which is not
/// necessarily the declaration symbol. For example, this might be the symbol
/// for an identifier in an export declaration (ex. `export { foo }`).
#[derive(Debug, Clone)]
pub struct ResolvedExport<'a> {
  pub module: ModuleInfoRef<'a>,
  pub symbol_id: SymbolId,
}

impl<'a> ResolvedExport<'a> {
  pub fn symbol(&self) -> &'a Symbol {
    self.module.symbol(self.symbol_id).unwrap()
  }
}

#[derive(Debug, Clone)]
pub struct ResolvedReExportAllPath<'a> {
  /// Module that contains this re-export.
  pub referrer_module: ModuleInfoRef<'a>,
  /// Specifier from the referrer that led to the resolved module.
  pub specifier: &'a str,
  /// Holds the next resolved export or re-export.
  pub next: Box<ResolvedExportOrReExportAllPath<'a>>,
}

impl<'a> ResolvedReExportAllPath<'a> {
  pub fn resolved_module(&self) -> ModuleInfoRef {
    match &*self.next {
      ResolvedExportOrReExportAllPath::Export(e) => e.module,
      ResolvedExportOrReExportAllPath::ReExportAllPath(e) => e.referrer_module,
    }
  }
}

#[derive(Debug, Clone)]
pub enum ResolvedExportOrReExportAllPath<'a> {
  Export(ResolvedExport<'a>),
  ReExportAllPath(ResolvedReExportAllPath<'a>),
}

impl<'a> ResolvedExportOrReExportAllPath<'a> {
  pub fn as_resolved_export(&self) -> &ResolvedExport<'a> {
    match self {
      ResolvedExportOrReExportAllPath::Export(export) => export,
      ResolvedExportOrReExportAllPath::ReExportAllPath(re_export) => {
        re_export.next.as_resolved_export()
      }
    }
  }

  pub fn iter(
    &self,
  ) -> impl Iterator<Item = &ResolvedExportOrReExportAllPath<'a>> {
    std::iter::successors(Some(self), |last| match last {
      ResolvedExportOrReExportAllPath::Export(_) => None,
      ResolvedExportOrReExportAllPath::ReExportAllPath(re_export) => {
        Some(&re_export.next)
      }
    })
  }
}

#[derive(Debug, Clone)]
pub struct UnresolvedSpecifier<'a> {
  pub referrer: ModuleInfoRef<'a>,
  pub specifier: &'a str,
}

pub fn exports_and_re_exports<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
) -> ModuleExports<'a> {
  exports_and_re_exports_inner(
    module_graph,
    module,
    specifier_to_module,
    &mut Default::default(),
  )
}

fn exports_and_re_exports_inner<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleInfoRef<'a>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleInfoRef<'a>>,
  visited: &mut HashSet<&'a ModuleSpecifier>,
) -> ModuleExports<'a> {
  if !visited.insert(module.specifier()) {
    return ModuleExports::default();
  }

  let mut unresolved_specifiers = Vec::new();
  let mut resolved = IndexMap::new();
  for (name, symbol_id) in module.module_symbol().exports() {
    resolved.insert(
      name.clone(),
      ResolvedExportOrReExportAllPath::Export(ResolvedExport {
        module,
        symbol_id: *symbol_id,
      }),
    );
  }
  if let Some(re_export_all_specifier) = module.re_export_all_specifiers() {
    let referrer_module = module;
    for re_export_specifier in re_export_all_specifier {
      let maybe_specifier = module_graph.resolve_dependency(
        re_export_specifier,
        module.specifier(),
        /* prefer_types */ true,
      );
      let maybe_module = maybe_specifier.and_then(|s| specifier_to_module(&s));
      if let Some(module) = maybe_module {
        let inner = exports_and_re_exports_inner(
          module_graph,
          module,
          specifier_to_module,
          visited,
        );
        for (name, item) in inner.resolved {
          if name != "default" && !resolved.contains_key(&name) {
            resolved.insert(
              name,
              ResolvedExportOrReExportAllPath::ReExportAllPath(
                ResolvedReExportAllPath {
                  referrer_module,
                  specifier: re_export_specifier,
                  next: Box::new(item),
                },
              ),
            );
          }
        }
        unresolved_specifiers.extend(inner.unresolved_specifiers);
      } else {
        unresolved_specifiers.push(UnresolvedSpecifier {
          referrer: module,
          specifier: re_export_specifier,
        });
      }
    }
  }
  ModuleExports {
    resolved,
    unresolved_specifiers,
  }
}
