// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashSet;
use std::collections::VecDeque;

use deno_ast::SourceRange;
use indexmap::IndexMap;

use crate::ModuleGraph;
use crate::ModuleSpecifier;

use super::analyzer::SymbolDeclKind;
use super::analyzer::SymbolDep;
use super::FileDep;
use super::FileDepName;
use super::ModuleSymbolRef;
use super::Symbol;
use super::SymbolDecl;
use super::SymbolId;
use super::UniqueSymbolId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefinitionKind<'a> {
  ExportStar(&'a FileDep),
  Definition,
}

#[derive(Debug, Clone)]
pub struct Definition<'a> {
  pub kind: DefinitionKind<'a>,
  pub module: ModuleSymbolRef<'a>,
  pub symbol: &'a Symbol,
  pub symbol_decl: &'a SymbolDecl,
}

impl<'a> Definition<'a> {
  pub fn unique_id(&self) -> UniqueSymbolId {
    self.symbol.unique_id()
  }

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
pub enum DefinitionUnresolvedKind<'a> {
  /// Could not resolve the swc Id.
  Id(&'a deno_ast::swc::ast::Id),
  /// Could not resolve the specifier relative this module via deno_graph.
  Specifier(&'a String),
  /// Could not resolve the part on the symbol.
  Part(&'a String),
}

/// The point at which a definition could not be resolved.
#[derive(Debug, Clone)]
pub struct DefinitionUnresolved<'a> {
  pub module: ModuleSymbolRef<'a>,
  pub symbol: &'a Symbol,
  pub kind: DefinitionUnresolvedKind<'a>,
  pub parts: &'a [String],
}

/// A graph path to a definition.
#[derive(Debug, Clone)]
pub enum DefinitionPath<'a> {
  Path {
    module: ModuleSymbolRef<'a>,
    symbol: &'a Symbol,
    symbol_decl: &'a SymbolDecl,
    parts: &'a [String],
    next: Vec<DefinitionPath<'a>>,
  },
  Definition(Definition<'a>),
  Unresolved(DefinitionUnresolved<'a>),
}

impl<'a> DefinitionPath<'a> {
  pub fn module(&self) -> ModuleSymbolRef<'a> {
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
    struct IntoDefinitionIterator<'a> {
      queue: VecDeque<DefinitionPath<'a>>,
    }

    impl<'a> Iterator for IntoDefinitionIterator<'a> {
      type Item = Definition<'a>;

      fn next(&mut self) -> Option<Self::Item> {
        while let Some(path) = self.queue.pop_front() {
          match path {
            DefinitionPath::Path { next, .. } => {
              for child_path in next.into_iter().rev() {
                self.queue.push_front(child_path);
              }
            }
            DefinitionPath::Definition(def) => {
              return Some(def);
            }
            DefinitionPath::Unresolved { .. } => {
              // no definition
            }
          }
        }

        None
      }
    }

    IntoDefinitionIterator {
      queue: VecDeque::from([self]),
    }
  }
}

/// Finds the path to a definition.
pub fn find_definition_paths<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
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
  module: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  if !visited_symbols.insert(symbol.unique_id()) {
    return Vec::new();
  }
  let mut paths = Vec::new(); //Vec::with_capacity(symbol.decls().len());
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
      SymbolDeclKind::DefinitionPrivateFnImpl(_) => {
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
              parts: &[],
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
            parts,
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
              parts: &[],
              next: inner_paths,
            });
          }
        }
      },
      SymbolDeclKind::TargetSelf => {
        // ignore
      }
    }
  }
  paths
}

fn go_to_file_export<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleSymbolRef<'a>,
  file_ref: &FileDep,
  export_name: &str,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
  visited_symbols: &mut HashSet<UniqueSymbolId>,
) -> Vec<DefinitionPath<'a>> {
  let maybe_dep = module_graph.resolve_dependency(
    &file_ref.specifier,
    module.specifier(),
    /* prefer types */ true,
  );
  let Some(dep) = maybe_dep else {
    return Vec::new();
  };
  let Some(dep_module) = specifier_to_module(&dep) else {
    return Vec::new();
  };
  let maybe_export_symbol = dep_module
    .module_symbol()
    .exports()
    .get(export_name)
    .and_then(|symbol_id| dep_module.symbol(*symbol_id));
  let Some(export_symbol) = maybe_export_symbol else {
    return Vec::new();
  };
  find_definition_paths_internal(
    module_graph,
    dep_module,
    export_symbol,
    visited_symbols,
    specifier_to_module,
  )
}

/// A resolved `SymbolDep`.
#[derive(Debug)]
pub enum ResolvedSymbolDepEntry<'a> {
  /// The path to the definition of the symbol dep.
  DefinitionPath(DefinitionPath<'a>),
  /// If the symbol dep was an import type with no property access.
  ///
  /// Ex. `type MyType = typeof import("./my_module.ts");`
  ImportType(ModuleSymbolRef<'a>),
}

pub fn resolve_symbol_dep<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  dep: &'a SymbolDep,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<ResolvedSymbolDepEntry<'a>> {
  match dep {
    SymbolDep::Id(id) => {
      if let Some(dep_symbol) = module.esm().and_then(|m| m.symbol_from_swc(id))
      {
        find_definition_paths(
          module_graph,
          module,
          dep_symbol,
          specifier_to_module,
        )
        .into_iter()
        .map(ResolvedSymbolDepEntry::DefinitionPath)
        .collect()
      } else {
        vec![]
      }
    }
    SymbolDep::QualifiedId(id, parts) => go_to_id_and_parts_definition_paths(
      module_graph,
      module,
      symbol,
      id,
      parts,
      specifier_to_module,
    )
    .into_iter()
    .map(ResolvedSymbolDepEntry::DefinitionPath)
    .collect(),
    SymbolDep::ImportType(import_specifier, parts) => {
      let maybe_dep_specifier = module_graph.resolve_dependency(
        import_specifier,
        module.specifier(),
        /* prefer types */ true,
      );
      let Some(module) =
        maybe_dep_specifier.as_ref().and_then(specifier_to_module)
      else {
        return Vec::new();
      };
      if parts.is_empty() {
        // an ImportType includes default exports
        vec![ResolvedSymbolDepEntry::ImportType(module)]
      } else {
        resolve_qualified_export_name(
          module_graph,
          module,
          &parts,
          specifier_to_module,
        )
        .into_iter()
        .map(ResolvedSymbolDepEntry::DefinitionPath)
        .collect()
      }
    }
  }
}

fn go_to_id_and_parts_definition_paths<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  target_id: &'a deno_ast::swc::ast::Id,
  parts: &'a [String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
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
      kind: DefinitionUnresolvedKind::Id(target_id),
      parts,
    })]
  }
}

fn resolve_qualified_export_name<'a>(
  graph: &'a ModuleGraph,
  module: ModuleSymbolRef<'a>,
  parts: &'a [String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
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
  module: ModuleSymbolRef<'a>,
  parts: &'a [String],
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  debug_assert!(!parts.is_empty());
  let exports = exports_and_re_exports(graph, module, specifier_to_module);
  let export_name = &parts[0];
  if let Some(resolved) = exports.resolved.get(export_name) {
    resolve_qualified_name_internal(
      graph,
      resolved.module,
      resolved.module.symbol(resolved.symbol_id).unwrap(),
      &parts[1..],
      visited_symbols,
      specifier_to_module,
    )
  } else {
    vec![DefinitionPath::Unresolved(DefinitionUnresolved {
      module: module,
      symbol: module.module_symbol(),
      kind: DefinitionUnresolvedKind::Part(export_name),
      parts,
    })]
  }
}

pub fn resolve_qualified_name<'a>(
  graph: &'a ModuleGraph,
  module: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  parts: &'a [String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
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
  module: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  parts: &'a [String],
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  fn resolve_paths_with_parts<'a>(
    paths: Vec<DefinitionPath<'a>>,
    parts: &'a [String],
    graph: &'a ModuleGraph,
    visited_symbols: &mut HashSet<UniqueSymbolId>,
    specifier_to_module: &impl Fn(&url::Url) -> Option<ModuleSymbolRef<'a>>,
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
    parts: &'a [String],
    graph: &'a ModuleGraph,
    visited_symbols: &mut HashSet<UniqueSymbolId>,
    specifier_to_module: &impl Fn(&url::Url) -> Option<ModuleSymbolRef<'a>>,
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
            parts,
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
            } else {
              next.push(DefinitionPath::Unresolved(DefinitionUnresolved {
                module: definition.module,
                symbol: definition.symbol,
                kind: DefinitionUnresolvedKind::Part(next_part),
                parts,
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
                kind: DefinitionUnresolvedKind::Specifier(&file_dep.specifier),
                parts,
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
            parts,
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

pub struct ExportsAndReExports<'a> {
  pub resolved: IndexMap<String, ResolvedExportOrReExport<'a>>,
  pub unresolved_specifiers: Vec<UnresolvedSpecifier<'a>>,
}

pub struct ResolvedExportOrReExport<'a> {
  pub module: ModuleSymbolRef<'a>,
  pub symbol_id: SymbolId,
}

impl<'a> ResolvedExportOrReExport<'a> {
  pub fn symbol(&self) -> &'a Symbol {
    self.module.symbol(self.symbol_id).unwrap()
  }
}

pub struct UnresolvedSpecifier<'a> {
  pub referrer: ModuleSymbolRef<'a>,
  pub specifier: &'a String,
}

pub fn exports_and_re_exports<'a>(
  module_graph: &'a ModuleGraph,
  module: ModuleSymbolRef<'a>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> ExportsAndReExports<'a> {
  let mut unresolved_specifiers = Vec::new();
  let mut resolved = IndexMap::new();
  for (name, symbol_id) in module.module_symbol().exports() {
    resolved.insert(
      name.clone(),
      ResolvedExportOrReExport {
        module,
        symbol_id: *symbol_id,
      },
    );
  }
  if let Some(re_export_all_specifier) = module.re_export_all_specifiers() {
    for re_export_specifier in re_export_all_specifier.iter() {
      let maybe_specifier = module_graph.resolve_dependency(
        re_export_specifier,
        module.specifier(),
        /* prefer_types */ true,
      );
      let maybe_module = maybe_specifier.and_then(|s| specifier_to_module(&s));
      if let Some(module) = maybe_module {
        let inner =
          exports_and_re_exports(module_graph, module, specifier_to_module);
        for (name, item) in inner.resolved {
          if name != "default" && !resolved.contains_key(&name) {
            resolved.insert(name, item);
          }
        }
        unresolved_specifiers.extend(inner.unresolved_specifiers);
      } else {
        unresolved_specifiers.push(UnresolvedSpecifier {
          referrer: module,
          specifier: re_export_specifier,
        })
      }
    }
  }
  ExportsAndReExports {
    resolved,
    unresolved_specifiers,
  }
}
