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
}

impl<'a> DefinitionPath<'a> {
  pub fn module(&self) -> ModuleSymbolRef<'a> {
    match self {
      DefinitionPath::Path { module, .. } => *module,
      DefinitionPath::Definition(def) => def.module,
    }
  }

  pub fn symbol(&self) -> &'a Symbol {
    match self {
      DefinitionPath::Path { symbol, .. } => symbol,
      DefinitionPath::Definition(def) => def.symbol,
    }
  }

  pub fn symbol_decl(&self) -> &'a SymbolDecl {
    match self {
      DefinitionPath::Path { symbol_decl, .. } => symbol_decl,
      DefinitionPath::Definition(def) => def.symbol_decl,
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
  module_graph: &ModuleGraph,
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
  module_graph: &ModuleGraph,
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
  module_graph: &ModuleGraph,
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
  let Some(dep_module_symbol) = specifier_to_module(&dep) else {
    return Vec::new();
  };
  let maybe_export_symbol = dep_module_symbol
    .exports_map()
    .get(export_name)
    .and_then(|symbol_id| dep_module_symbol.symbol(*symbol_id));
  let Some(export_symbol) = maybe_export_symbol else {
    return Vec::new();
  };
  find_definition_paths_internal(
    module_graph,
    dep_module_symbol,
    export_symbol,
    visited_symbols,
    specifier_to_module,
  )
}

/// A resolved `SymbolDep`.
pub enum ResolvedSymbolDepEntry<'a> {
  /// The path to the definition of the symbol dep.
  DefinitionPath(DefinitionPath<'a>),
  /// If the symbol dep was an import type with no property access.
  ///
  /// Ex. `type MyType = typeof import("./my_module.ts");`
  ImportType(ModuleSymbolRef<'a>),
}

pub fn resolve_symbol_dep<'a>(
  module_graph: &ModuleGraph,
  module: ModuleSymbolRef<'a>,
  dep: &'a SymbolDep,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<ResolvedSymbolDepEntry<'a>> {
  match dep {
    SymbolDep::Id(id) => {
      if let Some(symbol) = module.esm().and_then(|m| m.symbol_from_swc(id)) {
        find_definition_paths(module_graph, module, symbol, specifier_to_module)
          .into_iter()
          .map(ResolvedSymbolDepEntry::DefinitionPath)
          .collect()
      } else {
        Vec::new()
      }
    }
    SymbolDep::QualifiedId(id, parts) => go_to_id_and_parts_definition_paths(
      module_graph,
      module,
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
      let Some(module_symbol) =
        maybe_dep_specifier.as_ref().and_then(specifier_to_module)
      else {
        return Vec::new();
      };
      if parts.is_empty() {
        // an ImportType includes default exports
        vec![ResolvedSymbolDepEntry::ImportType(module_symbol)]
      } else {
        resolve_qualified_export_name(
          module_graph,
          module_symbol,
          &parts[0],
          &parts[1..],
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
  module_graph: &ModuleGraph,
  module: ModuleSymbolRef<'a>,
  target_id: &'a deno_ast::swc::ast::Id,
  parts: &'a [String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
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
    Vec::new()
  }
}

fn resolve_qualified_export_name<'a>(
  graph: &ModuleGraph,
  module_symbol: ModuleSymbolRef<'a>,
  export_name: &str,
  parts: &'a [String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  resolve_qualified_export_name_internal(
    graph,
    module_symbol,
    export_name,
    parts,
    &mut HashSet::new(),
    specifier_to_module,
  )
}

fn resolve_qualified_export_name_internal<'a>(
  graph: &ModuleGraph,
  module_symbol: ModuleSymbolRef<'a>,
  export_name: &str,
  parts: &'a [String],
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  let exports =
    exports_and_re_exports(graph, module_symbol, specifier_to_module);
  if let Some((module, symbol_id)) = exports.get(export_name) {
    resolve_qualified_name_internal(
      graph,
      *module,
      module.symbol(*symbol_id).unwrap(),
      parts,
      visited_symbols,
      specifier_to_module,
    )
  } else {
    Vec::new()
  }
}

pub fn resolve_qualified_name<'a>(
  graph: &ModuleGraph,
  module_symbol: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  parts: &'a [String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  resolve_qualified_name_internal(
    graph,
    module_symbol,
    symbol,
    parts,
    &mut HashSet::new(),
    specifier_to_module,
  )
}

fn resolve_qualified_name_internal<'a>(
  graph: &ModuleGraph,
  module_symbol: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  parts: &'a [String],
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<DefinitionPath<'a>> {
  fn resolve_path_with_parts<'a>(
    path: &mut DefinitionPath<'a>,
    parts: &'a [String],
    graph: &ModuleGraph,
    visited_symbols: &mut HashSet<UniqueSymbolId>,
    specifier_to_module: &impl Fn(&url::Url) -> Option<ModuleSymbolRef<'a>>,
  ) {
    let next_part = &parts[0];
    match path {
      DefinitionPath::Path {
        parts: path_parts,
        next,
        ..
      } => {
        *path_parts = parts;
        for path in next {
          resolve_path_with_parts(
            path,
            parts,
            graph,
            visited_symbols,
            specifier_to_module,
          );
        }
      }
      DefinitionPath::Definition(definition) => {
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
            }
          }
          DefinitionKind::ExportStar(file_dep) => {
            let maybe_dep_specifier = graph.resolve_dependency(
              &file_dep.specifier,
              definition.module.specifier(),
              /* prefer types */ true,
            );
            if let Some(specifier) = maybe_dep_specifier {
              if let Some(module_symbol) = specifier_to_module(&specifier) {
                next.extend(resolve_qualified_export_name_internal(
                  graph,
                  module_symbol,
                  next_part,
                  &parts[1..],
                  visited_symbols,
                  specifier_to_module,
                ));
              }
            }
          }
        }

        // convert the definition into a path because the qualified name has yet to be resolved
        *path = DefinitionPath::Path {
          module: definition.module,
          symbol: definition.symbol,
          symbol_decl: definition.symbol_decl,
          parts,
          next,
        }
      }
    }
  }

  let mut paths = find_definition_paths_internal(
    graph,
    module_symbol,
    symbol,
    visited_symbols,
    specifier_to_module,
  );
  if !parts.is_empty() {
    for path in &mut paths {
      resolve_path_with_parts(
        path,
        parts,
        graph,
        visited_symbols,
        specifier_to_module,
      );
    }
  }

  paths
}

pub fn exports_and_re_exports<'a>(
  module_graph: &ModuleGraph,
  module: ModuleSymbolRef<'a>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> IndexMap<String, (ModuleSymbolRef<'a>, SymbolId)> {
  let mut result = IndexMap::new();
  for (name, symbol_id) in module.exports_map() {
    result.insert(name.clone(), (module, *symbol_id));
  }
  for re_export_specifier in module.re_export_all_specifiers().iter() {
    let maybe_specifier = module_graph.resolve_dependency(
      re_export_specifier,
      module.specifier(),
      /* prefer_types */ true,
    );
    if let Some(specifier) = maybe_specifier {
      if let Some(module_symbol) = specifier_to_module(&specifier) {
        let inner = exports_and_re_exports(
          module_graph,
          module_symbol,
          specifier_to_module,
        );
        for (name, unique_symbol) in inner {
          if name != "default" && !result.contains_key(&name) {
            result.insert(name, unique_symbol);
          }
        }
      }
    }
  }
  result
}
