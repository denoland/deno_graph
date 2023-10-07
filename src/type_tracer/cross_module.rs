// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::Result;
use deno_ast::SourceRange;

use crate::ModuleGraph;
use crate::ModuleSpecifier;

use super::analyzer::SymbolDeclKind;
use super::FileDep;
use super::FileDepName;
use super::ModuleSymbolRef;
use super::Symbol;
use super::SymbolId;
use super::UniqueSymbolId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefinitionKind<'a> {
  ExportStar(&'a FileDep),
  Definition,
}

#[derive(Debug)]
pub struct Definition<'a> {
  pub kind: DefinitionKind<'a>,
  pub module: ModuleSymbolRef<'a>,
  pub symbol: &'a Symbol,
  pub range: &'a SourceRange,
}

impl<'a> Definition<'a> {
  pub fn byte_range(&self) -> std::ops::Range<usize> {
    self
      .range
      .as_byte_range(self.module.text_info().range().start)
  }
  pub fn text(&self) -> &str {
    self.module.text_info().range_text(self.range)
  }
}

pub fn go_to_definitions<'a>(
  module_graph: &ModuleGraph,
  module: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<Definition<'a>> {
  go_to_definitions_internal(
    module_graph,
    module,
    symbol,
    &mut Default::default(),
    specifier_to_module,
  )
}

fn go_to_definitions_internal<'a>(
  module_graph: &ModuleGraph,
  module: ModuleSymbolRef<'a>,
  symbol: &'a Symbol,
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Vec<Definition<'a>> {
  if !visited_symbols.insert(UniqueSymbolId {
    module_id: module.module_id(),
    symbol_id: symbol.symbol_id(),
  }) {
    return Vec::new();
  }
  let mut definitions = Vec::new();
  for decl in &symbol.decls {
    match &decl.kind {
      SymbolDeclKind::Definition => {
        definitions.push(Definition {
          module,
          symbol,
          kind: DefinitionKind::Definition,
          range: &decl.range,
        });
      }
      SymbolDeclKind::Target(target_id) => {
        if let Some(symbol) = module
          .esm()
          .unwrap()
          .symbol_id_from_swc(target_id)
          .and_then(|id| module.symbol(id))
        {
          definitions.extend(go_to_definitions_internal(
            module_graph,
            module,
            symbol,
            visited_symbols,
            specifier_to_module,
          ));
        }
      }
      SymbolDeclKind::QualifiedTarget(target_id, parts) => {
        if let Some(symbol_id) =
          module.esm().unwrap().symbol_id_from_swc(target_id)
        {
          if let Ok(results) = resolve_qualified_name(
            module_graph,
            module,
            symbol_id,
            parts,
            specifier_to_module,
          ) {
            for (specifier, symbol_id) in results {
              if let Some(module_symbol) = specifier_to_module(&specifier) {
                if let Some(symbol) = module_symbol.symbol(symbol_id) {
                  definitions.extend(go_to_definitions_internal(
                    module_graph,
                    module_symbol,
                    symbol,
                    visited_symbols,
                    specifier_to_module,
                  ));
                }
              }
            }
          }
        }
      }
      SymbolDeclKind::FileRef(file_ref) => match &file_ref.name {
        FileDepName::Star => {
          definitions.push(Definition {
            module,
            symbol,
            kind: DefinitionKind::ExportStar(file_ref),
            range: &decl.range,
          });
        }
        FileDepName::Name(export_name) => {
          if let Some(dep) = module_graph.resolve_dependency(
            &file_ref.specifier,
            module.specifier(),
            /* prefer types */ true,
          ) {
            if let Some(module_symbol) = specifier_to_module(&dep) {
              let maybe_symbol = module_symbol
                .exports_map()
                .get(export_name)
                .and_then(|symbol_id| module_symbol.symbol(*symbol_id));
              if let Some(export_symbol) = maybe_symbol {
                definitions.extend(go_to_definitions_internal(
                  module_graph,
                  module_symbol,
                  export_symbol,
                  visited_symbols,
                  specifier_to_module,
                ));
              }
            }
          }
        }
      },
      SymbolDeclKind::TargetSelf => {
        // ignore
      }
    }
  }
  definitions
}

pub fn resolve_qualified_export_name<'a>(
  graph: &ModuleGraph,
  module_symbol: ModuleSymbolRef<'a>,
  export_name: &str,
  parts: &[String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Result<Vec<(ModuleSpecifier, SymbolId)>> {
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
  parts: &[String],
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Result<Vec<(ModuleSpecifier, SymbolId)>> {
  let exports =
    exports_and_re_exports(graph, module_symbol, specifier_to_module);
  if let Some((module, symbol_id)) = exports.get(export_name) {
    resolve_qualified_name_internal(
      graph,
      *module,
      *symbol_id,
      parts,
      visited_symbols,
      specifier_to_module,
    )
  } else {
    Ok(Vec::new())
  }
}

pub fn resolve_qualified_name<'a>(
  graph: &ModuleGraph,
  module_symbol: ModuleSymbolRef<'a>,
  symbol_id: SymbolId,
  parts: &[String],
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Result<Vec<(ModuleSpecifier, SymbolId)>> {
  resolve_qualified_name_internal(
    graph,
    module_symbol,
    symbol_id,
    parts,
    &mut HashSet::new(),
    specifier_to_module,
  )
}

fn resolve_qualified_name_internal<'a>(
  graph: &ModuleGraph,
  module_symbol: ModuleSymbolRef<'a>,
  symbol_id: SymbolId,
  parts: &[String],
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> Result<Vec<(ModuleSpecifier, SymbolId)>> {
  if parts.is_empty() {
    return Ok(vec![(module_symbol.specifier().clone(), symbol_id)]);
  }

  let mut result = Vec::new();
  let next_part = &parts[0];
  match module_symbol.symbol(symbol_id) {
    Some(symbol) => {
      let definitions = go_to_definitions_internal(
        graph,
        module_symbol,
        symbol,
        visited_symbols,
        specifier_to_module,
      );
      for definition in definitions {
        match definition.kind {
          DefinitionKind::Definition => {
            if let Some(export_symbol_id) = definition.symbol.export(next_part)
            {
              result.extend(resolve_qualified_name_internal(
                graph,
                definition.module,
                export_symbol_id,
                &parts[1..],
                visited_symbols,
                specifier_to_module,
              )?);
            }
          }
          DefinitionKind::ExportStar(file_dep) => {
            let maybe_dep_specifier = graph.resolve_dependency(
              &file_dep.specifier,
              module_symbol.specifier(),
              /* prefer types */ true,
            );
            if let Some(specifier) = maybe_dep_specifier {
              if let Some(module_symbol) = specifier_to_module(&specifier) {
                result.extend(resolve_qualified_export_name_internal(
                  graph,
                  module_symbol,
                  next_part,
                  &parts[1..],
                  visited_symbols,
                  specifier_to_module,
                )?);
              }
            }
          }
        }
      }
      Ok(result)
    }
    None => Ok(Vec::new()),
  }
}

pub fn exports_and_re_exports<'a>(
  module_graph: &ModuleGraph,
  module: ModuleSymbolRef<'a>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<ModuleSymbolRef<'a>>,
) -> HashMap<String, (ModuleSymbolRef<'a>, SymbolId)> {
  let mut result = HashMap::new();
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
