// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::collections::HashSet;

use deno_ast::SourceRange;

use crate::ModuleGraph;
use crate::ModuleSpecifier;

use super::analyzer::SymbolDeclKind;
use super::FileDep;
use super::FileDepName;
use super::ModuleSymbol;
use super::Symbol;
use super::SymbolId;
use super::UniqueSymbolId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefinitionKind<'a> {
  ExportStar(&'a FileDep),
  Definition,
}

pub struct Definition<'a> {
  pub kind: DefinitionKind<'a>,
  pub module: &'a ModuleSymbol,
  pub symbol: &'a Symbol,
  pub range: &'a SourceRange,
}

pub fn go_to_definitions<'a>(
  module_graph: &ModuleGraph,
  module: &'a ModuleSymbol,
  symbol: &'a Symbol,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<&'a ModuleSymbol>,
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
  module: &'a ModuleSymbol,
  symbol: &'a Symbol,
  visited_symbols: &mut HashSet<UniqueSymbolId>,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<&'a ModuleSymbol>,
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
        // todo...
        eprintln!("TODO");
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
            &module.specifier(),
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

pub fn exports_and_re_exports<'a>(
  module_graph: &ModuleGraph,
  module: &'a ModuleSymbol,
  specifier_to_module: &impl Fn(&ModuleSpecifier) -> Option<&'a ModuleSymbol>,
) -> HashMap<String, (&'a ModuleSymbol, SymbolId)> {
  let mut result = HashMap::new();
  let module_id = module.module_id();
  for (name, symbol_id) in module.exports_map() {
    result.insert(name.clone(), (module, *symbol_id));
  }
  for re_export_specifier in module.re_exports() {
    let maybe_specifier = module_graph.resolve_dependency(
      re_export_specifier,
      &module.specifier(),
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
