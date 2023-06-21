// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::Result;
use deno_ast::swc::ast::*;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::utils::find_pat_ids;
use deno_ast::swc::visit::*;
use deno_ast::ModuleSpecifier;
use deno_ast::ParsedSource;
use deno_ast::SourcePos;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use indexmap::IndexMap;
use indexmap::IndexSet;

use crate::CapturingModuleParser;
use crate::ModuleGraph;
use crate::ModuleParser;

use super::TypeTraceDiagnostic;
use super::TypeTraceDiagnosticKind;
use super::TypeTraceHandler;

#[derive(Debug, Clone)]
pub struct RootsGraphSymbol(HashMap<ModuleSpecifier, ModuleSymbol>);

impl RootsGraphSymbol {
  pub fn get(&self, specifier: &ModuleSpecifier) -> Option<&ModuleSymbol> {
    self.0.get(specifier)
  }

  pub fn into_inner(self) -> HashMap<ModuleSpecifier, ModuleSymbol> {
    self.0
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FileDepName {
  Star,
  Name(String),
}

impl FileDepName {
  pub fn maybe_name(&self) -> Option<&str> {
    match self {
      FileDepName::Name(name) => Some(name.as_str()),
      FileDepName::Star => None,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileDep {
  pub name: FileDepName,
  pub specifier: String,
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleId(usize);

#[derive(Default, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct SymbolId(u32);

impl std::fmt::Debug for SymbolId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // for less verbose debugging
    write!(f, "{}", self.0)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolDeclKind {
  Target(Id),
  TargetSelf,
  FileRef(FileDep),
  Definition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolDecl {
  pub range: SourceRange,
  pub kind: SymbolDeclKind,
}

#[derive(Default, Debug, Clone)]
pub struct Symbol {
  is_public: bool,
  decls: IndexSet<SymbolDecl>,
  deps: IndexSet<Id>,
}

impl Symbol {
  pub fn is_public(&self) -> bool {
    self.is_public
  }

  pub fn mark_public(&mut self) -> bool {
    if self.is_public {
      false
    } else {
      self.is_public = true;
      true
    }
  }

  pub fn swc_dep_ids(&self) -> impl Iterator<Item = &Id> {
    self.deps.iter()
  }

  pub fn file_dep(&self) -> Option<&FileDep> {
    for dep in &self.decls {
      if let SymbolDeclKind::FileRef(file_ref) = &dep.kind {
        return Some(file_ref);
      }
    }
    None
  }
}

#[derive(Debug, Clone)]
pub struct UniqueSymbol {
  pub module_id: ModuleId,
  pub symbol_id: SymbolId,
}

#[derive(Debug, Clone)]
pub struct ModuleSymbol {
  module_id: ModuleId,
  next_symbol_id: SymbolId,
  exports: IndexMap<String, SymbolId>,
  re_exports: Vec<String>,
  default_export_symbol_id: Option<SymbolId>,
  // note: not all symbol ids have an swc id. For example, default exports
  swc_id_to_symbol_id: IndexMap<Id, SymbolId>,
  symbols: IndexMap<SymbolId, Symbol>,
  traced_re_exports: IndexMap<String, UniqueSymbol>,
}

impl ModuleSymbol {
  pub fn module_id(&self) -> ModuleId {
    self.module_id
  }

  pub fn public_source_ranges(&self) -> HashSet<SourceRange> {
    self
      .symbols
      .values()
      .filter(|symbol| symbol.is_public)
      .flat_map(|symbol| symbol.decls.iter().map(|d| d.range.clone()))
      .collect()
  }

  pub fn traced_re_exports(&self) -> &IndexMap<String, UniqueSymbol> {
    &self.traced_re_exports
  }

  pub(crate) fn add_traced_re_export(
    &mut self,
    name: String,
    symbol: UniqueSymbol,
  ) {
    self.traced_re_exports.insert(name, symbol);
  }

  pub fn exports(&self) -> &IndexMap<String, SymbolId> {
    &self.exports
  }

  pub fn re_exports(&self) -> &Vec<String> {
    &self.re_exports
  }

  pub fn default_export_symbol_id(&self) -> Option<SymbolId> {
    self.default_export_symbol_id
  }

  pub(crate) fn ensure_default_export_symbol(
    &mut self,
    symbol_decl: SymbolDecl,
  ) -> SymbolId {
    if let Some(symbol_id) = &self.default_export_symbol_id {
      let default_export_symbol = self.symbols.get_mut(symbol_id).unwrap();
      default_export_symbol.decls.insert(symbol_decl);
      *symbol_id
    } else {
      let symbol_id = self.get_next_symbol_id();
      let mut symbol = Symbol::default();
      symbol.decls.insert(symbol_decl);
      self.symbols.insert(symbol_id, symbol);
      self.default_export_symbol_id = Some(symbol_id);
      symbol_id
    }
  }

  pub fn symbol_id_from_swc(&self, id: &Id) -> Option<SymbolId> {
    self.swc_id_to_symbol_id.get(id).copied()
  }

  pub fn symbol(&self, id: SymbolId) -> Option<&Symbol> {
    self.symbols.get(&id)
  }

  pub(crate) fn symbol_mut(&mut self, id: SymbolId) -> Option<&mut Symbol> {
    self.symbols.get_mut(&id)
  }

  fn get_next_symbol_id(&mut self) -> SymbolId {
    let next_id = self.next_symbol_id;
    self.next_symbol_id = SymbolId(self.next_symbol_id.0 + 1);
    next_id
  }

  fn add_export(&mut self, id: Id, symbol_decl: SymbolDecl) -> SymbolId {
    let symbol_id = self.ensure_symbol_for_swc_id(id.clone(), symbol_decl);
    self.exports.insert(id.0.to_string(), symbol_id);
    symbol_id
  }

  fn get_symbol_from_swc_id(
    &mut self,
    id: Id,
    symbol_decl: SymbolDecl,
  ) -> &mut Symbol {
    let symbol_id = self.ensure_symbol_for_swc_id(id, symbol_decl);
    self.symbols.get_mut(&symbol_id).unwrap()
  }

  fn ensure_symbol_for_swc_id(
    &mut self,
    id: Id,
    symbol_decl: SymbolDecl,
  ) -> SymbolId {
    let symbol_id = match self.swc_id_to_symbol_id.get(&id) {
      Some(symbol_id) => *symbol_id,
      None => {
        let symbol_id = self.get_next_symbol_id();
        self.swc_id_to_symbol_id.insert(id, symbol_id);
        symbol_id
      }
    };

    if let Some(symbol) = self.symbols.get_mut(&symbol_id) {
      symbol.decls.insert(symbol_decl);
    } else {
      let mut symbol = Symbol::default();
      symbol.decls.insert(symbol_decl);
      self.symbols.insert(symbol_id, symbol);
    }
    symbol_id
  }
}

pub struct TypeTraceModuleAnalyzer<'a, THandler: TypeTraceHandler> {
  graph: &'a ModuleGraph,
  parser: &'a CapturingModuleParser<'a>,
  handler: &'a THandler,
  modules: HashMap<ModuleSpecifier, ModuleSymbol>,
}

impl<'a, THandler: TypeTraceHandler> TypeTraceModuleAnalyzer<'a, THandler> {
  pub fn new(
    graph: &'a ModuleGraph,
    parser: &'a CapturingModuleParser<'a>,
    handler: &'a THandler,
  ) -> Self {
    Self {
      handler,
      parser,
      graph,
      modules: Default::default(),
    }
  }

  pub fn into_roots_graph_symbol(self) -> RootsGraphSymbol {
    RootsGraphSymbol(self.modules)
  }

  pub fn get_or_analyze(
    &mut self,
    specifier: &ModuleSpecifier,
  ) -> Result<&ModuleSymbol> {
    if self.modules.contains_key(specifier) {
      return Ok(self.modules.get(specifier).unwrap());
    }

    let source = self.parsed_source(specifier)?;
    let module = source.module();
    let mut module_symbol = ModuleSymbol {
      module_id: ModuleId(self.modules.len()),
      next_symbol_id: Default::default(),
      exports: Default::default(),
      re_exports: Default::default(),
      traced_re_exports: Default::default(),
      default_export_symbol_id: None,
      swc_id_to_symbol_id: Default::default(),
      symbols: Default::default(),
    };

    let filler = SymbolFiller {
      source: &source,
      specifier,
      handler: self.handler,
    };
    filler.fill_module(&mut module_symbol, module);
    self.modules.insert(specifier.clone(), module_symbol);
    Ok(self.modules.get(specifier).unwrap())
  }

  pub fn get_mut(
    &mut self,
    specifier: &ModuleSpecifier,
  ) -> Option<&mut ModuleSymbol> {
    self.modules.get_mut(specifier)
  }

  fn parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<ParsedSource, deno_ast::Diagnostic> {
    let graph_module = self
      .graph
      .get(specifier)
      .unwrap_or_else(|| panic!("not foud: {}", specifier));
    let graph_module = graph_module.esm().unwrap();
    self.parser.parse_module(
      &graph_module.specifier,
      graph_module.source.clone(),
      graph_module.media_type,
    )
  }
}

struct SymbolFiller<'a, THandler: TypeTraceHandler> {
  handler: &'a THandler,
  specifier: &'a ModuleSpecifier,
  source: &'a ParsedSource,
}

impl<'a, THandler: TypeTraceHandler> SymbolFiller<'a, THandler> {
  fn fill_module(&self, file_module: &mut ModuleSymbol, module: &Module) {
    let mut last_was_overload = false;
    for module_item in &module.body {
      let is_overload = is_module_item_overload(module_item);
      let is_implementation_with_overloads = !is_overload && last_was_overload;
      last_was_overload = is_overload;

      if is_implementation_with_overloads {
        continue;
      }

      self.fill_module_item(file_module, module_item);

      // now fill the file exports
      match module_item {
        ModuleItem::ModuleDecl(decl) => match decl {
          ModuleDecl::Import(_) => {
            // ignore
          }
          ModuleDecl::ExportDecl(export_decl) => match &export_decl.decl {
            Decl::Class(n) => {
              file_module.add_export(
                n.ident.to_id(),
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: export_decl.range(),
                },
              );
            }
            Decl::Fn(n) => {
              file_module.add_export(
                n.ident.to_id(),
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: export_decl.range(),
                },
              );
            }
            Decl::Var(n) => {
              for decl in &n.decls {
                let ids: Vec<Id> = find_pat_ids(&decl.name);
                for id in ids {
                  file_module.add_export(
                    id,
                    SymbolDecl {
                      kind: SymbolDeclKind::Definition,
                      range: decl.range(),
                    },
                  );
                }
              }
            }
            Decl::TsInterface(n) => {
              file_module.add_export(
                n.id.to_id(),
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: export_decl.range(),
                },
              );
            }
            Decl::TsTypeAlias(n) => {
              file_module.add_export(
                n.id.to_id(),
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: export_decl.range(),
                },
              );
            }
            Decl::TsEnum(n) => {
              file_module.add_export(
                n.id.to_id(),
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: export_decl.range(),
                },
              );
            }
            Decl::TsModule(n) => match &n.id {
              TsModuleName::Ident(ident) => {
                file_module.add_export(
                  ident.to_id(),
                  SymbolDecl {
                    kind: SymbolDeclKind::Definition,
                    range: export_decl.range(),
                  },
                );
              }
              TsModuleName::Str(_) => todo!("module id str"),
            },
            Decl::Using(_) => {
              unreachable!()
            }
          },
          ModuleDecl::ExportNamed(_)
          | ModuleDecl::ExportDefaultDecl(_)
          | ModuleDecl::ExportDefaultExpr(_)
          | ModuleDecl::ExportAll(_)
          | ModuleDecl::TsImportEquals(_)
          | ModuleDecl::TsExportAssignment(_)
          | ModuleDecl::TsNamespaceExport(_) => {
            // ignore
          }
        },
        ModuleItem::Stmt(_) => {
          // ignore
        }
      }
    }
  }

  fn fill_module_item(
    &self,
    file_module: &mut ModuleSymbol,
    module_item: &ModuleItem,
  ) {
    match module_item {
      ModuleItem::ModuleDecl(decl) => match decl {
        ModuleDecl::Import(import_decl) => {
          for specifier in &import_decl.specifiers {
            match specifier {
              ImportSpecifier::Named(n) => {
                // Don't create a symbol to the exported name identifier
                // because swc doesn't give that identifier its own ctxt,
                // which means that `default` in cases like this will have
                // the same ctxt:
                //   import { default as a } from '...';
                //   import { default as b } from '...';
                let imported_name = n
                  .imported
                  .as_ref()
                  .map(|n| match n {
                    ModuleExportName::Ident(ident) => ident.sym.to_string(),
                    ModuleExportName::Str(_) => todo!(),
                  })
                  .unwrap_or_else(|| n.local.sym.to_string());
                file_module.get_symbol_from_swc_id(
                  n.local.to_id(),
                  SymbolDecl {
                    range: n.range(),
                    kind: SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Name(imported_name),
                      specifier: import_decl.src.value.to_string(),
                    }),
                  },
                );
              }
              ImportSpecifier::Default(n) => {
                file_module.get_symbol_from_swc_id(
                  n.local.to_id(),
                  SymbolDecl {
                    range: n.range(),
                    kind: SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Name("default".to_string()),
                      specifier: import_decl.src.value.to_string(),
                    }),
                  },
                );
              }
              ImportSpecifier::Namespace(n) => {
                file_module.get_symbol_from_swc_id(
                  n.local.to_id(),
                  SymbolDecl {
                    range: n.range(),
                    kind: SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Star,
                      specifier: import_decl.src.value.to_string(),
                    }),
                  },
                );
              }
            }
          }
        }
        ModuleDecl::ExportDecl(export_decl) => match &export_decl.decl {
          Decl::Class(n) => {
            let symbol = file_module.get_symbol_from_swc_id(
              n.ident.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition,
                range: export_decl.range(),
              },
            );
            self.fill_class_decl(symbol, n);
          }
          Decl::Fn(n) => {
            let symbol = file_module.get_symbol_from_swc_id(
              n.ident.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition,
                range: export_decl.range(),
              },
            );
            self.fill_fn_decl(symbol, n);
          }
          Decl::Var(n) => {
            for decl in &n.decls {
              let ids: Vec<Id> = find_pat_ids(&decl.name);
              for id in ids {
                let symbol = file_module.get_symbol_from_swc_id(
                  id,
                  SymbolDecl {
                    kind: SymbolDeclKind::Definition,
                    range: decl.range(),
                  },
                );
                self.fill_var_declarator(symbol, decl);
              }
            }
          }
          Decl::TsInterface(n) => {
            let symbol = file_module.get_symbol_from_swc_id(
              n.id.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition,
                range: export_decl.range(),
              },
            );
            self.fill_ts_interface(symbol, n);
          }
          Decl::TsTypeAlias(n) => {
            let symbol = file_module.get_symbol_from_swc_id(
              n.id.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition,
                range: export_decl.range(),
              },
            );
            self.fill_ts_type_alias(symbol, n);
          }
          Decl::TsEnum(n) => {
            let symbol = file_module.get_symbol_from_swc_id(
              n.id.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition,
                range: export_decl.range(),
              },
            );
            self.fill_ts_enum(symbol, n);
          }
          Decl::TsModule(n) => self.fill_ts_module(
            file_module,
            SymbolDecl {
              kind: SymbolDeclKind::Definition,
              range: export_decl.range(),
            },
            n,
          ),
          Decl::Using(_) => {
            unreachable!()
          }
        },
        ModuleDecl::ExportNamed(n) => {
          for specifier in &n.specifiers {
            match specifier {
              ExportSpecifier::Named(named) => {
                if let Some(exported_name) = &named.exported {
                  match exported_name {
                    ModuleExportName::Ident(export_ident) => {
                      match &named.orig {
                        ModuleExportName::Ident(orig_ident) => {
                          let orig_symbol = file_module.get_symbol_from_swc_id(
                            orig_ident.to_id(),
                            SymbolDecl {
                              range: n.range(),
                              kind: match &n.src {
                                Some(src) => SymbolDeclKind::FileRef(FileDep {
                                  name: FileDepName::Name(
                                    orig_ident.sym.to_string(),
                                  ),
                                  specifier: src.value.to_string(),
                                }),
                                None => SymbolDeclKind::TargetSelf,
                              },
                            },
                          );
                          orig_symbol.deps.insert(export_ident.to_id());

                          let export_id = file_module.add_export(
                            export_ident.to_id(),
                            SymbolDecl {
                              range: named.range(),
                              kind: SymbolDeclKind::Target(orig_ident.to_id()),
                            },
                          );
                          file_module
                            .symbol_mut(export_id)
                            .unwrap()
                            .deps
                            .insert(orig_ident.to_id());
                        }
                        ModuleExportName::Str(_) => todo!(),
                      }
                    }
                    ModuleExportName::Str(_) => todo!(),
                  }
                } else {
                  match &named.orig {
                    ModuleExportName::Ident(orig_ident) => {
                      file_module.get_symbol_from_swc_id(
                        orig_ident.to_id(),
                        SymbolDecl {
                          range: n.range(),
                          kind: match &n.src {
                            Some(src) => SymbolDeclKind::FileRef(FileDep {
                              name: FileDepName::Name(
                                orig_ident.sym.to_string(),
                              ),
                              specifier: src.value.to_string(),
                            }),
                            None => SymbolDeclKind::TargetSelf,
                          },
                        },
                      );
                      file_module.add_export(
                        orig_ident.to_id(),
                        SymbolDecl {
                          range: named.range(),
                          kind: SymbolDeclKind::TargetSelf,
                        },
                      );
                    }
                    ModuleExportName::Str(_) => todo!(),
                  }
                }
              }
              ExportSpecifier::Namespace(specifier) => {
                let name = match &specifier.name {
                  ModuleExportName::Ident(ident) => ident,
                  ModuleExportName::Str(_) => todo!(),
                };
                let symbol_kind = if let Some(src) = &n.src {
                  let file_dep = FileDep {
                    name: FileDepName::Star,
                    specifier: src.value.to_string(),
                  };
                  SymbolDeclKind::FileRef(file_dep)
                } else {
                  SymbolDeclKind::Target(name.to_id())
                };
                file_module.get_symbol_from_swc_id(
                  name.to_id(),
                  SymbolDecl {
                    kind: symbol_kind.clone(),
                    range: n.range(),
                  },
                );
                file_module.add_export(
                  name.to_id(),
                  SymbolDecl {
                    kind: symbol_kind,
                    range: specifier.range(),
                  },
                );
              }
              ExportSpecifier::Default(_) => todo!("export default specifier"),
            }
          }
        }
        ModuleDecl::ExportDefaultDecl(default_decl) => {
          let default_export_symbol_id = file_module
            .ensure_default_export_symbol(SymbolDecl {
              range: default_decl.range(),
              kind: SymbolDeclKind::Definition,
            });
          let maybe_ident = match &default_decl.decl {
            DefaultDecl::Class(expr) => expr.ident.as_ref(),
            DefaultDecl::Fn(expr) => expr.ident.as_ref(),
            DefaultDecl::TsInterfaceDecl(decl) => Some(&decl.id),
          };
          let symbol_id = if let Some(ident) = maybe_ident {
            let id = ident.to_id();
            let symbol_id = file_module.ensure_symbol_for_swc_id(
              id.clone(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition,
                range: ident.range(),
              },
            );
            file_module
              .symbol_mut(default_export_symbol_id)
              .unwrap()
              .deps
              .insert(id);
            symbol_id
          } else {
            default_export_symbol_id
          };

          let symbol = file_module.symbol_mut(symbol_id).unwrap();
          match &default_decl.decl {
            DefaultDecl::Class(n) => {
              self.fill_class(symbol, &n.class);
            }
            DefaultDecl::Fn(n) => self.fill_function_decl(symbol, &n.function),
            DefaultDecl::TsInterfaceDecl(n) => {
              self.fill_ts_interface(symbol, n)
            }
          }
        }
        ModuleDecl::ExportDefaultExpr(expr) => match &*expr.expr {
          Expr::Ident(ident) => {
            let default_export_symbol_id = file_module
              .ensure_default_export_symbol(SymbolDecl {
                kind: SymbolDeclKind::Target(ident.to_id()),
                range: expr.range(),
              });
            file_module.ensure_symbol_for_swc_id(
              ident.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Target(ident.to_id()),
                range: ident.range(),
              },
            );
            file_module
              .symbol_mut(default_export_symbol_id)
              .unwrap()
              .deps
              .insert(ident.to_id());
          }
          _ => {
            let line_and_column = self
              .source
              .text_info()
              .line_and_column_display(expr.start());
            self.handler.diagnostic(TypeTraceDiagnostic {
              kind: TypeTraceDiagnosticKind::UnsupportedDefaultExpr,
              specifier: self.specifier.clone(),
              line_and_column: Some(line_and_column),
            });
          }
        },
        ModuleDecl::ExportAll(n) => {
          file_module.re_exports.push(n.src.value.to_string());
        }
        ModuleDecl::TsImportEquals(_) => todo!("import equals"),
        ModuleDecl::TsExportAssignment(_) => todo!("export assignment"),
        ModuleDecl::TsNamespaceExport(_) => todo!("namespace export"),
      },
      ModuleItem::Stmt(stmt) => match stmt {
        Stmt::Block(_)
        | Stmt::Empty(_)
        | Stmt::Debugger(_)
        | Stmt::With(_)
        | Stmt::Return(_)
        | Stmt::Labeled(_)
        | Stmt::Break(_)
        | Stmt::Continue(_)
        | Stmt::If(_)
        | Stmt::Switch(_)
        | Stmt::Throw(_)
        | Stmt::Try(_)
        | Stmt::While(_)
        | Stmt::DoWhile(_)
        | Stmt::For(_)
        | Stmt::ForIn(_)
        | Stmt::ForOf(_)
        | Stmt::Expr(_) => {
          // ignore
        }
        Stmt::Decl(n) => {
          match n {
            Decl::Class(n) => {
              let id = n.ident.to_id();
              let symbol = file_module.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: n.range(),
                },
              );
              self.fill_class_decl(symbol, n);
            }
            Decl::Fn(n) => {
              let id = n.ident.to_id();
              let symbol = file_module.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: n.range(),
                },
              );
              self.fill_fn_decl(symbol, n);
            }
            Decl::Var(var_decl) => {
              for decl in &var_decl.decls {
                let ids: Vec<Id> = find_pat_ids(&decl.name);
                for id in ids {
                  let symbol = file_module.get_symbol_from_swc_id(
                    id,
                    SymbolDecl {
                      kind: SymbolDeclKind::Definition,
                      range: decl.range(),
                    },
                  );
                  self.fill_var_declarator(symbol, decl);
                }
              }
            }
            Decl::TsInterface(n) => {
              let id = n.id.to_id();
              let symbol = file_module.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: n.range(),
                },
              );
              self.fill_ts_interface(symbol, n);
            }
            Decl::TsTypeAlias(n) => {
              let id = n.id.to_id();
              let symbol = file_module.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: n.range(),
                },
              );
              self.fill_ts_type_alias(symbol, n);
            }
            Decl::TsEnum(n) => {
              let id = n.id.to_id();
              let symbol = file_module.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: n.range(),
                },
              );
              self.fill_ts_enum(symbol, n);
            }
            Decl::TsModule(n) => {
              self.fill_ts_module(
                file_module,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition,
                  range: n.range(),
                },
                n,
              );
            }
            Decl::Using(_) => {
              // ignore
            }
          };
        }
      },
    }
  }

  fn fill_class_decl(&self, symbol: &mut Symbol, n: &ClassDecl) {
    self.fill_class(symbol, &n.class);
  }

  fn fill_class(&self, symbol: &mut Symbol, n: &Class) {
    if let Some(type_params) = &n.type_params {
      self.fill_ts_type_param_decl(symbol, type_params);
    }
    if let Some(expr) = &n.super_class {
      self.fill_expr(symbol, expr);
    }
    if let Some(type_params) = &n.super_type_params {
      self.fill_ts_type_param_instantiation(symbol, type_params)
    }
    for expr in &n.implements {
      self.fill_ts_expr_with_type_args(symbol, expr);
    }
    self.fill_ts_class_members(symbol, &n.body);
  }

  fn fill_var_declarator(&self, symbol: &mut Symbol, n: &VarDeclarator) {
    self.fill_pat(symbol, &n.name);
  }

  fn fill_fn_decl(&self, symbol: &mut Symbol, n: &FnDecl) {
    self.fill_function_decl(symbol, &n.function);
  }

  fn fill_function_decl(&self, symbol: &mut Symbol, n: &Function) {
    if let Some(type_params) = &n.type_params {
      self.fill_ts_type_param_decl(symbol, type_params);
    }
    for param in &n.params {
      self.fill_param(symbol, param);
    }
    if let Some(return_type) = &n.return_type {
      self.fill_ts_type_ann(symbol, return_type);
    }
  }

  fn fill_ts_interface(&self, symbol: &mut Symbol, n: &TsInterfaceDecl) {
    let mut visitor = SymbolFillVisitor { symbol };
    n.visit_with(&mut visitor);
  }

  fn fill_ts_type_alias(&self, symbol: &mut Symbol, n: &TsTypeAliasDecl) {
    let mut visitor = SymbolFillVisitor { symbol };
    n.visit_with(&mut visitor);
  }

  fn fill_ts_enum(&self, symbol: &mut Symbol, n: &TsEnumDecl) {
    for member in &n.members {
      if let Some(init) = &member.init {
        self.fill_expr(symbol, init);
      }
    }
  }

  fn fill_ts_module(
    &self,
    file_module: &mut ModuleSymbol,
    symbol_decl: SymbolDecl,
    n: &TsModuleDecl,
  ) {
    let mut id = match &n.id {
      TsModuleName::Ident(ident) => ident.to_id(),
      TsModuleName::Str(_) => todo!(),
    };
    let mut symbol_id =
      file_module.ensure_symbol_for_swc_id(id.clone(), symbol_decl);

    // fill the exported declarations
    if let Some(body) = &n.body {
      let mut current = body;
      let block = loop {
        match current {
          TsNamespaceBody::TsModuleBlock(block) => break block,
          TsNamespaceBody::TsNamespaceDecl(decl) => {
            let previous_symbol_id = symbol_id;
            let previous_id = id;
            id = decl.id.to_id();
            symbol_id = file_module.ensure_symbol_for_swc_id(
              id.clone(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition,
                range: decl.range(),
              },
            );
            file_module
              .symbol_mut(previous_symbol_id)
              .unwrap()
              .deps
              .insert(id.clone());
            file_module
              .symbol_mut(symbol_id)
              .unwrap()
              .deps
              .insert(previous_id);
            current = &decl.body;
          }
        }
      };
      let mut last_was_overload = false;
      for item in &block.body {
        let is_overload = is_module_item_overload(item);
        let is_implementation_with_overloads =
          !is_overload && last_was_overload;
        last_was_overload = is_overload;

        if is_implementation_with_overloads {
          continue;
        }

        self.fill_module_item(file_module, item);
        let symbol = file_module.symbol_mut(symbol_id).unwrap();
        match item {
          ModuleItem::ModuleDecl(decl) => match decl {
            ModuleDecl::Import(_)
            | ModuleDecl::ExportNamed(_)
            | ModuleDecl::ExportDefaultDecl(_)
            | ModuleDecl::ExportDefaultExpr(_)
            | ModuleDecl::ExportAll(_)
            | ModuleDecl::TsImportEquals(_)
            | ModuleDecl::TsExportAssignment(_)
            | ModuleDecl::TsNamespaceExport(_) => {
              // ignore
            }
            ModuleDecl::ExportDecl(decl) => match &decl.decl {
              Decl::Class(n) => {
                symbol.deps.insert(n.ident.to_id());
              }
              Decl::Fn(n) => {
                symbol.deps.insert(n.ident.to_id());
              }
              Decl::Var(n) => {
                for decl in &n.decls {
                  symbol.deps.extend(find_pat_ids(&decl.name));
                }
              }
              Decl::TsInterface(n) => {
                symbol.deps.insert(n.id.to_id());
              }
              Decl::TsTypeAlias(n) => {
                symbol.deps.insert(n.id.to_id());
              }
              Decl::TsEnum(n) => {
                symbol.deps.insert(n.id.to_id());
              }
              Decl::TsModule(n) => match &n.id {
                TsModuleName::Ident(ident) => {
                  symbol.deps.insert(ident.to_id());
                }
                TsModuleName::Str(_) => todo!(),
              },
              Decl::Using(_) => {
                // ignore
              }
            },
          },
          ModuleItem::Stmt(_) => {
            // ignore
          }
        }
      }
    }
  }

  fn fill_ts_expr_with_type_args(
    &self,
    symbol: &mut Symbol,
    n: &TsExprWithTypeArgs,
  ) {
    if let Some(type_args) = &n.type_args {
      self.fill_ts_type_param_instantiation(symbol, type_args);
    }
    self.fill_expr(symbol, &n.expr);
  }

  fn fill_ts_type_param_decl(
    &self,
    symbol: &mut Symbol,
    type_params: &TsTypeParamDecl,
  ) {
    for param in &type_params.params {
      self.fill_ts_type_param(symbol, param);
    }
  }

  fn fill_ts_type_param(&self, symbol: &mut Symbol, param: &TsTypeParam) {
    if let Some(constraint) = &param.constraint {
      self.fill_ts_type(symbol, constraint);
    }
    if let Some(default) = &param.default {
      self.fill_ts_type(symbol, default);
    }
  }

  fn fill_ts_type_param_instantiation(
    &self,
    symbol: &mut Symbol,
    type_params: &TsTypeParamInstantiation,
  ) {
    for param in &type_params.params {
      self.fill_ts_type(symbol, param);
    }
  }

  fn fill_expr(&self, symbol: &mut Symbol, n: &Expr) {
    let mut visitor = SymbolFillVisitor { symbol };
    n.visit_with(&mut visitor);
  }

  fn fill_ts_class_members(
    &self,
    symbol: &mut Symbol,
    members: &[ClassMember],
  ) {
    let mut last_was_overload = false;
    for member in members {
      let is_overload = is_class_member_overload(member);
      let is_implementation_with_overloads = !is_overload && last_was_overload;
      last_was_overload = is_overload;

      if is_implementation_with_overloads
        || self.has_internal_jsdoc(member.start())
      {
        continue;
      }

      match member {
        ClassMember::Constructor(ctor) => self.fill_ctor(symbol, ctor),
        ClassMember::Method(method) => self.fill_method(symbol, method),
        ClassMember::PrivateMethod(method) => {
          self.fill_private_method(symbol, method)
        }
        ClassMember::ClassProp(prop) => self.fill_class_prop(symbol, prop),
        ClassMember::PrivateProp(prop) => self.fill_private_prop(symbol, prop),
        ClassMember::TsIndexSignature(signature) => {
          self.fill_ts_index_signature(symbol, signature)
        }
        ClassMember::AutoAccessor(prop) => {
          self.fill_auto_accessor(symbol, prop)
        }
        ClassMember::StaticBlock(_) | ClassMember::Empty(_) => {
          // ignore
        }
      }
    }
  }

  fn fill_ctor(&self, symbol: &mut Symbol, ctor: &Constructor) {
    for param in &ctor.params {
      match param {
        ParamOrTsParamProp::TsParamProp(param) => {
          self.fill_ts_param_prop(symbol, param)
        }
        ParamOrTsParamProp::Param(param) => self.fill_param(symbol, param),
      }
    }
  }

  fn fill_method(&self, symbol: &mut Symbol, method: &ClassMethod) {
    self.fill_prop_name(symbol, &method.key);
    if let Some(type_params) = &method.function.type_params {
      self.fill_ts_type_param_decl(symbol, type_params)
    }
    for param in &method.function.params {
      self.fill_param(symbol, param)
    }
    if let Some(return_type) = &method.function.return_type {
      self.fill_ts_type_ann(symbol, return_type)
    }
  }

  fn fill_prop_name(&self, symbol: &mut Symbol, key: &PropName) {
    match key {
      PropName::Computed(name) => {
        self.fill_expr(symbol, &name.expr);
      }
      PropName::Ident(_)
      | PropName::Str(_)
      | PropName::Num(_)
      | PropName::BigInt(_) => {
        // ignore
      }
    }
  }

  fn fill_ts_param_prop(&self, symbol: &mut Symbol, param: &TsParamProp) {
    match &param.param {
      TsParamPropParam::Ident(ident) => {
        if let Some(type_ann) = &ident.type_ann {
          self.fill_ts_type_ann(symbol, type_ann)
        }
      }
      TsParamPropParam::Assign(assign) => {
        if let Some(type_ann) = &assign.type_ann {
          self.fill_ts_type_ann(symbol, type_ann)
        }
      }
    }
  }

  fn fill_param(&self, symbol: &mut Symbol, param: &Param) {
    self.fill_pat(symbol, &param.pat);
  }

  fn fill_pat(&self, symbol: &mut Symbol, pat: &Pat) {
    match pat {
      Pat::Ident(n) => {
        if let Some(type_ann) = &n.type_ann {
          self.fill_ts_type_ann(symbol, type_ann);
        }
      }
      Pat::Array(n) => {
        if let Some(type_ann) = &n.type_ann {
          self.fill_ts_type_ann(symbol, type_ann);
        }
      }
      Pat::Rest(n) => {
        if let Some(type_ann) = &n.type_ann {
          self.fill_ts_type_ann(symbol, type_ann);
        }
      }
      Pat::Object(n) => {
        if let Some(type_ann) = &n.type_ann {
          self.fill_ts_type_ann(symbol, type_ann);
        }
      }
      Pat::Assign(n) => {
        self.fill_pat(symbol, &n.left);
        // this will always be none (https://github.com/swc-project/swc/issues/7487)
        if let Some(type_ann) = &n.type_ann {
          self.fill_ts_type_ann(symbol, type_ann);
        }
      }
      Pat::Invalid(_) => {
        // ignore
      }
      Pat::Expr(expr) => {
        self.fill_expr(symbol, expr);
      }
    }
  }

  fn fill_ts_type_ann(&self, symbol: &mut Symbol, type_ann: &TsTypeAnn) {
    self.fill_ts_type(symbol, &type_ann.type_ann)
  }

  fn fill_ts_type(&self, symbol: &mut Symbol, n: &TsType) {
    let mut visitor = SymbolFillVisitor { symbol };
    n.visit_with(&mut visitor);
  }

  fn fill_private_method(&self, _symbol: &mut Symbol, _method: &PrivateMethod) {
    // do nothing, private
  }

  fn fill_class_prop(&self, symbol: &mut Symbol, prop: &ClassProp) {
    if let Some(type_ann) = &prop.type_ann {
      self.fill_ts_type_ann(symbol, type_ann)
    }
  }

  fn fill_private_prop(&self, _symbol: &mut Symbol, _prop: &PrivateProp) {
    // do nothing, private properties are not emitted with their type
  }

  fn fill_ts_index_signature(
    &self,
    symbol: &mut Symbol,
    signature: &TsIndexSignature,
  ) {
    if let Some(type_ann) = &signature.type_ann {
      self.fill_ts_type_ann(symbol, type_ann)
    }
  }

  fn fill_auto_accessor(&self, symbol: &mut Symbol, prop: &AutoAccessor) {
    if let Some(type_ann) = &prop.type_ann {
      self.fill_ts_type_ann(symbol, type_ann)
    }
  }

  fn has_internal_jsdoc(&self, pos: SourcePos) -> bool {
    has_internal_jsdoc(self.source, pos)
  }
}

struct SymbolFillVisitor<'a> {
  symbol: &'a mut Symbol,
}

impl<'a> Visit for SymbolFillVisitor<'a> {
  fn visit_ident(&mut self, n: &Ident) {
    let id = n.to_id();
    self.symbol.deps.insert(id);
  }

  fn visit_ts_import_type(&mut self, _n: &TsImportType) {
    // probably need to have another map for these
    todo!("import type");
  }

  fn visit_ts_qualified_name(&mut self, _n: &TsQualifiedName) {
    // todo!("qualified name");
  }
}

fn has_internal_jsdoc(source: &ParsedSource, pos: SourcePos) -> bool {
  if let Some(comments) = source.comments().get_leading(pos) {
    comments.iter().any(|c| {
      c.kind == CommentKind::Block
        && c.text.starts_with('*')
        && c.text.contains("@internal")
    })
  } else {
    false
  }
}

fn is_class_member_overload(member: &ClassMember) -> bool {
  match member {
    ClassMember::Constructor(ctor) => ctor.body.is_none(),
    ClassMember::Method(method) => method.function.body.is_none(),
    ClassMember::PrivateMethod(method) => method.function.body.is_none(),
    ClassMember::ClassProp(_)
    | ClassMember::PrivateProp(_)
    | ClassMember::TsIndexSignature(_)
    | ClassMember::AutoAccessor(_)
    | ClassMember::StaticBlock(_)
    | ClassMember::Empty(_) => false,
  }
}

fn is_module_item_overload(module_item: &ModuleItem) -> bool {
  match module_item {
    ModuleItem::ModuleDecl(module_decl) => match module_decl {
      ModuleDecl::ExportDecl(decl) => is_decl_overload(&decl.decl),
      _ => false,
    },
    ModuleItem::Stmt(stmt) => match stmt {
      Stmt::Decl(decl) => is_decl_overload(decl),
      _ => false,
    },
  }
}

fn is_decl_overload(decl: &Decl) -> bool {
  match decl {
    Decl::Fn(func) => func.function.body.is_none(),
    _ => false,
  }
}
