// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::hash::Hasher;

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
use deno_ast::SourceTextInfo;
use indexmap::IndexMap;
use indexmap::IndexSet;

use crate::CapturingModuleParser;
use crate::EsmModule;
use crate::JsonModule;
use crate::ModuleGraph;
use crate::ModuleParser;

use super::collections::AdditiveOnlyMap;
use super::collections::LockableRefCell;
use super::cross_module;
use super::cross_module::Definition;
use super::ImportedExports;
use super::TypeTraceDiagnostic;
use super::TypeTraceDiagnosticKind;
use super::TypeTraceHandler;

#[derive(Clone)]
pub struct RootSymbol {
  specifiers_to_ids: HashMap<ModuleSpecifier, ModuleId>,
  ids_to_symbols: HashMap<ModuleId, ModuleSymbol>,
}

impl RootSymbol {
  pub fn get_module_from_specifier(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ModuleSymbolRef> {
    let id = self.specifiers_to_ids.get(specifier)?;
    self.ids_to_symbols.get(id).map(|m| m.as_ref())
  }

  pub fn get_module_from_id(
    &self,
    module_id: ModuleId,
  ) -> Option<ModuleSymbolRef> {
    self.ids_to_symbols.get(&module_id).map(|s| s.as_ref())
  }

  pub fn into_specifier_map(self) -> IndexMap<ModuleSpecifier, ModuleSymbol> {
    use std::collections::BTreeMap;

    let ids_to_symbols =
      self.ids_to_symbols.into_iter().collect::<BTreeMap<_, _>>();
    let mut result = IndexMap::default();
    for (id, symbol) in ids_to_symbols {
      // todo(dsherret): improve
      result.insert(
        self
          .specifiers_to_ids
          .iter()
          .find(|(_, v)| **v == id)
          .unwrap()
          .0
          .clone(),
        symbol,
      );
    }

    result
  }

  pub fn go_to_definitions<'a>(
    &'a self,
    module_graph: &ModuleGraph,
    module: ModuleSymbolRef<'a>,
    symbol: &'a Symbol,
  ) -> Vec<Definition<'a>> {
    super::cross_module::go_to_definitions(
      module_graph,
      module,
      symbol,
      &|specifier| self.get_module_from_specifier(specifier),
    )
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct ModuleId(u32);

impl std::fmt::Display for ModuleId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[derive(Default, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct SymbolId(u32);

impl std::fmt::Debug for SymbolId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // for less verbose debugging
    write!(f, "{}", self.0)
  }
}

#[derive(Clone)]
pub struct NodeRefBox<T> {
  // the parsed source needs to be kept alive for the duration of the value
  source: ParsedSource,
  value: *const T,
}

impl<T> std::fmt::Debug for NodeRefBox<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("DeclRef")
      .field("value", &"<omitted>")
      .finish()
  }
}

impl<T> NodeRefBox<T> {
  /// WARNING: Ensure that T is a reference inside ParsedSource. Otherwise
  /// this is entirely unsafe.
  fn unsafe_new(parsed_source: &ParsedSource, value: &T) -> Self {
    Self {
      source: parsed_source.clone(),
      value: value as *const _,
    }
  }

  fn value(&self) -> &T {
    // SAFETY: This is safe because the parsed source is kept alive for the
    // duration of this struct and the reference is within the parsed source.
    unsafe { &*self.value }
  }

  fn source(&self) -> &ParsedSource {
    &self.source
  }
}

#[derive(Clone)]
pub struct SymbolNode(SymbolNodeInner);

impl std::fmt::Debug for SymbolNode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_tuple("SymbolNode").field(&"<omitted>").finish()
  }
}

#[derive(Debug, Clone)]
enum SymbolNodeInnerExportDecl {
  Class(NodeRefBox<ClassDecl>),
  Fn(NodeRefBox<FnDecl>),
  Var(NodeRefBox<VarDecl>, NodeRefBox<VarDeclarator>, Id),
  TsEnum(NodeRefBox<TsEnumDecl>),
  TsInterface(NodeRefBox<TsInterfaceDecl>),
  TsModule(NodeRefBox<TsModuleDecl>),
  TsTypeAlias(NodeRefBox<TsTypeAliasDecl>),
}

#[derive(Debug, Clone)]
enum SymbolNodeInner {
  Json,
  ClassDecl(NodeRefBox<ClassDecl>),
  ExportDecl(NodeRefBox<ExportDecl>, SymbolNodeInnerExportDecl),
  ExportDefaultDecl(NodeRefBox<ExportDefaultDecl>),
  ExportDefaultExprLit(NodeRefBox<ExportDefaultExpr>, NodeRefBox<Lit>),
  FnDecl(NodeRefBox<FnDecl>),
  TsEnum(NodeRefBox<TsEnumDecl>),
  TsNamespace(NodeRefBox<TsModuleDecl>),
  TsTypeAlias(NodeRefBox<TsTypeAliasDecl>),
  TsInterface(NodeRefBox<TsInterfaceDecl>),
  Var(NodeRefBox<VarDecl>, NodeRefBox<VarDeclarator>),
}

#[derive(Debug, Clone, Copy)]
pub enum ExportDeclRef<'a> {
  Class(&'a ClassDecl),
  Fn(&'a FnDecl),
  Var(&'a VarDecl, &'a VarDeclarator, &'a Id),
  TsEnum(&'a TsEnumDecl),
  TsInterface(&'a TsInterfaceDecl),
  TsModule(&'a TsModuleDecl),
  TsTypeAlias(&'a TsTypeAliasDecl),
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolNodeRef<'a> {
  ClassDecl(&'a ClassDecl),
  ExportDecl(&'a ExportDecl, ExportDeclRef<'a>),
  ExportDefaultDecl(&'a ExportDefaultDecl),
  ExportDefaultExprLit(&'a ExportDefaultExpr, &'a Lit),
  FnDecl(&'a FnDecl),
  TsEnum(&'a TsEnumDecl),
  TsInterface(&'a TsInterfaceDecl),
  TsNamespace(&'a TsModuleDecl),
  TsTypeAlias(&'a TsTypeAliasDecl),
  Var(&'a VarDecl, &'a VarDeclarator),
}

#[derive(Debug, Clone)]
pub enum SymbolDeclKind {
  Target(Id),
  QualifiedTarget(Id, Vec<String>),
  TargetSelf,
  FileRef(FileDep),
  Definition(SymbolNode),
}

impl SymbolDeclKind {
  pub fn maybe_node_and_source(
    &self,
  ) -> Option<(SymbolNodeRef, &ParsedSource)> {
    match self {
      SymbolDeclKind::Definition(SymbolNode(def)) => match def {
        SymbolNodeInner::Json => None,
        SymbolNodeInner::ClassDecl(n) => {
          Some((SymbolNodeRef::ClassDecl(n.value()), n.source()))
        }
        SymbolNodeInner::ExportDecl(export_decl, inner) => Some((
          SymbolNodeRef::ExportDecl(
            export_decl.value(),
            match inner {
              SymbolNodeInnerExportDecl::Class(n) => {
                ExportDeclRef::Class(n.value())
              }
              SymbolNodeInnerExportDecl::Fn(n) => ExportDeclRef::Fn(n.value()),
              SymbolNodeInnerExportDecl::Var(decl, declarator, id) => {
                ExportDeclRef::Var(decl.value(), declarator.value(), id)
              }
              SymbolNodeInnerExportDecl::TsEnum(n) => {
                ExportDeclRef::TsEnum(n.value())
              }
              SymbolNodeInnerExportDecl::TsInterface(n) => {
                ExportDeclRef::TsInterface(n.value())
              }
              SymbolNodeInnerExportDecl::TsModule(n) => {
                ExportDeclRef::TsModule(n.value())
              }
              SymbolNodeInnerExportDecl::TsTypeAlias(n) => {
                ExportDeclRef::TsTypeAlias(n.value())
              }
            },
          ),
          export_decl.source(),
        )),
        SymbolNodeInner::ExportDefaultDecl(n) => {
          Some((SymbolNodeRef::ExportDefaultDecl(n.value()), n.source()))
        }
        SymbolNodeInner::ExportDefaultExprLit(n, lit) => Some((
          SymbolNodeRef::ExportDefaultExprLit(n.value(), lit.value()),
          n.source(),
        )),
        SymbolNodeInner::FnDecl(n) => {
          Some((SymbolNodeRef::FnDecl(n.value()), n.source()))
        }
        SymbolNodeInner::TsEnum(n) => {
          Some((SymbolNodeRef::TsEnum(n.value()), n.source()))
        }
        SymbolNodeInner::TsNamespace(n) => {
          Some((SymbolNodeRef::TsNamespace(n.value()), n.source()))
        }
        SymbolNodeInner::TsTypeAlias(n) => {
          Some((SymbolNodeRef::TsTypeAlias(n.value()), n.source()))
        }
        SymbolNodeInner::TsInterface(n) => {
          Some((SymbolNodeRef::TsInterface(n.value()), n.source()))
        }
        SymbolNodeInner::Var(decl, declarator) => Some((
          SymbolNodeRef::Var(decl.value(), declarator.value()),
          decl.source(),
        )),
      },
      _ => None,
    }
  }
}

#[derive(Debug, Clone)]
pub struct SymbolDecl {
  pub range: SourceRange,
  pub kind: SymbolDeclKind,
}

impl SymbolDecl {
  pub fn maybe_source(&self) -> Option<&ParsedSource> {
    self.maybe_node_and_source().map(|n| n.1)
  }

  pub fn maybe_node(&self) -> Option<SymbolNodeRef> {
    self.maybe_node_and_source().map(|n| n.0)
  }

  pub fn maybe_node_and_source(
    &self,
  ) -> Option<(SymbolNodeRef, &ParsedSource)> {
    self.kind.maybe_node_and_source()
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolDep {
  Id(Id),
  QualifiedId(Id, Vec<String>),
  ImportType(String, Vec<String>),
}

impl From<Id> for SymbolDep {
  fn from(value: Id) -> Self {
    Self::Id(value)
  }
}

#[derive(Clone)]
pub struct Symbol {
  symbol_id: SymbolId,
  is_public: RefCell<bool>,
  // todo(dsherret): the IndexMap is just to prevent duplicates
  // a better solution should be thought out
  decls: IndexMap<SourceRange, SymbolDecl>,
  deps: IndexSet<SymbolDep>,
  /// The child declarations of module declarations.
  child_decls: IndexSet<SymbolId>,
  exports: IndexMap<String, SymbolId>,
}

impl std::fmt::Debug for Symbol {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Symbol")
      .field("symbol_id", &self.symbol_id)
      .field("is_public", &*self.is_public.borrow())
      .field("decls", &self.decls.values().collect::<Vec<_>>())
      .field("deps", &self.deps)
      .field("child_decls", &self.child_decls)
      .field("exports", &self.exports)
      .finish()
  }
}

impl Symbol {
  pub fn new(symbol_id: SymbolId) -> Self {
    Symbol {
      symbol_id,
      is_public: Default::default(),
      decls: Default::default(),
      deps: Default::default(),
      child_decls: Default::default(),
      exports: Default::default(),
    }
  }

  pub fn symbol_id(&self) -> SymbolId {
    self.symbol_id
  }

  pub fn is_public(&self) -> bool {
    *self.is_public.borrow()
  }

  pub(crate) fn mark_public(&self) -> bool {
    if self.is_public() {
      false
    } else {
      *self.is_public.borrow_mut() = true;
      true
    }
  }

  pub fn export(&self, name: &str) -> Option<SymbolId> {
    self.exports.get(name).copied()
  }

  pub fn exports(&self) -> impl Iterator<Item = SymbolId> + '_ {
    self.exports.values().copied()
  }

  pub fn child_decls(&self) -> impl Iterator<Item = SymbolId> + '_ {
    self.child_decls.iter().copied()
  }

  pub fn deps(&self) -> impl Iterator<Item = &SymbolDep> {
    self.deps.iter()
  }

  pub fn decls(&self) -> impl Iterator<Item = &SymbolDecl> {
    self.decls.values()
  }

  pub fn file_dep(&self) -> Option<&FileDep> {
    for dep in self.decls() {
      if let SymbolDeclKind::FileRef(file_ref) = &dep.kind {
        return Some(file_ref);
      }
    }
    None
  }

  pub fn ranges(&self) -> impl Iterator<Item = SourceRange> + '_ {
    self.decls().map(|d| d.range)
  }

  fn insert_decl(&mut self, symbol_decl: SymbolDecl) {
    self.decls.insert(symbol_decl.range, symbol_decl);
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct UniqueSymbolId {
  pub module_id: ModuleId,
  pub symbol_id: SymbolId,
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleSymbolRef<'a> {
  Json(&'a JsonModuleSymbol),
  Esm(&'a EsmModuleSymbol),
}

impl<'a> ModuleSymbolRef<'a> {
  pub fn json(&self) -> Option<&'a JsonModuleSymbol> {
    match self {
      Self::Json(json) => Some(json),
      Self::Esm(_) => None,
    }
  }

  pub fn esm(&self) -> Option<&'a EsmModuleSymbol> {
    match self {
      Self::Json(_) => None,
      Self::Esm(esm) => Some(esm),
    }
  }

  pub fn module_id(&self) -> ModuleId {
    match self {
      Self::Json(m) => m.module_id,
      Self::Esm(m) => m.module_id,
    }
  }

  pub fn text_info(&self) -> &'a SourceTextInfo {
    match self {
      Self::Json(m) => &m.source_text_info,
      Self::Esm(m) => m.source.text_info(),
    }
  }

  pub fn text(&self) -> &'a str {
    self.text_info().text_str()
  }

  pub fn specifier(&self) -> &'a ModuleSpecifier {
    match self {
      Self::Json(m) => &m.specifier,
      Self::Esm(m) => &m.specifier,
    }
  }

  pub fn symbols(&self) -> Box<dyn Iterator<Item = &'a Symbol> + 'a> {
    match self {
      Self::Json(m) => Box::new(m.symbols()),
      Self::Esm(m) => Box::new(m.symbols()),
    }
  }

  pub fn symbol(&self, id: SymbolId) -> Option<&'a Symbol> {
    match self {
      Self::Json(m) => m.symbol(id),
      Self::Esm(m) => m.symbol(id),
    }
  }

  pub fn exports(
    &self,
    module_graph: &ModuleGraph,
    root_symbol: &'a RootSymbol,
  ) -> IndexMap<String, (ModuleSymbolRef<'a>, SymbolId)> {
    cross_module::exports_and_re_exports(module_graph, *self, &|specifier| {
      root_symbol.get_module_from_specifier(specifier)
    })
  }

  pub(crate) fn exports_map(&self) -> &'a IndexMap<String, SymbolId> {
    match self {
      Self::Json(m) => &m.exports,
      Self::Esm(m) => &m.exports,
    }
  }

  pub(crate) fn re_export_all_specifiers(&self) -> Cow<'a, Vec<String>> {
    match self {
      Self::Json(_) => Cow::Owned(Vec::new()),
      Self::Esm(m) => Cow::Borrowed(&m.re_exports),
    }
  }
}

#[derive(Debug, Clone)]
pub enum ModuleSymbol {
  Json(Box<JsonModuleSymbol>),
  Esm(EsmModuleSymbol),
}

impl ModuleSymbol {
  pub fn json(&self) -> Option<&JsonModuleSymbol> {
    match self {
      Self::Json(json) => Some(json),
      Self::Esm(_) => None,
    }
  }

  pub fn esm(&self) -> Option<&EsmModuleSymbol> {
    match self {
      Self::Json(_) => None,
      Self::Esm(esm) => Some(esm),
    }
  }

  pub fn as_ref(&self) -> ModuleSymbolRef {
    match self {
      ModuleSymbol::Json(m) => (**m).as_ref(),
      ModuleSymbol::Esm(m) => m.as_ref(),
    }
  }

  pub fn module_id(&self) -> ModuleId {
    match self {
      Self::Json(m) => m.module_id,
      Self::Esm(m) => m.module_id,
    }
  }

  pub fn specifier(&self) -> &ModuleSpecifier {
    self.as_ref().specifier()
  }

  pub fn symbols(&self) -> Box<dyn Iterator<Item = &Symbol> + '_> {
    self.as_ref().symbols()
  }

  pub fn symbol(&self, id: SymbolId) -> Option<&Symbol> {
    self.as_ref().symbol(id)
  }
}

#[derive(Clone)]
pub struct JsonModuleSymbol {
  module_id: ModuleId,
  specifier: ModuleSpecifier,
  source_text_info: SourceTextInfo,
  exports: IndexMap<String, SymbolId>,
  default_symbol: Symbol,
}

impl std::fmt::Debug for JsonModuleSymbol {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("JsonModuleSymbol")
      .field("module_id", &self.module_id)
      .field("specifier", &self.specifier.as_str())
      .field("exports", &self.exports)
      .field("default_symbol", &self.default_symbol)
      .finish()
  }
}

impl JsonModuleSymbol {
  pub fn as_ref(&self) -> ModuleSymbolRef {
    ModuleSymbolRef::Json(self)
  }

  pub fn specifier(&self) -> &ModuleSpecifier {
    &self.specifier
  }

  pub fn text_info(&self) -> &SourceTextInfo {
    &self.source_text_info
  }

  pub fn symbols(&self) -> impl Iterator<Item = &Symbol> {
    std::iter::once(&self.default_symbol)
  }

  pub fn symbol(&self, id: SymbolId) -> Option<&Symbol> {
    if id.0 == 0 {
      Some(&self.default_symbol)
    } else {
      None
    }
  }
}

#[derive(Clone)]
pub struct EsmModuleSymbol {
  module_id: ModuleId,
  specifier: ModuleSpecifier,
  source: ParsedSource,
  next_symbol_id: SymbolId,
  exports: IndexMap<String, SymbolId>,
  child_decls: IndexSet<SymbolId>,
  /// The re-export specifiers.
  re_exports: Vec<String>,
  // note: not all symbol ids have an swc id. For example, default exports
  swc_id_to_symbol_id: IndexMap<Id, SymbolId>,
  symbols: IndexMap<SymbolId, Symbol>,
  traced_re_exports: LockableRefCell<IndexMap<String, UniqueSymbolId>>,
  traced_referrers: LockableRefCell<IndexMap<ModuleId, ImportedExports>>,
}

impl std::fmt::Debug for EsmModuleSymbol {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("EsmModuleSymbol")
      .field("module_id", &self.module_id)
      .field("specifier", &self.specifier.as_str())
      .field("child_decls", &self.child_decls)
      .field("exports", &self.exports)
      .field("re_exports", &self.re_exports)
      .field("swc_id_to_symbol_id", &self.swc_id_to_symbol_id)
      .field("symbols", &self.symbols)
      .field("traced_re_exports", &self.traced_re_exports.borrow())
      .field("traced_referrers", &self.traced_referrers.borrow())
      .finish()
  }
}

impl EsmModuleSymbol {
  pub fn as_ref(&self) -> ModuleSymbolRef {
    ModuleSymbolRef::Esm(self)
  }

  pub fn module_id(&self) -> ModuleId {
    self.module_id
  }

  pub fn specifier(&self) -> &ModuleSpecifier {
    &self.specifier
  }

  pub fn source(&self) -> &ParsedSource {
    &self.source
  }

  pub fn public_source_ranges(&self) -> HashSet<SourceRange> {
    self
      .symbols
      .values()
      .filter(|symbol| symbol.is_public())
      .flat_map(|symbol| symbol.decls().map(|d| d.range))
      .collect()
  }

  /// Re-exports from this module that were found during tracing.
  pub fn traced_re_exports(&self) -> &IndexMap<String, UniqueSymbolId> {
    self.traced_re_exports.lock_and_get_ref()
  }

  pub(crate) fn add_traced_re_export(
    &self,
    name: String,
    symbol: UniqueSymbolId,
  ) {
    self.traced_re_exports.borrow_mut().insert(name, symbol);
  }

  /// Referrers and their imported exports. This only includes referrers that
  /// were found during tracing and not all referrers.
  pub fn traced_referrers(&self) -> &IndexMap<ModuleId, ImportedExports> {
    self.traced_referrers.lock_and_get_ref()
  }

  pub(crate) fn add_traced_referrer(
    &self,
    module_id: ModuleId,
    imported_exports: ImportedExports,
  ) {
    let mut traced_referrers = self.traced_referrers.borrow_mut();
    if let Some(current_imported_exports) = traced_referrers.get_mut(&module_id)
    {
      current_imported_exports.add(imported_exports);
    } else {
      traced_referrers.insert(module_id, imported_exports);
    }
  }

  pub fn exports<'a>(
    &'a self,
    module_graph: &ModuleGraph,
    root_symbol: &'a RootSymbol,
  ) -> IndexMap<String, (ModuleSymbolRef<'a>, SymbolId)> {
    self.as_ref().exports(module_graph, root_symbol)
  }

  pub fn child_decls(&self) -> impl Iterator<Item = SymbolId> + '_ {
    self.child_decls.iter().copied()
  }

  pub(crate) fn ensure_default_export_symbol(
    &mut self,
    symbol_decl: SymbolDecl,
  ) -> SymbolId {
    if let Some(symbol_id) = self.exports.get("default") {
      let default_export_symbol = self.symbols.get_mut(symbol_id).unwrap();
      default_export_symbol.insert_decl(symbol_decl);
      *symbol_id
    } else {
      let symbol_id = self.get_next_symbol_id();
      let mut symbol = Symbol::new(symbol_id);
      symbol.insert_decl(symbol_decl);
      self.symbols.insert(symbol_id, symbol);
      self.exports.insert("default".to_string(), symbol_id);
      symbol_id
    }
  }

  pub(crate) fn create_new_symbol(&mut self) -> &mut Symbol {
    let symbol_id = self.get_next_symbol_id();
    self.symbols.insert(symbol_id, Symbol::new(symbol_id));
    self.symbols.get_mut(&symbol_id).unwrap()
  }

  pub fn symbol_id_from_swc(&self, id: &Id) -> Option<SymbolId> {
    self.swc_id_to_symbol_id.get(id).copied()
  }

  pub fn symbol_from_swc(&self, id: &Id) -> Option<&Symbol> {
    let id = self.symbol_id_from_swc(id)?;
    self.symbol(id)
  }

  pub fn symbols(&self) -> impl Iterator<Item = &Symbol> {
    self.symbols.values()
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
      symbol.insert_decl(symbol_decl);
    } else {
      let mut symbol = Symbol::new(symbol_id);
      symbol.insert_decl(symbol_decl);
      self.symbols.insert(symbol_id, symbol);
    }
    symbol_id
  }
}

pub struct TypeTraceModuleAnalyzer<'a, THandler: TypeTraceHandler> {
  graph: &'a ModuleGraph,
  parser: &'a CapturingModuleParser<'a>,
  handler: &'a THandler,
  modules: AdditiveOnlyMap<ModuleSpecifier, ModuleSymbol>,
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
      modules: AdditiveOnlyMap::new(),
    }
  }

  pub fn into_roots_graph_symbol(self) -> RootSymbol {
    let modules = self.modules.take();
    let mut specifiers_to_ids = HashMap::with_capacity(modules.len());
    let mut ids_to_symbols = HashMap::with_capacity(modules.len());
    for (specifier, symbol) in modules {
      specifiers_to_ids.insert(specifier, symbol.module_id());
      ids_to_symbols.insert(symbol.module_id(), *symbol);
    }
    RootSymbol {
      specifiers_to_ids,
      ids_to_symbols,
    }
  }

  pub fn get_or_analyze<'b>(
    &'b self,
    specifier: &ModuleSpecifier,
  ) -> Result<Option<ModuleSymbolRef<'b>>> {
    if let Some(module_symbol) = self.modules.get(specifier) {
      return Ok(Some(module_symbol.as_ref()));
    }

    let Some(graph_module) = self.graph.get(specifier) else {
      return Ok(None);
    };

    match graph_module {
      crate::Module::Esm(esm_module) => self.analyze_esm_module(esm_module),
      crate::Module::Json(json_module) => self.analyze_json_module(json_module),
      crate::Module::Npm(_)
      | crate::Module::Node(_)
      | crate::Module::External(_) => Ok(None),
    }
  }

  fn analyze_esm_module(
    &self,
    esm_module: &EsmModule,
  ) -> Result<Option<ModuleSymbolRef>, anyhow::Error> {
    let Some(source) = self.parsed_source(esm_module)? else {
      return Ok(None);
    };
    let specifier = &esm_module.specifier;
    let module = source.module();
    let mut module_symbol = EsmModuleSymbol {
      specifier: specifier.clone(),
      module_id: ModuleId(self.modules.len() as u32),
      source: source.clone(),
      next_symbol_id: Default::default(),
      child_decls: Default::default(),
      exports: Default::default(),
      re_exports: Default::default(),
      traced_re_exports: Default::default(),
      traced_referrers: Default::default(),
      swc_id_to_symbol_id: Default::default(),
      symbols: Default::default(),
    };

    let filler = SymbolFiller {
      source: &source,
      specifier,
      handler: self.handler,
    };
    filler.fill_module(&mut module_symbol, module);
    self
      .modules
      .insert(specifier.clone(), ModuleSymbol::Esm(module_symbol));
    Ok(Some(self.modules.get(specifier).unwrap().as_ref()))
  }

  fn analyze_json_module(
    &self,
    json_module: &JsonModule,
  ) -> Result<Option<ModuleSymbolRef>, anyhow::Error> {
    let specifier = &json_module.specifier;
    // it's not ideal having to use SourceTextInfo here, but it makes
    // it easier to interop with ParsedSource
    let source_text_info = SourceTextInfo::new(json_module.source.clone());
    let range = source_text_info.range();
    let module_symbol = JsonModuleSymbol {
      specifier: specifier.clone(),
      module_id: ModuleId(self.modules.len() as u32),
      exports: IndexMap::from([("default".to_string(), SymbolId(0))]),
      default_symbol: Symbol {
        symbol_id: SymbolId(0),
        is_public: RefCell::new(false),
        decls: {
          let range = {
            let source = source_text_info.text_str();
            let start_whitespace_len = source.len() - source.trim_start().len();
            let end_whitespace_len = source.len() - source.trim_end().len();
            SourceRange::new(
              range.start + start_whitespace_len,
              range.end - end_whitespace_len,
            )
          };
          IndexMap::from([(
            range,
            SymbolDecl {
              range,
              kind: SymbolDeclKind::Definition(SymbolNode(
                SymbolNodeInner::Json,
              )),
            },
          )])
        },
        deps: Default::default(),
        child_decls: Default::default(),
        exports: Default::default(),
      },
      source_text_info,
    };
    self.modules.insert(
      specifier.clone(),
      ModuleSymbol::Json(Box::new(module_symbol)),
    );
    Ok(Some(self.modules.get(specifier).unwrap().as_ref()))
  }

  fn parsed_source(
    &self,
    graph_module: &EsmModule,
  ) -> Result<Option<ParsedSource>, deno_ast::Diagnostic> {
    self
      .parser
      .parse_module(
        &graph_module.specifier,
        graph_module.source.clone(),
        graph_module.media_type,
      )
      .map(Some)
  }
}

struct SymbolFiller<'a, THandler: TypeTraceHandler> {
  handler: &'a THandler,
  specifier: &'a ModuleSpecifier,
  source: &'a ParsedSource,
}

impl<'a, THandler: TypeTraceHandler> SymbolFiller<'a, THandler> {
  fn fill_module(&self, file_module: &mut EsmModuleSymbol, module: &Module) {
    let mut last_was_overload = false;
    let mut last_override_key = 0;
    for module_item in &module.body {
      let current_override_key = module_item_overload_key(module_item);
      let is_overload = is_module_item_overload(module_item);
      let is_implementation_with_overloads = current_override_key
        == last_override_key
        && !is_overload
        && last_was_overload;
      last_was_overload = is_overload;
      last_override_key = current_override_key;

      if is_implementation_with_overloads {
        // don't examine implementation signatures
        continue;
      }

      self.fill_module_item(
        file_module,
        module_item,
        &|file_module, name, symbol_id| {
          file_module.exports.insert(name, symbol_id);
        },
        &|file_module, symbol_id| {
          file_module.child_decls.insert(symbol_id);
        },
      );
    }
  }

  fn fill_module_item(
    &self,
    file_module: &mut EsmModuleSymbol,
    module_item: &ModuleItem,
    add_export: &impl Fn(&mut EsmModuleSymbol, String, SymbolId),
    add_child: &impl Fn(&mut EsmModuleSymbol, SymbolId),
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
                    ModuleExportName::Str(str) => str.value.to_string(),
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
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(&file_module.source, export_decl),
                    SymbolNodeInnerExportDecl::Class(NodeRefBox::unsafe_new(
                      &file_module.source,
                      n,
                    )),
                  ),
                )),
                range: export_decl.range(),
              },
            );
            self.fill_class_decl(symbol, n);
            let symbol_id = symbol.symbol_id;
            add_export(file_module, n.ident.sym.to_string(), symbol_id);
            add_child(file_module, symbol_id);
          }
          Decl::Fn(n) => {
            let symbol = file_module.get_symbol_from_swc_id(
              n.ident.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(&file_module.source, export_decl),
                    SymbolNodeInnerExportDecl::Fn(NodeRefBox::unsafe_new(
                      &file_module.source,
                      n,
                    )),
                  ),
                )),
                range: export_decl.range(),
              },
            );
            self.fill_fn_decl(symbol, n);
            let symbol_id = symbol.symbol_id;
            add_export(file_module, n.ident.sym.to_string(), symbol_id);
            add_child(file_module, symbol_id);
          }
          Decl::Var(n) => {
            let symbol = file_module.create_new_symbol();
            symbol.decls.insert(
              export_decl.range(),
              SymbolDecl {
                kind: SymbolDeclKind::TargetSelf,
                range: export_decl.range(),
              },
            );
            let export_decl_symbol_id = symbol.symbol_id();
            add_child(file_module, export_decl_symbol_id);

            for decl in &n.decls {
              let ids: Vec<Id> = find_pat_ids(&decl.name);
              for id in ids {
                let name = id.0.to_string();
                let symbol = file_module.get_symbol_from_swc_id(
                  id.clone(),
                  SymbolDecl {
                    kind: SymbolDeclKind::Definition(SymbolNode(
                      SymbolNodeInner::ExportDecl(
                        NodeRefBox::unsafe_new(
                          &file_module.source,
                          export_decl,
                        ),
                        SymbolNodeInnerExportDecl::Var(
                          NodeRefBox::unsafe_new(&file_module.source, n),
                          NodeRefBox::unsafe_new(&file_module.source, decl),
                          id.clone(),
                        ),
                      ),
                    )),
                    range: decl.range(),
                  },
                );
                self.fill_var_declarator(symbol, decl);
                let symbol_id = symbol.symbol_id;
                add_export(file_module, name, symbol_id);
                file_module
                  .symbol_mut(export_decl_symbol_id)
                  .unwrap()
                  .deps
                  .insert(id.into());
              }
            }
          }
          Decl::TsInterface(n) => {
            let symbol = file_module.get_symbol_from_swc_id(
              n.id.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(&file_module.source, export_decl),
                    SymbolNodeInnerExportDecl::TsInterface(
                      NodeRefBox::unsafe_new(&file_module.source, n),
                    ),
                  ),
                )),
                range: export_decl.range(),
              },
            );
            self.fill_ts_interface(symbol, n);
            let symbol_id = symbol.symbol_id;
            add_export(file_module, n.id.sym.to_string(), symbol_id);
            add_child(file_module, symbol_id);
          }
          Decl::TsTypeAlias(n) => {
            let symbol = file_module.get_symbol_from_swc_id(
              n.id.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(&file_module.source, export_decl),
                    SymbolNodeInnerExportDecl::TsTypeAlias(
                      NodeRefBox::unsafe_new(&file_module.source, n),
                    ),
                  ),
                )),
                range: export_decl.range(),
              },
            );
            self.fill_ts_type_alias(symbol, n);
            let symbol_id = symbol.symbol_id;
            add_export(file_module, n.id.sym.to_string(), symbol_id);
            add_child(file_module, symbol_id);
          }
          Decl::TsEnum(n) => {
            let symbol = file_module.get_symbol_from_swc_id(
              n.id.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(&file_module.source, export_decl),
                    SymbolNodeInnerExportDecl::TsEnum(NodeRefBox::unsafe_new(
                      &file_module.source,
                      n,
                    )),
                  ),
                )),
                range: export_decl.range(),
              },
            );
            self.fill_ts_enum(symbol, n);
            let symbol_id = symbol.symbol_id;
            add_export(file_module, n.id.sym.to_string(), symbol_id);
            add_child(file_module, symbol_id);
          }
          Decl::TsModule(n) => {
            let maybe_symbol_id = self.fill_ts_module(
              file_module,
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(&file_module.source, export_decl),
                    SymbolNodeInnerExportDecl::TsModule(
                      NodeRefBox::unsafe_new(&file_module.source, n),
                    ),
                  ),
                )),
                range: export_decl.range(),
              },
              n,
            );
            if let Some(symbol_id) = maybe_symbol_id {
              match &n.id {
                TsModuleName::Ident(ident) => {
                  add_export(file_module, ident.sym.to_string(), symbol_id);
                  add_child(file_module, symbol_id);
                }
                TsModuleName::Str(_) => {}
              }
            }
          }
          Decl::Using(_) => {
            unreachable!()
          }
        },
        ModuleDecl::ExportNamed(n) => {
          for specifier in &n.specifiers {
            match specifier {
              ExportSpecifier::Named(named) => {
                match &n.src {
                  Some(src) => {
                    // we don't add the swc ids when there's a specifier because it might
                    // conflict with a re-exported name in another module due to how swc
                    // does its export analysis where all the identifiers in re-exports
                    // have the same scope. Additionally, it makes it easier working
                    // with ModuleExportName::Str
                    let imported_name = match &named.orig {
                      ModuleExportName::Ident(ident) => ident.sym.to_string(),
                      ModuleExportName::Str(str) => str.value.to_string(),
                    };
                    let export_name = named
                      .exported
                      .as_ref()
                      .map(|exported| match exported {
                        ModuleExportName::Ident(ident) => ident.sym.to_string(),
                        ModuleExportName::Str(str) => str.value.to_string(),
                      })
                      .unwrap_or_else(|| imported_name.clone());
                    let symbol = file_module.create_new_symbol();
                    symbol.insert_decl(SymbolDecl {
                      range: named.range(),
                      kind: SymbolDeclKind::FileRef(FileDep {
                        name: FileDepName::Name(imported_name),
                        specifier: src.value.to_string(),
                      }),
                    });
                    let symbol_id = symbol.symbol_id;
                    add_export(file_module, export_name, symbol_id);
                  }
                  None => {
                    let orig_ident = match &named.orig {
                      ModuleExportName::Ident(ident) => ident,
                      ModuleExportName::Str(_) => unreachable!(),
                    };
                    if let Some(exported_name) = &named.exported {
                      let exported_name = match exported_name {
                        ModuleExportName::Ident(ident) => ident.sym.to_string(),
                        ModuleExportName::Str(str) => str.value.to_string(),
                      };
                      let orig_symbol = file_module.get_symbol_from_swc_id(
                        orig_ident.to_id(),
                        SymbolDecl {
                          range: n.range(),
                          kind: SymbolDeclKind::TargetSelf,
                        },
                      );
                      let orig_symbol_id = orig_symbol.symbol_id;
                      add_export(file_module, exported_name, orig_symbol_id);
                    } else {
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
                      let symbol_id = file_module.ensure_symbol_for_swc_id(
                        orig_ident.to_id(),
                        SymbolDecl {
                          range: named.range(),
                          kind: SymbolDeclKind::TargetSelf,
                        },
                      );
                      add_export(
                        file_module,
                        orig_ident.sym.to_string(),
                        symbol_id,
                      );
                    }
                  }
                }
              }
              ExportSpecifier::Namespace(specifier) => {
                if let Some(src) = &n.src {
                  let name = match &specifier.name {
                    ModuleExportName::Ident(ident) => ident.sym.to_string(),
                    ModuleExportName::Str(str) => str.value.to_string(),
                  };
                  let symbol = file_module.create_new_symbol();
                  symbol.insert_decl(SymbolDecl {
                    kind: SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Star,
                      specifier: src.value.to_string(),
                    }),
                    range: specifier.range(),
                  });
                  let symbol_id = symbol.symbol_id;
                  add_export(file_module, name, symbol_id);
                } else {
                  let name = match &specifier.name {
                    ModuleExportName::Ident(ident) => ident,
                    ModuleExportName::Str(_) => unreachable!(),
                  };
                  let symbol_kind = SymbolDeclKind::Target(name.to_id());
                  let symbol_id = file_module
                    .get_symbol_from_swc_id(
                      name.to_id(),
                      SymbolDecl {
                        kind: symbol_kind.clone(),
                        range: n.range(),
                      },
                    )
                    .symbol_id;
                  add_export(file_module, name.sym.to_string(), symbol_id);
                }
              }
              // https://github.com/tc39/proposal-export-default-from
              ExportSpecifier::Default(_) => {
                unreachable!("export default from is stage 1")
              }
            }
          }
        }
        ModuleDecl::ExportDefaultDecl(default_decl) => {
          let symbol_id =
            file_module.ensure_default_export_symbol(SymbolDecl {
              range: default_decl.range(),
              kind: SymbolDeclKind::Definition(SymbolNode(
                SymbolNodeInner::ExportDefaultDecl(NodeRefBox::unsafe_new(
                  &file_module.source,
                  default_decl,
                )),
              )),
            });
          let maybe_ident = match &default_decl.decl {
            DefaultDecl::Class(expr) => expr.ident.as_ref(),
            DefaultDecl::Fn(expr) => expr.ident.as_ref(),
            DefaultDecl::TsInterfaceDecl(decl) => Some(&decl.id),
          };
          if let Some(ident) = maybe_ident {
            let id = ident.to_id();
            file_module.swc_id_to_symbol_id.insert(id, symbol_id);
          }

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
        ModuleDecl::ExportDefaultExpr(expr) => {
          self.handle_export_default_expr(
            expr.range(),
            &expr.expr,
            Some(expr),
            file_module,
          );
        }
        ModuleDecl::ExportAll(n) => {
          file_module.re_exports.push(n.src.value.to_string());
        }
        ModuleDecl::TsImportEquals(import_equals) => {
          let symbol_id =
            self.ensure_symbol_for_import_equals(file_module, import_equals);
          let symbol = file_module.symbol_mut(symbol_id).unwrap();
          match &import_equals.module_ref {
            TsModuleRef::TsEntityName(entity_name) => {
              let (leftmost_id, parts) = ts_entity_name_to_parts(entity_name);
              symbol
                .deps
                .insert(SymbolDep::QualifiedId(leftmost_id, parts));
            }
            TsModuleRef::TsExternalModuleRef(_) => {
              // don't need to do anything in this case
            }
          }
          if import_equals.is_export {
            add_export(
              file_module,
              import_equals.id.sym.to_string(),
              symbol_id,
            );
          }
        }
        ModuleDecl::TsExportAssignment(export_assignment) => {
          self.handle_export_default_expr(
            export_assignment.range(),
            &export_assignment.expr,
            None,
            file_module,
          );
        }
        ModuleDecl::TsNamespaceExport(_) => {
          // ignore
        }
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
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::ClassDecl(NodeRefBox::unsafe_new(
                      &file_module.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
              );
              self.fill_class_decl(symbol, n);
              let symbol_id = symbol.symbol_id;
              add_child(file_module, symbol_id);
            }
            Decl::Fn(n) => {
              let id = n.ident.to_id();
              let symbol = file_module.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::FnDecl(NodeRefBox::unsafe_new(
                      &file_module.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
              );
              self.fill_fn_decl(symbol, n);
              let symbol_id = symbol.symbol_id;
              add_child(file_module, symbol_id);
            }
            Decl::Var(var_decl) => {
              for decl in &var_decl.decls {
                let ids: Vec<Id> = find_pat_ids(&decl.name);
                for id in ids {
                  let symbol = file_module.get_symbol_from_swc_id(
                    id,
                    SymbolDecl {
                      kind: SymbolDeclKind::Definition(SymbolNode(
                        SymbolNodeInner::Var(
                          NodeRefBox::unsafe_new(&file_module.source, var_decl),
                          NodeRefBox::unsafe_new(&file_module.source, decl),
                        ),
                      )),
                      range: decl.range(),
                    },
                  );
                  self.fill_var_declarator(symbol, decl);
                  let symbol_id = symbol.symbol_id;
                  add_child(file_module, symbol_id);
                }
              }
            }
            Decl::TsInterface(n) => {
              let id = n.id.to_id();
              let symbol = file_module.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::TsInterface(NodeRefBox::unsafe_new(
                      &file_module.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
              );
              self.fill_ts_interface(symbol, n);
              let symbol_id = symbol.symbol_id;
              add_child(file_module, symbol_id);
            }
            Decl::TsTypeAlias(n) => {
              let id = n.id.to_id();
              let symbol = file_module.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::TsTypeAlias(NodeRefBox::unsafe_new(
                      &file_module.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
              );
              self.fill_ts_type_alias(symbol, n);
              let symbol_id = symbol.symbol_id;
              add_child(file_module, symbol_id);
            }
            Decl::TsEnum(n) => {
              let id = n.id.to_id();
              let symbol = file_module.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::TsEnum(NodeRefBox::unsafe_new(
                      &file_module.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
              );
              self.fill_ts_enum(symbol, n);
              let symbol_id = symbol.symbol_id;
              add_child(file_module, symbol_id);
            }
            Decl::TsModule(n) => {
              let symbol_id = self.fill_ts_module(
                file_module,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::TsNamespace(NodeRefBox::unsafe_new(
                      &file_module.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
                n,
              );
              if let Some(symbol_id) = symbol_id {
                add_child(file_module, symbol_id);
              }
            }
            Decl::Using(_) => {
              // ignore
            }
          };
        }
      },
    }
  }

  fn handle_export_default_expr(
    &self,
    default_export_range: SourceRange,
    expr: &Expr,
    maybe_parent: Option<&ExportDefaultExpr>,
    file_module: &mut EsmModuleSymbol,
  ) {
    match expr {
      Expr::Ident(ident) => {
        let default_export_symbol_id = file_module
          .ensure_default_export_symbol(SymbolDecl {
            kind: SymbolDeclKind::Target(ident.to_id()),
            range: default_export_range,
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
          .insert(ident.to_id().into());
      }
      Expr::Lit(lit) => {
        debug_assert!(maybe_parent.is_some());
        let Some(parent) = maybe_parent else {
          return;
        };
        file_module.ensure_default_export_symbol(SymbolDecl {
          kind: SymbolDeclKind::Definition(SymbolNode(
            SymbolNodeInner::ExportDefaultExprLit(
              NodeRefBox::unsafe_new(&file_module.source, parent),
              NodeRefBox::unsafe_new(&file_module.source, lit),
            ),
          )),
          range: default_export_range,
        });
      }
      _ => {
        let line_and_column = self
          .source
          .text_info()
          .line_and_column_display(default_export_range.start);
        self.handler.diagnostic(TypeTraceDiagnostic {
          kind: TypeTraceDiagnosticKind::UnsupportedDefaultExpr,
          specifier: self.specifier.clone(),
          line_and_column: Some(line_and_column),
        });
      }
    }
  }

  fn ensure_symbol_for_import_equals(
    &self,
    file_module: &mut EsmModuleSymbol,
    import_equals: &TsImportEqualsDecl,
  ) -> SymbolId {
    let id = import_equals.id.to_id();
    if let Some(symbol_id) = file_module.swc_id_to_symbol_id.get(&id) {
      return *symbol_id;
    }
    match &import_equals.module_ref {
      TsModuleRef::TsEntityName(entity_name) => {
        let (leftmost_id, parts) = ts_entity_name_to_parts(entity_name);
        file_module.ensure_symbol_for_swc_id(
          id,
          SymbolDecl {
            kind: SymbolDeclKind::QualifiedTarget(leftmost_id, parts),
            range: import_equals.range(),
          },
        )
      }
      TsModuleRef::TsExternalModuleRef(module_ref) => file_module
        .ensure_symbol_for_swc_id(
          id,
          SymbolDecl {
            kind: SymbolDeclKind::FileRef(FileDep {
              name: FileDepName::Name("default".to_string()),
              specifier: module_ref.expr.value.to_string(),
            }),
            range: import_equals.range(),
          },
        ),
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
    file_module: &mut EsmModuleSymbol,
    symbol_decl: SymbolDecl,
    n: &TsModuleDecl,
  ) -> Option<SymbolId> {
    let mut id = match &n.id {
      TsModuleName::Ident(ident) => ident.to_id(),
      TsModuleName::Str(_) => return None, // ignore for now
    };
    let mut mod_symbol_id =
      file_module.ensure_symbol_for_swc_id(id.clone(), symbol_decl);

    // fill the exported declarations
    if let Some(body) = &n.body {
      let mut current = body;
      let block = loop {
        match current {
          TsNamespaceBody::TsModuleBlock(block) => break block,
          TsNamespaceBody::TsNamespaceDecl(decl) => {
            let previous_symbol_id = mod_symbol_id;
            let previous_id = id;
            id = decl.id.to_id();
            mod_symbol_id = file_module.ensure_symbol_for_swc_id(
              id.clone(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::TsNamespace(NodeRefBox::unsafe_new(
                    &file_module.source,
                    n,
                  )),
                )),
                range: decl.range(),
              },
            );
            let previous_symbol =
              file_module.symbol_mut(previous_symbol_id).unwrap();
            previous_symbol.deps.insert(id.clone().into());
            previous_symbol
              .exports
              .insert(id.0.to_string(), mod_symbol_id);
            file_module
              .symbol_mut(mod_symbol_id)
              .unwrap()
              .deps
              .insert(previous_id.into());
            current = &decl.body;
          }
        }
      };
      let mut last_was_overload = false;
      let mut last_override_key = 0;
      for item in &block.body {
        let current_override_key = module_item_overload_key(item);
        let is_overload = is_module_item_overload(item);
        let is_implementation_with_overloads = current_override_key
          == last_override_key
          && !is_overload
          && last_was_overload;
        last_was_overload = is_overload;
        last_override_key = current_override_key;

        if is_implementation_with_overloads {
          // don't examine implementation signatures
          continue;
        }

        self.fill_module_item(
          file_module,
          item,
          &|file_module, name, symbol_id| {
            file_module
              .symbol_mut(mod_symbol_id)
              .unwrap()
              .exports
              .insert(name, symbol_id);
          },
          &|file_module, symbol_id| {
            file_module
              .symbol_mut(mod_symbol_id)
              .unwrap()
              .child_decls
              .insert(symbol_id);
          },
        );
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
            ModuleDecl::ExportDecl(export_decl) => match &export_decl.decl {
              Decl::Class(n) => {
                let id = n.ident.to_id();
                let def_symbol_id = file_module.ensure_symbol_for_swc_id(
                  id.clone(),
                  SymbolDecl {
                    kind: SymbolDeclKind::Definition(SymbolNode(
                      SymbolNodeInner::ClassDecl(NodeRefBox::unsafe_new(
                        &file_module.source,
                        n,
                      )),
                    )),
                    range: export_decl.range(),
                  },
                );
                let mod_symbol = file_module.symbol_mut(mod_symbol_id).unwrap();
                mod_symbol.deps.insert(id.into());
                mod_symbol
                  .exports
                  .insert(n.ident.sym.to_string(), def_symbol_id);
              }
              Decl::Fn(n) => {
                let id = n.ident.to_id();
                let def_symbol_id = file_module.ensure_symbol_for_swc_id(
                  id.clone(),
                  SymbolDecl {
                    kind: SymbolDeclKind::Definition(SymbolNode(
                      SymbolNodeInner::FnDecl(NodeRefBox::unsafe_new(
                        &file_module.source,
                        n,
                      )),
                    )),
                    range: export_decl.range(),
                  },
                );
                let mod_symbol = file_module.symbol_mut(mod_symbol_id).unwrap();
                mod_symbol.deps.insert(n.ident.to_id().into());
                mod_symbol
                  .exports
                  .insert(n.ident.sym.to_string(), def_symbol_id);
              }
              Decl::Var(n) => {
                for decl in &n.decls {
                  for id in find_pat_ids::<_, Id>(&decl.name) {
                    let def_symbol_id = file_module.ensure_symbol_for_swc_id(
                      id.clone(),
                      SymbolDecl {
                        kind: SymbolDeclKind::Definition(SymbolNode(
                          SymbolNodeInner::Var(
                            NodeRefBox::unsafe_new(&file_module.source, n),
                            NodeRefBox::unsafe_new(&file_module.source, decl),
                          ),
                        )),
                        range: decl.range(),
                      },
                    );
                    let mod_symbol =
                      file_module.symbol_mut(mod_symbol_id).unwrap();
                    mod_symbol.deps.extend(
                      find_pat_ids(&decl.name)
                        .into_iter()
                        .map(|i: Id| i.into()),
                    );
                    mod_symbol.exports.insert(id.0.to_string(), def_symbol_id);
                  }
                }
              }
              Decl::TsInterface(n) => {
                let id = n.id.to_id();
                let def_symbol_id = file_module.ensure_symbol_for_swc_id(
                  id.clone(),
                  SymbolDecl {
                    kind: SymbolDeclKind::Definition(SymbolNode(
                      SymbolNodeInner::TsInterface(NodeRefBox::unsafe_new(
                        &file_module.source,
                        n,
                      )),
                    )),
                    range: export_decl.range(),
                  },
                );
                let mod_symbol = file_module.symbol_mut(mod_symbol_id).unwrap();
                mod_symbol.deps.insert(id.into());
                mod_symbol
                  .exports
                  .insert(n.id.sym.to_string(), def_symbol_id);
              }
              Decl::TsTypeAlias(n) => {
                let id = n.id.to_id();
                let def_symbol_id = file_module.ensure_symbol_for_swc_id(
                  id.clone(),
                  SymbolDecl {
                    kind: SymbolDeclKind::Definition(SymbolNode(
                      SymbolNodeInner::TsTypeAlias(NodeRefBox::unsafe_new(
                        &file_module.source,
                        n,
                      )),
                    )),
                    range: export_decl.range(),
                  },
                );
                let mod_symbol = file_module.symbol_mut(mod_symbol_id).unwrap();
                mod_symbol.deps.insert(id.into());
                mod_symbol
                  .exports
                  .insert(n.id.sym.to_string(), def_symbol_id);
              }
              Decl::TsEnum(n) => {
                let id = n.id.to_id();
                let def_symbol_id = file_module.ensure_symbol_for_swc_id(
                  id.clone(),
                  SymbolDecl {
                    kind: SymbolDeclKind::Definition(SymbolNode(
                      SymbolNodeInner::TsEnum(NodeRefBox::unsafe_new(
                        &file_module.source,
                        n,
                      )),
                    )),
                    range: export_decl.range(),
                  },
                );
                let mod_symbol = file_module.symbol_mut(mod_symbol_id).unwrap();
                mod_symbol.deps.insert(id.into());
                mod_symbol
                  .exports
                  .insert(n.id.sym.to_string(), def_symbol_id);
              }
              Decl::TsModule(n) => match &n.id {
                TsModuleName::Ident(ident) => {
                  let id = ident.to_id();
                  let def_symbol_id = file_module.ensure_symbol_for_swc_id(
                    id.clone(),
                    SymbolDecl {
                      kind: SymbolDeclKind::Definition(SymbolNode(
                        SymbolNodeInner::TsNamespace(NodeRefBox::unsafe_new(
                          &file_module.source,
                          n,
                        )),
                      )),
                      range: export_decl.range(),
                    },
                  );
                  let mod_symbol =
                    file_module.symbol_mut(mod_symbol_id).unwrap();
                  mod_symbol.deps.insert(id.into());
                  mod_symbol
                    .exports
                    .insert(ident.sym.to_string(), def_symbol_id);
                }
                TsModuleName::Str(_) => {
                  // ignore for now
                }
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
    Some(mod_symbol_id)
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
    fn overload_key(member: &ClassMember) -> u8 {
      match member {
        ClassMember::Constructor(_) => 1,
        ClassMember::Method(_) => 2,
        ClassMember::PrivateMethod(_) => 3,
        ClassMember::ClassProp(_)
        | ClassMember::PrivateProp(_)
        | ClassMember::TsIndexSignature(_)
        | ClassMember::AutoAccessor(_)
        | ClassMember::StaticBlock(_)
        | ClassMember::Empty(_) => 4,
      }
    }

    let mut last_was_overload = false;
    let mut last_overload_key = 0;
    for member in members {
      let current_overload_key = overload_key(member);
      if current_overload_key == last_overload_key {
        let is_overload = is_class_member_overload(member);
        let is_implementation_with_overloads =
          !is_overload && last_was_overload;
        last_was_overload = is_overload;
        last_overload_key = current_overload_key;

        if is_implementation_with_overloads {
          // filter out implementation signatures
          continue;
        }
      }

      if self.has_internal_jsdoc(member.start()) {
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
    if ctor.accessibility == Some(Accessibility::Private) {
      return; // ignore, private
    }

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
    if method.accessibility == Some(Accessibility::Private) {
      return; // ignore, private
    }

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
      TsParamPropParam::Assign(_) => {
        // nothing to fill
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
    if prop.accessibility == Some(Accessibility::Private) {
      return; // ignore, private
    }

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
    self.symbol.deps.insert(id.into());
  }

  fn visit_ts_import_type(&mut self, n: &TsImportType) {
    let parts = match &n.qualifier {
      Some(qualifier) => {
        let (leftmost_id, mut parts) = ts_entity_name_to_parts(qualifier);
        parts.insert(0, leftmost_id.0.to_string());
        parts
      }
      None => Vec::new(),
    };
    self
      .symbol
      .deps
      .insert(SymbolDep::ImportType(n.arg.value.to_string(), parts));
    n.type_args.visit_with(self);
  }

  fn visit_ts_qualified_name(&mut self, n: &TsQualifiedName) {
    let (id, parts) = ts_qualified_name_parts(n);
    self.symbol.deps.insert(SymbolDep::QualifiedId(id, parts));
  }
}

fn ts_entity_name_to_parts(entity_name: &TsEntityName) -> (Id, Vec<String>) {
  match entity_name {
    TsEntityName::TsQualifiedName(qualified_name) => {
      ts_qualified_name_parts(qualified_name)
    }
    TsEntityName::Ident(ident) => (ident.to_id(), Vec::new()),
  }
}

fn ts_qualified_name_parts(
  mut qualified_name: &TsQualifiedName,
) -> (Id, Vec<String>) {
  let mut parts = Vec::new();
  loop {
    parts.push(qualified_name.right.sym.to_string());
    match &qualified_name.left {
      TsEntityName::TsQualifiedName(n) => {
        qualified_name = n;
      }
      TsEntityName::Ident(n) => {
        parts.reverse();
        return (n.to_id(), parts);
      }
    }
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

fn module_item_overload_key(module_item: &ModuleItem) -> u64 {
  match module_item {
    ModuleItem::ModuleDecl(module_decl) => match module_decl {
      ModuleDecl::ExportDecl(decl) => decl_overload_key(&decl.decl),
      _ => 0,
    },
    ModuleItem::Stmt(stmt) => match stmt {
      Stmt::Decl(decl) => decl_overload_key(decl),
      _ => 0,
    },
  }
}

fn decl_overload_key(decl: &Decl) -> u64 {
  match decl {
    Decl::Fn(decl) => {
      let mut hasher = std::collections::hash_map::DefaultHasher::default();
      decl.ident.sym.hash(&mut hasher);
      hasher.finish()
    }
    _ => 0,
  }
}
