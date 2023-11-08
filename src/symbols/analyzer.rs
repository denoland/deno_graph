// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::cell::Cell;
use std::cell::Ref;
use std::cell::RefCell;
use std::hash::Hash;
use std::hash::Hasher;

use anyhow::Result;
use deno_ast::swc::ast::*;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::utils::find_pat_ids;
use deno_ast::swc::visit::*;
use deno_ast::LineAndColumnDisplay;
use deno_ast::ModuleSpecifier;
use deno_ast::ParsedSource;
use deno_ast::SourcePos;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfo;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::Deserialize;
use serde::Serialize;

use crate::CapturingModuleParser;
use crate::EsmModule;
use crate::JsonModule;
use crate::ModuleGraph;
use crate::ModuleParser;

use super::collections::AdditiveOnlyIndexMap;
use super::collections::AdditiveOnlyIndexMapForCopyValues;
use super::collections::AdditiveOnlyMap;
use super::collections::AdditiveOnlyMapForCopyValues;
use super::cross_module;
use super::cross_module::Definition;
use super::cross_module::DefinitionOrUnresolved;
use super::cross_module::DefinitionPath;
use super::cross_module::ExportsAndReExports;
use super::ResolvedSymbolDepEntry;

/// The root symbol from which module symbols can be retrieved.
///
/// Building the symbols for modules is lazy.
pub struct RootSymbol<'a> {
  module_graph: &'a ModuleGraph,
  parser: CapturingModuleParser<'a>,
  specifiers_to_ids: AdditiveOnlyMapForCopyValues<ModuleSpecifier, ModuleId>,
  ids_to_modules: AdditiveOnlyMap<ModuleId, ModuleInfo>,
  diagnostics: RefCell<Vec<SymbolFillDiagnostic>>,
}

impl<'a> RootSymbol<'a> {
  pub fn new(
    module_graph: &'a ModuleGraph,
    parser: CapturingModuleParser<'a>,
  ) -> Self {
    Self {
      module_graph,
      parser,
      specifiers_to_ids: Default::default(),
      ids_to_modules: Default::default(),
      diagnostics: Default::default(),
    }
  }

  /// Checks if a specifier has been analyzed before.
  ///
  /// This does not lazily analyze the module.
  pub fn has_analyzed(&self, specifier: &ModuleSpecifier) -> bool {
    self.specifiers_to_ids.contains_key(specifier)
  }

  /// Gets a module from the provided specifier. This will lazily analyze
  /// the module if it has not already been analyzed.
  pub fn get_module_from_specifier(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ModuleInfoRef> {
    if let Some(module_id) = self.specifiers_to_ids.get(specifier) {
      let module_symbol = self.ids_to_modules.get(&module_id).unwrap();
      return Some(module_symbol.as_ref());
    }

    let Some(graph_module) = self.module_graph.get(specifier) else {
      return None;
    };

    match graph_module {
      crate::Module::Esm(esm_module) => self.analyze_esm_module(esm_module),
      crate::Module::Json(json_module) => {
        Some(self.analyze_json_module(json_module))
      }
      crate::Module::Npm(_)
      | crate::Module::Node(_)
      | crate::Module::External(_) => None,
    }
  }

  pub fn get_module_from_id(
    &self,
    module_id: ModuleId,
  ) -> Option<ModuleInfoRef> {
    self.ids_to_modules.get(&module_id).map(|s| s.as_ref())
  }

  /// Goes to the definitions of the specified symbol.
  pub fn go_to_definitions<'b>(
    &'b self,
    module: ModuleInfoRef<'b>,
    symbol: &'b Symbol,
  ) -> impl Iterator<Item = Definition<'b>> {
    self
      .find_definition_paths(module, symbol)
      .into_iter()
      .flat_map(|d| d.into_definitions())
  }

  /// Goes to the definitions of the specified symbol.
  pub fn go_to_definitions_or_unresolveds<'b>(
    &'b self,
    module: ModuleInfoRef<'b>,
    symbol: &'b Symbol,
  ) -> impl Iterator<Item = DefinitionOrUnresolved<'b>> {
    self
      .find_definition_paths(module, symbol)
      .into_iter()
      .flat_map(|d| d.into_definitions_or_unresolveds())
  }

  /// Finds the graph paths to the definition of the specified symbol.
  pub fn find_definition_paths<'b>(
    &'b self,
    module: ModuleInfoRef<'b>,
    symbol: &'b Symbol,
  ) -> Vec<DefinitionPath<'b>> {
    debug_assert_eq!(symbol.module_id(), module.module_id());
    super::cross_module::find_definition_paths(
      self.module_graph,
      module,
      symbol,
      &|specifier| self.get_module_from_specifier(specifier),
    )
  }

  pub fn resolve_symbol_dep<'b>(
    &'b self,
    module: ModuleInfoRef<'b>,
    symbol: &'b Symbol,
    dep: &'b SymbolDep,
  ) -> Vec<ResolvedSymbolDepEntry<'b>> {
    super::cross_module::resolve_symbol_dep(
      self.module_graph,
      module,
      symbol,
      dep,
      &|specifier| self.get_module_from_specifier(specifier),
    )
  }

  fn analyze_esm_module(
    &self,
    esm_module: &EsmModule,
  ) -> Option<ModuleInfoRef> {
    let Ok(source) = self.parsed_source(esm_module) else {
      return None;
    };
    let specifier = &esm_module.specifier;
    let module = source.module();

    let module_id = ModuleId(self.ids_to_modules.len() as u32);
    let builder = ModuleBuilder::new(module_id);
    let filler = SymbolFiller {
      source: &source,
      specifier,
      diagnostics: RefCell::new(Vec::new()),
      builder: &builder,
    };
    filler.fill(module);
    let diagnostics = filler.diagnostics.take();
    if !diagnostics.is_empty() {
      self.diagnostics.borrow_mut().extend(diagnostics);
    }
    let module_symbol = EsmModuleInfo {
      specifier: specifier.clone(),
      module_id,
      source: source.clone(),
      re_exports: builder.re_exports.take(),
      swc_id_to_symbol_id: builder.swc_id_to_symbol_id.take(),
      symbols: builder
        .symbols
        .take()
        .into_iter()
        .map(|(k, v)| (k, v.0.into_inner()))
        .collect(),
    };
    Some(self.finalize_insert(ModuleInfo::Esm(module_symbol)))
  }

  fn analyze_json_module(&self, json_module: &JsonModule) -> ModuleInfoRef {
    let specifier = &json_module.specifier;
    // it's not ideal having to use SourceTextInfo here, but it makes
    // it easier to interop with ParsedSource
    let source_text_info = SourceTextInfo::new(json_module.source.clone());
    let range = source_text_info.range();
    let module_id = ModuleId(self.ids_to_modules.len() as u32);
    let decls = {
      let range = {
        let source = source_text_info.text_str();
        let start_whitespace_len = source.len() - source.trim_start().len();
        let end_whitespace_len = source.len() - source.trim_end().len();
        SourceRange::new(
          range.start + start_whitespace_len,
          range.end - end_whitespace_len,
        )
      };
      Vec::from([SymbolDecl {
        range,
        kind: SymbolDeclKind::Definition(SymbolNode(SymbolNodeInner::Json)),
      }])
    };
    let module_symbol = JsonModuleInfo {
      specifier: specifier.clone(),
      module_id,
      module_symbol: Symbol {
        module_id,
        symbol_id: SymbolId(0),
        parent_id: None,
        exports: IndexMap::from([("default".to_string(), SymbolId(1))]),
        child_ids: IndexSet::from([SymbolId(1)]),
        decls: decls.clone(),
        deps: Default::default(),
        members: Default::default(),
      },
      default_symbol: Symbol {
        module_id,
        symbol_id: SymbolId(1),
        parent_id: Some(SymbolId(0)),
        decls,
        deps: Default::default(),
        child_ids: Default::default(),
        exports: Default::default(),
        members: Default::default(),
      },
      source_text_info,
    };
    self.finalize_insert(ModuleInfo::Json(Box::new(module_symbol)))
  }

  fn finalize_insert(&self, module: ModuleInfo) -> ModuleInfoRef {
    self
      .specifiers_to_ids
      .insert(module.specifier().clone(), module.module_id());
    let module_id = module.module_id();
    self.ids_to_modules.insert(module_id, module);
    self.ids_to_modules.get(&module_id).unwrap().as_ref()
  }

  fn parsed_source(
    &self,
    graph_module: &EsmModule,
  ) -> Result<ParsedSource, deno_ast::Diagnostic> {
    self.parser.parse_module(
      &graph_module.specifier,
      graph_module.source.clone(),
      graph_module.media_type,
    )
  }

  pub fn take_diagnostics(&self) -> Vec<SymbolFillDiagnostic> {
    std::mem::take(&mut *self.diagnostics.borrow_mut())
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
    f.debug_tuple("SymbolNode")
      .field(&match &self.0 {
        SymbolNodeInner::Json => "<json>".to_string(),
        SymbolNodeInner::Module(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::ClassDecl(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::ExportDecl(d, _) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::ExportDefaultDecl(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::ExportDefaultExprLit(d, _) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::FnDecl(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsEnum(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsNamespace(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsTypeAlias(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsInterface(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::Var(d, _, ident) => {
          format!(
            "{}: {}",
            ident.sym,
            d.value().text_fast(d.source.text_info())
          )
        }
        SymbolNodeInner::AutoAccessor(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::ClassMethod(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::ClassProp(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::Constructor(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsIndexSignature(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsCallSignatureDecl(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsConstructSignatureDecl(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsPropertySignature(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsGetterSignature(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsSetterSignature(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
        SymbolNodeInner::TsMethodSignature(d) => {
          d.value().text_fast(d.source.text_info()).to_string()
        }
      })
      .finish()
  }
}

#[derive(Debug, Clone)]
enum SymbolNodeInnerExportDecl {
  Class(NodeRefBox<ClassDecl>),
  Fn(NodeRefBox<FnDecl>),
  Var(NodeRefBox<VarDecl>, NodeRefBox<VarDeclarator>, Ident),
  TsEnum(NodeRefBox<TsEnumDecl>),
  TsInterface(NodeRefBox<TsInterfaceDecl>),
  TsNamespace(NodeRefBox<TsModuleDecl>),
  TsTypeAlias(NodeRefBox<TsTypeAliasDecl>),
}

#[derive(Debug, Clone)]
enum SymbolNodeInner {
  Json,
  Module(NodeRefBox<Module>),
  ClassDecl(NodeRefBox<ClassDecl>),
  ExportDecl(NodeRefBox<ExportDecl>, SymbolNodeInnerExportDecl),
  ExportDefaultDecl(NodeRefBox<ExportDefaultDecl>),
  ExportDefaultExprLit(NodeRefBox<ExportDefaultExpr>, NodeRefBox<Lit>),
  FnDecl(NodeRefBox<FnDecl>),
  TsEnum(NodeRefBox<TsEnumDecl>),
  TsNamespace(NodeRefBox<TsModuleDecl>),
  TsTypeAlias(NodeRefBox<TsTypeAliasDecl>),
  TsInterface(NodeRefBox<TsInterfaceDecl>),
  Var(NodeRefBox<VarDecl>, NodeRefBox<VarDeclarator>, Ident),
  AutoAccessor(NodeRefBox<AutoAccessor>),
  ClassMethod(NodeRefBox<ClassMethod>),
  ClassProp(NodeRefBox<ClassProp>),
  Constructor(NodeRefBox<Constructor>),
  TsIndexSignature(NodeRefBox<TsIndexSignature>),
  TsCallSignatureDecl(NodeRefBox<TsCallSignatureDecl>),
  TsConstructSignatureDecl(NodeRefBox<TsConstructSignatureDecl>),
  TsPropertySignature(NodeRefBox<TsPropertySignature>),
  TsGetterSignature(NodeRefBox<TsGetterSignature>),
  TsSetterSignature(NodeRefBox<TsSetterSignature>),
  TsMethodSignature(NodeRefBox<TsMethodSignature>),
}

#[derive(Debug, Clone, Copy)]
pub enum ExportDeclRef<'a> {
  Class(&'a ClassDecl),
  Fn(&'a FnDecl),
  Var(&'a VarDecl, &'a VarDeclarator, &'a Ident),
  TsEnum(&'a TsEnumDecl),
  TsInterface(&'a TsInterfaceDecl),
  TsModule(&'a TsModuleDecl),
  TsTypeAlias(&'a TsTypeAliasDecl),
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolNodeRef<'a> {
  Module(&'a Module),
  ClassDecl(&'a ClassDecl),
  ExportDecl(&'a ExportDecl, ExportDeclRef<'a>),
  ExportDefaultDecl(&'a ExportDefaultDecl),
  ExportDefaultExprLit(&'a ExportDefaultExpr, &'a Lit),
  FnDecl(&'a FnDecl),
  TsEnum(&'a TsEnumDecl),
  TsInterface(&'a TsInterfaceDecl),
  TsNamespace(&'a TsModuleDecl),
  TsTypeAlias(&'a TsTypeAliasDecl),
  Var(&'a VarDecl, &'a VarDeclarator, &'a Ident),
  // members
  // todo(dsherret): split this up into two separate enums (one for declarations and one for members)
  AutoAccessor(&'a AutoAccessor),
  ClassMethod(&'a ClassMethod),
  ClassProp(&'a ClassProp),
  Constructor(&'a Constructor),
  TsIndexSignature(&'a TsIndexSignature),
  TsCallSignatureDecl(&'a TsCallSignatureDecl),
  TsConstructSignatureDecl(&'a TsConstructSignatureDecl),
  TsPropertySignature(&'a TsPropertySignature),
  TsGetterSignature(&'a TsGetterSignature),
  TsSetterSignature(&'a TsSetterSignature),
  TsMethodSignature(&'a TsMethodSignature),
}

impl<'a> SymbolNodeRef<'a> {
  /// The local name of the node, if it has a name.
  pub fn maybe_name(&self) -> Option<Cow<'a, str>> {
    fn ts_module_name_to_string(module_name: &TsModuleName) -> &str {
      match module_name {
        TsModuleName::Ident(ident) => &ident.sym,
        TsModuleName::Str(str) => &str.value,
      }
    }

    fn maybe_key_name(key: &Key) -> Option<Cow<str>> {
      match key {
        Key::Private(_) => None,
        Key::Public(n) => maybe_prop_name(n),
      }
    }

    fn maybe_prop_name(prop_name: &PropName) -> Option<Cow<str>> {
      match prop_name {
        PropName::Ident(n) => Some(Cow::Borrowed(&n.sym)),
        PropName::Str(n) => Some(Cow::Borrowed(&n.value)),
        PropName::Num(n) => Some(Cow::Owned(n.value.to_string())),
        PropName::Computed(prop_name) => maybe_expr(&prop_name.expr),
        PropName::BigInt(_) => None,
      }
    }

    fn maybe_expr(expr: &Expr) -> Option<Cow<str>> {
      match expr {
        Expr::Ident(n) => Some(Cow::Borrowed(&n.sym)),
        Expr::Lit(n) => match n {
          Lit::Str(n) => Some(Cow::Borrowed(&n.value)),
          Lit::Num(n) => Some(Cow::Owned(n.value.to_string())),
          Lit::BigInt(n) => Some(Cow::Owned(n.value.to_string())),
          _ => None,
        },
        _ => None,
      }
    }

    match self {
      Self::Module(_) => None,
      Self::ClassDecl(n) => Some(Cow::Borrowed(&n.ident.sym)),
      Self::ExportDecl(_, n) => match n {
        ExportDeclRef::Class(n) => Some(Cow::Borrowed(&n.ident.sym)),
        ExportDeclRef::Fn(n) => Some(Cow::Borrowed(&n.ident.sym)),
        ExportDeclRef::Var(_, _, ident) => Some(Cow::Borrowed(&ident.sym)),
        ExportDeclRef::TsEnum(n) => Some(Cow::Borrowed(&n.id.sym)),
        ExportDeclRef::TsInterface(n) => Some(Cow::Borrowed(&n.id.sym)),
        ExportDeclRef::TsModule(n) => {
          Some(Cow::Borrowed(ts_module_name_to_string(&n.id)))
        }
        ExportDeclRef::TsTypeAlias(n) => Some(Cow::Borrowed(&n.id.sym)),
      },
      Self::ExportDefaultDecl(n) => match &n.decl {
        DefaultDecl::Class(n) => Some(Cow::Borrowed(&n.ident.as_ref()?.sym)),
        DefaultDecl::Fn(n) => Some(Cow::Borrowed(&n.ident.as_ref()?.sym)),
        DefaultDecl::TsInterfaceDecl(n) => Some(Cow::Borrowed(&n.id.sym)),
      },
      Self::ExportDefaultExprLit(_, _) => None,
      Self::FnDecl(n) => Some(Cow::Borrowed(&n.ident.sym)),
      Self::TsEnum(n) => Some(Cow::Borrowed(&n.id.sym)),
      Self::TsInterface(n) => Some(Cow::Borrowed(&n.id.sym)),
      Self::TsNamespace(n) => {
        Some(Cow::Borrowed(ts_module_name_to_string(&n.id)))
      }
      Self::TsTypeAlias(n) => Some(Cow::Borrowed(&n.id.sym)),
      Self::Var(_, _, ident) => Some(Cow::Borrowed(&ident.sym)),
      Self::AutoAccessor(n) => maybe_key_name(&n.key),
      Self::ClassMethod(n) => maybe_prop_name(&n.key),
      Self::ClassProp(n) => maybe_prop_name(&n.key),
      Self::TsPropertySignature(n) => maybe_expr(&n.key),
      Self::TsGetterSignature(n) => maybe_expr(&n.key),
      Self::TsSetterSignature(n) => maybe_expr(&n.key),
      Self::TsMethodSignature(n) => maybe_expr(&n.key),
      // These are unique enough names to avoid collisions with user code.
      // They allow having these as exports and resolving them.
      Self::Constructor(_) => Some(Cow::Borrowed("%%dg_ctor%%")),
      Self::TsIndexSignature(_) => Some(Cow::Borrowed("%%dg_index%%")),
      Self::TsCallSignatureDecl(_) => Some(Cow::Borrowed("%%dg_call%%")),
      Self::TsConstructSignatureDecl(_) => {
        Some(Cow::Borrowed("%%dg_construct%%"))
      }
    }
  }

  // If the decl is a class.
  pub fn is_class(&self) -> bool {
    matches!(
      self,
      Self::ClassDecl(_)
        | Self::ExportDecl(_, ExportDeclRef::Class(_))
        | Self::ExportDefaultDecl(ExportDefaultDecl {
          decl: DefaultDecl::Class(_),
          ..
        })
    )
  }
}

#[derive(Debug, Clone)]
pub enum SymbolDeclKind {
  Target(Id),
  QualifiedTarget(Id, Vec<String>),
  FileRef(FileDep),
  Definition(SymbolNode),
  /// An implementation signature of a function where the function has
  /// overloads. In this case, the implementation signature is considered
  /// "private" and it's types are not traced, but we include it in the
  /// information returned for deno_doc to use.
  DefinitionPrivateFnImpl(SymbolNode),
}

impl SymbolDeclKind {
  pub fn is_definition(&self) -> bool {
    matches!(self, Self::Definition(_) | Self::DefinitionPrivateFnImpl(_))
  }

  pub fn maybe_node_and_source(
    &self,
  ) -> Option<(SymbolNodeRef, &ParsedSource)> {
    match self {
      SymbolDeclKind::Definition(SymbolNode(def))
      | SymbolDeclKind::DefinitionPrivateFnImpl(SymbolNode(def)) => match def {
        SymbolNodeInner::Json => None,
        SymbolNodeInner::Module(n) => {
          Some((SymbolNodeRef::Module(n.value()), n.source()))
        }
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
              SymbolNodeInnerExportDecl::TsNamespace(n) => {
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
        SymbolNodeInner::Var(decl, declarator, ident) => Some((
          SymbolNodeRef::Var(decl.value(), declarator.value(), ident),
          decl.source(),
        )),
        SymbolNodeInner::AutoAccessor(n) => {
          Some((SymbolNodeRef::AutoAccessor(n.value()), n.source()))
        }
        SymbolNodeInner::ClassMethod(n) => {
          Some((SymbolNodeRef::ClassMethod(n.value()), n.source()))
        }
        SymbolNodeInner::ClassProp(n) => {
          Some((SymbolNodeRef::ClassProp(n.value()), n.source()))
        }
        SymbolNodeInner::Constructor(n) => {
          Some((SymbolNodeRef::Constructor(n.value()), n.source()))
        }
        SymbolNodeInner::TsIndexSignature(n) => {
          Some((SymbolNodeRef::TsIndexSignature(n.value()), n.source()))
        }
        SymbolNodeInner::TsCallSignatureDecl(n) => {
          Some((SymbolNodeRef::TsCallSignatureDecl(n.value()), n.source()))
        }
        SymbolNodeInner::TsConstructSignatureDecl(n) => Some((
          SymbolNodeRef::TsConstructSignatureDecl(n.value()),
          n.source(),
        )),
        SymbolNodeInner::TsPropertySignature(n) => {
          Some((SymbolNodeRef::TsPropertySignature(n.value()), n.source()))
        }
        SymbolNodeInner::TsGetterSignature(n) => {
          Some((SymbolNodeRef::TsGetterSignature(n.value()), n.source()))
        }
        SymbolNodeInner::TsSetterSignature(n) => {
          Some((SymbolNodeRef::TsSetterSignature(n.value()), n.source()))
        }
        SymbolNodeInner::TsMethodSignature(n) => {
          Some((SymbolNodeRef::TsMethodSignature(n.value()), n.source()))
        }
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

  /// The local name of the decl, if it has a name or node.
  pub fn maybe_name(&self) -> Option<Cow<str>> {
    self.maybe_node().and_then(|n| n.maybe_name())
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

#[derive(Debug, Clone)]
pub struct Symbol {
  module_id: ModuleId,
  symbol_id: SymbolId,
  parent_id: Option<SymbolId>,
  decls: Vec<SymbolDecl>,
  deps: IndexSet<SymbolDep>,
  /// The child declarations of module declarations.
  child_ids: IndexSet<SymbolId>,
  exports: IndexMap<String, SymbolId>,
  /// Members
  members: IndexSet<SymbolId>,
}

impl Symbol {
  pub fn new(
    module_id: ModuleId,
    symbol_id: SymbolId,
    parent_id: Option<SymbolId>,
  ) -> Self {
    Symbol {
      module_id,
      symbol_id,
      parent_id,
      decls: Default::default(),
      deps: Default::default(),
      child_ids: Default::default(),
      exports: Default::default(),
      members: Default::default(),
    }
  }

  /// A unique identifier of the symbol, which consists of the module id and symbol id.
  pub fn unique_id(&self) -> UniqueSymbolId {
    UniqueSymbolId::new(self.module_id, self.symbol_id)
  }

  /// Module id of where the symbol is located.
  pub fn module_id(&self) -> ModuleId {
    self.module_id
  }

  /// Symbol id within the module.
  ///
  /// This is module specific. If you need a global identifier, then use unique_id().
  pub fn symbol_id(&self) -> SymbolId {
    self.symbol_id
  }

  /// Gets the symbol's parent id.
  pub fn parent_id(&self) -> Option<SymbolId> {
    self.parent_id
  }

  /// The local name of the symbol if it has one.
  pub fn maybe_name(&self) -> Option<Cow<str>> {
    self.decls.first().and_then(|d| d.maybe_name())
  }

  /// Export of the symbol by name.
  pub fn export(&self, name: &str) -> Option<SymbolId> {
    self.exports.get(name).copied()
  }

  /// Map of exports
  pub fn exports(&self) -> &IndexMap<String, SymbolId> {
    &self.exports
  }

  /// Identifiers of namespace children (ex. functions, interface found in a namespace).
  pub fn child_ids(&self) -> impl Iterator<Item = SymbolId> + '_ {
    self.child_ids.iter().copied()
  }

  /// Gets the dependencies of the symbol (not including member or child dependencies).
  pub fn deps(&self) -> impl Iterator<Item = &SymbolDep> {
    self.deps.iter()
  }

  /// The symbol's associated declarations. A symbol can represent many declarations
  /// if they have the same name via declaration merging.
  pub fn decls(&self) -> &[SymbolDecl] {
    &self.decls
  }

  /// Gets a set of the members.
  ///
  /// * For interfaces this is all the children.
  /// * For classes this is the instance members. The static members are
  ///   available as the children.
  pub fn members(&self) -> &IndexSet<SymbolId> {
    &self.members
  }

  /// Gets if this symbol represents a file dependency.
  pub fn file_dep(&self) -> Option<&FileDep> {
    for dep in self.decls() {
      if let SymbolDeclKind::FileRef(file_ref) = &dep.kind {
        return Some(file_ref);
      }
    }
    None
  }
}

/// A unique identifier for a symbol, which consists of the module id and symbol id.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct UniqueSymbolId {
  pub module_id: ModuleId,
  pub symbol_id: SymbolId,
}

impl UniqueSymbolId {
  pub fn new(module_id: ModuleId, symbol_id: SymbolId) -> Self {
    Self {
      module_id,
      symbol_id,
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleInfoRef<'a> {
  Json(&'a JsonModuleInfo),
  Esm(&'a EsmModuleInfo),
}

impl<'a> ModuleInfoRef<'a> {
  pub fn json(&self) -> Option<&'a JsonModuleInfo> {
    match self {
      Self::Json(json) => Some(json),
      Self::Esm(_) => None,
    }
  }

  pub fn esm(&self) -> Option<&'a EsmModuleInfo> {
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

  pub fn module_symbol(&self) -> &'a Symbol {
    match self {
      Self::Json(m) => &m.module_symbol,
      Self::Esm(m) => m.module_symbol(),
    }
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
    root_symbol: &'a RootSymbol,
  ) -> ExportsAndReExports<'a> {
    cross_module::exports_and_re_exports(
      root_symbol.module_graph,
      *self,
      &|specifier| root_symbol.get_module_from_specifier(specifier),
    )
  }

  pub(crate) fn re_export_all_specifiers(&self) -> Option<&'a Vec<String>> {
    match self {
      Self::Json(_) => None,
      Self::Esm(m) => Some(&m.re_exports),
    }
  }
}

/// Holds information about the module like symbols and re-exports.
#[derive(Debug, Clone)]
pub enum ModuleInfo {
  Json(Box<JsonModuleInfo>),
  Esm(EsmModuleInfo),
}

impl ModuleInfo {
  pub fn json(&self) -> Option<&JsonModuleInfo> {
    match self {
      Self::Json(json) => Some(json),
      Self::Esm(_) => None,
    }
  }

  pub fn esm(&self) -> Option<&EsmModuleInfo> {
    match self {
      Self::Json(_) => None,
      Self::Esm(esm) => Some(esm),
    }
  }

  pub fn as_ref(&self) -> ModuleInfoRef {
    match self {
      ModuleInfo::Json(m) => (**m).as_ref(),
      ModuleInfo::Esm(m) => m.as_ref(),
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
pub struct JsonModuleInfo {
  module_id: ModuleId,
  specifier: ModuleSpecifier,
  source_text_info: SourceTextInfo,
  module_symbol: Symbol,
  default_symbol: Symbol,
}

impl std::fmt::Debug for JsonModuleInfo {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("JsonModuleInfo")
      .field("module_id", &self.module_id)
      .field("specifier", &self.specifier.as_str())
      .field("module_symbol", &self.module_symbol)
      .finish()
  }
}

impl JsonModuleInfo {
  pub fn as_ref(&self) -> ModuleInfoRef {
    ModuleInfoRef::Json(self)
  }

  pub fn specifier(&self) -> &ModuleSpecifier {
    &self.specifier
  }

  pub fn text_info(&self) -> &SourceTextInfo {
    &self.source_text_info
  }

  pub fn symbols(&self) -> impl Iterator<Item = &Symbol> {
    std::iter::once(&self.module_symbol)
      .chain(std::iter::once(&self.default_symbol))
  }

  pub fn symbol(&self, id: SymbolId) -> Option<&Symbol> {
    match id.0 {
      0 => Some(&self.module_symbol),
      1 => Some(&self.default_symbol),
      _ => {
        debug_assert!(false, "invalid symbol id");
        None
      }
    }
  }
}

#[derive(Clone)]
pub struct EsmModuleInfo {
  module_id: ModuleId,
  specifier: ModuleSpecifier,
  source: ParsedSource,
  /// The re-export specifiers.
  re_exports: Vec<String>,
  // note: not all symbol ids have an swc id. For example, default exports
  swc_id_to_symbol_id: IndexMap<Id, SymbolId>,
  symbols: IndexMap<SymbolId, Symbol>,
}

impl std::fmt::Debug for EsmModuleInfo {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("EsmModuleInfo")
      .field("module_id", &self.module_id)
      .field("specifier", &self.specifier.as_str())
      .field("re_exports", &self.re_exports)
      .field("swc_id_to_symbol_id", &self.swc_id_to_symbol_id)
      .field("symbols", &self.symbols)
      .finish()
  }
}

impl EsmModuleInfo {
  pub fn as_ref(&self) -> ModuleInfoRef {
    ModuleInfoRef::Esm(self)
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

  pub fn exports<'a>(
    &'a self,
    root_symbol: &'a RootSymbol,
  ) -> ExportsAndReExports<'a> {
    self.as_ref().exports(root_symbol)
  }

  pub fn module_symbol(&self) -> &Symbol {
    self.symbol(SymbolId(0)).unwrap()
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
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SymbolFillDiagnosticKind {
  UnsupportedDefaultExpr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SymbolFillDiagnostic {
  pub kind: SymbolFillDiagnosticKind,
  pub specifier: ModuleSpecifier,
  pub line_and_column: Option<LineAndColumnDisplay>,
}

struct SymbolMut(RefCell<Symbol>);

impl SymbolMut {
  pub fn new(symbol: Symbol) -> Self {
    Self(RefCell::new(symbol))
  }

  pub fn add_decl(&self, symbol_decl: SymbolDecl) {
    self.0.borrow_mut().decls.push(symbol_decl);
  }

  pub fn add_dep(&self, symbol_dep: SymbolDep) {
    self.0.borrow_mut().deps.insert(symbol_dep);
  }

  pub fn symbol_id(&self) -> SymbolId {
    self.0.borrow_mut().symbol_id
  }

  pub fn add_export(&self, name: String, symbol_id: SymbolId) {
    self.0.borrow_mut().exports.insert(name, symbol_id);
  }

  pub fn get_export(&self, name: &str) -> Option<SymbolId> {
    self.0.borrow().exports.get(name).copied()
  }

  pub fn add_child_id(&self, symbol_id: SymbolId) {
    self.0.borrow_mut().child_ids.insert(symbol_id);
  }

  pub fn add_member(&self, symbol_id: SymbolId) {
    self.0.borrow_mut().members.insert(symbol_id);
  }

  pub fn borrow_inner(&self) -> Ref<'_, Symbol> {
    self.0.borrow()
  }
}

struct ModuleBuilder {
  module_id: ModuleId,
  // todo(dsherret): make this not an IndexMap
  swc_id_to_symbol_id: AdditiveOnlyIndexMapForCopyValues<Id, SymbolId>,
  // todo(dsherret): make this not an IndexMap
  symbols: AdditiveOnlyIndexMap<SymbolId, SymbolMut>,
  next_symbol_id: Cell<SymbolId>,
  re_exports: RefCell<Vec<String>>,
}

impl ModuleBuilder {
  pub fn new(module_id: ModuleId) -> Self {
    Self {
      module_id,
      swc_id_to_symbol_id: Default::default(),
      symbols: Default::default(),
      next_symbol_id: Default::default(),
      re_exports: Default::default(),
    }
  }

  pub fn ensure_default_export_symbol(
    &self,
    module_symbol: &SymbolMut,
    symbol_decl: SymbolDecl,
  ) -> SymbolId {
    if let Some(symbol_id) = module_symbol.get_export("default") {
      let default_export_symbol = self.symbols.get(&symbol_id).unwrap();
      default_export_symbol.add_decl(symbol_decl);
      symbol_id
    } else {
      let symbol_id = self.get_next_symbol_id();
      let symbol = SymbolMut::new(Symbol::new(
        self.module_id,
        symbol_id,
        Some(module_symbol.symbol_id()),
      ));
      let should_add_child =
        matches!(symbol_decl.kind, SymbolDeclKind::Definition(_));
      symbol.add_decl(symbol_decl);
      self.symbols.insert(symbol_id, symbol);
      module_symbol.add_export("default".to_string(), symbol_id);
      if should_add_child {
        module_symbol.add_child_id(symbol_id);
      }
      symbol_id
    }
  }

  pub fn create_new_symbol(&self, parent_id: SymbolId) -> &SymbolMut {
    self.create_new_symbol_inner(Some(parent_id))
  }

  pub fn create_new_symbol_for_root(&self) -> &SymbolMut {
    let symbol = self.create_new_symbol_inner(None);
    debug_assert_eq!(symbol.symbol_id(), SymbolId(0));
    symbol
  }

  fn create_new_symbol_inner(&self, parent_id: Option<SymbolId>) -> &SymbolMut {
    let symbol_id = self.get_next_symbol_id();
    self.symbols.insert(
      symbol_id,
      SymbolMut::new(Symbol::new(self.module_id, symbol_id, parent_id)),
    );
    self.symbols.get(&symbol_id).unwrap()
  }

  pub fn symbol_mut(&self, id: SymbolId) -> Option<&SymbolMut> {
    self.symbols.get(&id)
  }

  pub fn get_symbol_from_swc_id(
    &self,
    id: Id,
    symbol_decl: SymbolDecl,
    parent_id: SymbolId,
  ) -> &SymbolMut {
    let symbol_id = self.ensure_symbol_for_swc_id(id, symbol_decl, parent_id);
    self.symbols.get(&symbol_id).unwrap()
  }

  pub fn ensure_symbol_for_swc_id(
    &self,
    id: Id,
    symbol_decl: SymbolDecl,
    parent_id: SymbolId,
  ) -> SymbolId {
    match self.swc_id_to_symbol_id.get(&id) {
      Some(symbol_id) => {
        self.symbols.get(&symbol_id).unwrap().add_decl(symbol_decl);
        symbol_id
      }
      None => {
        let symbol_id = self.get_next_symbol_id();
        self.swc_id_to_symbol_id.insert(id, symbol_id);
        let symbol = SymbolMut::new(Symbol::new(
          self.module_id,
          symbol_id,
          Some(parent_id),
        ));
        symbol.add_decl(symbol_decl);
        self.symbols.insert(symbol_id, symbol);
        symbol_id
      }
    }
  }

  fn get_next_symbol_id(&self) -> SymbolId {
    let symbol_id = self.next_symbol_id.get();
    self.next_symbol_id.set(SymbolId(symbol_id.0 + 1));
    symbol_id
  }

  fn add_re_export(&self, value: String) {
    self.re_exports.borrow_mut().push(value)
  }
}

struct SymbolFiller<'a> {
  specifier: &'a ModuleSpecifier,
  source: &'a ParsedSource,
  diagnostics: RefCell<Vec<SymbolFillDiagnostic>>,
  builder: &'a ModuleBuilder,
}

impl<'a> SymbolFiller<'a> {
  fn fill(&self, module: &Module) {
    let mut last_was_overload = false;
    let mut last_override_key = 0;
    let module_symbol = self.builder.create_new_symbol_for_root();
    module_symbol.add_decl(SymbolDecl {
      kind: SymbolDeclKind::Definition(SymbolNode(SymbolNodeInner::Module(
        NodeRefBox::unsafe_new(self.source, module),
      ))),
      range: module.range(),
    });
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
        self.handle_module_item_fn_overload(module_item, module_symbol);
        continue; // don't fill the types on implementation signatures as they're private
      }

      self.fill_module_item(module_item, module_symbol);
    }
  }

  fn fill_module_item(
    &self,
    module_item: &ModuleItem,
    module_symbol: &SymbolMut,
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
                self.builder.ensure_symbol_for_swc_id(
                  n.local.to_id(),
                  SymbolDecl {
                    range: n.range(),
                    kind: SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Name(imported_name),
                      specifier: import_decl.src.value.to_string(),
                    }),
                  },
                  module_symbol.symbol_id(),
                );
              }
              ImportSpecifier::Default(n) => {
                self.builder.ensure_symbol_for_swc_id(
                  n.local.to_id(),
                  SymbolDecl {
                    range: n.range(),
                    kind: SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Name("default".to_string()),
                      specifier: import_decl.src.value.to_string(),
                    }),
                  },
                  module_symbol.symbol_id(),
                );
              }
              ImportSpecifier::Namespace(n) => {
                self.builder.ensure_symbol_for_swc_id(
                  n.local.to_id(),
                  SymbolDecl {
                    range: n.range(),
                    kind: SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Star,
                      specifier: import_decl.src.value.to_string(),
                    }),
                  },
                  module_symbol.symbol_id(),
                );
              }
            }
          }
        }
        ModuleDecl::ExportDecl(export_decl) => match &export_decl.decl {
          Decl::Class(n) => {
            let symbol = self.builder.get_symbol_from_swc_id(
              n.ident.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(self.source, export_decl),
                    SymbolNodeInnerExportDecl::Class(NodeRefBox::unsafe_new(
                      self.source,
                      n,
                    )),
                  ),
                )),
                range: export_decl.range(),
              },
              module_symbol.symbol_id(),
            );
            self.fill_class_decl(symbol, n);
            let symbol_id = symbol.symbol_id();
            module_symbol.add_export(n.ident.sym.to_string(), symbol_id);
            module_symbol.add_child_id(symbol_id);
          }
          Decl::Fn(n) => {
            let symbol = self.builder.get_symbol_from_swc_id(
              n.ident.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(self.source, export_decl),
                    SymbolNodeInnerExportDecl::Fn(NodeRefBox::unsafe_new(
                      self.source,
                      n,
                    )),
                  ),
                )),
                range: export_decl.range(),
              },
              module_symbol.symbol_id(),
            );
            self.fill_fn_decl(symbol, n);
            let symbol_id = symbol.symbol_id();
            module_symbol.add_export(n.ident.sym.to_string(), symbol_id);
            module_symbol.add_child_id(symbol_id);
          }
          Decl::Var(n) => {
            for decl in &n.decls {
              for ident in find_pat_ids::<_, Ident>(&decl.name) {
                let export_name = ident.sym.to_string();
                let id = ident.to_id();
                let symbol = self.builder.get_symbol_from_swc_id(
                  id.clone(),
                  SymbolDecl {
                    kind: SymbolDeclKind::Definition(SymbolNode(
                      SymbolNodeInner::ExportDecl(
                        NodeRefBox::unsafe_new(self.source, export_decl),
                        SymbolNodeInnerExportDecl::Var(
                          NodeRefBox::unsafe_new(self.source, n),
                          NodeRefBox::unsafe_new(self.source, decl),
                          ident,
                        ),
                      ),
                    )),
                    range: decl.range(),
                  },
                  module_symbol.symbol_id(),
                );
                self.fill_var_declarator(symbol, decl);
                module_symbol.add_child_id(symbol.symbol_id());
                module_symbol.add_export(export_name, symbol.symbol_id());
              }
            }
          }
          Decl::TsInterface(n) => {
            let symbol = self.builder.get_symbol_from_swc_id(
              n.id.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(self.source, export_decl),
                    SymbolNodeInnerExportDecl::TsInterface(
                      NodeRefBox::unsafe_new(self.source, n),
                    ),
                  ),
                )),
                range: export_decl.range(),
              },
              module_symbol.symbol_id(),
            );
            self.fill_ts_interface(symbol, n);
            let symbol_id = symbol.symbol_id();
            module_symbol.add_export(n.id.sym.to_string(), symbol_id);
            module_symbol.add_child_id(symbol_id);
          }
          Decl::TsTypeAlias(n) => {
            let symbol = self.builder.get_symbol_from_swc_id(
              n.id.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(self.source, export_decl),
                    SymbolNodeInnerExportDecl::TsTypeAlias(
                      NodeRefBox::unsafe_new(self.source, n),
                    ),
                  ),
                )),
                range: export_decl.range(),
              },
              module_symbol.symbol_id(),
            );
            self.fill_ts_type_alias(symbol, n);
            let symbol_id = symbol.symbol_id();
            module_symbol.add_export(n.id.sym.to_string(), symbol_id);
            module_symbol.add_child_id(symbol_id);
          }
          Decl::TsEnum(n) => {
            let symbol = self.builder.get_symbol_from_swc_id(
              n.id.to_id(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(self.source, export_decl),
                    SymbolNodeInnerExportDecl::TsEnum(NodeRefBox::unsafe_new(
                      self.source,
                      n,
                    )),
                  ),
                )),
                range: export_decl.range(),
              },
              module_symbol.symbol_id(),
            );
            self.fill_ts_enum(symbol, n);
            let symbol_id = symbol.symbol_id();
            module_symbol.add_export(n.id.sym.to_string(), symbol_id);
            module_symbol.add_child_id(symbol_id);
          }
          Decl::TsModule(n) => {
            let maybe_symbol_id = self.fill_ts_module(
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    NodeRefBox::unsafe_new(self.source, export_decl),
                    SymbolNodeInnerExportDecl::TsNamespace(
                      NodeRefBox::unsafe_new(self.source, n),
                    ),
                  ),
                )),
                range: export_decl.range(),
              },
              n,
              module_symbol,
            );
            if let Some(symbol_id) = maybe_symbol_id {
              match &n.id {
                TsModuleName::Ident(ident) => {
                  module_symbol.add_export(ident.sym.to_string(), symbol_id);
                  module_symbol.add_child_id(symbol_id);
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
                    let symbol =
                      self.builder.create_new_symbol(module_symbol.symbol_id());
                    symbol.add_decl(SymbolDecl {
                      range: named.range(),
                      kind: SymbolDeclKind::FileRef(FileDep {
                        name: FileDepName::Name(imported_name),
                        specifier: src.value.to_string(),
                      }),
                    });
                    let symbol_id = symbol.symbol_id();
                    module_symbol.add_export(export_name, symbol_id);
                  }
                  None => {
                    let orig_ident = match &named.orig {
                      ModuleExportName::Ident(ident) => ident,
                      ModuleExportName::Str(_) => unreachable!(),
                    };

                    // note: don't associate identifier exports with the swc id because
                    // we don't want the SymbolDeclKind appearing with the definition symbols
                    if let Some(exported_name) = &named.exported {
                      let exported_name = match exported_name {
                        ModuleExportName::Ident(ident) => ident.sym.to_string(),
                        ModuleExportName::Str(str) => str.value.to_string(),
                      };
                      let orig_symbol = self
                        .builder
                        .create_new_symbol(module_symbol.symbol_id());
                      orig_symbol.add_decl(SymbolDecl {
                        range: n.range(),
                        kind: SymbolDeclKind::Target(orig_ident.to_id()),
                      });
                      module_symbol
                        .add_export(exported_name, orig_symbol.symbol_id());
                    } else {
                      let named_symbol = self
                        .builder
                        .create_new_symbol(module_symbol.symbol_id());
                      named_symbol.add_decl(SymbolDecl {
                        range: n.range(),
                        kind: match &n.src {
                          Some(src) => SymbolDeclKind::FileRef(FileDep {
                            name: FileDepName::Name(orig_ident.sym.to_string()),
                            specifier: src.value.to_string(),
                          }),
                          None => SymbolDeclKind::Target(orig_ident.to_id()),
                        },
                      });
                      module_symbol.add_export(
                        orig_ident.sym.to_string(),
                        named_symbol.symbol_id(),
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
                  let symbol =
                    self.builder.create_new_symbol(module_symbol.symbol_id());
                  symbol.add_decl(SymbolDecl {
                    kind: SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Star,
                      specifier: src.value.to_string(),
                    }),
                    range: specifier.range(),
                  });
                  let symbol_id = symbol.symbol_id();
                  module_symbol.add_export(name, symbol_id);
                } else {
                  let name = match &specifier.name {
                    ModuleExportName::Ident(ident) => ident,
                    ModuleExportName::Str(_) => unreachable!(),
                  };
                  let symbol_kind = SymbolDeclKind::Target(name.to_id());
                  let symbol_id = self
                    .builder
                    .get_symbol_from_swc_id(
                      name.to_id(),
                      SymbolDecl {
                        kind: symbol_kind.clone(),
                        range: n.range(),
                      },
                      module_symbol.symbol_id(),
                    )
                    .symbol_id();
                  module_symbol.add_export(name.sym.to_string(), symbol_id);
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
          let default_export_symbol_id =
            self.builder.ensure_default_export_symbol(
              module_symbol,
              SymbolDecl {
                range: default_decl.range(),
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDefaultDecl(NodeRefBox::unsafe_new(
                    self.source,
                    default_decl,
                  )),
                )),
              },
            );
          let maybe_ident = match &default_decl.decl {
            DefaultDecl::Class(expr) => expr.ident.as_ref(),
            DefaultDecl::Fn(expr) => expr.ident.as_ref(),
            DefaultDecl::TsInterfaceDecl(decl) => Some(&decl.id),
          };
          if let Some(ident) = maybe_ident {
            let id = ident.to_id();
            self
              .builder
              .swc_id_to_symbol_id
              .insert(id, default_export_symbol_id);
          }

          let symbol =
            self.builder.symbol_mut(default_export_symbol_id).unwrap();
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
            module_symbol,
            expr.range(),
            &expr.expr,
            Some(expr),
          );
        }
        ModuleDecl::ExportAll(n) => {
          self.builder.add_re_export(n.src.value.to_string());
        }
        ModuleDecl::TsImportEquals(import_equals) => {
          let symbol_id =
            self.ensure_symbol_for_import_equals(import_equals, module_symbol);
          let symbol = self.builder.symbol_mut(symbol_id).unwrap();
          match &import_equals.module_ref {
            TsModuleRef::TsEntityName(entity_name) => {
              let (leftmost_id, parts) = ts_entity_name_to_parts(entity_name);
              symbol.add_dep(SymbolDep::QualifiedId(leftmost_id, parts));
            }
            TsModuleRef::TsExternalModuleRef(_) => {
              // don't need to do anything in this case
            }
          }
          if import_equals.is_export {
            module_symbol
              .add_export(import_equals.id.sym.to_string(), symbol_id);
          }
        }
        ModuleDecl::TsExportAssignment(export_assignment) => {
          self.handle_export_default_expr(
            module_symbol,
            export_assignment.range(),
            &export_assignment.expr,
            None,
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
              let symbol = self.builder.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::ClassDecl(NodeRefBox::unsafe_new(
                      self.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
                module_symbol.symbol_id(),
              );
              self.fill_class_decl(symbol, n);
              let symbol_id = symbol.symbol_id();
              module_symbol.add_child_id(symbol_id);
            }
            Decl::Fn(n) => {
              let id = n.ident.to_id();
              let symbol = self.builder.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::FnDecl(NodeRefBox::unsafe_new(
                      self.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
                module_symbol.symbol_id(),
              );
              self.fill_fn_decl(symbol, n);
              let symbol_id = symbol.symbol_id();
              module_symbol.add_child_id(symbol_id);
            }
            Decl::Var(var_decl) => {
              for decl in &var_decl.decls {
                for ident in find_pat_ids::<_, Ident>(&decl.name) {
                  let symbol = self.builder.get_symbol_from_swc_id(
                    ident.to_id(),
                    SymbolDecl {
                      kind: SymbolDeclKind::Definition(SymbolNode(
                        SymbolNodeInner::Var(
                          NodeRefBox::unsafe_new(self.source, var_decl),
                          NodeRefBox::unsafe_new(self.source, decl),
                          ident,
                        ),
                      )),
                      range: decl.range(),
                    },
                    module_symbol.symbol_id(),
                  );
                  self.fill_var_declarator(symbol, decl);
                  let symbol_id = symbol.symbol_id();
                  module_symbol.add_child_id(symbol_id);
                }
              }
            }
            Decl::TsInterface(n) => {
              let id = n.id.to_id();
              let symbol = self.builder.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::TsInterface(NodeRefBox::unsafe_new(
                      self.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
                module_symbol.symbol_id(),
              );
              self.fill_ts_interface(symbol, n);
              let symbol_id = symbol.symbol_id();
              module_symbol.add_child_id(symbol_id);
            }
            Decl::TsTypeAlias(n) => {
              let id = n.id.to_id();
              let symbol = self.builder.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::TsTypeAlias(NodeRefBox::unsafe_new(
                      self.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
                module_symbol.symbol_id(),
              );
              self.fill_ts_type_alias(symbol, n);
              let symbol_id = symbol.symbol_id();
              module_symbol.add_child_id(symbol_id);
            }
            Decl::TsEnum(n) => {
              let id = n.id.to_id();
              let symbol = self.builder.get_symbol_from_swc_id(
                id,
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::TsEnum(NodeRefBox::unsafe_new(
                      self.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
                module_symbol.symbol_id(),
              );
              self.fill_ts_enum(symbol, n);
              let symbol_id = symbol.symbol_id();
              module_symbol.add_child_id(symbol_id);
            }
            Decl::TsModule(n) => {
              let symbol_id = self.fill_ts_module(
                SymbolDecl {
                  kind: SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::TsNamespace(NodeRefBox::unsafe_new(
                      self.source,
                      n,
                    )),
                  )),
                  range: n.range(),
                },
                n,
                module_symbol,
              );
              if let Some(symbol_id) = symbol_id {
                module_symbol.add_child_id(symbol_id);
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
    module_symbol: &SymbolMut,
    default_export_range: SourceRange,
    expr: &Expr,
    maybe_parent: Option<&ExportDefaultExpr>,
  ) {
    match expr {
      Expr::Ident(ident) => {
        // note: don't associate identifier exports with the swc id because
        // we don't want the SymbolDeclKind appearing with the definition symbols
        self.builder.ensure_default_export_symbol(
          module_symbol,
          SymbolDecl {
            kind: SymbolDeclKind::Target(ident.to_id()),
            range: ident.range(),
          },
        );
      }
      Expr::Lit(lit) => {
        debug_assert!(maybe_parent.is_some());
        let Some(parent) = maybe_parent else {
          return;
        };
        self.builder.ensure_default_export_symbol(
          module_symbol,
          SymbolDecl {
            kind: SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::ExportDefaultExprLit(
                NodeRefBox::unsafe_new(self.source, parent),
                NodeRefBox::unsafe_new(self.source, lit),
              ),
            )),
            range: default_export_range,
          },
        );
      }
      _ => {
        let line_and_column = self
          .source
          .text_info()
          .line_and_column_display(default_export_range.start);
        self.add_diagnostic(SymbolFillDiagnostic {
          kind: SymbolFillDiagnosticKind::UnsupportedDefaultExpr,
          specifier: self.specifier.clone(),
          line_and_column: Some(line_and_column),
        });
      }
    }
  }

  fn ensure_symbol_for_import_equals(
    &self,
    import_equals: &TsImportEqualsDecl,
    parent_symbol: &SymbolMut,
  ) -> SymbolId {
    let id = import_equals.id.to_id();
    if let Some(symbol_id) = self.builder.swc_id_to_symbol_id.get(&id) {
      return symbol_id;
    }
    match &import_equals.module_ref {
      TsModuleRef::TsEntityName(entity_name) => {
        let (leftmost_id, parts) = ts_entity_name_to_parts(entity_name);
        self.builder.ensure_symbol_for_swc_id(
          id,
          SymbolDecl {
            kind: SymbolDeclKind::QualifiedTarget(leftmost_id, parts),
            range: import_equals.range(),
          },
          parent_symbol.symbol_id(),
        )
      }
      TsModuleRef::TsExternalModuleRef(module_ref) => {
        self.builder.ensure_symbol_for_swc_id(
          id,
          SymbolDecl {
            kind: SymbolDeclKind::FileRef(FileDep {
              name: FileDepName::Name("default".to_string()),
              specifier: module_ref.expr.value.to_string(),
            }),
            range: import_equals.range(),
          },
          parent_symbol.symbol_id(),
        )
      }
    }
  }

  fn fill_class_decl(&self, symbol: &SymbolMut, n: &ClassDecl) {
    self.fill_class(symbol, &n.class);
  }

  fn fill_class(&self, symbol: &SymbolMut, n: &Class) {
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

  fn fill_var_declarator(&self, symbol: &SymbolMut, n: &VarDeclarator) {
    self.fill_pat(symbol, &n.name);
  }

  fn fill_fn_decl(&self, symbol: &SymbolMut, n: &FnDecl) {
    self.fill_function_decl(symbol, &n.function);
  }

  fn fill_function_decl(&self, symbol: &SymbolMut, n: &Function) {
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

  fn fill_ts_interface(&self, symbol: &SymbolMut, n: &TsInterfaceDecl) {
    if let Some(type_params) = &n.type_params {
      self.fill_ts_type_param_decl(symbol, type_params);
    }
    for extends in &n.extends {
      self.fill_ts_expr_with_type_args(symbol, extends);
    }

    for member in &n.body.body {
      match member {
        TsTypeElement::TsCallSignatureDecl(n) => {
          self.fill_ts_call_signature_decl(symbol, n);
        }
        TsTypeElement::TsConstructSignatureDecl(n) => {
          self.fill_ts_construct_signature(symbol, n);
        }
        TsTypeElement::TsPropertySignature(n) => {
          self.fill_ts_property_signature(symbol, n);
        }
        TsTypeElement::TsIndexSignature(n) => {
          self.fill_ts_index_signature(symbol, n);
        }
        TsTypeElement::TsMethodSignature(n) => {
          self.fill_ts_method_signature(symbol, n);
        }
        TsTypeElement::TsGetterSignature(n) => {
          self.fill_ts_getter_signature(symbol, n);
        }
        TsTypeElement::TsSetterSignature(n) => {
          self.fill_ts_setter_signature(symbol, n);
        }
      }
    }
  }

  fn fill_ts_type_alias(&self, symbol: &SymbolMut, n: &TsTypeAliasDecl) {
    if let Some(type_params) = &n.type_params {
      self.fill_ts_type_param_decl(symbol, type_params);
    }
    self.fill_ts_type(symbol, &n.type_ann)
  }

  fn fill_ts_enum(&self, symbol: &SymbolMut, n: &TsEnumDecl) {
    for member in &n.members {
      if let Some(init) = &member.init {
        self.fill_expr(symbol, init);
      }
    }
  }

  fn fill_ts_module(
    &self,
    symbol_decl: SymbolDecl,
    n: &TsModuleDecl,
    parent_symbol: &SymbolMut,
  ) -> Option<SymbolId> {
    let mut id = match &n.id {
      TsModuleName::Ident(ident) => ident.to_id(),
      TsModuleName::Str(_) => return None, // ignore for now
    };
    let mod_symbol = self.builder.get_symbol_from_swc_id(
      id.clone(),
      symbol_decl,
      parent_symbol.symbol_id(),
    );

    // fill the exported declarations
    if let Some(body) = &n.body {
      let mut mod_symbol = mod_symbol;
      let mut current = body;
      let block = loop {
        match current {
          TsNamespaceBody::TsModuleBlock(block) => break block,
          TsNamespaceBody::TsNamespaceDecl(decl) => {
            let previous_symbol = mod_symbol;
            let previous_id = id;
            id = decl.id.to_id();
            mod_symbol = self.builder.get_symbol_from_swc_id(
              id.clone(),
              SymbolDecl {
                kind: SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::TsNamespace(NodeRefBox::unsafe_new(
                    self.source,
                    n,
                  )),
                )),
                range: decl.range(),
              },
              previous_symbol.symbol_id(),
            );
            previous_symbol.add_dep(id.clone().into());
            previous_symbol
              .add_export(id.0.to_string(), mod_symbol.symbol_id());
            previous_symbol.add_child_id(mod_symbol.symbol_id());
            mod_symbol.add_dep(previous_id.into());
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
          self.handle_module_item_fn_overload(item, mod_symbol);
          continue; // don't fill the types on implementation signatures as they're private
        }

        self.fill_module_item(item, mod_symbol);
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
                mod_symbol.add_dep(id.into());
              }
              Decl::Fn(n) => {
                mod_symbol.add_dep(n.ident.to_id().into());
              }
              Decl::Var(n) => {
                for decl in &n.decls {
                  for ident in find_pat_ids::<_, Ident>(&decl.name) {
                    let id = ident.to_id();
                    mod_symbol.add_dep(id.into());
                  }
                }
              }
              Decl::TsInterface(n) => {
                let id = n.id.to_id();
                mod_symbol.add_dep(id.into());
              }
              Decl::TsTypeAlias(n) => {
                let id = n.id.to_id();
                mod_symbol.add_dep(id.into());
              }
              Decl::TsEnum(n) => {
                let id = n.id.to_id();
                mod_symbol.add_dep(id.into());
              }
              Decl::TsModule(n) => match &n.id {
                TsModuleName::Ident(ident) => {
                  let id = ident.to_id();
                  mod_symbol.add_dep(id.into());
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
    Some(mod_symbol.symbol_id())
  }

  fn handle_module_item_fn_overload(
    &self,
    module_item: &ModuleItem,
    module_symbol: &SymbolMut,
  ) {
    // For function implementations with overloads, only add them
    // to the list of declarations, but don't examine their types
    // as the types they use are private.
    let (id, symbol_node, range) = match module_item {
      ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export_decl)) => {
        let fn_decl = match &export_decl.decl {
          Decl::Fn(n) => n,
          _ => unreachable!(),
        };
        (
          fn_decl.ident.to_id(),
          SymbolNodeInner::ExportDecl(
            NodeRefBox::unsafe_new(self.source, export_decl),
            SymbolNodeInnerExportDecl::Fn(NodeRefBox::unsafe_new(
              self.source,
              fn_decl,
            )),
          ),
          module_item.range(),
        )
      }
      ModuleItem::Stmt(Stmt::Decl(Decl::Fn(n))) => (
        n.ident.to_id(),
        SymbolNodeInner::FnDecl(NodeRefBox::unsafe_new(self.source, n)),
        n.range(),
      ),
      _ => unreachable!(), // shouldn't happen
    };

    // include the implementation signature in the collection of declarations
    self.builder.ensure_symbol_for_swc_id(
      id,
      SymbolDecl {
        kind: SymbolDeclKind::DefinitionPrivateFnImpl(SymbolNode(symbol_node)),
        range,
      },
      module_symbol.symbol_id(),
    );
  }

  fn fill_ts_expr_with_type_args(
    &self,
    symbol: &SymbolMut,
    n: &TsExprWithTypeArgs,
  ) {
    if let Some(type_args) = &n.type_args {
      self.fill_ts_type_param_instantiation(symbol, type_args);
    }
    self.fill_expr(symbol, &n.expr);
  }

  fn fill_ts_type_param_decl(
    &self,
    symbol: &SymbolMut,
    type_params: &TsTypeParamDecl,
  ) {
    for param in &type_params.params {
      self.fill_ts_type_param(symbol, param);
    }
  }

  fn fill_ts_type_param(&self, symbol: &SymbolMut, param: &TsTypeParam) {
    if let Some(constraint) = &param.constraint {
      self.fill_ts_type(symbol, constraint);
    }
    if let Some(default) = &param.default {
      self.fill_ts_type(symbol, default);
    }
  }

  fn fill_ts_type_param_instantiation(
    &self,
    symbol: &SymbolMut,
    type_params: &TsTypeParamInstantiation,
  ) {
    for param in &type_params.params {
      self.fill_ts_type(symbol, param);
    }
  }

  fn fill_expr(&self, symbol: &SymbolMut, n: &Expr) {
    let mut visitor = SymbolFillVisitor { symbol };
    n.visit_with(&mut visitor);
  }

  fn fill_ts_class_members(&self, symbol: &SymbolMut, members: &[ClassMember]) {
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
        ClassMember::PrivateMethod(_) => {
          // do nothing, private
        }
        ClassMember::ClassProp(prop) => self.fill_class_prop(symbol, prop),
        ClassMember::PrivateProp(_) => {
          // do nothing, private properties are not emitted with their type
        }
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

  fn fill_ctor(&self, symbol: &SymbolMut, ctor: &Constructor) {
    if ctor.accessibility == Some(Accessibility::Private) {
      return; // ignore, private
    }

    let member = self
      .get_symbol_member_or_export(symbol, SymbolNodeRef::Constructor(ctor));

    for param in &ctor.params {
      match param {
        ParamOrTsParamProp::TsParamProp(param) => {
          self.fill_ts_param_prop(member, param)
        }
        ParamOrTsParamProp::Param(param) => self.fill_param(member, param),
      }
    }
  }

  fn fill_method(&self, symbol: &SymbolMut, method: &ClassMethod) {
    if method.accessibility == Some(Accessibility::Private) {
      return; // ignore, private
    }

    let member = self
      .get_symbol_member_or_export(symbol, SymbolNodeRef::ClassMethod(method));

    self.fill_prop_name(member, &method.key);
    if let Some(type_params) = &method.function.type_params {
      self.fill_ts_type_param_decl(member, type_params)
    }
    for param in &method.function.params {
      self.fill_param(member, param)
    }
    if let Some(return_type) = &method.function.return_type {
      self.fill_ts_type_ann(member, return_type)
    }
  }

  fn fill_prop_name(&self, symbol: &SymbolMut, key: &PropName) {
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

  fn fill_ts_param_prop(&self, symbol: &SymbolMut, param: &TsParamProp) {
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

  fn fill_param(&self, symbol: &SymbolMut, param: &Param) {
    self.fill_pat(symbol, &param.pat);
  }

  fn fill_pat(&self, symbol: &SymbolMut, pat: &Pat) {
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

  fn fill_ts_type_ann(&self, symbol: &SymbolMut, type_ann: &TsTypeAnn) {
    self.fill_ts_type(symbol, &type_ann.type_ann)
  }

  fn fill_ts_type(&self, symbol: &SymbolMut, n: &TsType) {
    let mut visitor = SymbolFillVisitor { symbol };
    n.visit_with(&mut visitor);
  }

  fn fill_class_prop(&self, symbol: &SymbolMut, prop: &ClassProp) {
    if prop.accessibility == Some(Accessibility::Private) {
      return; // ignore, private
    }

    let member =
      self.get_symbol_member_or_export(symbol, SymbolNodeRef::ClassProp(prop));

    if let Some(type_ann) = &prop.type_ann {
      self.fill_ts_type_ann(member, type_ann)
    }
  }

  fn fill_ts_call_signature_decl(
    &self,
    symbol: &SymbolMut,
    n: &TsCallSignatureDecl,
  ) {
    let member = self.get_symbol_member_or_export(
      symbol,
      SymbolNodeRef::TsCallSignatureDecl(n),
    );

    if let Some(type_params) = &n.type_params {
      self.fill_ts_type_param_decl(member, type_params);
    }
    for param in &n.params {
      self.fill_ts_fn_param(member, param);
    }
    if let Some(type_ann) = &n.type_ann {
      self.fill_ts_type_ann(member, type_ann)
    }
  }

  fn fill_ts_construct_signature(
    &self,
    symbol: &SymbolMut,
    n: &TsConstructSignatureDecl,
  ) {
    let member = self.get_symbol_member_or_export(
      symbol,
      SymbolNodeRef::TsConstructSignatureDecl(n),
    );

    if let Some(type_params) = &n.type_params {
      self.fill_ts_type_param_decl(member, type_params);
    }
    for param in &n.params {
      self.fill_ts_fn_param(member, param);
    }
    if let Some(type_ann) = &n.type_ann {
      self.fill_ts_type_ann(member, type_ann)
    }
  }

  fn fill_ts_property_signature(
    &self,
    symbol: &SymbolMut,
    n: &TsPropertySignature,
  ) {
    let member = self.get_symbol_member_or_export(
      symbol,
      SymbolNodeRef::TsPropertySignature(n),
    );

    if let Some(init) = &n.init {
      self.fill_expr(member, init);
    }
    if let Some(type_params) = &n.type_params {
      self.fill_ts_type_param_decl(member, type_params);
    }
    for param in &n.params {
      self.fill_ts_fn_param(member, param);
    }
    if let Some(type_ann) = &n.type_ann {
      self.fill_ts_type_ann(member, type_ann)
    }
  }

  fn fill_ts_index_signature(&self, symbol: &SymbolMut, n: &TsIndexSignature) {
    let member = self
      .get_symbol_member_or_export(symbol, SymbolNodeRef::TsIndexSignature(n));
    for param in &n.params {
      self.fill_ts_fn_param(member, param)
    }
    if let Some(type_ann) = &n.type_ann {
      self.fill_ts_type_ann(member, type_ann)
    }
  }

  fn fill_ts_method_signature(
    &self,
    symbol: &SymbolMut,
    n: &TsMethodSignature,
  ) {
    let member = self
      .get_symbol_member_or_export(symbol, SymbolNodeRef::TsMethodSignature(n));

    if let Some(type_params) = &n.type_params {
      self.fill_ts_type_param_decl(member, type_params);
    }
    for param in &n.params {
      self.fill_ts_fn_param(member, param)
    }
    if let Some(type_ann) = &n.type_ann {
      self.fill_ts_type_ann(member, type_ann)
    }
  }

  fn fill_ts_getter_signature(
    &self,
    symbol: &SymbolMut,
    n: &TsGetterSignature,
  ) {
    let member = self
      .get_symbol_member_or_export(symbol, SymbolNodeRef::TsGetterSignature(n));

    if let Some(type_ann) = &n.type_ann {
      self.fill_ts_type_ann(member, type_ann)
    }
  }

  fn fill_ts_setter_signature(
    &self,
    symbol: &SymbolMut,
    n: &TsSetterSignature,
  ) {
    let member = self
      .get_symbol_member_or_export(symbol, SymbolNodeRef::TsSetterSignature(n));

    self.fill_ts_fn_param(member, &n.param);
  }

  fn fill_auto_accessor(&self, symbol: &SymbolMut, n: &AutoAccessor) {
    match &n.key {
      Key::Private(_) => {
        return; // ignore, private
      }
      Key::Public(_) => {}
    }

    let member =
      self.get_symbol_member_or_export(symbol, SymbolNodeRef::AutoAccessor(n));
    if let Some(type_ann) = &n.type_ann {
      self.fill_ts_type_ann(member, type_ann)
    }
  }

  fn fill_ts_fn_param(&self, symbol: &SymbolMut, param: &TsFnParam) {
    let mut visitor = SymbolFillVisitor { symbol };
    param.visit_with(&mut visitor);
  }

  fn has_internal_jsdoc(&self, pos: SourcePos) -> bool {
    has_internal_jsdoc(self.source, pos)
  }

  fn get_symbol_member_or_export<'b>(
    &'b self,
    parent_symbol: &'b SymbolMut,
    node_ref: SymbolNodeRef,
  ) -> &'b SymbolMut {
    let (node_inner, is_static, source_range) = match node_ref {
      SymbolNodeRef::AutoAccessor(n) => (
        SymbolNodeInner::AutoAccessor(NodeRefBox::unsafe_new(self.source, n)),
        n.is_static,
        n.range(),
      ),

      SymbolNodeRef::ClassMethod(n) => (
        SymbolNodeInner::ClassMethod(NodeRefBox::unsafe_new(self.source, n)),
        n.is_static,
        n.range(),
      ),

      SymbolNodeRef::ClassProp(n) => (
        SymbolNodeInner::ClassProp(NodeRefBox::unsafe_new(self.source, n)),
        n.is_static,
        n.range(),
      ),

      SymbolNodeRef::Constructor(n) => (
        SymbolNodeInner::Constructor(NodeRefBox::unsafe_new(self.source, n)),
        true,
        n.range(),
      ),

      SymbolNodeRef::TsIndexSignature(n) => (
        SymbolNodeInner::TsIndexSignature(NodeRefBox::unsafe_new(
          self.source,
          n,
        )),
        n.is_static,
        n.range(),
      ),

      SymbolNodeRef::TsCallSignatureDecl(n) => (
        SymbolNodeInner::TsCallSignatureDecl(NodeRefBox::unsafe_new(
          self.source,
          n,
        )),
        false,
        n.range(),
      ),

      SymbolNodeRef::TsConstructSignatureDecl(n) => (
        SymbolNodeInner::TsConstructSignatureDecl(NodeRefBox::unsafe_new(
          self.source,
          n,
        )),
        false,
        n.range(),
      ),

      SymbolNodeRef::TsPropertySignature(n) => (
        SymbolNodeInner::TsPropertySignature(NodeRefBox::unsafe_new(
          self.source,
          n,
        )),
        false,
        n.range(),
      ),
      SymbolNodeRef::TsGetterSignature(n) => (
        SymbolNodeInner::TsGetterSignature(NodeRefBox::unsafe_new(
          self.source,
          n,
        )),
        false,
        n.range(),
      ),

      SymbolNodeRef::TsSetterSignature(n) => (
        SymbolNodeInner::TsSetterSignature(NodeRefBox::unsafe_new(
          self.source,
          n,
        )),
        false,
        n.range(),
      ),

      SymbolNodeRef::TsMethodSignature(n) => (
        SymbolNodeInner::TsMethodSignature(NodeRefBox::unsafe_new(
          self.source,
          n,
        )),
        false,
        n.range(),
      ),
      SymbolNodeRef::Module(_)
      | SymbolNodeRef::ClassDecl(_)
      | SymbolNodeRef::ExportDecl(_, _)
      | SymbolNodeRef::ExportDefaultDecl(_)
      | SymbolNodeRef::ExportDefaultExprLit(_, _)
      | SymbolNodeRef::FnDecl(_)
      | SymbolNodeRef::TsEnum(_)
      | SymbolNodeRef::TsInterface(_)
      | SymbolNodeRef::TsNamespace(_)
      | SymbolNodeRef::TsTypeAlias(_)
      | SymbolNodeRef::Var(_, _, _) => unreachable!(),
    };

    let decl = SymbolDecl {
      kind: SymbolDeclKind::Definition(SymbolNode(node_inner)),
      range: source_range,
    };

    let child_symbol =
      if let Some(decl_name) = decl.maybe_name().map(|n| n.to_string()) {
        // static members on a class are children and an export if they have a name
        // while instance members are "members"
        if is_static {
          match parent_symbol.get_export(&decl_name) {
            Some(symbol_id) => self.builder.symbol_mut(symbol_id).unwrap(),
            None => {
              if let Some(child_symbol) =
                self.get_child_symbol_with_name(parent_symbol, &decl_name)
              {
                child_symbol
              } else {
                let child_symbol =
                  self.builder.create_new_symbol(parent_symbol.symbol_id());
                parent_symbol.add_export(decl_name, child_symbol.symbol_id());
                parent_symbol.add_child_id(child_symbol.symbol_id());
                child_symbol
              }
            }
          }
        } else if let Some(child_symbol) =
          self.get_member_symbol_with_name(parent_symbol, &decl_name)
        {
          child_symbol
        } else {
          let child_symbol =
            self.builder.create_new_symbol(parent_symbol.symbol_id());
          parent_symbol.add_member(child_symbol.symbol_id());
          child_symbol
        }
      } else if is_static {
        let child_symbol =
          self.builder.create_new_symbol(parent_symbol.symbol_id());
        parent_symbol.add_child_id(child_symbol.symbol_id());
        child_symbol
      } else {
        let child_symbol =
          self.builder.create_new_symbol(parent_symbol.symbol_id());
        parent_symbol.add_member(child_symbol.symbol_id());
        child_symbol
      };

    child_symbol.add_decl(decl);
    child_symbol
  }

  fn get_child_symbol_with_name<'b>(
    &'b self,
    parent_symbol: &'b SymbolMut,
    searching_name: &str,
  ) -> Option<&'b SymbolMut> {
    let parent_symbol = parent_symbol.borrow_inner();
    for child_id in parent_symbol.child_ids() {
      let child_symbol_mut = self.builder.symbol_mut(child_id).unwrap();
      let child_symbol = child_symbol_mut.borrow_inner();
      if let Some(name) = child_symbol.maybe_name() {
        if name == searching_name {
          return Some(child_symbol_mut);
        }
      }
    }
    None
  }

  fn get_member_symbol_with_name<'b>(
    &'b self,
    parent_symbol: &'b SymbolMut,
    searching_name: &str,
  ) -> Option<&'b SymbolMut> {
    let parent_symbol = parent_symbol.borrow_inner();
    for member_id in parent_symbol.members() {
      let child_symbol_mut = self.builder.symbol_mut(*member_id).unwrap();
      let child_symbol = child_symbol_mut.borrow_inner();
      if let Some(name) = child_symbol.maybe_name() {
        if name == searching_name {
          return Some(child_symbol_mut);
        }
      }
    }
    None
  }

  fn add_diagnostic(&self, diagnostic: SymbolFillDiagnostic) {
    self.diagnostics.borrow_mut().push(diagnostic);
  }
}

struct SymbolFillVisitor<'a> {
  symbol: &'a SymbolMut,
}

impl<'a> Visit for SymbolFillVisitor<'a> {
  fn visit_ident(&mut self, n: &Ident) {
    let id = n.to_id();
    self.symbol.add_dep(id.into());
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
      .add_dep(SymbolDep::ImportType(n.arg.value.to_string(), parts));
    n.type_args.visit_with(self);
  }

  fn visit_ts_qualified_name(&mut self, n: &TsQualifiedName) {
    let (id, parts) = ts_qualified_name_parts(n);
    self.symbol.add_dep(SymbolDep::QualifiedId(id, parts));
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
