// Copyright 2018-2024 the Deno authors. MIT license.

use std::borrow::Cow;
use std::cell::Cell;
use std::cell::Ref;
use std::cell::RefCell;
use std::hash::Hash;

use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::ParsedSource;
use deno_ast::SourceTextInfo;
use deno_ast::Comment;
use deno_ast::oxc::ast::ast::*;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Span;
use deno_ast::oxc::syntax::identifier::is_identifier_name as is_valid_ident;
use indexmap::IndexMap;
use indexmap::IndexSet;

use super::helpers::Id;
use super::helpers::ToId;

use crate::JsModule;
use crate::JsonModule;
use crate::ModuleGraph;
use crate::ast::EsParser;
use crate::ast::ParseOptions;
use crate::graph::WasmModule;

use super::ResolvedSymbolDepEntry;
use super::SymbolNodeDep;
use super::collections::AdditiveOnlyIndexMap;
use super::collections::AdditiveOnlyIndexMapForCopyValues;
use super::collections::AdditiveOnlyMap;
use super::collections::AdditiveOnlyMapForCopyValues;
use super::cross_module;
use super::cross_module::Definition;
use super::cross_module::DefinitionOrUnresolved;
use super::cross_module::DefinitionPathNode;
use super::cross_module::ModuleExports;
use super::dep_analyzer::ResolveDepsMode;
use super::helpers::ts_entity_name_to_parts;

/// The root symbol from which module symbols can be retrieved.
///
/// Building the symbols for modules is lazy.
pub struct RootSymbol<'a> {
  module_graph: &'a ModuleGraph,
  parser: &'a dyn EsParser,
  allocator: &'a deno_ast::oxc::allocator::Allocator,
  specifiers_to_ids: AdditiveOnlyMapForCopyValues<ModuleSpecifier, ModuleId>,
  ids_to_modules: AdditiveOnlyMap<ModuleId, ModuleInfo<'a>>,
  /// Stores parsed sources so they live long enough for symbol references.
  /// Uses UnsafeCell + Vec<Box<_>> to provide stable `&'a` references.
  parsed_sources: std::cell::UnsafeCell<Vec<Box<ParsedSource<'a>>>>,
}

impl<'a> RootSymbol<'a> {
  pub fn new(
    module_graph: &'a ModuleGraph,
    parser: &'a dyn EsParser,
    allocator: &'a deno_ast::oxc::allocator::Allocator,
  ) -> Self {
    Self {
      module_graph,
      parser,
      allocator,
      specifiers_to_ids: Default::default(),
      ids_to_modules: Default::default(),
      parsed_sources: Default::default(),
    }
  }

  /// Store a ParsedSource and return a reference that lives for 'a.
  /// Safety: The Box provides heap stability, and we never remove entries,
  /// so the reference remains valid for the lifetime of RootSymbol.
  fn store_parsed_source(&self, source: ParsedSource<'a>) -> &'a ParsedSource<'a> {
    let sources = unsafe { &mut *self.parsed_sources.get() };
    sources.push(Box::new(source));
    let boxed = sources.last().unwrap();
    // SAFETY: The Box is heap-allocated and stable. We never remove from the Vec
    // or replace entries, so this reference is valid as long as `self` lives.
    // Since `self` is `&RootSymbol<'a>` and the caller ensures RootSymbol lives
    // for 'a, this is safe.
    unsafe { &*(boxed.as_ref() as *const ParsedSource<'a>) }
  }

  /// Checks if a specifier has been analyzed before.
  ///
  /// This does not lazily analyze the module.
  pub fn has_analyzed(&self, specifier: &ModuleSpecifier) -> bool {
    self.specifiers_to_ids.contains_key(specifier)
  }

  /// Gets a module from the provided specifier. This will lazily analyze
  /// the module if it has not already been analyzed.
  pub fn module_from_specifier(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ModuleInfoRef<'_>> {
    if let Some(module_id) = self.specifiers_to_ids.get(specifier) {
      let module_symbol = self.ids_to_modules.get(&module_id).unwrap();
      return Some(module_symbol.as_ref());
    }

    let graph_module = self.module_graph.get(specifier)?;

    match graph_module {
      crate::Module::Js(js_module) => js_module
        .maybe_types_dependency
        .as_ref()
        .and_then(|types| {
          types.dependency.maybe_specifier().and_then(|specifier| {
            // shouldn't happen, but prevent circular loops
            if specifier != &js_module.specifier {
              self.module_from_specifier(specifier)
            } else {
              None
            }
          })
        })
        .or_else(|| self.analyze_js_module(js_module)),
      crate::Module::Json(json_module) => {
        Some(self.analyze_json_module(json_module))
      }
      crate::Module::Wasm(wasm_module) => self.analyze_wasm_module(wasm_module),
      crate::Module::Npm(_)
      | crate::Module::Node(_)
      | crate::Module::External(_) => None,
    }
  }

  pub fn module_from_id(
    &self,
    module_id: ModuleId,
  ) -> Option<ModuleInfoRef<'_>> {
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
  ) -> Vec<DefinitionPathNode<'b>> {
    debug_assert_eq!(symbol.module_id(), module.module_id());
    super::cross_module::find_definition_paths(
      self.module_graph,
      module,
      symbol,
      &|specifier| self.module_from_specifier(specifier),
    )
  }

  pub fn resolve_symbol_dep<'b>(
    &'b self,
    module: ModuleInfoRef<'b>,
    dep: &SymbolNodeDep,
  ) -> Vec<ResolvedSymbolDepEntry<'b>> {
    super::cross_module::resolve_symbol_dep(
      self.module_graph,
      module,
      dep,
      &|specifier| self.module_from_specifier(specifier),
    )
  }

  fn analyze_js_module(
    &self,
    script_module: &JsModule,
  ) -> Option<ModuleInfoRef<'_>> {
    let Ok(source) = self.parsed_source(script_module) else {
      return None;
    };
    let source = self.store_parsed_source(source);
    Some(self.build_raw_es_module_info(&script_module.specifier, source))
  }

  fn analyze_json_module(&self, json_module: &JsonModule) -> ModuleInfoRef<'_> {
    let specifier = &json_module.specifier;
    // it's not ideal having to use SourceTextInfo here, but it makes
    // it easier to interop with ParsedSource
    let source_text_info = SourceTextInfo::new(json_module.source.text.clone());
    let text_len = source_text_info.text_str().len();
    let module_id = ModuleId(self.ids_to_modules.len() as u32);
    let decls = {
      let span = {
        let source = source_text_info.text_str();
        let start_whitespace_len = source.len() - source.trim_start().len();
        let end_whitespace_len = source.len() - source.trim_end().len();
        Span::new(
          start_whitespace_len as u32,
          (text_len - end_whitespace_len) as u32,
        )
      };
      Vec::from([SymbolDecl::new(
        SymbolDeclKind::Definition(SymbolNode(SymbolNodeInner::Json)),
        span,
      )])
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
        members: Default::default(),
      },
      default_symbol: Symbol {
        module_id,
        symbol_id: SymbolId(1),
        parent_id: Some(SymbolId(0)),
        decls,
        child_ids: Default::default(),
        exports: Default::default(),
        members: Default::default(),
      },
      source_text_info,
    };
    self.finalize_insert(ModuleInfo::Json(Box::new(module_symbol)))
  }

  fn analyze_wasm_module(
    &self,
    wasm_module: &WasmModule,
  ) -> Option<ModuleInfoRef<'_>> {
    let maybe_parsed_source =
      self.parser.parse_program(self.allocator, ParseOptions {
        specifier: &wasm_module.specifier,
        source: wasm_module.source_dts.clone(),
        media_type: MediaType::Dmts,
        scope_analysis: true,
      });
    let Ok(source) = maybe_parsed_source else {
      return None;
    };
    let source = self.store_parsed_source(source);
    Some(self.build_raw_es_module_info(&wasm_module.specifier, source))
  }

  fn build_raw_es_module_info(
    &self,
    specifier: &ModuleSpecifier,
    source: &'a ParsedSource<'a>,
  ) -> ModuleInfoRef<'_> {
    let program = source.program();

    let module_id = ModuleId(self.ids_to_modules.len() as u32);
    let filler = SymbolFiller {
      is_declaration: source.media_type().is_declaration(),
      builder: ModuleBuilder::new(module_id),
    };
    filler.fill(program);
    let builder = filler.builder;
    let module_symbol = EsModuleInfo {
      specifier: specifier.clone(),
      module_id,
      source_text: source.text().clone(),
      source_text_info: source.text_info_lazy().clone(),
      media_type: source.media_type(),
      comments: source.comments().iter().copied().collect(),
      statements: source.body().as_slice(),
      re_exports: builder.re_exports.take(),
      swc_id_to_symbol_id: builder.swc_id_to_symbol_id.take(),
      symbols: builder
        .symbols
        .take()
        .into_iter()
        .map(|(k, v)| (k, v.0.into_inner()))
        .collect(),
    };
    self.finalize_insert(ModuleInfo::Esm(module_symbol))
  }

  fn finalize_insert(&self, module: ModuleInfo<'a>) -> ModuleInfoRef<'_> {
    self
      .specifiers_to_ids
      .insert(module.specifier().clone(), module.module_id());
    let module_id = module.module_id();
    self.ids_to_modules.insert(module_id, module);
    self.ids_to_modules.get(&module_id).unwrap().as_ref()
  }

  fn parsed_source(
    &self,
    graph_module: &JsModule,
  ) -> Result<ParsedSource<'a>, deno_ast::ParseDiagnostic> {
    self.parser.parse_program(self.allocator, ParseOptions {
      specifier: &graph_module.specifier,
      source: graph_module.source.text.clone(),
      media_type: graph_module.media_type,
      scope_analysis: true,
    })
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


#[derive(Clone, Copy)]
pub struct SymbolNode<'a>(SymbolNodeInner<'a>);

impl std::fmt::Debug for SymbolNode<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let span = match &self.0 {
      SymbolNodeInner::Json => return f.debug_tuple("SymbolNode").field(&"<json>").finish(),
      SymbolNodeInner::Module(span) => *span,
      SymbolNodeInner::ClassDecl(n) => n.span(),
      SymbolNodeInner::ExportDecl(n, _) => n.span(),
      SymbolNodeInner::ExportDefaultDecl(n) => n.span(),
      SymbolNodeInner::ExportDefaultExpr(n) => n.span(),
      SymbolNodeInner::FnDecl(n) => n.span(),
      SymbolNodeInner::TsEnum(n) => n.span(),
      SymbolNodeInner::TsNamespace(n) => n.span(),
      SymbolNodeInner::TsTypeAlias(n) => n.span(),
      SymbolNodeInner::TsInterface(n) => n.span(),
      SymbolNodeInner::Var(_, _, ident) => ident.span(),
      SymbolNodeInner::UsingVar(_, _, ident) => ident.span(),
      SymbolNodeInner::AutoAccessor(n) => n.span(),
      SymbolNodeInner::ClassMethod(n) => n.span(),
      SymbolNodeInner::ClassProp(n) => n.span(),
      SymbolNodeInner::ClassParamProp(n) => n.span(),
      SymbolNodeInner::Constructor(n) => n.span(),
      SymbolNodeInner::ExpandoProperty(n) => n.span(),
      SymbolNodeInner::TsIndexSignature(n) => n.span(),
      SymbolNodeInner::TsCallSignatureDecl(n) => n.span(),
      SymbolNodeInner::TsConstructSignatureDecl(n) => n.span(),
      SymbolNodeInner::TsPropertySignature(n) => n.span(),
      SymbolNodeInner::TsGetterSignature(n) => n.span(),
      SymbolNodeInner::TsSetterSignature(n) => n.span(),
      SymbolNodeInner::TsMethodSignature(n) => n.span(),
    };
    f.debug_tuple("SymbolNode")
      .field(&format_args!("<node @ {}..{}>", span.start, span.end))
      .finish()
  }
}

impl<'a> SymbolNode<'a> {
  pub fn maybe_name(&self) -> Option<Cow<'_, str>> {
    self.maybe_ref().and_then(|r| r.maybe_name())
  }

  pub fn maybe_ref(&self) -> Option<SymbolNodeRef<'a>> {
    match &self.0 {
      SymbolNodeInner::Json => None,
      SymbolNodeInner::Module(span) => Some(SymbolNodeRef::Module(*span)),
      SymbolNodeInner::ClassDecl(n) => Some(SymbolNodeRef::ClassDecl(n)),
      SymbolNodeInner::ExportDecl(export_decl, inner) => Some(
        SymbolNodeRef::ExportDecl(
          export_decl,
          match inner {
            SymbolNodeInnerExportDecl::Class(n) => ExportDeclRef::Class(n),
            SymbolNodeInnerExportDecl::Fn(n) => ExportDeclRef::Fn(n),
            SymbolNodeInnerExportDecl::Var(decl, declarator, id) => {
              ExportDeclRef::Var(decl, declarator, id)
            }
            SymbolNodeInnerExportDecl::TsEnum(n) => ExportDeclRef::TsEnum(n),
            SymbolNodeInnerExportDecl::TsInterface(n) => ExportDeclRef::TsInterface(n),
            SymbolNodeInnerExportDecl::TsNamespace(n) => ExportDeclRef::TsModule(n),
            SymbolNodeInnerExportDecl::TsTypeAlias(n) => ExportDeclRef::TsTypeAlias(n),
            SymbolNodeInnerExportDecl::TsImportEquals(n) => ExportDeclRef::TsImportEquals(n),
          },
        ),
      ),
      SymbolNodeInner::ExportDefaultDecl(n) => Some(SymbolNodeRef::ExportDefaultDecl(n)),
      SymbolNodeInner::ExportDefaultExpr(n) => Some(SymbolNodeRef::ExportDefaultExpr(n)),
      SymbolNodeInner::FnDecl(n) => Some(SymbolNodeRef::FnDecl(n)),
      SymbolNodeInner::TsEnum(n) => Some(SymbolNodeRef::TsEnum(n)),
      SymbolNodeInner::TsNamespace(n) => Some(SymbolNodeRef::TsNamespace(n)),
      SymbolNodeInner::TsTypeAlias(n) => Some(SymbolNodeRef::TsTypeAlias(n)),
      SymbolNodeInner::TsInterface(n) => Some(SymbolNodeRef::TsInterface(n)),
      SymbolNodeInner::Var(decl, declarator, ident) => {
        Some(SymbolNodeRef::Var(decl, declarator, ident))
      }
      SymbolNodeInner::UsingVar(decl, declarator, ident) => {
        Some(SymbolNodeRef::UsingVar(decl, declarator, ident))
      }
      SymbolNodeInner::AutoAccessor(n) => Some(SymbolNodeRef::AutoAccessor(n)),
      SymbolNodeInner::ClassMethod(n) => Some(SymbolNodeRef::ClassMethod(n)),
      SymbolNodeInner::ClassProp(n) => Some(SymbolNodeRef::ClassProp(n)),
      SymbolNodeInner::ClassParamProp(n) => Some(SymbolNodeRef::ClassParamProp(n)),
      SymbolNodeInner::Constructor(n) => Some(SymbolNodeRef::Constructor(n)),
      SymbolNodeInner::ExpandoProperty(n) => {
        Some(SymbolNodeRef::ExpandoProperty(ExpandoPropertyRef(n)))
      }
      SymbolNodeInner::TsIndexSignature(n) => Some(SymbolNodeRef::TsIndexSignature(n)),
      SymbolNodeInner::TsCallSignatureDecl(n) => Some(SymbolNodeRef::TsCallSignatureDecl(n)),
      SymbolNodeInner::TsConstructSignatureDecl(n) => Some(SymbolNodeRef::TsConstructSignatureDecl(n)),
      SymbolNodeInner::TsPropertySignature(n) => Some(SymbolNodeRef::TsPropertySignature(n)),
      SymbolNodeInner::TsGetterSignature(n) => Some(SymbolNodeRef::TsGetterSignature(n)),
      SymbolNodeInner::TsSetterSignature(n) => Some(SymbolNodeRef::TsSetterSignature(n)),
      SymbolNodeInner::TsMethodSignature(n) => Some(SymbolNodeRef::TsMethodSignature(n)),
    }
  }
}

#[derive(Debug, Clone, Copy)]
enum SymbolNodeInnerExportDecl<'a> {
  Class(&'a Class<'a>),
  Fn(&'a Function<'a>),
  Var(&'a VariableDeclaration<'a>, &'a VariableDeclarator<'a>, &'a BindingIdentifier<'a>),
  TsEnum(&'a TSEnumDeclaration<'a>),
  TsInterface(&'a TSInterfaceDeclaration<'a>),
  TsNamespace(&'a TSModuleDeclaration<'a>),
  TsTypeAlias(&'a TSTypeAliasDeclaration<'a>),
  TsImportEquals(&'a TSImportEqualsDeclaration<'a>),
}

#[derive(Debug, Clone, Copy)]
enum SymbolNodeInner<'a> {
  Json,
  Module(Span),
  ClassDecl(&'a Class<'a>),
  ExportDecl(&'a ExportNamedDeclaration<'a>, SymbolNodeInnerExportDecl<'a>),
  ExportDefaultDecl(&'a ExportDefaultDeclaration<'a>),
  ExportDefaultExpr(&'a ExportDefaultDeclarationKind<'a>),
  FnDecl(&'a Function<'a>),
  TsEnum(&'a TSEnumDeclaration<'a>),
  TsNamespace(&'a TSModuleDeclaration<'a>),
  TsTypeAlias(&'a TSTypeAliasDeclaration<'a>),
  TsInterface(&'a TSInterfaceDeclaration<'a>),
  Var(&'a VariableDeclaration<'a>, &'a VariableDeclarator<'a>, &'a BindingIdentifier<'a>),
  UsingVar(&'a VariableDeclaration<'a>, &'a VariableDeclarator<'a>, &'a BindingIdentifier<'a>),
  AutoAccessor(&'a AccessorProperty<'a>),
  ClassMethod(&'a MethodDefinition<'a>),
  ClassProp(&'a PropertyDefinition<'a>),
  ClassParamProp(&'a FormalParameter<'a>),
  Constructor(&'a MethodDefinition<'a>),
  ExpandoProperty(&'a AssignmentExpression<'a>),
  TsIndexSignature(&'a TSIndexSignature<'a>),
  TsCallSignatureDecl(&'a TSCallSignatureDeclaration<'a>),
  TsConstructSignatureDecl(&'a TSConstructSignatureDeclaration<'a>),
  TsPropertySignature(&'a TSPropertySignature<'a>),
  TsGetterSignature(&'a TSMethodSignature<'a>),
  TsSetterSignature(&'a TSMethodSignature<'a>),
  TsMethodSignature(&'a TSMethodSignature<'a>),
}

#[derive(Debug, Clone, Copy)]
pub enum ExportDeclRef<'a> {
  Class(&'a Class<'a>),
  Fn(&'a Function<'a>),
  Var(&'a VariableDeclaration<'a>, &'a VariableDeclarator<'a>, &'a BindingIdentifier<'a>),
  TsEnum(&'a TSEnumDeclaration<'a>),
  TsInterface(&'a TSInterfaceDeclaration<'a>),
  TsModule(&'a TSModuleDeclaration<'a>),
  TsTypeAlias(&'a TSTypeAliasDeclaration<'a>),
  TsImportEquals(&'a TSImportEqualsDeclaration<'a>),
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolNodeRef<'a> {
  Module(Span),
  ExportDecl(&'a ExportNamedDeclaration<'a>, ExportDeclRef<'a>),
  ExportDefaultDecl(&'a ExportDefaultDeclaration<'a>),
  ExportDefaultExpr(&'a ExportDefaultDeclarationKind<'a>),
  ClassDecl(&'a Class<'a>),
  FnDecl(&'a Function<'a>),
  TsEnum(&'a TSEnumDeclaration<'a>),
  TsInterface(&'a TSInterfaceDeclaration<'a>),
  TsNamespace(&'a TSModuleDeclaration<'a>),
  TsTypeAlias(&'a TSTypeAliasDeclaration<'a>),
  Var(&'a VariableDeclaration<'a>, &'a VariableDeclarator<'a>, &'a BindingIdentifier<'a>),
  UsingVar(&'a VariableDeclaration<'a>, &'a VariableDeclarator<'a>, &'a BindingIdentifier<'a>),
  // members
  AutoAccessor(&'a AccessorProperty<'a>),
  ClassMethod(&'a MethodDefinition<'a>),
  ClassProp(&'a PropertyDefinition<'a>),
  ClassParamProp(&'a FormalParameter<'a>),
  Constructor(&'a MethodDefinition<'a>),
  ExpandoProperty(ExpandoPropertyRef<'a>),
  TsIndexSignature(&'a TSIndexSignature<'a>),
  TsCallSignatureDecl(&'a TSCallSignatureDeclaration<'a>),
  TsConstructSignatureDecl(&'a TSConstructSignatureDeclaration<'a>),
  TsPropertySignature(&'a TSPropertySignature<'a>),
  TsGetterSignature(&'a TSMethodSignature<'a>),
  TsSetterSignature(&'a TSMethodSignature<'a>),
  TsMethodSignature(&'a TSMethodSignature<'a>),
}

impl<'a> SymbolNodeRef<'a> {
  /// The local name of the node, if it has a name.
  pub fn maybe_name(&self) -> Option<Cow<'a, str>> {
    fn ts_module_name_to_string<'b>(module_name: &'b TSModuleDeclarationName<'b>) -> Option<&'b str> {
      match module_name {
        TSModuleDeclarationName::Identifier(ident) => Some(&ident.name),
        TSModuleDeclarationName::StringLiteral(str) => Some(&str.value),
      }
    }

    fn maybe_property_key_name<'b>(key: &'b PropertyKey<'b>) -> Option<Cow<'b, str>> {
      match key {
        PropertyKey::StaticIdentifier(n) => Some(Cow::Borrowed(n.name.as_str())),
        PropertyKey::PrivateIdentifier(n) => Some(Cow::Owned(format!("#{}", n.name))),
        PropertyKey::StringLiteral(n) => Some(Cow::Borrowed(n.value.as_str())),
        PropertyKey::NumericLiteral(n) => Some(Cow::Owned(n.value.to_string())),
        _ => None,
      }
    }

    fn maybe_param_prop_name<'b>(param: &'b FormalParameter<'b>) -> Option<Cow<'b, str>> {
      match &param.pattern {
        BindingPattern::BindingIdentifier(ident) => Some(Cow::Borrowed(ident.name.as_str())),
        BindingPattern::AssignmentPattern(assign_pat) => match &assign_pat.left {
          BindingPattern::BindingIdentifier(ident) => Some(Cow::Borrowed(ident.name.as_str())),
          _ => unreachable!(),
        },
        _ => unreachable!(),
      }
    }

    fn maybe_expr<'b>(expr: &'b Expression<'b>) -> Option<Cow<'b, str>> {
      match expr {
        Expression::Identifier(n) => Some(Cow::Borrowed(n.name.as_str())),
        Expression::StringLiteral(n) => Some(Cow::Borrowed(n.value.as_str())),
        Expression::NumericLiteral(n) => Some(Cow::Owned(n.value.to_string())),
        Expression::BigIntLiteral(n) => Some(Cow::Owned(n.raw.as_ref().map(|r| r.to_string()).unwrap_or_default())),
        _ => None,
      }
    }

    match self {
      Self::Module(_) => None,
      Self::ClassDecl(n) => n.id.as_ref().map(|id| Cow::Borrowed(id.name.as_str())),
      Self::ExportDecl(_, n) => match n {
        ExportDeclRef::Class(n) => n.id.as_ref().map(|id| Cow::Borrowed(id.name.as_str())),
        ExportDeclRef::Fn(n) => n.id.as_ref().map(|id| Cow::Borrowed(id.name.as_str())),
        ExportDeclRef::Var(_, _, ident) => Some(Cow::Borrowed(ident.name.as_str())),
        ExportDeclRef::TsEnum(n) => Some(Cow::Borrowed(n.id.name.as_str())),
        ExportDeclRef::TsInterface(n) => Some(Cow::Borrowed(n.id.name.as_str())),
        ExportDeclRef::TsModule(n) => {
          ts_module_name_to_string(&n.id).map(Cow::Borrowed)
        }
        ExportDeclRef::TsTypeAlias(n) => Some(Cow::Borrowed(n.id.name.as_str())),
        ExportDeclRef::TsImportEquals(n) => Some(Cow::Borrowed(n.id.name.as_str())),
      },
      Self::ExportDefaultDecl(n) => match &n.declaration {
        ExportDefaultDeclarationKind::ClassDeclaration(n) => n.id.as_ref().map(|id| Cow::Borrowed(id.name.as_str())),
        ExportDefaultDeclarationKind::FunctionDeclaration(n) => n.id.as_ref().map(|id| Cow::Borrowed(id.name.as_str())),
        ExportDefaultDeclarationKind::TSInterfaceDeclaration(n) => Some(Cow::Borrowed(n.id.name.as_str())),
        _ => None,
      },
      Self::ExportDefaultExpr(_) => None,
      Self::FnDecl(n) => n.id.as_ref().map(|id| Cow::Borrowed(id.name.as_str())),
      Self::TsEnum(n) => Some(Cow::Borrowed(n.id.name.as_str())),
      Self::TsInterface(n) => Some(Cow::Borrowed(n.id.name.as_str())),
      Self::TsNamespace(n) => {
        ts_module_name_to_string(&n.id).map(Cow::Borrowed)
      }
      Self::TsTypeAlias(n) => Some(Cow::Borrowed(n.id.name.as_str())),
      Self::Var(_, _, ident) => Some(Cow::Borrowed(ident.name.as_str())),
      Self::UsingVar(_, _, ident) => Some(Cow::Borrowed(ident.name.as_str())),
      Self::AutoAccessor(n) => maybe_property_key_name(&n.key),
      Self::ClassMethod(n) => maybe_property_key_name(&n.key),
      Self::ClassProp(n) => maybe_property_key_name(&n.key),
      Self::ClassParamProp(n) => maybe_param_prop_name(n),
      Self::ExpandoProperty(n) => Some(Cow::Borrowed(n.prop_name())),
      Self::TsPropertySignature(n) => maybe_property_key_name(&n.key),
      Self::TsGetterSignature(n) => maybe_property_key_name(&n.key),
      Self::TsSetterSignature(n) => maybe_property_key_name(&n.key),
      Self::TsMethodSignature(n) => maybe_property_key_name(&n.key),
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

  // todo(dsherret): rename to is_class_decl

  /// If the node is a class.
  pub fn is_class(&self) -> bool {
    match self {
      Self::ClassDecl(_) | Self::ExportDecl(_, ExportDeclRef::Class(_)) => true,
      Self::ExportDefaultDecl(n) => matches!(
        n.declaration,
        ExportDefaultDeclarationKind::ClassDeclaration(_)
      ),
      _ => false,
    }
  }

  /// If the node is a function.
  pub fn is_function(&self) -> bool {
    match self {
      Self::FnDecl(_) | Self::ExportDecl(_, ExportDeclRef::Fn(_)) => true,
      Self::ExportDefaultDecl(n) => matches!(
        n.declaration,
        ExportDefaultDeclarationKind::FunctionDeclaration(_)
      ),
      _ => false,
    }
  }

  /// If the node is an interface.
  pub fn is_interface(&self) -> bool {
    match self {
      Self::TsInterface(_) | Self::ExportDecl(_, ExportDeclRef::TsInterface(_)) => true,
      Self::ExportDefaultDecl(n) => matches!(
        n.declaration,
        ExportDefaultDeclarationKind::TSInterfaceDeclaration(_)
      ),
      _ => false,
    }
  }

  /// If the node is a typescript namespace.
  pub fn is_ts_namespace(&self) -> bool {
    matches!(
      self,
      Self::TsNamespace(_) | Self::ExportDecl(_, ExportDeclRef::TsModule(_))
    )
  }

  /// If the node is a variable.
  pub fn is_var(&self) -> bool {
    matches!(
      self,
      Self::Var(..) | Self::ExportDecl(_, ExportDeclRef::Var(..))
    )
  }

  /// If the node is a contructor.
  pub fn is_ctor(&self) -> bool {
    matches!(self, Self::Constructor(_))
  }

  /// If the node has a body.
  pub fn has_body(&self) -> bool {
    match self {
      SymbolNodeRef::FnDecl(n) => n.body.is_some(),
      SymbolNodeRef::TsNamespace(n) => n.body.is_some(),
      SymbolNodeRef::AutoAccessor(_) => todo!(),
      SymbolNodeRef::ClassMethod(m) => m.value.body.is_some(),
      SymbolNodeRef::Constructor(n) => n.value.body.is_some(),
      SymbolNodeRef::ExportDefaultDecl(n) => match &n.declaration {
        ExportDefaultDeclarationKind::FunctionDeclaration(n) => n.body.is_some(),
        ExportDefaultDeclarationKind::ClassDeclaration(_)
        | ExportDefaultDeclarationKind::TSInterfaceDeclaration(_) => true,
        _ => false,
      },
      SymbolNodeRef::ExportDecl(_, decl) => match decl {
        ExportDeclRef::TsModule(n) => n.body.is_some(),
        ExportDeclRef::Fn(n) => n.body.is_some(),
        ExportDeclRef::Class(_)
        | ExportDeclRef::TsEnum(_)
        | ExportDeclRef::TsInterface(_) => true,
        ExportDeclRef::TsTypeAlias(_) | ExportDeclRef::Var(..) | ExportDeclRef::TsImportEquals(_) => false,
      },
      SymbolNodeRef::Module(_)
      | SymbolNodeRef::ClassDecl(_)
      | SymbolNodeRef::TsEnum(_)
      | SymbolNodeRef::TsInterface(_) => true,
      SymbolNodeRef::TsTypeAlias(_)
      | SymbolNodeRef::ExportDefaultExpr(_)
      | SymbolNodeRef::Var(..)
      | SymbolNodeRef::UsingVar(..)
      | SymbolNodeRef::ClassProp(_)
      | SymbolNodeRef::ClassParamProp(_)
      | SymbolNodeRef::ExpandoProperty(..)
      | SymbolNodeRef::TsIndexSignature(_)
      | SymbolNodeRef::TsCallSignatureDecl(_)
      | SymbolNodeRef::TsConstructSignatureDecl(_)
      | SymbolNodeRef::TsPropertySignature(_)
      | SymbolNodeRef::TsGetterSignature(_)
      | SymbolNodeRef::TsSetterSignature(_)
      | SymbolNodeRef::TsMethodSignature(_) => false,
    }
  }

  /// If the node is a declaration that can be found in a module.
  pub fn is_module(&self) -> bool {
    matches!(self, SymbolNodeRef::Module(_))
  }

  pub fn has_export_keyword(&self) -> bool {
    match self {
      SymbolNodeRef::ExportDecl(..)
      | SymbolNodeRef::ExportDefaultDecl(_)
      | SymbolNodeRef::ExportDefaultExpr(_) => true,
      SymbolNodeRef::Module(_)
      | SymbolNodeRef::ClassDecl(_)
      | SymbolNodeRef::FnDecl(_)
      | SymbolNodeRef::TsEnum(_)
      | SymbolNodeRef::TsInterface(_)
      | SymbolNodeRef::TsNamespace(_)
      | SymbolNodeRef::TsTypeAlias(_)
      | SymbolNodeRef::Var(..)
      | SymbolNodeRef::UsingVar(..)
      | SymbolNodeRef::AutoAccessor(_)
      | SymbolNodeRef::ClassMethod(_)
      | SymbolNodeRef::ClassProp(_)
      | SymbolNodeRef::ClassParamProp(_)
      | SymbolNodeRef::Constructor(_)
      | SymbolNodeRef::ExpandoProperty(_)
      | SymbolNodeRef::TsIndexSignature(_)
      | SymbolNodeRef::TsCallSignatureDecl(_)
      | SymbolNodeRef::TsConstructSignatureDecl(_)
      | SymbolNodeRef::TsPropertySignature(_)
      | SymbolNodeRef::TsGetterSignature(_)
      | SymbolNodeRef::TsSetterSignature(_)
      | SymbolNodeRef::TsMethodSignature(_) => false,
    }
  }

  /// If the node is a declaration that can be found in a module.
  pub fn is_decl(&self) -> bool {
    match self {
      SymbolNodeRef::Module(_) => false,
      SymbolNodeRef::ClassDecl(_)
      | SymbolNodeRef::ExportDecl(..)
      | SymbolNodeRef::ExportDefaultDecl(_)
      | SymbolNodeRef::ExportDefaultExpr(_)
      | SymbolNodeRef::FnDecl(_)
      | SymbolNodeRef::TsEnum(_)
      | SymbolNodeRef::TsInterface(_)
      | SymbolNodeRef::TsNamespace(_)
      | SymbolNodeRef::TsTypeAlias(_)
      | SymbolNodeRef::Var(..)
      | SymbolNodeRef::UsingVar(..) => true,
      SymbolNodeRef::AutoAccessor(_)
      | SymbolNodeRef::ClassMethod(_)
      | SymbolNodeRef::ClassProp(_)
      | SymbolNodeRef::ClassParamProp(_)
      | SymbolNodeRef::Constructor(_)
      | SymbolNodeRef::ExpandoProperty(..)
      | SymbolNodeRef::TsIndexSignature(_)
      | SymbolNodeRef::TsCallSignatureDecl(_)
      | SymbolNodeRef::TsConstructSignatureDecl(_)
      | SymbolNodeRef::TsPropertySignature(_)
      | SymbolNodeRef::TsGetterSignature(_)
      | SymbolNodeRef::TsSetterSignature(_)
      | SymbolNodeRef::TsMethodSignature(_) => false,
    }
  }

  /// If the node is a member found in a class or interface.
  pub fn is_member(&self) -> bool {
    match self {
      SymbolNodeRef::Module(_)
      | SymbolNodeRef::ClassDecl(_)
      | SymbolNodeRef::ExportDecl(..)
      | SymbolNodeRef::ExportDefaultDecl(_)
      | SymbolNodeRef::ExportDefaultExpr(_)
      | SymbolNodeRef::FnDecl(_)
      | SymbolNodeRef::TsEnum(_)
      | SymbolNodeRef::TsInterface(_)
      | SymbolNodeRef::TsNamespace(_)
      | SymbolNodeRef::TsTypeAlias(_)
      | SymbolNodeRef::Var(..)
      | SymbolNodeRef::UsingVar(..) => false,
      SymbolNodeRef::AutoAccessor(_)
      | SymbolNodeRef::ClassMethod(_)
      | SymbolNodeRef::ClassProp(_)
      | SymbolNodeRef::ClassParamProp(_)
      | SymbolNodeRef::Constructor(_)
      | SymbolNodeRef::ExpandoProperty(..)
      | SymbolNodeRef::TsIndexSignature(_)
      | SymbolNodeRef::TsCallSignatureDecl(_)
      | SymbolNodeRef::TsConstructSignatureDecl(_)
      | SymbolNodeRef::TsPropertySignature(_)
      | SymbolNodeRef::TsGetterSignature(_)
      | SymbolNodeRef::TsSetterSignature(_)
      | SymbolNodeRef::TsMethodSignature(_) => true,
    }
  }

  /// If the node is a private member.
  pub fn is_private_member(&self) -> bool {
    match self {
      SymbolNodeRef::Module(_)
      | SymbolNodeRef::ClassDecl(_)
      | SymbolNodeRef::ExportDecl(..)
      | SymbolNodeRef::ExportDefaultDecl(_)
      | SymbolNodeRef::ExportDefaultExpr(_)
      | SymbolNodeRef::FnDecl(_)
      | SymbolNodeRef::TsEnum(_)
      | SymbolNodeRef::TsInterface(_)
      | SymbolNodeRef::TsNamespace(_)
      | SymbolNodeRef::TsTypeAlias(_)
      | SymbolNodeRef::Var(..)
      | SymbolNodeRef::UsingVar(..) => false,
      SymbolNodeRef::AutoAccessor(n) => {
        n.accessibility == Some(TSAccessibility::Private)
          || matches!(&n.key, PropertyKey::PrivateIdentifier(_))
      }
      SymbolNodeRef::ClassMethod(n) => {
        n.accessibility == Some(TSAccessibility::Private)
      }
      SymbolNodeRef::ClassProp(n) => {
        n.accessibility == Some(TSAccessibility::Private)
      }
      SymbolNodeRef::ClassParamProp(n) => {
        n.accessibility == Some(TSAccessibility::Private)
      }
      SymbolNodeRef::Constructor(n) => {
        n.accessibility == Some(TSAccessibility::Private)
      }
      SymbolNodeRef::ExpandoProperty(..)
      | SymbolNodeRef::TsIndexSignature(_)
      | SymbolNodeRef::TsCallSignatureDecl(_)
      | SymbolNodeRef::TsConstructSignatureDecl(_)
      | SymbolNodeRef::TsPropertySignature(_)
      | SymbolNodeRef::TsGetterSignature(_)
      | SymbolNodeRef::TsSetterSignature(_)
      | SymbolNodeRef::TsMethodSignature(_) => false,
    }
  }

  pub fn deps(&self, mode: ResolveDepsMode) -> Vec<SymbolNodeDep> {
    super::dep_analyzer::resolve_deps(*self, mode)
  }
}

/// An "expando property" is a property that's assigned to a declaration
/// in order to add properties.
///
/// ```ts
/// function myFunction() {}
/// myFunction.myProperty = 123;
/// ```
#[derive(Debug, Clone, Copy)]
pub struct ExpandoPropertyRef<'a>(&'a AssignmentExpression<'a>);

impl<'a> ExpandoPropertyRef<'a> {
  /// Assignment expressions provided here must be in one of these forms:
  /// * obj.prop = expr;
  /// * obj["prop"] = expr;
  pub fn maybe_new(expr: &'a AssignmentExpression<'a>) -> Option<Self> {
    if Self::is_valid(expr) {
      Some(Self(expr))
    } else {
      None
    }
  }

  fn is_valid(expr: &AssignmentExpression) -> bool {
    expr.operator == AssignmentOperator::Assign
      && Self::maybe_obj_ident(expr).is_some()
      && match Self::maybe_prop_name_str(expr) {
        Some(prop_name) => is_valid_ident(prop_name),
        None => false,
      }
  }

  fn maybe_static_member<'b>(expr: &'b AssignmentExpression<'b>) -> Option<&'b StaticMemberExpression<'b>> {
    match &expr.left {
      AssignmentTarget::StaticMemberExpression(member) => Some(member),
      _ => None,
    }
  }

  fn maybe_computed_member<'b>(expr: &'b AssignmentExpression<'b>) -> Option<&'b ComputedMemberExpression<'b>> {
    match &expr.left {
      AssignmentTarget::ComputedMemberExpression(member) => Some(member),
      _ => None,
    }
  }

  fn maybe_obj_ident<'b>(expr: &'b AssignmentExpression<'b>) -> Option<&'b IdentifierReference<'b>> {
    // Try static member first, then computed member
    if let Some(member) = Self::maybe_static_member(expr) {
      match &member.object {
        Expression::Identifier(ident) => return Some(ident),
        _ => {}
      }
    }
    if let Some(member) = Self::maybe_computed_member(expr) {
      match &member.object {
        Expression::Identifier(ident) => return Some(ident),
        _ => {}
      }
    }
    None
  }

  fn maybe_prop_name_str<'b>(expr: &'b AssignmentExpression<'b>) -> Option<&'b str> {
    if let Some(member) = Self::maybe_static_member(expr) {
      return Some(member.property.name.as_str());
    }
    if let Some(member) = Self::maybe_computed_member(expr) {
      if let Expression::StringLiteral(str) = &member.expression {
        return Some(str.value.as_str());
      }
    }
    None
  }

  /// The inner assignment expression.
  pub fn inner(&self) -> &'a AssignmentExpression<'a> {
    self.0
  }

  pub fn obj_ident(&self) -> &'a IdentifierReference<'a> {
    Self::maybe_obj_ident(self.0).unwrap()
  }

  pub fn prop_name(&self) -> &'a str {
    Self::maybe_prop_name_str(self.0).unwrap()
  }

  pub fn prop_name_span(&self) -> Span {
    if let Some(member) = Self::maybe_static_member(self.0) {
      return member.property.span;
    }
    if let Some(member) = Self::maybe_computed_member(self.0) {
      return member.expression.span();
    }
    unreachable!()
  }

  pub fn assignment(&self) -> &'a Expression<'a> {
    &self.0.right
  }
}

#[derive(Debug, Clone)]
pub enum SymbolDeclKind<'a> {
  Target(Id),
  QualifiedTarget(Id, Vec<String>),
  FileRef(FileDep),
  Definition(SymbolNode<'a>),
}

impl<'a> SymbolDeclKind<'a> {
  pub fn is_definition(&self) -> bool {
    matches!(self, Self::Definition(_))
  }

  pub fn maybe_node(&self) -> Option<SymbolNodeRef<'a>> {
    match self {
      SymbolDeclKind::Definition(node) => node.maybe_ref(),
      _ => None,
    }
  }
}

mod symbol_decl_flags {
  const FLAG_HAS_OVERLOADS: u8 = 1 << 0;

  #[derive(Default, Clone, Copy)]
  pub struct SymbolDeclFlags(u8);

  impl std::fmt::Debug for SymbolDeclFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{}", self.0)
    }
  }

  impl SymbolDeclFlags {
    pub fn has_overloads(&self) -> bool {
      self.0 & FLAG_HAS_OVERLOADS != 0
    }

    pub(crate) fn set_has_overloads(&mut self) {
      self.0 |= FLAG_HAS_OVERLOADS;
    }
  }
}

#[derive(Debug, Clone)]
pub struct SymbolDecl<'a> {
  pub kind: SymbolDeclKind<'a>,
  pub range: Span,
  flags: symbol_decl_flags::SymbolDeclFlags,
}

impl<'a> SymbolDecl<'a> {
  pub(crate) fn new(kind: SymbolDeclKind<'a>, range: Span) -> Self {
    Self {
      kind,
      range,
      flags: Default::default(),
    }
  }
  pub fn maybe_node(&self) -> Option<SymbolNodeRef<'_>> {
    self.kind.maybe_node()
  }

  /// The local name of the decl, if it has a name or node.
  pub fn maybe_name(&self) -> Option<Cow<'_, str>> {
    self.maybe_node().and_then(|n| n.maybe_name())
  }

  pub fn deps(&self, mode: ResolveDepsMode) -> Vec<SymbolNodeDep> {
    match self.maybe_node() {
      Some(node) => node.deps(mode),
      None => Vec::new(),
    }
  }

  pub fn has_overloads(&self) -> bool {
    self.flags.has_overloads()
  }

  pub fn is_class(&self) -> bool {
    self.maybe_node().map(|n| n.is_class()).unwrap_or(false)
  }

  pub fn is_var(&self) -> bool {
    self.maybe_node().map(|n| n.is_var()).unwrap_or(false)
  }

  pub fn is_ctor(&self) -> bool {
    self.maybe_node().map(|n| n.is_ctor()).unwrap_or(false)
  }

  pub fn is_function(&self) -> bool {
    self.maybe_node().map(|n| n.is_function()).unwrap_or(false)
  }

  pub fn is_interface(&self) -> bool {
    self.maybe_node().map(|n| n.is_interface()).unwrap_or(false)
  }

  pub fn has_body(&self) -> bool {
    self.maybe_node().map(|n| n.has_body()).unwrap_or(false)
  }
}

#[derive(Debug, Clone)]
pub struct Symbol<'a> {
  module_id: ModuleId,
  symbol_id: SymbolId,
  parent_id: Option<SymbolId>,
  decls: Vec<SymbolDecl<'a>>,
  /// The child declarations of module declarations.
  child_ids: IndexSet<SymbolId>,
  exports: IndexMap<String, SymbolId>,
  /// Members
  members: IndexSet<SymbolId>,
}

impl<'a> Symbol<'a> {
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

  /// Gets the symbol's parent's unique symbol id.
  pub fn parent_unique_id(&self) -> Option<UniqueSymbolId> {
    self
      .parent_id
      .map(|id| UniqueSymbolId::new(self.module_id, id))
  }

  /// The local name of the symbol if it has one.
  pub fn maybe_name(&self) -> Option<Cow<'_, str>> {
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

  /// The symbol's associated declarations. A symbol can represent many declarations
  /// if they have the same name via declaration merging or if they are overloads.
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

  /// If the symbol is a declaration that can be found in a module.
  pub fn is_module(&self) -> bool {
    self
      .decls()
      .first()
      .and_then(|d| d.maybe_node())
      .map(|n| n.is_module())
      .unwrap_or(false)
  }

  /// If the symbol is a declaration that can be found in a module.
  pub fn is_decl(&self) -> bool {
    self
      .decls()
      .first()
      .and_then(|d| d.maybe_node())
      .map(|n| n.is_decl())
      .unwrap_or(false)
  }

  /// If the symbol is a member found in a class or interface.
  pub fn is_member(&self) -> bool {
    self
      .decls()
      .first()
      .and_then(|d| d.maybe_node())
      .map(|n| n.is_member())
      .unwrap_or(false)
  }

  /// If the symbol is a private member.
  pub fn is_private_member(&self) -> bool {
    self
      .decls()
      .first()
      .and_then(|d| d.maybe_node())
      .map(|n| n.is_private_member())
      .unwrap_or(false)
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
  Esm(&'a EsModuleInfo<'a>),
}

impl<'a> ModuleInfoRef<'a> {
  pub fn json(&self) -> Option<&'a JsonModuleInfo> {
    match self {
      Self::Json(json) => Some(json),
      Self::Esm(_) => None,
    }
  }

  pub fn esm(&self) -> Option<&'a EsModuleInfo<'a>> {
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
      Self::Esm(m) => &m.source_text_info,
    }
  }

  pub fn text(&self) -> &'a str {
    match self {
      ModuleInfoRef::Json(m) => m.source_text_info.text_str(),
      ModuleInfoRef::Esm(m) => &m.source_text,
    }
  }

  pub fn module_symbol(&self) -> &'a Symbol<'a> {
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

  pub fn symbols(&self) -> Box<dyn Iterator<Item = &'a Symbol<'a>> + 'a> {
    match *self {
      Self::Json(m) => Box::new(m.symbols().map(|s| s as &Symbol<'a>)),
      Self::Esm(m) => Box::new(m.symbols()),
    }
  }

  pub fn symbol(&self, id: SymbolId) -> Option<&'a Symbol<'a>> {
    match *self {
      Self::Json(m) => m.symbol(id).map(|s| s as &Symbol<'a>),
      Self::Esm(m) => m.symbol(id),
    }
  }

  pub fn exports(&self, root_symbol: &'a RootSymbol) -> ModuleExports<'a> {
    cross_module::exports_and_re_exports(
      root_symbol.module_graph,
      *self,
      &|specifier| root_symbol.module_from_specifier(specifier),
    )
  }

  pub fn re_export_all_nodes(
    &self,
  ) -> Option<impl Iterator<Item = &'a ExportAllDeclaration<'a>> + 'a> {
    match self {
      Self::Json(_) => None,
      Self::Esm(m) => Some(m.re_exports.iter().copied()),
    }
  }

  pub(crate) fn re_export_all_specifiers(
    &self,
  ) -> Option<impl Iterator<Item = &'a str> + use<'a>> {
    match self {
      Self::Json(_) => None,
      Self::Esm(m) => Some(
        m.re_exports
          .iter()
          .map(|e| e.source.value.as_str()),
      ),
    }
  }

  pub fn fully_qualified_symbol_name(&self, symbol: &Symbol) -> Option<String> {
    debug_assert_eq!(symbol.module_id(), self.module_id());
    let mut text = String::new();
    let mut last: Option<&Symbol> = None;
    let mut next = Some(symbol);
    while let Some(symbol) = next {
      if symbol.parent_id().is_none() {
        break; // ignore the source file
      }
      if !text.is_empty() {
        let prop_was_member = last
          .map(|l| symbol.members().contains(&l.symbol_id()))
          .unwrap_or(false);
        let part_name = symbol.maybe_name()?;
        let prop_was_class_member = prop_was_member
          && symbol
            .decls()
            .first()
            .map(|d| d.is_class())
            .unwrap_or(false);
        text = if prop_was_class_member {
          format!("{}.prototype.{}", part_name, text)
        } else if prop_was_member {
          // not the best, but good enough
          format!("{}[\"{}\"]", part_name, text.replace('"', "\\\""))
        } else {
          format!("{}.{}", part_name, text)
        };
      } else {
        text = symbol.maybe_name()?.to_string();
      }
      last = next;
      next = symbol.parent_id().and_then(|id| self.symbol(id));
    }
    if text.is_empty() { None } else { Some(text) }
  }
}

/// Holds information about the module like symbols and re-exports.
#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)] // ok because EsModuleInfo is way more common
pub enum ModuleInfo<'a> {
  Json(Box<JsonModuleInfo>),
  Esm(EsModuleInfo<'a>),
}

impl<'a> ModuleInfo<'a> {
  pub fn json(&self) -> Option<&JsonModuleInfo> {
    match self {
      Self::Json(json) => Some(json),
      Self::Esm(_) => None,
    }
  }

  pub fn esm(&self) -> Option<&EsModuleInfo<'a>> {
    match self {
      Self::Json(_) => None,
      Self::Esm(esm) => Some(esm),
    }
  }

  pub fn as_ref(&self) -> ModuleInfoRef<'_> {
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

  pub fn symbols(&self) -> Box<dyn Iterator<Item = &Symbol<'a>> + '_> {
    match self {
      Self::Json(m) => Box::new(m.symbols().map(|s| s as &Symbol<'a>)),
      Self::Esm(m) => Box::new(m.symbols()),
    }
  }

  pub fn symbol(&self, id: SymbolId) -> Option<&Symbol<'a>> {
    match self {
      Self::Json(m) => m.symbol(id).map(|s| s as &Symbol<'a>),
      Self::Esm(m) => m.symbol(id),
    }
  }
}

#[derive(Clone)]
pub struct JsonModuleInfo {
  module_id: ModuleId,
  specifier: ModuleSpecifier,
  source_text_info: SourceTextInfo,
  module_symbol: Symbol<'static>,
  default_symbol: Symbol<'static>,
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
  pub fn as_ref(&self) -> ModuleInfoRef<'_> {
    ModuleInfoRef::Json(self)
  }

  pub fn specifier(&self) -> &ModuleSpecifier {
    &self.specifier
  }

  pub fn text_info(&self) -> &SourceTextInfo {
    &self.source_text_info
  }

  pub fn symbols(&self) -> impl Iterator<Item = &Symbol<'static>> {
    std::iter::once(&self.module_symbol)
      .chain(std::iter::once(&self.default_symbol))
  }

  pub fn symbol(&self, id: SymbolId) -> Option<&Symbol<'static>> {
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
pub struct EsModuleInfo<'a> {
  module_id: ModuleId,
  specifier: ModuleSpecifier,
  source_text: std::sync::Arc<str>,
  source_text_info: SourceTextInfo,
  media_type: MediaType,
  comments: Vec<Comment>,
  statements: &'a [Statement<'a>],
  /// The re-export specifiers.
  re_exports: Vec<&'a ExportAllDeclaration<'a>>,
  // note: not all symbol ids have an swc id. For example, default exports
  swc_id_to_symbol_id: IndexMap<Id, SymbolId>,
  symbols: IndexMap<SymbolId, Symbol<'a>>,
}

impl std::fmt::Debug for EsModuleInfo<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("EsModuleInfo")
      .field("module_id", &self.module_id)
      .field("specifier", &self.specifier.as_str())
      .field(
        "re_exports",
        &self
          .re_exports
          .iter()
          .map(|e| e.source.value.to_string())
          .collect::<Vec<_>>(),
      )
      .field("id_to_symbol_id", &self.swc_id_to_symbol_id)
      .field("symbols", &self.symbols)
      .finish()
  }
}

impl<'a> EsModuleInfo<'a> {
  pub fn as_ref(&self) -> ModuleInfoRef<'_> {
    ModuleInfoRef::Esm(self)
  }

  pub fn module_id(&self) -> ModuleId {
    self.module_id
  }

  pub fn specifier(&self) -> &ModuleSpecifier {
    &self.specifier
  }

  pub fn source_text(&self) -> &str {
    &self.source_text
  }

  pub fn source_text_info(&self) -> &SourceTextInfo {
    &self.source_text_info
  }

  pub fn media_type(&self) -> MediaType {
    self.media_type
  }

  pub fn comments(&self) -> &[Comment] {
    &self.comments
  }

  pub fn statements(&self) -> &'a [Statement<'a>] {
    self.statements
  }

  pub fn exports<'b>(
    &'b self,
    root_symbol: &'b RootSymbol,
  ) -> ModuleExports<'b> {
    self.as_ref().exports(root_symbol)
  }

  pub fn module_symbol(&self) -> &Symbol<'a> {
    self.symbol(SymbolId(0)).unwrap()
  }

  pub fn symbol_id_from_swc(&self, id: &Id) -> Option<SymbolId> {
    self.swc_id_to_symbol_id.get(id).copied()
  }

  pub fn symbol_from_swc(&self, id: &Id) -> Option<&Symbol<'a>> {
    let id = self.symbol_id_from_swc(id)?;
    self.symbol(id)
  }

  pub fn symbols(&self) -> impl Iterator<Item = &Symbol<'a>> {
    self.symbols.values()
  }

  pub fn symbol(&self, id: SymbolId) -> Option<&Symbol<'a>> {
    self.symbols.get(&id)
  }
}

struct SymbolMut<'a>(RefCell<Symbol<'a>>);

impl<'a> SymbolMut<'a> {
  pub fn new(symbol: Symbol<'a>) -> Self {
    Self(RefCell::new(symbol))
  }

  pub fn add_decl(&self, mut symbol_decl: SymbolDecl<'a>) {
    fn is_class_method(symbol_decl: &SymbolDecl) -> bool {
      symbol_decl
        .maybe_node()
        .map(|node| {
          matches!(
            node,
            SymbolNodeRef::ClassMethod(m) if m.kind == MethodDefinitionKind::Method
          )
        })
        .unwrap_or(false)
    }

    let mut inner = self.0.borrow_mut();
    // check if this is an implementation with overloads
    if !inner.decls.is_empty()
      && (symbol_decl.is_function()
        || is_class_method(&symbol_decl)
        || symbol_decl.is_ctor())
      && symbol_decl.has_body()
      && inner
        .decls
        .last()
        .map(|d| {
          d.is_function()
            || is_class_method(&symbol_decl)
            || symbol_decl.is_ctor()
        })
        .unwrap_or(false)
    {
      symbol_decl.flags.set_has_overloads();
    }

    inner.decls.push(symbol_decl);
  }

  pub fn symbol_id(&self) -> SymbolId {
    self.0.borrow_mut().symbol_id
  }

  pub fn add_export(&self, name: String, symbol_id: SymbolId) {
    self.0.borrow_mut().exports.insert(name, symbol_id);
  }

  pub fn export(&self, name: &str) -> Option<SymbolId> {
    self.0.borrow().exports.get(name).copied()
  }

  pub fn add_child_id(&self, symbol_id: SymbolId) {
    self.0.borrow_mut().child_ids.insert(symbol_id);
  }

  pub fn add_member(&self, symbol_id: SymbolId) {
    self.0.borrow_mut().members.insert(symbol_id);
  }

  pub fn borrow_inner(&self) -> Ref<'_, Symbol<'a>> {
    self.0.borrow()
  }
}

struct ModuleBuilder<'a> {
  module_id: ModuleId,
  // todo(dsherret): make this not an IndexMap
  swc_id_to_symbol_id: AdditiveOnlyIndexMapForCopyValues<Id, SymbolId>,
  // todo(dsherret): make this not an IndexMap
  symbols: AdditiveOnlyIndexMap<SymbolId, SymbolMut<'a>>,
  next_symbol_id: Cell<SymbolId>,
  re_exports: RefCell<Vec<&'a ExportAllDeclaration<'a>>>,
}

impl<'a> ModuleBuilder<'a> {
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
    module_symbol: &SymbolMut<'a>,
    symbol_decl: SymbolDecl<'a>,
  ) -> SymbolId {
    if let Some(symbol_id) = module_symbol.export("default") {
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

  pub fn create_new_symbol(&self, parent_id: SymbolId) -> &SymbolMut<'a> {
    self.create_new_symbol_inner(Some(parent_id))
  }

  pub fn create_new_symbol_for_root(&self) -> &SymbolMut<'a> {
    let symbol = self.create_new_symbol_inner(None);
    debug_assert_eq!(symbol.symbol_id(), SymbolId(0));
    symbol
  }

  fn create_new_symbol_inner(&self, parent_id: Option<SymbolId>) -> &SymbolMut<'a> {
    let symbol_id = self.get_next_symbol_id();
    self.symbols.insert(
      symbol_id,
      SymbolMut::new(Symbol::new(self.module_id, symbol_id, parent_id)),
    );
    self.symbols.get(&symbol_id).unwrap()
  }

  pub fn symbol_mut(&self, id: SymbolId) -> Option<&SymbolMut<'a>> {
    self.symbols.get(&id)
  }

  pub fn get_symbol_from_swc_id(
    &self,
    id: Id,
    symbol_decl: SymbolDecl<'a>,
    parent_id: SymbolId,
  ) -> &SymbolMut<'a> {
    let symbol_id = self.ensure_symbol_for_swc_id(id, symbol_decl, parent_id);
    self.symbols.get(&symbol_id).unwrap()
  }

  pub fn ensure_symbol_for_swc_id(
    &self,
    id: Id,
    symbol_decl: SymbolDecl<'a>,
    parent_id: SymbolId,
  ) -> SymbolId {
    match self.swc_id_to_symbol_id.get(&id) {
      Some(symbol_id) => {
        let existing = self.symbols.get(&symbol_id).unwrap();
        // OXC doesn't have SWC's SyntaxContext for scope discrimination,
        // so same-named identifiers in different scopes produce the same Id.
        // Check if the existing symbol has the same parent to avoid collisions
        // between e.g. `namespace A { class Foo {} }` and `namespace B { class Foo {} }`.
        if existing.borrow_inner().parent_id() == Some(parent_id) {
          existing.add_decl(symbol_decl);
          symbol_id
        } else {
          // Different scope - create a new symbol without updating the flat map.
          let new_symbol_id = self.get_next_symbol_id();
          let symbol = SymbolMut::new(Symbol::new(
            self.module_id,
            new_symbol_id,
            Some(parent_id),
          ));
          symbol.add_decl(symbol_decl);
          self.symbols.insert(new_symbol_id, symbol);
          new_symbol_id
        }
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

  fn add_re_export(&self, export_all: &'a ExportAllDeclaration<'a>) {
    self.re_exports.borrow_mut().push(export_all)
  }
}

struct SymbolFiller<'a> {
  is_declaration: bool,
  builder: ModuleBuilder<'a>,
}

impl<'a> SymbolFiller<'a> {
  fn fill(&self, program: &'a Program<'a>) {
    let module_symbol = self.builder.create_new_symbol_for_root();
    module_symbol.add_decl(SymbolDecl::new(
      SymbolDeclKind::Definition(SymbolNode(SymbolNodeInner::Module(
        program.span,
      ))),
      program.span,
    ));
    let is_declaration = self.is_declaration;
    for stmt in &program.body {
      self.fill_stmt(
        stmt,
        module_symbol,
        is_declaration,
      );
    }
  }

  fn fill_stmt(
    &self,
    stmt: &'a Statement<'a>,
    module_symbol: &SymbolMut<'a>,
    is_ambient: bool,
  ) {
    match stmt {
      // Module declarations (imports/exports)
      Statement::ImportDeclaration(import_decl) => {
        if let Some(specifiers) = &import_decl.specifiers {
          for specifier in specifiers {
            match specifier {
              ImportDeclarationSpecifier::ImportSpecifier(n) => {
                let imported_name = module_export_name_to_string(&n.imported);
                self.builder.ensure_symbol_for_swc_id(
                  n.local.to_id(),
                  SymbolDecl::new(
                    SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Name(imported_name),
                      specifier: import_decl.source.value.to_string(),
                    }),
                    n.span,
                  ),
                  module_symbol.symbol_id(),
                );
              }
              ImportDeclarationSpecifier::ImportDefaultSpecifier(n) => {
                self.builder.ensure_symbol_for_swc_id(
                  n.local.to_id(),
                  SymbolDecl::new(
                    SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Name("default".to_string()),
                      specifier: import_decl.source.value.to_string(),
                    }),
                    n.span,
                  ),
                  module_symbol.symbol_id(),
                );
              }
              ImportDeclarationSpecifier::ImportNamespaceSpecifier(n) => {
                self.builder.ensure_symbol_for_swc_id(
                  n.local.to_id(),
                  SymbolDecl::new(
                    SymbolDeclKind::FileRef(FileDep {
                      name: FileDepName::Star,
                      specifier: import_decl.source.value.to_string(),
                    }),
                    n.span,
                  ),
                  module_symbol.symbol_id(),
                );
              }
            }
          }
        }
      }
      Statement::ExportNamedDeclaration(export_decl) => {
        if let Some(decl) = &export_decl.declaration {
          // export { declaration }
          self.fill_export_named_decl(export_decl, decl, module_symbol, is_ambient);
        } else {
          // export { specifiers } or export { specifiers } from 'source'
          self.fill_export_named_specifiers(export_decl, module_symbol);
        }
      }
      Statement::ExportDefaultDeclaration(default_decl) => {
        match &default_decl.declaration {
          ExportDefaultDeclarationKind::ClassDeclaration(n) => {
            let default_export_symbol_id =
              self.builder.ensure_default_export_symbol(
                module_symbol,
                SymbolDecl::new(
                  SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::ExportDefaultDecl(default_decl),
                  )),
                  default_decl.span,
                ),
              );
            if let Some(ident) = n.id.as_ref() {
              self.builder.swc_id_to_symbol_id.insert(ident.to_id(), default_export_symbol_id);
            }
            let symbol = self.builder.symbol_mut(default_export_symbol_id).unwrap();
            self.fill_class(symbol, n);
          }
          ExportDefaultDeclarationKind::FunctionDeclaration(n) => {
            let default_export_symbol_id =
              self.builder.ensure_default_export_symbol(
                module_symbol,
                SymbolDecl::new(
                  SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::ExportDefaultDecl(default_decl),
                  )),
                  default_decl.span,
                ),
              );
            if let Some(ident) = n.id.as_ref() {
              self.builder.swc_id_to_symbol_id.insert(ident.to_id(), default_export_symbol_id);
            }
            // nothing to fill for functions
          }
          ExportDefaultDeclarationKind::TSInterfaceDeclaration(n) => {
            let default_export_symbol_id =
              self.builder.ensure_default_export_symbol(
                module_symbol,
                SymbolDecl::new(
                  SymbolDeclKind::Definition(SymbolNode(
                    SymbolNodeInner::ExportDefaultDecl(default_decl),
                  )),
                  default_decl.span,
                ),
              );
            self.builder.swc_id_to_symbol_id.insert(n.id.to_id(), default_export_symbol_id);
            let symbol = self.builder.symbol_mut(default_export_symbol_id).unwrap();
            self.fill_ts_interface(symbol, n)
          }
          _ => {
            // Expression - handle as default expr (Target decl, not Definition)
            self.handle_export_default_expr_from_decl(
              module_symbol,
              default_decl,
            );
          }
        }
      }
      Statement::ExportAllDeclaration(n) => {
        if let Some(exported) = &n.exported {
          // `export * as name from "..."` - this is a named namespace re-export
          let export_name = module_export_name_to_string(exported);
          let symbol =
            self.builder.create_new_symbol(module_symbol.symbol_id());
          let decl_span = n.span;
          symbol.add_decl(SymbolDecl::new(
            SymbolDeclKind::FileRef(FileDep {
              name: FileDepName::Star,
              specifier: n.source.value.to_string(),
            }),
            decl_span,
          ));
          let symbol_id = symbol.symbol_id();
          module_symbol.add_export(export_name, symbol_id);
        } else {
          // `export * from "..."` - bare star re-export
          self
            .builder
            .add_re_export(n);
        }
      }
      Statement::TSImportEqualsDeclaration(import_equals) => {
        self.ensure_symbol_for_import_equals(import_equals, module_symbol);
      }
      Statement::TSExportAssignment(export_assignment) => {
        self.handle_export_default_expr_from_assignment(
          module_symbol,
          export_assignment,
        );
      }
      Statement::TSNamespaceExportDeclaration(_) => {
        // ignore
      }
      // Regular statements
      Statement::BlockStatement(_)
      | Statement::EmptyStatement(_)
      | Statement::DebuggerStatement(_)
      | Statement::WithStatement(_)
      | Statement::ReturnStatement(_)
      | Statement::LabeledStatement(_)
      | Statement::BreakStatement(_)
      | Statement::ContinueStatement(_)
      | Statement::IfStatement(_)
      | Statement::SwitchStatement(_)
      | Statement::ThrowStatement(_)
      | Statement::TryStatement(_)
      | Statement::WhileStatement(_)
      | Statement::DoWhileStatement(_)
      | Statement::ForStatement(_)
      | Statement::ForInStatement(_)
      | Statement::ForOfStatement(_) => {
        // ignore
      }
      Statement::ExpressionStatement(n) => {
        if let Expression::AssignmentExpression(assign_expr) = &n.expression {
          if let Some(expando_ref) = ExpandoPropertyRef::maybe_new(assign_expr)
            && let Some(symbol_id) = self
              .builder
              .swc_id_to_symbol_id
              .get(&expando_ref.obj_ident().to_id())
          {
            let symbol = self.builder.symbol_mut(symbol_id).unwrap();
            // expando properties are only valid on a function
            if symbol
              .borrow_inner()
              .decls()
              .iter()
              .any(|d| d.is_function())
            {
              self.create_symbol_member_or_export(
                symbol,
                SymbolNodeRef::ExpandoProperty(expando_ref),
              );
            }
          }
        }
      }
      // Declarations (non-exported)
      Statement::ClassDeclaration(n) => {
        let decls_are_exports =
          is_ambient && !module_symbol.borrow_inner().is_module();
        if let Some(ident) = &n.id {
          let id = ident.to_id();
          let symbol = self.builder.get_symbol_from_swc_id(
            id,
            SymbolDecl::new(
              SymbolDeclKind::Definition(SymbolNode(
                SymbolNodeInner::ClassDecl(
                  n,
                ),
              )),
              n.span,
            ),
            module_symbol.symbol_id(),
          );
          self.fill_class(symbol, n);
          let symbol_id = symbol.symbol_id();
          module_symbol.add_child_id(symbol_id);
          if decls_are_exports {
            module_symbol.add_export(ident.name.to_string(), symbol_id);
          }
        }
      }
      Statement::FunctionDeclaration(n) => {
        let decls_are_exports =
          is_ambient && !module_symbol.borrow_inner().is_module();
        if let Some(ident) = &n.id {
          let symbol_id = self.builder.ensure_symbol_for_swc_id(
            ident.to_id(),
            SymbolDecl::new(
              SymbolDeclKind::Definition(SymbolNode(
                SymbolNodeInner::FnDecl(
                  n,
                ),
              )),
              n.span,
            ),
            module_symbol.symbol_id(),
          );
          module_symbol.add_child_id(symbol_id);
          if decls_are_exports {
            module_symbol.add_export(ident.name.to_string(), symbol_id);
          }
        }
      }
      Statement::VariableDeclaration(var_decl) => {
        let decls_are_exports =
          is_ambient && !module_symbol.borrow_inner().is_module();
        let is_using = matches!(
          var_decl.kind,
          VariableDeclarationKind::Using | VariableDeclarationKind::AwaitUsing
        );
        for decl in &var_decl.declarations {
          for ident in find_binding_ids(&decl.id) {
            let export_name =
              decls_are_exports.then(|| ident.name.to_string());
            let node_inner = if is_using {
              SymbolNodeInner::UsingVar(
                var_decl,
                decl,
                ident,
              )
            } else {
              SymbolNodeInner::Var(
                var_decl,
                decl,
                ident,
              )
            };
            let symbol_id = self.builder.ensure_symbol_for_swc_id(
              ident.to_id(),
              SymbolDecl::new(
                SymbolDeclKind::Definition(SymbolNode(node_inner)),
                decl.span,
              ),
              module_symbol.symbol_id(),
            );
            module_symbol.add_child_id(symbol_id);
            if let Some(export_name) = export_name {
              module_symbol.add_export(export_name, symbol_id);
            }
          }
        }
      }
      Statement::TSInterfaceDeclaration(n) => {
        let decls_are_exports =
          is_ambient && !module_symbol.borrow_inner().is_module();
        let id = n.id.to_id();
        let symbol = self.builder.get_symbol_from_swc_id(
          id,
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::TsInterface(
                n,
              ),
            )),
            n.span,
          ),
          module_symbol.symbol_id(),
        );
        self.fill_ts_interface(symbol, n);
        let symbol_id = symbol.symbol_id();
        module_symbol.add_child_id(symbol_id);
        if decls_are_exports {
          module_symbol.add_export(n.id.name.to_string(), symbol_id);
        }
      }
      Statement::TSTypeAliasDeclaration(n) => {
        let decls_are_exports =
          is_ambient && !module_symbol.borrow_inner().is_module();
        let symbol_id = self.builder.ensure_symbol_for_swc_id(
          n.id.to_id(),
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::TsTypeAlias(
                n,
              ),
            )),
            n.span,
          ),
          module_symbol.symbol_id(),
        );
        module_symbol.add_child_id(symbol_id);
        if decls_are_exports {
          module_symbol.add_export(n.id.name.to_string(), symbol_id);
        }
      }
      Statement::TSEnumDeclaration(n) => {
        let decls_are_exports =
          is_ambient && !module_symbol.borrow_inner().is_module();
        let symbol_id = self.builder.ensure_symbol_for_swc_id(
          n.id.to_id(),
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::TsEnum(n),
            )),
            n.span,
          ),
          module_symbol.symbol_id(),
        );
        module_symbol.add_child_id(symbol_id);
        if decls_are_exports {
          module_symbol.add_export(n.id.name.to_string(), symbol_id);
        }
      }
      Statement::TSModuleDeclaration(n) => {
        let decls_are_exports =
          is_ambient && !module_symbol.borrow_inner().is_module();
        let symbol_id = self.fill_ts_module(
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::TsNamespace(n),
            )),
            n.span,
          ),
          n,
          module_symbol,
          is_ambient || n.declare,
        );
        if let Some(symbol_id) = symbol_id {
          module_symbol.add_child_id(symbol_id);
          if decls_are_exports {
            match &n.id {
              TSModuleDeclarationName::Identifier(ident) => {
                module_symbol.add_export(ident.name.to_string(), symbol_id);
              }
              TSModuleDeclarationName::StringLiteral(_) => {}
            }
          }
        }
      }
      // TSGlobalDeclaration and any other statements we don't handle
      _ => {
        // ignore
      }
    }
  }

  fn fill_export_named_decl(
    &self,
    export_decl: &'a ExportNamedDeclaration<'a>,
    decl: &'a Declaration<'a>,
    module_symbol: &SymbolMut<'a>,
    is_ambient: bool,
  ) {
    match decl {
      Declaration::ClassDeclaration(n) => {
        if let Some(ident) = &n.id {
          let symbol = self.builder.get_symbol_from_swc_id(
            ident.to_id(),
            SymbolDecl::new(
              SymbolDeclKind::Definition(SymbolNode(
                SymbolNodeInner::ExportDecl(
                  export_decl,
                  SymbolNodeInnerExportDecl::Class(n),
                ),
              )),
              export_decl.span,
            ),
            module_symbol.symbol_id(),
          );
          self.fill_class(symbol, n);
          let symbol_id = symbol.symbol_id();
          module_symbol.add_export(ident.name.to_string(), symbol_id);
          module_symbol.add_child_id(symbol_id);
        }
      }
      Declaration::FunctionDeclaration(n) => {
        if let Some(ident) = &n.id {
          let symbol_id = self.builder.ensure_symbol_for_swc_id(
            ident.to_id(),
            SymbolDecl::new(
              SymbolDeclKind::Definition(SymbolNode(
                SymbolNodeInner::ExportDecl(
                  export_decl,
                  SymbolNodeInnerExportDecl::Fn(n),
                ),
              )),
              export_decl.span,
            ),
            module_symbol.symbol_id(),
          );
          module_symbol.add_export(ident.name.to_string(), symbol_id);
          module_symbol.add_child_id(symbol_id);
        }
      }
      Declaration::VariableDeclaration(n) => {
        for decl in &n.declarations {
          for ident in find_binding_ids(&decl.id) {
            let export_name = ident.name.to_string();
            let id = ident.to_id();
            let symbol_id = self.builder.ensure_symbol_for_swc_id(
              id.clone(),
              SymbolDecl::new(
                SymbolDeclKind::Definition(SymbolNode(
                  SymbolNodeInner::ExportDecl(
                    export_decl,
                    SymbolNodeInnerExportDecl::Var(
                      n,
                      decl,
                      ident,
                    ),
                  ),
                )),
                decl.span,
              ),
              module_symbol.symbol_id(),
            );
            module_symbol.add_child_id(symbol_id);
            module_symbol.add_export(export_name, symbol_id);
          }
        }
      }
      Declaration::TSInterfaceDeclaration(n) => {
        let symbol = self.builder.get_symbol_from_swc_id(
          n.id.to_id(),
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::ExportDecl(
                export_decl,
                SymbolNodeInnerExportDecl::TsInterface(
                  n,
                ),
              ),
            )),
            export_decl.span,
          ),
          module_symbol.symbol_id(),
        );
        self.fill_ts_interface(symbol, n);
        let symbol_id = symbol.symbol_id();
        module_symbol.add_export(n.id.name.to_string(), symbol_id);
        module_symbol.add_child_id(symbol_id);
      }
      Declaration::TSTypeAliasDeclaration(n) => {
        let symbol_id = self.builder.ensure_symbol_for_swc_id(
          n.id.to_id(),
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::ExportDecl(
                export_decl,
                SymbolNodeInnerExportDecl::TsTypeAlias(
                  n,
                ),
              ),
            )),
            export_decl.span,
          ),
          module_symbol.symbol_id(),
        );
        module_symbol.add_export(n.id.name.to_string(), symbol_id);
        module_symbol.add_child_id(symbol_id);
      }
      Declaration::TSEnumDeclaration(n) => {
        let symbol_id = self.builder.ensure_symbol_for_swc_id(
          n.id.to_id(),
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::ExportDecl(
                export_decl,
                SymbolNodeInnerExportDecl::TsEnum(n),
              ),
            )),
            export_decl.span,
          ),
          module_symbol.symbol_id(),
        );
        module_symbol.add_export(n.id.name.to_string(), symbol_id);
        module_symbol.add_child_id(symbol_id);
      }
      Declaration::TSModuleDeclaration(n) => {
        let maybe_symbol_id = self.fill_ts_module(
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::ExportDecl(
                export_decl,
                SymbolNodeInnerExportDecl::TsNamespace(
                  n,
                ),
              ),
            )),
            export_decl.span,
          ),
          n,
          module_symbol,
          is_ambient || n.declare,
        );
        if let Some(symbol_id) = maybe_symbol_id {
          match &n.id {
            TSModuleDeclarationName::Identifier(ident) => {
              module_symbol.add_export(ident.name.to_string(), symbol_id);
              module_symbol.add_child_id(symbol_id);
            }
            TSModuleDeclarationName::StringLiteral(_) => {}
          }
        }
      }
      Declaration::TSGlobalDeclaration(_) => {
        // ignore
      }
      Declaration::TSImportEqualsDeclaration(n) => {
        let symbol_id = self.builder.ensure_symbol_for_swc_id(
          n.id.to_id(),
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::ExportDecl(
                export_decl,
                SymbolNodeInnerExportDecl::TsImportEquals(
                  n.as_ref(),
                ),
              ),
            )),
            export_decl.span,
          ),
          module_symbol.symbol_id(),
        );
        module_symbol.add_export(n.id.name.to_string(), symbol_id);
        module_symbol.add_child_id(symbol_id);
      }
    }
  }

  fn fill_export_named_specifiers(
    &self,
    n: &ExportNamedDeclaration,
    module_symbol: &SymbolMut,
  ) {
    for specifier in &n.specifiers {
      let local_name = module_export_name_to_string(&specifier.local);
      let exported_name = module_export_name_to_string(&specifier.exported);

      match &n.source {
        Some(src) => {
          let symbol =
            self.builder.create_new_symbol(module_symbol.symbol_id());
          symbol.add_decl(SymbolDecl::new(
            SymbolDeclKind::FileRef(FileDep {
              name: FileDepName::Name(local_name),
              specifier: src.value.to_string(),
            }),
            specifier.span,
          ));
          let symbol_id = symbol.symbol_id();
          module_symbol.add_export(exported_name, symbol_id);
        }
        None => {
          let local_id = module_export_name_to_id(&specifier.local);
          if local_name != exported_name {
            let orig_symbol = self
              .builder
              .create_new_symbol(module_symbol.symbol_id());
            orig_symbol.add_decl(SymbolDecl::new(
              SymbolDeclKind::Target(local_id),
              specifier.span,
            ));
            module_symbol
              .add_export(exported_name, orig_symbol.symbol_id());
          } else {
            let named_symbol = self
              .builder
              .create_new_symbol(module_symbol.symbol_id());
            named_symbol.add_decl(SymbolDecl::new(
              SymbolDeclKind::Target(local_id),
              specifier.span,
            ));
            module_symbol.add_export(
              local_name,
              named_symbol.symbol_id(),
            );
          }
        }
      }
    }
  }

  fn handle_export_default_expr_from_decl(
    &self,
    module_symbol: &SymbolMut<'a>,
    default_decl: &'a ExportDefaultDeclaration<'a>,
  ) {
    match &default_decl.declaration {
      ExportDefaultDeclarationKind::Identifier(ident) => {
        // note: don't associate identifier exports with the swc id because
        // we don't want the SymbolDeclKind appearing with the definition symbols
        self.builder.ensure_default_export_symbol(
          module_symbol,
          SymbolDecl::new(SymbolDeclKind::Target(ident.to_id()), ident.span),
        );
      }
      _expr => {
        self.builder.ensure_default_export_symbol(
          module_symbol,
          SymbolDecl::new(
            SymbolDeclKind::Definition(SymbolNode(
              SymbolNodeInner::ExportDefaultExpr(&default_decl.declaration),
            )),
            default_decl.span,
          ),
        );
      }
    }
  }

  fn handle_export_default_expr_from_assignment(
    &self,
    module_symbol: &SymbolMut<'a>,
    export_assignment: &'a TSExportAssignment<'a>,
  ) {
    match &export_assignment.expression {
      Expression::Identifier(ident) => {
        self.builder.ensure_default_export_symbol(
          module_symbol,
          SymbolDecl::new(SymbolDeclKind::Target(ident.to_id()), ident.span),
        );
      }
      _ => {
        // For TSExportAssignment with non-identifier expressions, we can't easily
        // Skip for now.
      }
    }
  }

  fn ensure_symbol_for_import_equals(
    &self,
    import_equals: &TSImportEqualsDeclaration,
    parent_symbol: &SymbolMut,
  ) -> SymbolId {
    let id = import_equals.id.to_id();
    if let Some(symbol_id) = self.builder.swc_id_to_symbol_id.get(&id) {
      return symbol_id;
    }
    match &import_equals.module_reference {
      TSModuleReference::IdentifierReference(ident) => {
        let (leftmost_id, parts) = ((ident.name.to_string(), 0), Vec::new());
        self.builder.ensure_symbol_for_swc_id(
          id,
          SymbolDecl::new(
            SymbolDeclKind::QualifiedTarget(leftmost_id, parts),
            import_equals.span,
          ),
          parent_symbol.symbol_id(),
        )
      }
      TSModuleReference::QualifiedName(qualified_name) => {
        let (leftmost_id, parts) = super::helpers::ts_qualified_name_parts(qualified_name);
        self.builder.ensure_symbol_for_swc_id(
          id,
          SymbolDecl::new(
            SymbolDeclKind::QualifiedTarget(leftmost_id, parts),
            import_equals.span,
          ),
          parent_symbol.symbol_id(),
        )
      }
      TSModuleReference::ExternalModuleReference(module_ref) => {
        self.builder.ensure_symbol_for_swc_id(
          id,
          SymbolDecl::new(
            SymbolDeclKind::FileRef(FileDep {
              name: FileDepName::Name("default".to_string()),
              specifier: module_ref.expression.value.to_string(),
            }),
            import_equals.span,
          ),
          parent_symbol.symbol_id(),
        )
      }
    }
  }

  fn fill_class(&self, symbol: &SymbolMut<'a>, n: &'a Class<'a>) {
    self.fill_class_elements(symbol, &n.body.body);
  }

  fn fill_ts_interface(&self, symbol: &SymbolMut<'a>, n: &'a TSInterfaceDeclaration<'a>) {
    for member in &n.body.body {
      match member {
        TSSignature::TSCallSignatureDeclaration(n) => {
          self.create_symbol_member_or_export(
            symbol,
            SymbolNodeRef::TsCallSignatureDecl(n),
          );
        }
        TSSignature::TSConstructSignatureDeclaration(n) => {
          self.create_symbol_member_or_export(
            symbol,
            SymbolNodeRef::TsConstructSignatureDecl(n),
          );
        }
        TSSignature::TSPropertySignature(n) => {
          self.create_symbol_member_or_export(
            symbol,
            SymbolNodeRef::TsPropertySignature(n),
          );
        }
        TSSignature::TSIndexSignature(n) => {
          self.create_symbol_member_or_export(
            symbol,
            SymbolNodeRef::TsIndexSignature(n),
          );
        }
        TSSignature::TSMethodSignature(n) => {
          let node_ref = if n.kind == TSMethodSignatureKind::Get {
            SymbolNodeRef::TsGetterSignature(n)
          } else if n.kind == TSMethodSignatureKind::Set {
            SymbolNodeRef::TsSetterSignature(n)
          } else {
            SymbolNodeRef::TsMethodSignature(n)
          };
          self.create_symbol_member_or_export(symbol, node_ref);
        }
      }
    }
  }

  fn fill_ts_module(
    &self,
    symbol_decl: SymbolDecl<'a>,
    n: &'a TSModuleDeclaration<'a>,
    parent_symbol: &SymbolMut<'a>,
    is_ambient: bool,
  ) -> Option<SymbolId> {
    let mut id = match &n.id {
      TSModuleDeclarationName::Identifier(ident) => ident.to_id(),
      TSModuleDeclarationName::StringLiteral(_) => return None, // ignore for now
    };
    let mod_symbol = self.builder.get_symbol_from_swc_id(
      id.clone(),
      symbol_decl,
      parent_symbol.symbol_id(),
    );

    // fill the exported declarations
    if let Some(body) = &n.body {
      let mod_symbol = mod_symbol;
      match body {
        TSModuleDeclarationBody::TSModuleBlock(block) => {
          for item in &block.body {
            self.fill_stmt(item, mod_symbol, is_ambient || n.declare);
          }
        }
        TSModuleDeclarationBody::TSModuleDeclaration(inner_decl) => {
          // nested namespace: `namespace A.B { ... }`
          let previous_symbol = mod_symbol;
          id = match &inner_decl.id {
            TSModuleDeclarationName::Identifier(ident) => ident.to_id(),
            TSModuleDeclarationName::StringLiteral(_) => return Some(mod_symbol.symbol_id()),
          };
          let nested_mod_symbol = self.builder.get_symbol_from_swc_id(
            id.clone(),
            SymbolDecl::new(
              SymbolDeclKind::Definition(SymbolNode(
                SymbolNodeInner::TsNamespace(inner_decl),
              )),
              inner_decl.span,
            ),
            previous_symbol.symbol_id(),
          );
          previous_symbol
            .add_export(id.0.to_string(), nested_mod_symbol.symbol_id());
          previous_symbol.add_child_id(nested_mod_symbol.symbol_id());

          // Recursively handle the inner module body
          if let Some(inner_body) = &inner_decl.body {
            match inner_body {
              TSModuleDeclarationBody::TSModuleBlock(block) => {
                for item in &block.body {
                  self.fill_stmt(item, nested_mod_symbol, is_ambient || inner_decl.declare);
                }
              }
              TSModuleDeclarationBody::TSModuleDeclaration(_) => {
                // TODO: handle deeply nested namespaces
              }
            }
          }
        }
      }
    }

    Some(mod_symbol.symbol_id())
  }

  fn fill_class_elements(&self, symbol: &SymbolMut<'a>, members: &'a [ClassElement<'a>]) {
    for member in members {
      match member {
        ClassElement::MethodDefinition(method) => {
          if method.kind == MethodDefinitionKind::Constructor {
            self.create_symbol_member_or_export(
              symbol,
              SymbolNodeRef::Constructor(method),
            );
            // Check for parameter properties in the constructor
            for param in &method.value.params.items {
              if param.accessibility.is_some() || param.r#override || param.readonly {
                self.create_symbol_member_or_export(
                  symbol,
                  SymbolNodeRef::ClassParamProp(param),
                );
              }
            }
          } else {
            self.create_symbol_member_or_export(
              symbol,
              SymbolNodeRef::ClassMethod(method),
            );
          }
        }
        ClassElement::PropertyDefinition(prop) => {
          self.create_symbol_member_or_export(
            symbol,
            SymbolNodeRef::ClassProp(prop),
          );
        }
        ClassElement::TSIndexSignature(signature) => {
          self.create_symbol_member_or_export(
            symbol,
            SymbolNodeRef::TsIndexSignature(signature),
          );
        }
        ClassElement::AccessorProperty(prop) => {
          self.create_symbol_member_or_export(
            symbol,
            SymbolNodeRef::AutoAccessor(prop),
          );
        }
        ClassElement::StaticBlock(_) => {
          // ignore
        }
      }
    }
  }

  fn create_symbol_member_or_export<'b>(
    &'b self,
    parent_symbol: &'b SymbolMut<'a>,
    node_ref: SymbolNodeRef<'a>,
  ) -> &'b SymbolMut<'a> {
    let (node_inner, is_static, source_range) = match node_ref {
      SymbolNodeRef::AutoAccessor(n) => (
        SymbolNodeInner::AutoAccessor(n),
        n.r#static,
        n.span,
      ),
      SymbolNodeRef::ClassMethod(n) => (
        SymbolNodeInner::ClassMethod(n),
        n.r#static,
        n.span,
      ),
      SymbolNodeRef::ClassProp(n) => (
        SymbolNodeInner::ClassProp(n),
        n.r#static,
        n.span,
      ),
      SymbolNodeRef::ClassParamProp(n) => (
        SymbolNodeInner::ClassParamProp(n),
        /* is_static */ false,
        n.span,
      ),
      SymbolNodeRef::Constructor(n) => (
        SymbolNodeInner::Constructor(n),
        true,
        n.span,
      ),
      SymbolNodeRef::ExpandoProperty(n) => (
        SymbolNodeInner::ExpandoProperty(n.0),
        true,
        n.0.span(),
      ),
      SymbolNodeRef::TsIndexSignature(n) => (
        SymbolNodeInner::TsIndexSignature(n),
        false,
        n.span,
      ),
      SymbolNodeRef::TsCallSignatureDecl(n) => (
        SymbolNodeInner::TsCallSignatureDecl(n),
        false,
        n.span,
      ),
      SymbolNodeRef::TsConstructSignatureDecl(n) => (
        SymbolNodeInner::TsConstructSignatureDecl(n),
        false,
        n.span,
      ),
      SymbolNodeRef::TsPropertySignature(n) => (
        SymbolNodeInner::TsPropertySignature(n),
        false,
        n.span,
      ),
      SymbolNodeRef::TsGetterSignature(n) => (
        SymbolNodeInner::TsGetterSignature(n),
        false,
        n.span,
      ),
      SymbolNodeRef::TsSetterSignature(n) => (
        SymbolNodeInner::TsSetterSignature(n),
        false,
        n.span,
      ),
      SymbolNodeRef::TsMethodSignature(n) => (
        SymbolNodeInner::TsMethodSignature(n),
        false,
        n.span,
      ),
      SymbolNodeRef::Module(_)
      | SymbolNodeRef::ClassDecl(_)
      | SymbolNodeRef::ExportDecl(..)
      | SymbolNodeRef::ExportDefaultDecl(_)
      | SymbolNodeRef::ExportDefaultExpr(_)
      | SymbolNodeRef::FnDecl(_)
      | SymbolNodeRef::TsEnum(_)
      | SymbolNodeRef::TsInterface(_)
      | SymbolNodeRef::TsNamespace(_)
      | SymbolNodeRef::TsTypeAlias(_)
      | SymbolNodeRef::Var(..)
      | SymbolNodeRef::UsingVar(..) => unreachable!(),
    };

    let decl = SymbolDecl::new(
      SymbolDeclKind::Definition(SymbolNode(node_inner)),
      source_range,
    );

    let child_symbol =
      if let Some(decl_name) = decl.maybe_name().map(|n| n.to_string()) {
        // static members on a class are children and an export if they have a name
        // while instance members are "members"
        if is_static {
          match parent_symbol.export(&decl_name) {
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
    parent_symbol: &'b SymbolMut<'a>,
    searching_name: &str,
  ) -> Option<&'b SymbolMut<'a>> {
    let parent_symbol = parent_symbol.borrow_inner();
    for child_id in parent_symbol.child_ids() {
      let child_symbol_mut = self.builder.symbol_mut(child_id).unwrap();
      let child_symbol = child_symbol_mut.borrow_inner();
      if let Some(name) = child_symbol.maybe_name()
        && name == searching_name
      {
        return Some(child_symbol_mut);
      }
    }
    None
  }

  fn get_member_symbol_with_name<'b>(
    &'b self,
    parent_symbol: &'b SymbolMut<'a>,
    searching_name: &str,
  ) -> Option<&'b SymbolMut<'a>> {
    let parent_symbol = parent_symbol.borrow_inner();
    for member_id in parent_symbol.members() {
      let child_symbol_mut = self.builder.symbol_mut(*member_id).unwrap();
      let child_symbol = child_symbol_mut.borrow_inner();
      if let Some(name) = child_symbol.maybe_name()
        && name == searching_name
      {
        return Some(child_symbol_mut);
      }
    }
    None
  }
}

/// Extract all binding identifiers from a binding pattern.
fn find_binding_ids<'a>(pattern: &'a BindingPattern<'a>) -> Vec<&'a BindingIdentifier<'a>> {
  let mut result = Vec::new();
  collect_binding_ids(pattern, &mut result);
  result
}

fn collect_binding_ids<'a>(pattern: &'a BindingPattern<'a>, result: &mut Vec<&'a BindingIdentifier<'a>>) {
  match pattern {
    BindingPattern::BindingIdentifier(ident) => {
      result.push(ident);
    }
    BindingPattern::ObjectPattern(obj) => {
      for prop in &obj.properties {
        collect_binding_ids(&prop.value, result);
      }
      if let Some(rest) = &obj.rest {
        collect_binding_ids(&rest.argument, result);
      }
    }
    BindingPattern::ArrayPattern(arr) => {
      for elem in arr.elements.iter().flatten() {
        collect_binding_ids(elem, result);
      }
      if let Some(rest) = &arr.rest {
        collect_binding_ids(&rest.argument, result);
      }
    }
    BindingPattern::AssignmentPattern(assign) => {
      collect_binding_ids(&assign.left, result);
    }
  }
}

fn module_export_name_to_string(name: &ModuleExportName) -> String {
  match name {
    ModuleExportName::IdentifierName(n) => n.name.to_string(),
    ModuleExportName::IdentifierReference(n) => n.name.to_string(),
    ModuleExportName::StringLiteral(n) => n.value.to_string(),
  }
}

fn module_export_name_to_id(name: &ModuleExportName) -> Id {
  match name {
    ModuleExportName::IdentifierName(n) => n.to_id(),
    ModuleExportName::IdentifierReference(n) => n.to_id(),
    ModuleExportName::StringLiteral(n) => (n.value.to_string(), 0),
  }
}

