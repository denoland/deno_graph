// Copyright 2018-2024 the Deno authors. MIT license.

// for span methods, which actually make sense to use here in the transforms
#![allow(clippy::disallowed_methods)]
#![allow(clippy::disallowed_types)]

use std::cell::Cell;
use std::collections::HashSet;
use std::sync::Arc;

use deno_ast::EmitOptions;
use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::SourceTextInfo;
use deno_ast::emit;
use deno_ast::oxc::allocator::Allocator;
use deno_ast::oxc::allocator::Box as OxcBox;
use deno_ast::oxc::allocator::CloneIn;
use deno_ast::oxc::allocator::Vec as OxcVec;
use deno_ast::oxc::ast::Comment;
use deno_ast::oxc::ast::ast::*;
use deno_ast::oxc::span::Atom;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Ident;
use deno_ast::oxc::span::SPAN;
use deno_ast::oxc::span::Span;
use deno_ast::oxc::syntax::node::NodeId;
use indexmap::IndexMap;

use crate::ModuleGraph;
use crate::WorkspaceMember;
use crate::analysis::ModuleInfo;
use crate::ast::ParserModuleAnalyzer;
use crate::symbols::EsModuleInfo;
use crate::symbols::Symbol;

use super::FastCheckDiagnostic;
use super::FastCheckDiagnosticRange;
use super::helpers::DeclMutabilityKind;
use super::helpers::ReturnStatementAnalysis;
use super::helpers::TSKeywordKind;
use super::helpers::analyze_return_stmts_in_function_body;
use super::helpers::any_type_ann;
use super::helpers::is_void_type;
use super::helpers::maybe_lit_to_ts_type;
use super::helpers::ts_keyword_type;
use super::helpers::type_ann;
use super::range_finder::ModulePublicRanges;
use super::transform_dts::FastCheckDtsDiagnostic;
use super::transform_dts::FastCheckDtsTransformer;

/// Comments management for fast check transforms.
/// In OXC, comments are stored as `Vec<Comment>` in `Program.comments`.
pub struct CommentsMut<'a> {
  comments: OxcVec<'a, Comment>,
}

impl<'a> CommentsMut<'a> {
  pub fn new(allocator: &'a Allocator, comments: &[Comment]) -> Self {
    let mut filtered = OxcVec::new_in(allocator);
    for c in comments {
      // keep all comments (line, single-line block, multi-line block)
      filtered.push(c.clone_in(allocator));
    }
    Self { comments: filtered }
  }

  pub fn remove_leading(&mut self, span: Span) {
    // Remove comments attached to the given span start position
    self.comments.retain(|c| c.attached_to != span.start);
  }

  pub fn into_comments(self) -> OxcVec<'a, Comment> {
    self.comments
  }
}

#[derive(Debug, Clone)]
pub struct FastCheckDtsModule {
  pub program_text: String,
  pub diagnostics: Vec<FastCheckDtsDiagnostic>,
}

#[derive(Debug)]
pub struct FastCheckModule {
  pub module_info: Arc<ModuleInfo>,
  pub text: Arc<str>,
  pub source_map: Arc<str>,
  pub dts: Option<FastCheckDtsModule>,
}

pub struct TransformOptions<'a> {
  pub workspace_members: &'a [WorkspaceMember],
  pub should_error_on_first_diagnostic: bool,
  pub dts: bool,
}

pub fn transform(
  graph: &ModuleGraph,
  es_module_info: &EsModuleInfo,
  public_ranges: &ModulePublicRanges,
  options: &TransformOptions,
) -> Result<FastCheckModule, Vec<FastCheckDiagnostic>> {
  let allocator = Allocator::default();
  let mut transformer = FastCheckTransformer::new(
    &allocator,
    graph,
    es_module_info,
    public_ranges,
    options.should_error_on_first_diagnostic,
  );
  let program = transformer.transform()?;
  if !transformer.diagnostics.is_empty() {
    return Err(transformer.diagnostics);
  }
  let specifier = es_module_info.specifier();
  let source_text = es_module_info.source_text();
  let text_info = es_module_info.source_text_info();

  let module_info = ParserModuleAnalyzer::module_info_from_program(
    es_module_info.media_type(),
    &program,
    text_info,
    source_text,
  );

  // now emit
  let emitted_source = emit(
    &program,
    source_text,
    specifier.as_str(),
    &EmitOptions {
      remove_comments: false,
      source_map: deno_ast::SourceMapOption::Separate,
      source_map_base: None,
      source_map_file: None,
      inline_sources: false,
      ascii_only: false,
    },
  )
  .map_err(|e| {
    vec![FastCheckDiagnostic::Emit {
      specifier: specifier.clone(),
      inner: Arc::new(e),
    }]
  })?;
  let emitted_text = emitted_source.text;

  let dts = if options.dts {
    let mut dts_transformer = FastCheckDtsTransformer::new(
      &allocator,
      text_info,
      public_ranges,
      specifier,
    );

    let dts_program = dts_transformer.transform(program);

    let dts_emitted = emit(
      &dts_program,
      source_text,
      specifier.as_str(),
      &EmitOptions {
        remove_comments: false,
        source_map: deno_ast::SourceMapOption::None,
        source_map_base: None,
        source_map_file: None,
        inline_sources: false,
        ascii_only: false,
      },
    )
    .map_err(|e| {
      vec![FastCheckDiagnostic::Emit {
        specifier: specifier.clone(),
        inner: Arc::new(e),
      }]
    })?;

    Some(FastCheckDtsModule {
      program_text: dts_emitted.text,
      diagnostics: dts_transformer.diagnostics,
    })
  } else {
    None
  };

  Ok(FastCheckModule {
    module_info: module_info.into(),
    text: emitted_text.into(),
    dts,
    source_map: emitted_source.source_map.unwrap().into(),
  })
}

enum TransformItemResult {
  /// Retain the item as is.
  Retain,
  /// Remove the item.
  Remove,
}

impl TransformItemResult {
  fn from_retain(retain: bool) -> Self {
    if retain { Self::Retain } else { Self::Remove }
  }
}

/// OXC equivalent of SWC's Id type: (name, scope_id)
type Id = (String, usize);

struct FastCheckTransformer<'a> {
  allocator: &'a Allocator,
  graph: &'a ModuleGraph,
  specifier: &'a ModuleSpecifier,
  es_module_info: &'a EsModuleInfo<'a>,
  public_ranges: &'a ModulePublicRanges,
  media_type: MediaType,
  text_info: SourceTextInfo,
  should_error_on_first_diagnostic: bool,
  diagnostics: Vec<FastCheckDiagnostic>,
  expando_namespaces: IndexMap<Id, Vec<VariableDeclarator<'a>>>,
}

impl<'a> FastCheckTransformer<'a> {
  pub fn new(
    allocator: &'a Allocator,
    graph: &'a ModuleGraph,
    es_module_info: &'a EsModuleInfo<'a>,
    public_ranges: &'a ModulePublicRanges,
    should_error_on_first_diagnostic: bool,
  ) -> Self {
    Self {
      allocator,
      graph,
      specifier: es_module_info.specifier(),
      es_module_info,
      public_ranges,
      media_type: es_module_info.media_type(),
      text_info: es_module_info.source_text_info().clone(),
      should_error_on_first_diagnostic,
      diagnostics: Default::default(),
      expando_namespaces: Default::default(),
    }
  }

  pub fn transform(&mut self) -> Result<Program<'a>, Vec<FastCheckDiagnostic>> {
    let is_ambient = self.media_type.is_declaration();
    let mut program = self.es_module_info.program().clone_in(self.allocator);
    let mut comments = CommentsMut::new(self.allocator, &program.comments);

    let body =
      std::mem::replace(&mut program.body, OxcVec::new_in(self.allocator));
    let body_vec: Vec<Statement<'a>> = body.into_iter().collect();
    let new_body =
      self.transform_module_body(body_vec, &mut comments, is_ambient)?;
    program.body = OxcVec::from_iter_in(new_body, self.allocator);
    program.comments = comments.into_comments();
    Ok(program)
  }

  fn transform_module_body(
    &mut self,
    body: Vec<Statement<'a>>,
    comments: &mut CommentsMut<'a>,
    is_ambient: bool,
  ) -> Result<Vec<Statement<'a>>, Vec<FastCheckDiagnostic>> {
    let parent_expando_namespaces =
      std::mem::take(&mut self.expando_namespaces);
    let mut final_body = Vec::with_capacity(body.len());
    for mut item in body {
      let result = self.transform_item(&mut item, comments, is_ambient)?;
      match result {
        TransformItemResult::Retain => final_body.push(item),
        TransformItemResult::Remove => {
          comments.remove_leading(item.span());
        }
      }
    }

    // Add accumulated namespaces
    final_body.reserve(self.expando_namespaces.len());
    for (id, var_decls) in
      std::mem::take(&mut self.expando_namespaces).drain(..)
    {
      let swc_id = (id.0.clone(), id.1);
      let symbol = self.es_module_info.symbol_from_swc(&swc_id).unwrap();
      for decl in &var_decls {
        self.check_expando_property_diagnostics(decl, &id, symbol)?;
      }

      // typescript requires the export keyword to match the other
      // declarations so only add an export keyword if the other
      // decls have one and also we don't want to export something
      // that's not exported
      let has_export_keyword = symbol.decls().iter().any(|d| {
        d.maybe_node()
          .map(|n| n.has_export_keyword())
          .unwrap_or(false)
      });

      let mut decls_vec =
        OxcVec::with_capacity_in(var_decls.len(), self.allocator);
      for d in var_decls {
        decls_vec.push(d);
      }

      let var_decl_inner = OxcBox::new_in(
        VariableDeclaration {
          node_id: Cell::new(NodeId::DUMMY),
          span: SPAN,
          kind: VariableDeclarationKind::Var,
          declarations: decls_vec,
          declare: false,
        },
        self.allocator,
      );

      let export_named = Statement::ExportNamedDeclaration(OxcBox::new_in(
        ExportNamedDeclaration {
          node_id: Cell::new(NodeId::DUMMY),
          span: SPAN,
          declaration: Some(Declaration::VariableDeclaration(var_decl_inner)),
          specifiers: OxcVec::new_in(self.allocator),
          source: None,
          export_kind: ImportOrExportKind::Value,
          with_clause: None,
        },
        self.allocator,
      ));

      let module_body = {
        let mut body = OxcVec::with_capacity_in(1, self.allocator);
        body.push(export_named);
        body
      };

      let ts_module = TSModuleDeclaration {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
        id: TSModuleDeclarationName::Identifier(BindingIdentifier {
          node_id: Cell::new(NodeId::DUMMY),
          span: SPAN,
          name: Ident::from(self.allocator.alloc_str(&id.0)),
          symbol_id: Cell::new(None),
        }),
        body: Some(TSModuleDeclarationBody::TSModuleBlock(OxcBox::new_in(
          TSModuleBlock {
            node_id: Cell::new(NodeId::DUMMY),
            span: SPAN,
            directives: OxcVec::new_in(self.allocator),
            body: module_body,
          },
          self.allocator,
        ))),
        kind: TSModuleDeclarationKind::Namespace,
        declare: false,
        scope_id: Cell::new(None),
      };

      let ts_module_box = OxcBox::new_in(ts_module, self.allocator);

      final_body.push(if has_export_keyword {
        Statement::ExportNamedDeclaration(OxcBox::new_in(
          ExportNamedDeclaration {
            node_id: Cell::new(NodeId::DUMMY),
            span: SPAN,
            declaration: Some(Declaration::TSModuleDeclaration(ts_module_box)),
            specifiers: OxcVec::new_in(self.allocator),
            source: None,
            export_kind: ImportOrExportKind::Value,
            with_clause: None,
          },
          self.allocator,
        ))
      } else {
        Statement::TSModuleDeclaration(ts_module_box)
      });
    }

    self.expando_namespaces = parent_expando_namespaces;

    Ok(final_body)
  }

  fn check_expando_property_diagnostics(
    &mut self,
    decl: &VariableDeclarator<'a>,
    parent_id: &Id,
    parent_symbol: &Symbol,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    // In OXC we don't have the same visitor infrastructure, so we do a simpler check.
    // Walk the init expression looking for identifier references that match exports
    // of the parent symbol.
    if let Some(init) = &decl.init {
      let mut diagnostics: IndexMap<String, Span> = Default::default();
      collect_ident_refs_in_expr(init, &mut |name: &str, span: Span| {
        if parent_symbol.export(name).is_some() {
          diagnostics.insert(name.to_string(), span);
        }
      });
      for (reference_name, span) in diagnostics {
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedExpandoProperty {
            object_name: parent_id.0.to_string(),
            reference_name,
            range: self.source_range_to_range(span),
          },
        )?;
      }
    }
    Ok(())
  }

  fn transform_module_specifier(&mut self, src: &mut StringLiteral<'a>) {
    // only do this for relative specifiers (specifiers to specifiers within the package)
    let specifier = src.value.as_str();
    if !specifier.starts_with('.') {
      return;
    }
    let Some(resolved_specifier) =
      self
        .graph
        .resolve_dependency(specifier, self.specifier, true)
    else {
      return;
    };
    if let Some(relative) = self.specifier.make_relative(resolved_specifier) {
      if !relative.starts_with("../") {
        src.value =
          Atom::from(self.allocator.alloc_str(&format!("./{}", relative)));
      } else {
        src.value = Atom::from(self.allocator.alloc_str(&relative));
      }
    }
  }

  fn transform_item(
    &mut self,
    item: &mut Statement<'a>,
    comments: &mut CommentsMut<'a>,
    is_ambient: bool,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    match item {
      Statement::ImportDeclaration(n) => {
        let n = n.as_mut();
        if let Some(specifiers) = &mut n.specifiers {
          specifiers.retain(|s| self.public_ranges.contains(&s.span()));
          let retain = !specifiers.is_empty();
          if retain {
            self.transform_module_specifier(&mut n.source);
          }
          Ok(TransformItemResult::from_retain(retain))
        } else {
          // Side-effect import (no specifiers), keep if in public ranges
          Ok(TransformItemResult::from_retain(
            self.public_ranges.contains(&n.span),
          ))
        }
      }
      Statement::ExportNamedDeclaration(n) => {
        let n = n.as_mut();
        // If it has a declaration, handle it as a decl export
        if n.declaration.is_some() {
          let export_decl_span = n.span;
          let decl = n.declaration.as_mut().unwrap();
          return self.transform_decl_stmt(
            decl,
            comments,
            Some(export_decl_span),
            is_ambient,
          );
        }
        // Otherwise it's a re-export with specifiers
        n.specifiers
          .retain(|s| self.public_ranges.contains(&s.span()));
        let retain = !n.specifiers.is_empty();
        if retain && let Some(src) = &mut n.source {
          self.transform_module_specifier(src);
        }
        Ok(TransformItemResult::from_retain(retain))
      }
      Statement::ExportAllDeclaration(n) => {
        let n = n.as_mut();
        let retain = self.public_ranges.contains(&n.span);
        if retain {
          self.transform_module_specifier(&mut n.source);
        }
        Ok(TransformItemResult::from_retain(retain))
      }
      Statement::ExportDefaultDeclaration(n) => {
        let n = n.as_mut();
        if !self.public_ranges.contains(&n.span) {
          return Ok(TransformItemResult::Remove);
        }

        let node_span = n.span;
        match &mut n.declaration {
          ExportDefaultDeclarationKind::FunctionDeclaration(func) => {
            let ident_span = func.id.as_ref().map(|i| i.span);
            self.transform_fn(
              func,
              ident_span,
              FunctionKind::DeclarationLike,
              self.public_ranges.is_impl_with_overloads(&node_span),
              is_ambient,
            )?;
            Ok(TransformItemResult::Retain)
          }
          ExportDefaultDeclarationKind::ClassDeclaration(class) => {
            self.transform_class(
              class, comments, /* has declare keyword */ false,
            )?;
            Ok(TransformItemResult::Retain)
          }
          ExportDefaultDeclarationKind::TSInterfaceDeclaration(_) => {
            Ok(TransformItemResult::Retain)
          }
          _ => {
            // Expression variant - corresponds to old ExportDefaultExpr
            if let Some(expr) = n.declaration.as_expression_mut() {
              if self.maybe_transform_expr_if_leavable(expr, None)?
                || is_expr_ident_or_member_idents(expr)
              {
                Ok(TransformItemResult::Retain)
              } else {
                self.mark_diagnostic(
                  FastCheckDiagnostic::UnsupportedDefaultExportExpr {
                    range: self.source_range_to_range(node_span),
                  },
                )?;
                Ok(TransformItemResult::Remove)
              }
            } else {
              Ok(TransformItemResult::Retain)
            }
          }
        }
      }
      Statement::TSExportAssignment(n) => {
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedTsExportAssignment {
            range: self.source_range_to_range(n.span),
          },
        )?;
        Ok(TransformItemResult::Remove)
      }
      Statement::TSNamespaceExportDeclaration(n) => {
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedTsNamespaceExport {
            range: self.source_range_to_range(n.span),
          },
        )?;
        Ok(TransformItemResult::Remove)
      }
      Statement::TSImportEqualsDeclaration(n) => {
        let n_span = n.span;
        match &n.module_reference {
          TSModuleReference::IdentifierReference(_)
          | TSModuleReference::QualifiedName(_) => {
            let retain = self.public_ranges.contains(&n_span);
            Ok(TransformItemResult::from_retain(retain))
          }
          TSModuleReference::ExternalModuleReference(_) => {
            self.mark_diagnostic(FastCheckDiagnostic::UnsupportedRequire {
              range: self.source_range_to_range(n_span),
            })?;
            Ok(TransformItemResult::Remove)
          }
        }
      }
      // Declaration statements (not exported)
      Statement::ClassDeclaration(n) => {
        let n_span = n.span;
        self.transform_decl_class(n, comments, None, is_ambient, n_span)
      }
      Statement::FunctionDeclaration(n) => {
        let n_span = n.span;
        self.transform_decl_fn(n, None, is_ambient, n_span)
      }
      Statement::VariableDeclaration(n) => {
        let n_span = n.span;
        if matches!(
          n.kind,
          VariableDeclarationKind::Using | VariableDeclarationKind::AwaitUsing
        ) {
          if self.public_ranges.contains(&n_span)
            || n
              .declarations
              .iter()
              .any(|d| self.public_ranges.contains(&d.span))
          {
            self.mark_diagnostic(FastCheckDiagnostic::UnsupportedUsing {
              range: self.source_range_to_range(
                n.declarations.first().map(|d| d.span).unwrap_or(n_span),
              ),
            })?;
          }
          return Ok(TransformItemResult::Remove);
        }
        let is_declare = n.declare;
        self.transform_var(n.as_mut(), is_ambient || is_declare, n_span)
      }
      Statement::TSInterfaceDeclaration(n) => Ok(
        TransformItemResult::from_retain(self.public_ranges.contains(&n.span)),
      ),
      Statement::TSTypeAliasDeclaration(n) => Ok(
        TransformItemResult::from_retain(self.public_ranges.contains(&n.span)),
      ),
      Statement::TSEnumDeclaration(n) => Ok(TransformItemResult::from_retain(
        self.public_ranges.contains(&n.span),
      )),
      Statement::TSModuleDeclaration(n) => {
        let n_span = n.span;
        self.transform_ts_module(n.as_mut(), &n_span, comments, is_ambient)
      }
      Statement::TSGlobalDeclaration(n) => {
        self.mark_diagnostic(FastCheckDiagnostic::UnsupportedGlobalModule {
          range: self.source_range_to_range(n.span),
        })?;
        Ok(TransformItemResult::Remove)
      }
      Statement::ExpressionStatement(n) => match &mut n.expression {
        Expression::AssignmentExpression(assign_expr) => {
          self.transform_assign_expr(assign_expr.as_mut())
        }
        _ => Ok(TransformItemResult::Remove),
      },
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
      | Statement::ForOfStatement(_) => Ok(TransformItemResult::Remove),
    }
  }

  /// Handle declaration statements that appear inside ExportNamedDeclaration
  fn transform_decl_stmt(
    &mut self,
    decl: &mut Declaration<'a>,
    comments: &mut CommentsMut<'a>,
    parent_span: Option<Span>,
    is_ambient: bool,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    let public_span = parent_span.unwrap_or_else(|| GetSpan::span(decl));
    match decl {
      Declaration::ClassDeclaration(n) => self.transform_decl_class(
        n,
        comments,
        parent_span,
        is_ambient,
        public_span,
      ),
      Declaration::FunctionDeclaration(n) => {
        self.transform_decl_fn(n, parent_span, is_ambient, public_span)
      }
      Declaration::VariableDeclaration(n) => {
        if matches!(
          n.kind,
          VariableDeclarationKind::Using | VariableDeclarationKind::AwaitUsing
        ) {
          if self.public_ranges.contains(&public_span)
            || n
              .declarations
              .iter()
              .any(|d| self.public_ranges.contains(&d.span))
          {
            self.mark_diagnostic(FastCheckDiagnostic::UnsupportedUsing {
              range: self.source_range_to_range(
                n.declarations
                  .first()
                  .map(|d| d.span)
                  .unwrap_or(public_span),
              ),
            })?;
          }
          return Ok(TransformItemResult::Remove);
        }
        let is_declare = n.declare;
        self.transform_var(n.as_mut(), is_ambient || is_declare, public_span)
      }
      Declaration::TSInterfaceDeclaration(_) => {
        Ok(TransformItemResult::from_retain(
          self.public_ranges.contains(&public_span),
        ))
      }
      Declaration::TSTypeAliasDeclaration(_) => {
        Ok(TransformItemResult::from_retain(
          self.public_ranges.contains(&public_span),
        ))
      }
      Declaration::TSEnumDeclaration(_) => {
        Ok(TransformItemResult::from_retain(
          self.public_ranges.contains(&public_span),
        ))
      }
      Declaration::TSModuleDeclaration(n) => {
        let is_declare = n.declare;
        self.transform_ts_module(
          n.as_mut(),
          &public_span,
          comments,
          is_ambient || is_declare,
        )
      }
      Declaration::TSGlobalDeclaration(_) => {
        self.mark_diagnostic(FastCheckDiagnostic::UnsupportedGlobalModule {
          range: self.source_range_to_range(public_span),
        })?;
        Ok(TransformItemResult::Remove)
      }
      Declaration::TSImportEqualsDeclaration(_) => {
        Ok(TransformItemResult::Remove)
      }
    }
  }

  fn transform_decl_class(
    &mut self,
    n: &mut Class<'a>,
    comments: &mut CommentsMut<'a>,
    _parent_span: Option<Span>,
    is_ambient: bool,
    public_span: Span,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    if !self.public_ranges.contains(&public_span) {
      return Ok(TransformItemResult::Remove);
    }
    self.transform_class(n, comments, is_ambient || n.declare)?;
    Ok(TransformItemResult::Retain)
  }

  fn transform_decl_fn(
    &mut self,
    n: &mut Function<'a>,
    _parent_span: Option<Span>,
    is_ambient: bool,
    public_span: Span,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    if !self.public_ranges.contains(&public_span) {
      return Ok(TransformItemResult::Remove);
    }

    let is_overload = self.public_ranges.is_impl_with_overloads(&public_span);
    let ident_span = n.id.as_ref().map(|i| i.span);
    self.transform_fn(
      n,
      ident_span,
      FunctionKind::DeclarationLike,
      is_overload,
      is_ambient,
    )?;
    Ok(TransformItemResult::Retain)
  }

  fn transform_class(
    &mut self,
    n: &mut Class<'a>,
    comments: &mut CommentsMut<'a>,
    is_ambient: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    if is_ambient {
      // ignore private computed members
      n.body
        .body
        .retain(|m| !is_ts_private_computed_class_element(m));
      return Ok(());
    }

    let mut members: Vec<ClassElement<'a>> =
      Vec::with_capacity(n.body.body.len());
    let mut had_private = false;
    if let Some(super_class) = &n.super_class
      && !is_expr_ident_or_member_idents(super_class)
    {
      let super_span = super_class.span();
      self.mark_diagnostic(FastCheckDiagnostic::UnsupportedSuperClassExpr {
        range: self.source_range_to_range(super_span),
      })?;
    }
    let mut insert_members: Vec<ClassElement<'a>> = Vec::new();
    let mut had_private_constructor = false;
    let mut seen_ts_private_methods = HashSet::new();
    for mut member in
      std::mem::replace(&mut n.body.body, OxcVec::new_in(self.allocator))
        .into_iter()
    {
      had_private = had_private
        || matches!(
          &member,
          ClassElement::PropertyDefinition(p) if p.key.is_private_identifier()
        )
        || matches!(
          &member,
          ClassElement::MethodDefinition(m) if m.key.is_private_identifier()
        )
        || matches!(
          &member,
          ClassElement::AccessorProperty(a) if a.key.is_private_identifier()
        );

      let mut retain = !is_ts_private_computed_class_element(&member);
      if retain {
        // do some extra checks to see whether it should be removed
        if let ClassElement::MethodDefinition(method) = &member {
          if method.kind == MethodDefinitionKind::Constructor {
            if method.accessibility == Some(TSAccessibility::Private) {
              if had_private_constructor {
                retain = false;
              } else if is_ambient || method.value.body.is_some() {
                had_private_constructor = true;
              } else {
                retain = false;
              }
            }
          } else if method.accessibility == Some(TSAccessibility::Private) {
            let key = property_key_to_string(&method.key);
            retain = match key {
              Some(key) => seen_ts_private_methods.insert(key),
              None => false,
            };
          }
        }
      }

      if retain {
        retain = self.transform_class_member(
          &mut member,
          &mut insert_members,
          is_ambient,
        )?;
      }

      if retain {
        members.push(member);
      } else {
        comments.remove_leading(member.span());
      }
    }

    if had_private {
      // Insert a `#private!: unknown;` property definition
      insert_members.insert(
        0,
        ClassElement::PropertyDefinition(OxcBox::new_in(
          PropertyDefinition {
            node_id: Cell::new(NodeId::DUMMY),
            span: SPAN,
            r#type: PropertyDefinitionType::PropertyDefinition,
            decorators: OxcVec::new_in(self.allocator),
            key: PropertyKey::PrivateIdentifier(OxcBox::new_in(
              PrivateIdentifier {
                node_id: Cell::new(NodeId::DUMMY),
                span: SPAN,
                name: Ident::from(self.allocator.alloc_str("private")),
              },
              self.allocator,
            )),
            value: None,
            computed: false,
            r#static: false,
            declare: true,
            r#override: false,
            optional: false,
            definite: true,
            readonly: false,
            type_annotation: Some(type_ann(
              self.allocator,
              ts_keyword_type(self.allocator, TSKeywordKind::Any),
            )),
            accessibility: None,
          },
          self.allocator,
        )),
      );
    }

    let mut final_body = OxcVec::with_capacity_in(
      insert_members.len() + members.len(),
      self.allocator,
    );
    for m in insert_members {
      final_body.push(m);
    }
    for m in members {
      final_body.push(m);
    }
    n.body.body = final_body;
    n.decorators = OxcVec::new_in(self.allocator);
    Ok(())
  }

  fn transform_class_member(
    &mut self,
    member: &mut ClassElement<'a>,
    insert_members: &mut Vec<ClassElement<'a>>,
    is_ambient: bool,
  ) -> Result<bool, Vec<FastCheckDiagnostic>> {
    match member {
      ClassElement::MethodDefinition(n) => {
        if n.kind == MethodDefinitionKind::Constructor {
          return self.transform_constructor(
            n.as_mut(),
            insert_members,
            is_ambient,
          );
        }

        if n.accessibility == Some(TSAccessibility::Private) {
          // Convert private method to private property definition
          *member = ClassElement::PropertyDefinition(OxcBox::new_in(
            PropertyDefinition {
              node_id: Cell::new(NodeId::DUMMY),
              span: SPAN,
              r#type: PropertyDefinitionType::PropertyDefinition,
              decorators: OxcVec::new_in(self.allocator),
              key: n.key.clone_in(self.allocator),
              value: None,
              computed: n.computed,
              r#static: n.r#static,
              declare: true,
              r#override: false,
              optional: n.optional,
              definite: false,
              readonly: false,
              type_annotation: Some(any_type_ann(self.allocator)),
              accessibility: Some(TSAccessibility::Private),
            },
            self.allocator,
          ));
          return Ok(true);
        }
        let is_overload = self.public_ranges.is_impl_with_overloads(&n.span);
        let key_span = n.key.span();
        let method_kind = n.kind;
        self.transform_fn(
          &mut n.value,
          Some(key_span),
          match method_kind {
            MethodDefinitionKind::Method => FunctionKind::DeclarationLike,
            MethodDefinitionKind::Get => FunctionKind::Getter,
            MethodDefinitionKind::Set => FunctionKind::Setter,
            MethodDefinitionKind::Constructor => unreachable!(),
          },
          is_overload,
          is_ambient,
        )?;
        Ok(true)
      }
      ClassElement::PropertyDefinition(n) => {
        if n.accessibility == Some(TSAccessibility::Private) {
          n.type_annotation = Some(any_type_ann(self.allocator));
          n.declare = true;
          n.definite = false;
          n.r#override = false;
          n.value = None;
          return Ok(true);
        }
        if n.type_annotation.is_none() {
          let inferred_type = n.value.as_ref().and_then(|e| {
            self.maybe_infer_type_from_expr(
              e,
              match n.readonly {
                true => DeclMutabilityKind::Const,
                false => DeclMutabilityKind::Mutable,
              },
            )
          });
          match inferred_type {
            Some(t) => {
              n.type_annotation = Some(type_ann(self.allocator, t));
              n.value = None;
            }
            None => {
              let key_span = n.key.span();
              let is_value_leavable = match n.value.as_mut() {
                Some(init) => {
                  self.maybe_transform_expr_if_leavable(init, Some(key_span))?
                }
                None => false,
              };
              if !is_value_leavable {
                self.mark_diagnostic(
                  FastCheckDiagnostic::MissingExplicitType {
                    range: self.source_range_to_range(key_span),
                  },
                )?;
              }
            }
          }
        } else {
          n.value = None;
        }
        n.declare = n.value.is_none();
        n.definite = false;
        n.r#override = n.value.is_some() && n.r#override;
        n.decorators = OxcVec::new_in(self.allocator);
        Ok(true)
      }
      ClassElement::AccessorProperty(n) => {
        if n.key.is_private_identifier() {
          return Ok(false);
        }
        let type_annotation = if n.accessibility
          == Some(TSAccessibility::Private)
        {
          any_type_ann(self.allocator)
        } else {
          match n.type_annotation.as_ref() {
            Some(ta) => ta.clone_in(self.allocator),
            None => {
              let inferred_type = n.value.as_ref().and_then(|e| {
                self.maybe_infer_type_from_expr(e, DeclMutabilityKind::Mutable)
              });
              match inferred_type {
                Some(t) => type_ann(self.allocator, t),
                None => {
                  let key_span = n.key.span();
                  self.mark_diagnostic(
                    FastCheckDiagnostic::MissingExplicitType {
                      range: self.source_range_to_range(key_span),
                    },
                  )?;
                  any_type_ann(self.allocator)
                }
              }
            }
          }
        };
        // Convert accessor to property definition
        *member = ClassElement::PropertyDefinition(OxcBox::new_in(
          PropertyDefinition {
            node_id: Cell::new(NodeId::DUMMY),
            span: n.span,
            r#type: PropertyDefinitionType::PropertyDefinition,
            decorators: OxcVec::new_in(self.allocator),
            key: n.key.clone_in(self.allocator),
            value: None,
            computed: n.computed,
            r#static: n.r#static,
            declare: true,
            r#override: false,
            optional: false,
            definite: false,
            readonly: false,
            type_annotation: Some(type_annotation),
            accessibility: n.accessibility,
          },
          self.allocator,
        ));
        Ok(true)
      }
      ClassElement::TSIndexSignature(_) => {
        // ok, as-is
        Ok(true)
      }
      ClassElement::StaticBlock(_) => Ok(false),
    }
  }

  fn transform_constructor(
    &mut self,
    n: &mut MethodDefinition<'a>,
    insert_members: &mut Vec<ClassElement<'a>>,
    _is_ambient: bool,
  ) -> Result<bool, Vec<FastCheckDiagnostic>> {
    if let Some(body) = &mut n.value.body {
      body.statements.retain_mut(|stmt| match stmt {
        Statement::ExpressionStatement(e) => match &mut e.expression {
          Expression::CallExpression(c) => {
            if !matches!(&c.callee, Expression::Super(_)) {
              return false;
            }
            for arg in c.arguments.iter_mut() {
              match arg {
                Argument::SpreadElement(spread) => {
                  spread.argument = paren_expr(
                    self.allocator,
                    array_as_never_array_expr(self.allocator),
                  );
                }
                _ => {
                  *arg = Argument::from(obj_as_never_expr(self.allocator));
                }
              }
            }
            true
          }
          _ => false,
        },
        _ => false,
      });
    }

    // Handle parameter properties (TSParamProp equivalent)
    // In OXC, FormalParameter has an `accessibility` field for parameter properties
    {
      let params = &mut n.value.params;
      for param in params.items.iter_mut() {
        if param.accessibility.is_some() {
          // This is a parameter property
          let is_optional = param.optional;
          let key = match &param.pattern {
            BindingPattern::BindingIdentifier(ident) => {
              PropertyKey::StaticIdentifier(OxcBox::new_in(
                IdentifierName {
                  node_id: Cell::new(NodeId::DUMMY),
                  span: ident.span,
                  name: ident.name,
                },
                self.allocator,
              ))
            }
            BindingPattern::AssignmentPattern(assign) => match &assign.left {
              BindingPattern::BindingIdentifier(ident) => {
                PropertyKey::StaticIdentifier(OxcBox::new_in(
                  IdentifierName {
                    node_id: Cell::new(NodeId::DUMMY),
                    span: ident.span,
                    name: ident.name,
                  },
                  self.allocator,
                ))
              }
              _ => {
                let left_span = assign.left.span();
                self.mark_diagnostic(
                  FastCheckDiagnostic::UnsupportedDestructuring {
                    range: self.source_range_to_range(left_span),
                  },
                )?;
                return Ok(false);
              }
            },
            _ => {
              let pat_span = param.pattern.span();
              self.mark_diagnostic(
                FastCheckDiagnostic::UnsupportedDestructuring {
                  range: self.source_range_to_range(pat_span),
                },
              )?;
              return Ok(false);
            }
          };

          let type_annotation =
            if param.accessibility == Some(TSAccessibility::Private) {
              Some(any_type_ann(self.allocator))
            } else {
              match &param.pattern {
                BindingPattern::BindingIdentifier(_) => {
                  param.type_annotation.clone_in(self.allocator)
                }
                BindingPattern::AssignmentPattern(assign) => {
                  let explicit = param.type_annotation.clone_in(self.allocator);
                  explicit.or_else(|| {
                    self
                      .maybe_infer_type_from_expr(
                        &assign.right,
                        match param.readonly {
                          true => DeclMutabilityKind::Const,
                          false => DeclMutabilityKind::Mutable,
                        },
                      )
                      .map(|t| type_ann(self.allocator, t))
                  })
                }
                _ => None,
              }
            };

          insert_members.push(ClassElement::PropertyDefinition(
            OxcBox::new_in(
              PropertyDefinition {
                node_id: Cell::new(NodeId::DUMMY),
                span: SPAN,
                r#type: PropertyDefinitionType::PropertyDefinition,
                decorators: OxcVec::new_in(self.allocator),
                key,
                value: None,
                computed: false,
                r#static: false,
                declare: true,
                r#override: false,
                optional: is_optional,
                definite: false,
                readonly: param.readonly,
                type_annotation,
                accessibility: match param.accessibility {
                  Some(TSAccessibility::Public) => None,
                  other => other,
                },
              },
              self.allocator,
            ),
          ));

          // Convert parameter property to regular parameter
          param.accessibility = None;
          param.readonly = false;
          param.r#override = false;
        }
      }

      if n.accessibility == Some(TSAccessibility::Private) {
        n.value.params.as_mut().items = OxcVec::new_in(self.allocator);
        n.value.params.as_mut().rest = None;
        return Ok(true);
      }

      let is_overload = self.public_ranges.is_impl_with_overloads(&n.span);
      if is_overload {
        let params = n.value.params.as_mut();
        let items_len = params.items.len();
        let has_rest = params.rest.is_some();
        let mut new_items = OxcVec::with_capacity_in(items_len, self.allocator);
        for (i, _param) in params.items.iter().enumerate() {
          new_items.push(FormalParameter {
            node_id: Cell::new(NodeId::DUMMY),
            span: SPAN,
            decorators: OxcVec::new_in(self.allocator),
            pattern: BindingPattern::BindingIdentifier(OxcBox::new_in(
              BindingIdentifier {
                node_id: Cell::new(NodeId::DUMMY),
                span: SPAN,
                name: Ident::from(
                  self.allocator.alloc_str(&format!("param{}", i)),
                ),
                symbol_id: Cell::new(None),
              },
              self.allocator,
            )),
            type_annotation: Some(any_type_ann(self.allocator)),
            initializer: None,
            optional: true,
            accessibility: None,
            readonly: false,
            r#override: false,
          });
        }
        params.items = new_items;
        if has_rest {
          let rest_idx = items_len;
          params.rest = Some(OxcBox::new_in(
            FormalParameterRest {
              node_id: Cell::new(NodeId::DUMMY),
              span: SPAN,
              decorators: OxcVec::new_in(self.allocator),
              rest: BindingRestElement {
                node_id: Cell::new(NodeId::DUMMY),
                span: SPAN,
                argument: BindingPattern::BindingIdentifier(OxcBox::new_in(
                  BindingIdentifier {
                    node_id: Cell::new(NodeId::DUMMY),
                    span: SPAN,
                    name: Ident::from(
                      self.allocator.alloc_str(&format!("param{}", rest_idx)),
                    ),
                    symbol_id: Cell::new(None),
                  },
                  self.allocator,
                )),
              },
              type_annotation: Some(any_type_ann(self.allocator)),
            },
            self.allocator,
          ));
        }
      }

      let params = n.value.params.as_mut();
      let optional_start_index = ParamsOptionalStartIndex::build(params);
      for (i, param) in params.items.iter_mut().enumerate() {
        self.handle_param_pattern(
          param,
          optional_start_index.is_optional_at_index(i),
        )?;
        param.decorators = OxcVec::new_in(self.allocator);
      }
    }

    Ok(true)
  }

  fn transform_fn(
    &mut self,
    n: &mut Function<'a>,
    parent_id_span: Option<Span>,
    function_kind: FunctionKind,
    is_overload: bool,
    is_ambient: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    if is_ambient {
      return Ok(()); // no need to do anything
    }
    if is_overload {
      {
        let params = n.params.as_mut();
        let items_len = params.items.len();
        let has_rest = params.rest.is_some();
        let mut new_items = OxcVec::with_capacity_in(items_len, self.allocator);
        for (i, _param) in params.items.iter().enumerate() {
          new_items.push(FormalParameter {
            node_id: Cell::new(NodeId::DUMMY),
            span: SPAN,
            decorators: OxcVec::new_in(self.allocator),
            pattern: BindingPattern::BindingIdentifier(OxcBox::new_in(
              BindingIdentifier {
                node_id: Cell::new(NodeId::DUMMY),
                span: SPAN,
                name: Ident::from(
                  self.allocator.alloc_str(&format!("param{}", i)),
                ),
                symbol_id: Cell::new(None),
              },
              self.allocator,
            )),
            type_annotation: Some(any_type_ann(self.allocator)),
            initializer: None,
            optional: true,
            accessibility: None,
            readonly: false,
            r#override: false,
          });
        }
        params.items = new_items;
        if has_rest {
          let rest_idx = items_len;
          params.rest = Some(OxcBox::new_in(
            FormalParameterRest {
              node_id: Cell::new(NodeId::DUMMY),
              span: SPAN,
              decorators: OxcVec::new_in(self.allocator),
              rest: BindingRestElement {
                node_id: Cell::new(NodeId::DUMMY),
                span: SPAN,
                argument: BindingPattern::BindingIdentifier(OxcBox::new_in(
                  BindingIdentifier {
                    node_id: Cell::new(NodeId::DUMMY),
                    span: SPAN,
                    name: Ident::from(
                      self.allocator.alloc_str(&format!("param{}", rest_idx)),
                    ),
                    symbol_id: Cell::new(None),
                  },
                  self.allocator,
                )),
              },
              type_annotation: Some(any_type_ann(self.allocator)),
            },
            self.allocator,
          ));
        }
      }
      n.return_type = Some(any_type_ann(self.allocator));
    }

    let missing_return_type = match function_kind {
      FunctionKind::DeclarationLike
      | FunctionKind::ExpressionLike
      | FunctionKind::Getter => n.return_type.is_none(),
      FunctionKind::Setter => {
        // Having a return type is actually invalid for a setter, but TypeScript
        // already catches that so we don't have to emit a diagnostic for it
        // here.
        false
      }
    };

    if missing_return_type {
      let span = parent_id_span.unwrap_or(n.span);
      if n.generator {
        self.mark_diagnostic(
          FastCheckDiagnostic::MissingExplicitReturnType {
            range: self.source_range_to_range(span),
            is_definitely_void_or_never: false,
            is_async: n.r#async,
          },
        )?;
      } else if let Some(body) = &mut n.body {
        self.transform_function_body_block_stmt(
          span,
          &mut n.return_type,
          body,
          function_kind,
          n.r#async,
        )?;
      }
    }

    if let Some(body) = &mut n.body {
      body.statements.clear();

      // push a return stmt to suppress type errors, if the function is not a
      // void function
      let return_expr = n.return_type.as_ref().and_then(|t| {
        replacement_return_value(self.allocator, &t.type_annotation)
      });
      if let Some(return_expr) = return_expr {
        body
          .statements
          .push(Statement::ReturnStatement(OxcBox::new_in(
            ReturnStatement {
              node_id: Cell::new(NodeId::DUMMY),
              span: SPAN,
              argument: Some(return_expr),
            },
            self.allocator,
          )));
      }
    }

    {
      let params = n.params.as_mut();
      let optional_start_index = ParamsOptionalStartIndex::build(params);
      for (i, param) in params.items.iter_mut().enumerate() {
        self.handle_param_pattern(
          param,
          optional_start_index.is_optional_at_index(i),
        )?;
        param.decorators = OxcVec::new_in(self.allocator);
      }
    }

    n.r#async = false;
    n.generator = false;

    Ok(())
  }

  fn transform_arrow(
    &mut self,
    n: &mut ArrowFunctionExpression<'a>,
    parent_id_span: Option<Span>,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    let span = parent_id_span.unwrap_or(n.span);

    if n.return_type.is_none() {
      // arrow functions can not be generators, let's ignore

      if n.expression {
        // Arrow with expression body
        if let Some(expr_stmt) = n.body.statements.first_mut()
          && let Statement::ExpressionStatement(expr_s) = expr_stmt
        {
          let inferred_type = self.maybe_infer_type_from_expr(
            &expr_s.expression,
            DeclMutabilityKind::Mutable,
          );
          match inferred_type {
            Some(t) => {
              let mut return_type_inner = t;
              if n.r#async {
                return_type_inner =
                  self.promise_wrap_type_owned(return_type_inner);
              }
              n.return_type = Some(type_ann(self.allocator, return_type_inner));
            }
            None => {
              let is_expr_leavable = self.maybe_transform_expr_if_leavable(
                &mut expr_s.expression,
                None,
              )?;
              if !is_expr_leavable {
                self.mark_diagnostic(
                  FastCheckDiagnostic::MissingExplicitReturnType {
                    range: self.source_range_to_range(span),
                    is_definitely_void_or_never: false,
                    is_async: n.r#async,
                  },
                )?;
              }
            }
          }
        }
      } else {
        // Arrow with block body
        self.transform_function_body_block_stmt(
          span,
          &mut n.return_type,
          &mut n.body,
          FunctionKind::ExpressionLike,
          n.r#async,
        )?;
      }
    }

    if let Some(t) = &n.return_type {
      // there is an explicit return type, so we can clear the body
      let return_expr =
        replacement_return_value(self.allocator, &t.type_annotation);
      n.body.statements.clear();
      if let Some(e) = return_expr {
        if n.expression {
          // Keep as expression
          n.body.statements.push(Statement::ExpressionStatement(
            OxcBox::new_in(
              ExpressionStatement {
                node_id: Cell::new(NodeId::DUMMY),
                span: SPAN,
                expression: paren_expr(self.allocator, e),
              },
              self.allocator,
            ),
          ));
        } else {
          n.body
            .statements
            .push(Statement::ReturnStatement(OxcBox::new_in(
              ReturnStatement {
                node_id: Cell::new(NodeId::DUMMY),
                span: SPAN,
                argument: Some(paren_expr(self.allocator, e)),
              },
              self.allocator,
            )));
          n.expression = false;
        }
      } else {
        n.expression = false;
      }

      // Only reset is_async if there is an explicit return type
      n.r#async = false;
    }

    {
      let params = n.params.as_mut();
      let optional_start_index = ParamsOptionalStartIndex::build(params);
      for (i, param) in params.items.iter_mut().enumerate() {
        self.handle_param_pattern(
          param,
          optional_start_index.is_optional_at_index(i),
        )?;
      }
    }

    Ok(())
  }

  fn transform_function_body_block_stmt(
    &mut self,
    span: Span,
    return_type: &mut Option<OxcBox<'a, TSTypeAnnotation<'a>>>,
    body: &mut FunctionBody<'a>,
    function_kind: FunctionKind,
    is_async: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    let analysis = analyze_return_stmts_in_function_body(body);
    match (analysis, function_kind) {
      (_, FunctionKind::Setter) => unreachable!(),
      (ReturnStatementAnalysis::None, FunctionKind::DeclarationLike)
      | (ReturnStatementAnalysis::Void, FunctionKind::DeclarationLike)
      | (ReturnStatementAnalysis::Void, FunctionKind::ExpressionLike) => {
        *return_type = Some(type_ann(
          self.allocator,
          void_or_promise_void(self.allocator, is_async),
        ));
      }
      (ReturnStatementAnalysis::None, FunctionKind::ExpressionLike) => {
        self.mark_diagnostic(
          FastCheckDiagnostic::MissingExplicitReturnType {
            range: self.source_range_to_range(span),
            is_definitely_void_or_never: true,
            is_async,
          },
        )?;
      }
      (ReturnStatementAnalysis::Single, _) => {
        // TODO: infer return type based on return type
        self.mark_diagnostic(
          FastCheckDiagnostic::MissingExplicitReturnType {
            range: self.source_range_to_range(span),
            is_definitely_void_or_never: false,
            is_async,
          },
        )?;
      }
      (ReturnStatementAnalysis::None, FunctionKind::Getter)
      | (ReturnStatementAnalysis::Void, FunctionKind::Getter)
      | (ReturnStatementAnalysis::Multiple, _) => {
        self.mark_diagnostic(
          FastCheckDiagnostic::MissingExplicitReturnType {
            range: self.source_range_to_range(span),
            is_definitely_void_or_never: false,
            is_async,
          },
        )?;
      }
    }

    Ok(())
  }

  fn handle_param_pattern(
    &mut self,
    param: &mut FormalParameter<'a>,
    is_optional: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    match &param.pattern {
      BindingPattern::BindingIdentifier(_ident) => {
        if param.type_annotation.is_none() {
          if let Some(initializer) = &param.initializer {
            // Try to infer type from the default value (e.g. `param = 2` → `param?: number`)
            let inferred_type = self.maybe_infer_type_from_expr(
              initializer,
              DeclMutabilityKind::Mutable,
            );
            match inferred_type {
              Some(t) => {
                let ta = type_ann(self.allocator, t);
                if !is_optional {
                  let inner_type = ta.type_annotation.clone_in(self.allocator);
                  let union_type = TSType::TSUnionType(OxcBox::new_in(
                    TSUnionType {
                      node_id: Cell::new(NodeId::DUMMY),
                      span: SPAN,
                      types: {
                        let mut types =
                          OxcVec::with_capacity_in(2, self.allocator);
                        types.push(inner_type);
                        types.push(TSType::TSUndefinedKeyword(OxcBox::new_in(
                          TSUndefinedKeyword {
                            node_id: Cell::new(NodeId::DUMMY),
                            span: SPAN,
                          },
                          self.allocator,
                        )));
                        types
                      },
                    },
                    self.allocator,
                  ));
                  param.type_annotation =
                    Some(type_ann(self.allocator, union_type));
                  param.optional = false;
                } else {
                  param.type_annotation = Some(ta);
                  param.optional = true;
                }
                param.initializer = None;
              }
              None => {
                let pat_span = param.pattern.span();
                let is_expr_leavable = self.maybe_transform_expr_if_leavable(
                  param.initializer.as_mut().unwrap(),
                  Some(pat_span),
                )?;
                if !is_expr_leavable {
                  self.mark_diagnostic(
                    FastCheckDiagnostic::MissingExplicitType {
                      range: self.source_range_to_range(pat_span),
                    },
                  )?;
                }
              }
            }
          } else {
            let pat_span = param.pattern.span();
            self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
              range: self.source_range_to_range(pat_span),
            })?;
          }
        }
      }
      BindingPattern::AssignmentPattern(_) => {
        self.handle_param_assignment_pattern(param, is_optional)?;
      }
      BindingPattern::ArrayPattern(_) => {
        if param.type_annotation.is_none() {
          let p_span = param.pattern.span();
          self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
            range: self.source_range_to_range(p_span),
          })?;
        }
        if let BindingPattern::ArrayPattern(p) = &mut param.pattern {
          p.elements = OxcVec::new_in(self.allocator);
        }
      }
      BindingPattern::ObjectPattern(_) => {
        if param.type_annotation.is_none() {
          let p_span = param.pattern.span();
          self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
            range: self.source_range_to_range(p_span),
          })?;
        }
        if let BindingPattern::ObjectPattern(p) = &mut param.pattern {
          p.properties = OxcVec::new_in(self.allocator);
        }
      }
    }
    Ok(())
  }

  fn handle_param_assignment_pattern(
    &mut self,
    param: &mut FormalParameter<'a>,
    is_optional: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    // Determine the kind of the left side of the assignment pattern
    let left_kind =
      if let BindingPattern::AssignmentPattern(assign) = &param.pattern {
        match &assign.left {
          BindingPattern::BindingIdentifier(_) => 0u8,
          BindingPattern::ArrayPattern(_) => 1u8,
          BindingPattern::ObjectPattern(_) => 2u8,
          _ => 3u8,
        }
      } else {
        return Ok(());
      };

    let has_type_annotation = param.type_annotation.is_some();

    match left_kind {
      0 => {
        // BindingIdentifier case
        if !has_type_annotation {
          // Try to infer type from the right side
          let inferred_type =
            if let BindingPattern::AssignmentPattern(assign) = &param.pattern {
              self.maybe_infer_type_from_expr(
                &assign.right,
                DeclMutabilityKind::Mutable,
              )
            } else {
              None
            };
          match inferred_type {
            Some(t) => {
              let ta = type_ann(self.allocator, t);
              if !is_optional {
                let inner_type = ta.type_annotation.clone_in(self.allocator);
                let union_type = TSType::TSUnionType(OxcBox::new_in(
                  TSUnionType {
                    node_id: Cell::new(NodeId::DUMMY),
                    span: SPAN,
                    types: {
                      let mut types =
                        OxcVec::with_capacity_in(2, self.allocator);
                      types.push(inner_type);
                      types.push(TSType::TSUndefinedKeyword(OxcBox::new_in(
                        TSUndefinedKeyword {
                          node_id: Cell::new(NodeId::DUMMY),
                          span: SPAN,
                        },
                        self.allocator,
                      )));
                      types
                    },
                  },
                  self.allocator,
                ));
                param.type_annotation =
                  Some(type_ann(self.allocator, union_type));
                param.optional = false;
              } else {
                param.type_annotation = Some(ta);
                param.optional = true;
              }
              // Convert to plain ident pattern
              if let BindingPattern::AssignmentPattern(assign) = &param.pattern
              {
                let new_left = assign.left.clone_in(self.allocator);
                param.pattern = new_left;
              }
            }
            None => {
              let ident_span =
                if let BindingPattern::AssignmentPattern(assign) =
                  &param.pattern
                {
                  if let BindingPattern::BindingIdentifier(ident) = &assign.left
                  {
                    ident.span
                  } else {
                    SPAN
                  }
                } else {
                  SPAN
                };
              if let BindingPattern::AssignmentPattern(assign) =
                &mut param.pattern
              {
                let is_expr_leavable = self.maybe_transform_expr_if_leavable(
                  &mut assign.right,
                  Some(ident_span),
                )?;
                if !is_expr_leavable {
                  self.mark_diagnostic(
                    FastCheckDiagnostic::MissingExplicitType {
                      range: self.source_range_to_range(ident_span),
                    },
                  )?;
                }
              }
            }
          }
        } else {
          if !is_optional {
            if let Some(ta) = param.type_annotation.take() {
              let inner_type = ta.type_annotation.clone_in(self.allocator);
              let union_type = TSType::TSUnionType(OxcBox::new_in(
                TSUnionType {
                  node_id: Cell::new(NodeId::DUMMY),
                  span: SPAN,
                  types: {
                    let mut types = OxcVec::with_capacity_in(2, self.allocator);
                    types.push(inner_type);
                    types.push(TSType::TSUndefinedKeyword(OxcBox::new_in(
                      TSUndefinedKeyword {
                        node_id: Cell::new(NodeId::DUMMY),
                        span: SPAN,
                      },
                      self.allocator,
                    )));
                    types
                  },
                },
                self.allocator,
              ));
              param.type_annotation =
                Some(type_ann(self.allocator, union_type));
            }
            param.optional = false;
          } else {
            param.optional = true;
          }
          if let BindingPattern::AssignmentPattern(assign) = &param.pattern {
            let new_left = assign.left.clone_in(self.allocator);
            param.pattern = new_left;
          }
        }
      }
      1 => {
        // ArrayPattern case
        if !has_type_annotation {
          if let BindingPattern::AssignmentPattern(assign) = &mut param.pattern
            && !self
              .maybe_transform_expr_if_leavable(&mut assign.right, None)?
          {
            let p_span = assign.left.span();
            self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
              range: self.source_range_to_range(p_span),
            })?;
          }
        } else if let BindingPattern::AssignmentPattern(assign) =
          &mut param.pattern
        {
          assign.right = array_as_never_expr(self.allocator);
        }
        if let BindingPattern::AssignmentPattern(assign) = &mut param.pattern
          && let BindingPattern::ArrayPattern(p) = &mut assign.left
        {
          p.elements = OxcVec::new_in(self.allocator);
        }
      }
      2 => {
        // ObjectPattern case
        if !has_type_annotation {
          if let BindingPattern::AssignmentPattern(assign) = &mut param.pattern
            && !self
              .maybe_transform_expr_if_leavable(&mut assign.right, None)?
          {
            let p_span = assign.left.span();
            self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
              range: self.source_range_to_range(p_span),
            })?;
          }
        } else if let BindingPattern::AssignmentPattern(assign) =
          &mut param.pattern
        {
          assign.right = obj_as_never_expr(self.allocator);
        }
        if let BindingPattern::AssignmentPattern(assign) = &mut param.pattern
          && let BindingPattern::ObjectPattern(p) = &mut assign.left
        {
          p.properties = OxcVec::new_in(self.allocator);
        }
      }
      _ => {
        // AssignmentPattern or other unsupported
        let pat_span = param.pattern.span();
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedDestructuring {
            range: self.source_range_to_range(pat_span),
          },
        )?;
      }
    }

    Ok(())
  }

  fn transform_var(
    &mut self,
    n: &mut VariableDeclaration<'a>,
    is_ambient: bool,
    _public_span: Span,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    n.declarations
      .retain(|d| self.public_ranges.contains(&d.span));

    // don't need to do anything for ambient decls
    if !is_ambient {
      for decl in n.declarations.iter_mut() {
        self.transform_var_declarator(n.kind, decl)?;
      }
    }

    Ok(TransformItemResult::from_retain(!n.declarations.is_empty()))
  }

  fn transform_var_declarator(
    &mut self,
    decl_kind: VariableDeclarationKind,
    n: &mut VariableDeclarator<'a>,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    match &mut n.id {
      BindingPattern::BindingIdentifier(ident) => {
        if n.type_annotation.is_none() {
          let inferred_type = n.init.as_ref().and_then(|e| {
            self.maybe_infer_type_from_expr(
              e,
              match decl_kind {
                VariableDeclarationKind::Var | VariableDeclarationKind::Let => {
                  DeclMutabilityKind::Mutable
                }
                VariableDeclarationKind::Const => DeclMutabilityKind::Const,
                VariableDeclarationKind::Using
                | VariableDeclarationKind::AwaitUsing => {
                  DeclMutabilityKind::Mutable
                }
              },
            )
          });
          match inferred_type {
            Some(t) => {
              n.type_annotation = Some(type_ann(self.allocator, t));
              n.init = Some(obj_as_never_expr(self.allocator));
            }
            None => {
              let ident_span = ident.span;
              let is_init_leavable = match n.init.as_mut() {
                Some(init) => self
                  .maybe_transform_expr_if_leavable(init, Some(ident_span))?,
                None => false,
              };
              if !is_init_leavable {
                self.mark_diagnostic(
                  FastCheckDiagnostic::MissingExplicitType {
                    range: self.source_range_to_range(ident_span),
                  },
                )?;
              }
            }
          }
        } else {
          n.init = Some(obj_as_never_expr(self.allocator));
        }
      }
      BindingPattern::ArrayPattern(_)
      | BindingPattern::ObjectPattern(_)
      | BindingPattern::AssignmentPattern(_) => {
        let name_span = n.id.span();
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedDestructuring {
            range: self.source_range_to_range(name_span),
          },
        )?;
      }
    }

    Ok(())
  }

  fn transform_ts_module(
    &mut self,
    n: &mut TSModuleDeclaration<'a>,
    public_span: &Span,
    comments: &mut CommentsMut<'a>,
    is_ambient: bool,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    match &n.id {
      TSModuleDeclarationName::Identifier(_) => {
        // ok
      }
      TSModuleDeclarationName::StringLiteral(_) => {
        // not ok
        let id_span = n.id.span();
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedAmbientModule {
            range: self.source_range_to_range(id_span),
          },
        )?;
        return Ok(TransformItemResult::Remove);
      }
    }

    let ts_module_block = match &mut n.body {
      Some(body) => get_innermost_module_block(body),
      None => {
        let id_span = n.id.span();
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedAmbientModule {
            range: self.source_range_to_range(id_span),
          },
        )?;
        return Ok(TransformItemResult::Remove);
      }
    };

    // allow the above diagnostics to error before checking for a public range
    if !self.public_ranges.contains(public_span) {
      return Ok(TransformItemResult::Remove);
    }

    let body: Vec<Statement<'a>> = std::mem::replace(
      &mut ts_module_block.body,
      OxcVec::new_in(self.allocator),
    )
    .into_iter()
    .collect();
    let new_body = self.transform_module_body(body, comments, is_ambient)?;
    ts_module_block.body = OxcVec::from_iter_in(new_body, self.allocator);

    Ok(TransformItemResult::Retain)
  }

  // Keep expando properties. Example:
  //
  //   export function it() {}
  //   it.skip = () => {};
  //   it.prop = "value";
  //
  // Otherwise TS will error when calling `it.skip()`
  fn transform_assign_expr(
    &mut self,
    n: &mut AssignmentExpression<'a>,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    if !self.public_ranges.contains(&n.span) {
      return Ok(TransformItemResult::Remove);
    }

    // it will be an expando property at this point, but we still need n.right
    // to be mutable, so transform before getting the expando property
    let right_span = n.right.span();
    let is_init_leavable =
      self.maybe_transform_expr_if_leavable(&mut n.right, Some(right_span))?;

    // Check if this is an expando property (obj.prop = expr or obj["prop"] = expr)
    let expando_info = self.get_expando_info(n);
    let Some((obj_id, prop_name, prop_span)) = expando_info else {
      return Ok(TransformItemResult::Remove);
    };

    if !is_init_leavable {
      self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
        range: self.source_range_to_range(prop_span),
      })?;
    } else {
      let id: Id = (obj_id.to_string(), 0);
      let var_decls = self.expando_namespaces.entry(id).or_default();
      var_decls.push(VariableDeclarator {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
        kind: VariableDeclarationKind::Var,
        id: BindingPattern::BindingIdentifier(OxcBox::new_in(
          BindingIdentifier {
            node_id: Cell::new(NodeId::DUMMY),
            span: prop_span,
            name: Ident::from(self.allocator.alloc_str(&prop_name)),
            symbol_id: Cell::new(None),
          },
          self.allocator,
        )),
        type_annotation: None,
        init: Some(n.right.clone_in(self.allocator)),
        definite: false,
      });
    }
    Ok(TransformItemResult::Remove)
  }

  /// Extract expando property info: (object_ident_name, property_name, property_span)
  fn get_expando_info(
    &self,
    n: &AssignmentExpression<'a>,
  ) -> Option<(String, String, Span)> {
    if n.operator != AssignmentOperator::Assign {
      return None;
    }
    match &n.left {
      AssignmentTarget::StaticMemberExpression(member) => {
        let obj_name = match &member.object {
          Expression::Identifier(ident) => ident.name.to_string(),
          _ => return None,
        };
        let prop_name = member.property.name.to_string();
        if !is_valid_js_ident(&prop_name) {
          return None;
        }
        Some((obj_name, prop_name, member.property.span))
      }
      AssignmentTarget::ComputedMemberExpression(member) => {
        let obj_name = match &member.object {
          Expression::Identifier(ident) => ident.name.to_string(),
          _ => return None,
        };
        match &member.expression {
          Expression::StringLiteral(s) => {
            let prop_name = s.value.to_string();
            if !is_valid_js_ident(&prop_name) {
              return None;
            }
            Some((obj_name, prop_name, s.span))
          }
          _ => None,
        }
      }
      _ => None,
    }
  }

  fn mark_diagnostic(
    &mut self,
    diagnostic: FastCheckDiagnostic,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    if self.should_error_on_first_diagnostic {
      Err(vec![diagnostic])
    } else {
      self.diagnostics.push(diagnostic);
      Ok(())
    }
  }

  fn source_range_to_range(&self, span: Span) -> FastCheckDiagnosticRange {
    FastCheckDiagnosticRange {
      specifier: self.specifier.clone(),
      text_info: self.text_info.clone(),
      range: span,
    }
  }

  fn maybe_transform_expr_if_leavable(
    &mut self,
    expr: &mut Expression<'a>,
    parent_id_span: Option<Span>,
  ) -> Result<bool, Vec<FastCheckDiagnostic>> {
    let mut recurse = |expr: &mut Expression<'a>| {
      self.maybe_transform_expr_if_leavable(expr, None)
    };

    let is_leavable = match expr {
      Expression::ThisExpression(_) => true,
      Expression::ArrayExpression(n) => {
        let mut is_leavable = true;
        for elem in n.elements.iter_mut() {
          is_leavable = match elem {
            ArrayExpressionElement::SpreadElement(spread) => {
              recurse(&mut spread.argument)?
            }
            ArrayExpressionElement::Elision(_) => true,
            _ => {
              if let Some(expr) = elem.as_expression_mut() {
                recurse(expr)?
              } else {
                true
              }
            }
          };
          if !is_leavable {
            break
          }
        }
        is_leavable
      },
      Expression::ObjectExpression(n) => {
        let mut is_leavable = true;
        for prop in n.properties.iter_mut() {
          is_leavable = match prop {
            ObjectPropertyKind::ObjectProperty(prop) => {
              if prop.shorthand {
                true
              } else if prop.method || matches!(prop.kind, PropertyKind::Get | PropertyKind::Set) {
                false
              } else {
                recurse(&mut prop.value)?
              }
            }
            ObjectPropertyKind::SpreadProperty(spread) => {
              recurse(&mut spread.argument)?
            }
          };
          if !is_leavable {
            break
          }
        }
        is_leavable
      }
      Expression::UnaryExpression(n) => recurse(&mut n.argument)?,
      Expression::UpdateExpression(_n) => true, // UpdateExpression.argument is SimpleAssignmentTarget, not Expression
      Expression::BinaryExpression(n) => recurse(&mut n.left)? && recurse(&mut n.right)?,
      Expression::AssignmentExpression(_) | Expression::Super(_) => false,
      Expression::ConditionalExpression(n) => {
        recurse(&mut n.test)?
          && recurse(&mut n.consequent)?
          && recurse(&mut n.alternate)?
      }
      Expression::StaticMemberExpression(n) => {
        recurse(&mut n.object)?
      }
      Expression::ComputedMemberExpression(n) => {
        recurse(&mut n.object)? && recurse(&mut n.expression)?
      }
      Expression::PrivateFieldExpression(_) => false,
      Expression::Identifier(_) => true,
      Expression::CallExpression(_) | Expression::NewExpression(_) | Expression::SequenceExpression(_) => false,
      Expression::StringLiteral(_)
      | Expression::BooleanLiteral(_)
      | Expression::NullLiteral(_)
      | Expression::NumericLiteral(_)
      | Expression::BigIntLiteral(_)
      | Expression::RegExpLiteral(_) => true,
      Expression::AwaitExpression(n) => recurse(&mut n.argument)?,
      Expression::ParenthesizedExpression(n) => recurse(&mut n.expression)?,
      Expression::TSTypeAssertion(assertion) => {
        assertion.expression = paren_expr(self.allocator, obj_as_never_expr(self.allocator));
        true
      }
      Expression::TSAsExpression(assertion) => {
        assertion.expression = paren_expr(self.allocator, obj_as_never_expr(self.allocator));
        true
      }
      Expression::TSSatisfiesExpression(_) => false,
      Expression::TSNonNullExpression(n) => recurse(&mut n.expression)?,
      Expression::FunctionExpression(n) => {
        self.transform_fn(n, parent_id_span, FunctionKind::ExpressionLike, /* is overload */ false, /* is ambient */ false)?;
        true
      }
      Expression::ArrowFunctionExpression(n) => {
        self.transform_arrow(n, parent_id_span)?;
        true
      }
      Expression::TemplateLiteral(_)
      | Expression::TaggedTemplateExpression(_)
      | Expression::ClassExpression(_)
      | Expression::YieldExpression(_)
      | Expression::MetaProperty(_)
      | Expression::JSXElement(_)
      | Expression::JSXFragment(_)
      | Expression::TSInstantiationExpression(_)
      // todo(dsherret): probably could analyze this more
      | Expression::ChainExpression(_)
      | Expression::ImportExpression(_)
      | Expression::LogicalExpression(_)
      | Expression::PrivateInExpression(_)
      | Expression::V8IntrinsicExpression(_) => false,
    };
    Ok(is_leavable)
  }

  fn maybe_infer_type_from_expr(
    &self,
    expr: &Expression<'a>,
    decl_kind: DeclMutabilityKind,
  ) -> Option<TSType<'a>> {
    match expr {
      Expression::TSTypeAssertion(n) => {
        infer_simple_type_from_type(self.allocator, &n.type_annotation)
      }
      Expression::TSAsExpression(n) => {
        infer_simple_type_from_type(self.allocator, &n.type_annotation)
      }
      Expression::StringLiteral(_)
      | Expression::BooleanLiteral(_)
      | Expression::NullLiteral(_)
      | Expression::NumericLiteral(_)
      | Expression::BigIntLiteral(_)
      | Expression::RegExpLiteral(_) => {
        maybe_lit_to_ts_type(self.allocator, expr, decl_kind)
      }
      Expression::CallExpression(call_expr) => {
        if self.is_call_expr_symbol_create(call_expr) {
          Some(TSType::TSTypeOperatorType(OxcBox::new_in(
            TSTypeOperator {
              node_id: Cell::new(NodeId::DUMMY),
              span: SPAN,
              operator: TSTypeOperatorOperator::Unique,
              type_annotation: TSType::TSTypeReference(OxcBox::new_in(
                super::helpers::type_ref(self.allocator, "symbol"),
                self.allocator,
              )),
            },
            self.allocator,
          )))
        } else {
          None
        }
      }
      Expression::ParenthesizedExpression(n) => {
        self.maybe_infer_type_from_expr(&n.expression, decl_kind)
      }
      _ => None,
    }
  }

  /// Looks if the call expr is `Symbol("example")` or `Symbol.for("example")`
  fn is_call_expr_symbol_create(&self, call_expr: &CallExpression<'a>) -> bool {
    let expr_ident = match &call_expr.callee {
      Expression::Identifier(ident) => ident,
      Expression::StaticMemberExpression(member_expr) => {
        let Expression::Identifier(ident) = &member_expr.object else {
          return false;
        };
        if member_expr.property.name != "for" {
          return false;
        }
        ident
      }
      _ => return false,
    };

    let is_symbol_global = expr_ident.name == "Symbol";
    // TODO: In OXC we don't have unresolved_context() in the same way.
    // For now we check just the name. This may need refinement.
    if !is_symbol_global || call_expr.arguments.len() != 1 {
      return false;
    }
    let Some(arg) = call_expr.arguments.first() else {
      return false;
    };
    matches!(arg, Argument::StringLiteral(_))
  }

  fn promise_wrap_type_owned(&self, ty: TSType<'a>) -> TSType<'a> {
    // Check if it's already Promise<T>
    if let TSType::TSTypeReference(type_ref) = &ty
      && let TSTypeName::IdentifierReference(ident) = &type_ref.type_name
      && ident.name == "Promise"
      && let Some(type_args) = &type_ref.type_arguments
      && type_args.params.len() == 1
    {
      return ty;
    }

    let mut params = OxcVec::with_capacity_in(1, self.allocator);
    params.push(ty);
    TSType::TSTypeReference(OxcBox::new_in(
      TSTypeReference {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
        type_name: TSTypeName::IdentifierReference(OxcBox::new_in(
          IdentifierReference {
            node_id: Cell::new(NodeId::DUMMY),
            span: SPAN,
            name: Ident::from(self.allocator.alloc_str("Promise")),
            reference_id: Cell::new(None),
          },
          self.allocator,
        )),
        type_arguments: Some(OxcBox::new_in(
          TSTypeParameterInstantiation {
            node_id: Cell::new(NodeId::DUMMY),
            span: SPAN,
            params,
          },
          self.allocator,
        )),
      },
      self.allocator,
    ))
  }
}

enum FunctionKind {
  /// function declarations, class method declarations (both class decl and class expr)
  DeclarationLike,
  /// function expressions, arrow functions, object method shorthand properties
  ExpressionLike,
  /// getters, both on classes and object literals
  Getter,
  /// setters, both on classes and object literals
  Setter,
}

fn is_ts_private_computed_class_element(m: &ClassElement) -> bool {
  match m {
    ClassElement::MethodDefinition(m) => {
      if m.accessibility == Some(TSAccessibility::Private) {
        is_computed_property_key(&m.key)
      } else {
        false
      }
    }
    ClassElement::AccessorProperty(a) => {
      if a.accessibility == Some(TSAccessibility::Private) {
        is_computed_property_key(&a.key)
      } else {
        false
      }
    }
    ClassElement::PropertyDefinition(p) => {
      if p.accessibility == Some(TSAccessibility::Private) {
        is_computed_property_key(&p.key)
      } else {
        false
      }
    }
    _ => false,
  }
}

fn is_computed_property_key(key: &PropertyKey) -> bool {
  match key {
    PropertyKey::StaticIdentifier(_) | PropertyKey::PrivateIdentifier(_) => {
      false
    }
    _ => key.is_expression(),
  }
}

fn property_key_to_string(key: &PropertyKey) -> Option<String> {
  match key {
    PropertyKey::StaticIdentifier(ident) => Some(ident.name.to_string()),
    PropertyKey::StringLiteral(s) => Some(s.value.to_string()),
    PropertyKey::NumericLiteral(n) => Some(n.value.to_string()),
    PropertyKey::BigIntLiteral(n) => n.raw.as_ref().map(|r| r.to_string()),
    PropertyKey::PrivateIdentifier(p) => Some(p.name.to_string()),
    _ => None,
  }
}

fn void_or_promise_void<'a>(
  allocator: &'a Allocator,
  is_async: bool,
) -> TSType<'a> {
  let void_type = ts_keyword_type(allocator, TSKeywordKind::Void);
  if is_async {
    let mut params = OxcVec::with_capacity_in(1, allocator);
    params.push(void_type);
    TSType::TSTypeReference(OxcBox::new_in(
      TSTypeReference {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
        type_name: TSTypeName::IdentifierReference(OxcBox::new_in(
          IdentifierReference {
            node_id: Cell::new(NodeId::DUMMY),
            span: SPAN,
            name: Ident::from(allocator.alloc_str("Promise")),
            reference_id: Cell::new(None),
          },
          allocator,
        )),
        type_arguments: Some(OxcBox::new_in(
          TSTypeParameterInstantiation {
            node_id: Cell::new(NodeId::DUMMY),
            span: SPAN,
            params,
          },
          allocator,
        )),
      },
      allocator,
    ))
  } else {
    void_type
  }
}

fn replacement_return_value<'a>(
  allocator: &'a Allocator,
  ty: &TSType<'a>,
) -> Option<Expression<'a>> {
  if is_void_type(ty) {
    None
  } else {
    Some(obj_as_never_expr(allocator))
  }
}

fn infer_simple_type_from_type<'a>(
  allocator: &'a Allocator,
  t: &TSType<'a>,
) -> Option<TSType<'a>> {
  match t {
    TSType::TSAnyKeyword(_)
    | TSType::TSBigIntKeyword(_)
    | TSType::TSBooleanKeyword(_)
    | TSType::TSNeverKeyword(_)
    | TSType::TSNullKeyword(_)
    | TSType::TSNumberKeyword(_)
    | TSType::TSObjectKeyword(_)
    | TSType::TSStringKeyword(_)
    | TSType::TSSymbolKeyword(_)
    | TSType::TSUndefinedKeyword(_)
    | TSType::TSUnknownKeyword(_)
    | TSType::TSVoidKeyword(_)
    | TSType::TSIntrinsicKeyword(_) => Some(t.clone_in(allocator)),
    TSType::TSThisType(_) => Some(t.clone_in(allocator)),
    TSType::TSFunctionType(_) | TSType::TSConstructorType(_) => None,
    TSType::TSTypeReference(type_ref) => {
      let type_arguments = match &type_ref.type_arguments {
        Some(type_args) => {
          let mut params =
            OxcVec::with_capacity_in(type_args.params.len(), allocator);
          for param in &type_args.params {
            params.push(infer_simple_type_from_type(allocator, param)?);
          }
          Some(OxcBox::new_in(
            TSTypeParameterInstantiation {
              node_id: Cell::new(NodeId::DUMMY),
              span: type_args.span,
              params,
            },
            allocator,
          ))
        }
        None => None,
      };
      Some(TSType::TSTypeReference(OxcBox::new_in(
        TSTypeReference {
          node_id: Cell::new(NodeId::DUMMY),
          span: type_ref.span,
          type_name: type_ref.type_name.clone_in(allocator),
          type_arguments,
        },
        allocator,
      )))
    }
    TSType::TSTypeQuery(_) => None,
    TSType::TSTypeLiteral(t) => {
      Some(TSType::TSTypeLiteral(t.clone_in(allocator)))
    }
    TSType::TSTupleType(t) => {
      let mut elems =
        OxcVec::with_capacity_in(t.element_types.len(), allocator);
      for elem in &t.element_types {
        // TSTupleElement shares the same structure as TSType via @inherit
        let elem_as_type = tuple_element_to_type(elem)?;
        let inferred = infer_simple_type_from_type(allocator, &elem_as_type)?;
        elems.push(super::helpers::ts_tuple_element(inferred));
      }
      Some(TSType::TSTupleType(OxcBox::new_in(
        TSTupleType {
          node_id: Cell::new(NodeId::DUMMY),
          span: t.span,
          element_types: elems,
        },
        allocator,
      )))
    }
    TSType::TSArrayType(t) => {
      infer_simple_type_from_type(allocator, &t.element_type).map(|inner| {
        TSType::TSArrayType(OxcBox::new_in(
          TSArrayType {
            node_id: Cell::new(NodeId::DUMMY),
            span: t.span,
            element_type: inner,
          },
          allocator,
        ))
      })
    }
    TSType::TSUnionType(t) => {
      let mut types = OxcVec::with_capacity_in(t.types.len(), allocator);
      for ty in &t.types {
        let inferred_type = infer_simple_type_from_type(allocator, ty)?;
        types.push(inferred_type);
      }
      Some(TSType::TSUnionType(OxcBox::new_in(
        TSUnionType {
          node_id: Cell::new(NodeId::DUMMY),
          span: t.span,
          types,
        },
        allocator,
      )))
    }
    TSType::TSIntersectionType(t) => {
      let mut types = OxcVec::with_capacity_in(t.types.len(), allocator);
      for ty in &t.types {
        let inferred_type = infer_simple_type_from_type(allocator, ty)?;
        types.push(inferred_type);
      }
      Some(TSType::TSIntersectionType(OxcBox::new_in(
        TSIntersectionType {
          node_id: Cell::new(NodeId::DUMMY),
          span: t.span,
          types,
        },
        allocator,
      )))
    }
    TSType::TSConditionalType(_) => None,
    TSType::TSInferType(_) => None,
    TSType::TSParenthesizedType(t) => {
      infer_simple_type_from_type(allocator, &t.type_annotation).map(|inner| {
        TSType::TSParenthesizedType(OxcBox::new_in(
          TSParenthesizedType {
            node_id: Cell::new(NodeId::DUMMY),
            span: t.span,
            type_annotation: inner,
          },
          allocator,
        ))
      })
    }
    TSType::TSTypeOperatorType(t) => {
      infer_simple_type_from_type(allocator, &t.type_annotation).map(|inner| {
        TSType::TSTypeOperatorType(OxcBox::new_in(
          TSTypeOperator {
            node_id: Cell::new(NodeId::DUMMY),
            span: t.span,
            operator: t.operator,
            type_annotation: inner,
          },
          allocator,
        ))
      })
    }
    TSType::TSIndexedAccessType(_) => None,
    TSType::TSMappedType(_) => None,
    TSType::TSLiteralType(t) => match &t.literal {
      TSLiteral::NumericLiteral(_)
      | TSLiteral::StringLiteral(_)
      | TSLiteral::BooleanLiteral(_)
      | TSLiteral::BigIntLiteral(_) => {
        Some(TSType::TSLiteralType(t.clone_in(allocator)))
      }
      TSLiteral::TemplateLiteral(_) | TSLiteral::UnaryExpression(_) => None,
    },
    TSType::TSTypePredicate(_) | TSType::TSImportType(_) => None,
    TSType::TSNamedTupleMember(t) => {
      let inner = tuple_element_to_type_from_named(t)?;
      infer_simple_type_from_type(allocator, &inner).map(|inferred| {
        // Wrap back as named tuple member
        TSType::TSNamedTupleMember(OxcBox::new_in(
          TSNamedTupleMember {
            node_id: Cell::new(NodeId::DUMMY),
            span: t.span,
            element_type: super::helpers::ts_tuple_element(inferred),
            label: t.label.clone_in(allocator),
            optional: t.optional,
          },
          allocator,
        ))
      })
    }
    TSType::TSTemplateLiteralType(_) => None,
    _ => None,
  }
}

/// Convert a TSTupleElement to a TSType for analysis purposes.
/// TSTupleElement inherits TSType variants via @inherit, so the non-unique variants
/// share the same discriminant layout and can be transmuted safely.
fn tuple_element_to_type<'a>(elem: &TSTupleElement<'a>) -> Option<TSType<'a>> {
  match elem {
    TSTupleElement::TSOptionalType(_) | TSTupleElement::TSRestType(_) => {
      // These don't have a direct TSType equivalent
      None
    }
    TSTupleElement::TSNamedTupleMember(t) => {
      tuple_element_to_type(&t.element_type)
    }
    // All other variants are inherited from TSType and share the same layout
    _ => Some(unsafe { std::mem::transmute_copy(elem) }),
  }
}

fn tuple_element_to_type_from_named<'a>(
  t: &TSNamedTupleMember<'a>,
) -> Option<TSType<'a>> {
  tuple_element_to_type(&t.element_type)
}

/// Holds when the first optional parameter occurs.
struct ParamsOptionalStartIndex(Option<usize>);

impl ParamsOptionalStartIndex {
  pub fn build(params: &FormalParameters<'_>) -> Self {
    fn is_param_optional(param: &FormalParameter) -> bool {
      param.optional
        || param.initializer.is_some()
        || matches!(&param.pattern, BindingPattern::AssignmentPattern(_))
    }

    let mut optional_start_index = None;
    for (i, param) in params.items.iter().enumerate() {
      if is_param_optional(param) {
        if optional_start_index.is_none() {
          optional_start_index = Some(i);
        }
      } else {
        optional_start_index = None;
      }
    }

    Self(optional_start_index)
  }

  pub fn is_optional_at_index(&self, index: usize) -> bool {
    self
      .0
      .as_ref()
      .map(|start_index| index >= *start_index)
      .unwrap_or(false)
  }
}

fn is_expr_ident_or_member_idents(expr: &Expression) -> bool {
  match expr {
    Expression::Identifier(_) => true,
    Expression::StaticMemberExpression(n) => {
      is_expr_ident_or_member_idents(&n.object)
    }
    Expression::ComputedMemberExpression(n) => {
      is_expr_ident_or_member_idents(&n.object)
        && is_expr_ident_or_member_idents(&n.expression)
    }
    _ => false,
  }
}

fn array_as_never_expr<'a>(allocator: &'a Allocator) -> Expression<'a> {
  expr_as_keyword_expr(
    allocator,
    empty_array_lit_expr(allocator),
    TSKeywordKind::Any, // We'll use the proper never keyword below
  )
}

fn obj_as_never_expr<'a>(allocator: &'a Allocator) -> Expression<'a> {
  as_expr(
    allocator,
    empty_obj_lit_expr(allocator),
    ts_never_type(allocator),
  )
}

fn array_as_never_array_expr<'a>(allocator: &'a Allocator) -> Expression<'a> {
  as_expr(
    allocator,
    empty_array_lit_expr(allocator),
    TSType::TSArrayType(OxcBox::new_in(
      TSArrayType {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
        element_type: ts_never_type(allocator),
      },
      allocator,
    )),
  )
}

fn ts_never_type<'a>(allocator: &'a Allocator) -> TSType<'a> {
  TSType::TSNeverKeyword(OxcBox::new_in(
    TSNeverKeyword {
      node_id: Cell::new(NodeId::DUMMY),
      span: SPAN,
    },
    allocator,
  ))
}

fn empty_array_lit_expr<'a>(allocator: &'a Allocator) -> Expression<'a> {
  Expression::ArrayExpression(OxcBox::new_in(
    ArrayExpression {
      node_id: Cell::new(NodeId::DUMMY),
      span: SPAN,
      elements: OxcVec::new_in(allocator),
    },
    allocator,
  ))
}

fn empty_obj_lit_expr<'a>(allocator: &'a Allocator) -> Expression<'a> {
  Expression::ObjectExpression(OxcBox::new_in(
    ObjectExpression {
      node_id: Cell::new(NodeId::DUMMY),
      span: SPAN,
      properties: OxcVec::new_in(allocator),
    },
    allocator,
  ))
}

fn expr_as_keyword_expr<'a>(
  allocator: &'a Allocator,
  expr: Expression<'a>,
  _keyword: TSKeywordKind,
) -> Expression<'a> {
  as_expr(allocator, expr, ts_never_type(allocator))
}

fn as_expr<'a>(
  allocator: &'a Allocator,
  expr: Expression<'a>,
  type_annotation: TSType<'a>,
) -> Expression<'a> {
  Expression::TSAsExpression(OxcBox::new_in(
    TSAsExpression {
      node_id: Cell::new(NodeId::DUMMY),
      span: SPAN,
      expression: expr,
      type_annotation,
    },
    allocator,
  ))
}

fn paren_expr<'a>(
  allocator: &'a Allocator,
  expr: Expression<'a>,
) -> Expression<'a> {
  Expression::ParenthesizedExpression(OxcBox::new_in(
    ParenthesizedExpression {
      node_id: Cell::new(NodeId::DUMMY),
      span: SPAN,
      expression: expr,
    },
    allocator,
  ))
}

/// Simple check for valid JavaScript identifier.
fn is_valid_js_ident(s: &str) -> bool {
  if s.is_empty() {
    return false;
  }
  let mut chars = s.chars();
  let first = chars.next().unwrap();
  if !first.is_ascii_alphabetic() && first != '_' && first != '$' {
    return false;
  }
  chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$')
}

/// Walk an expression tree collecting identifier references.
fn collect_ident_refs_in_expr<'a>(
  expr: &Expression<'a>,
  callback: &mut dyn FnMut(&str, Span),
) {
  match expr {
    Expression::Identifier(ident) => {
      callback(&ident.name, ident.span);
    }
    Expression::ArrayExpression(n) => {
      for elem in &n.elements {
        if let Some(e) = elem.as_expression() {
          collect_ident_refs_in_expr(e, callback);
        }
      }
    }
    Expression::ObjectExpression(n) => {
      for prop in &n.properties {
        match prop {
          ObjectPropertyKind::ObjectProperty(p) => {
            collect_ident_refs_in_expr(&p.value, callback);
          }
          ObjectPropertyKind::SpreadProperty(s) => {
            collect_ident_refs_in_expr(&s.argument, callback);
          }
        }
      }
    }
    Expression::UnaryExpression(n) => {
      collect_ident_refs_in_expr(&n.argument, callback)
    }
    Expression::BinaryExpression(n) => {
      collect_ident_refs_in_expr(&n.left, callback);
      collect_ident_refs_in_expr(&n.right, callback);
    }
    Expression::ConditionalExpression(n) => {
      collect_ident_refs_in_expr(&n.test, callback);
      collect_ident_refs_in_expr(&n.consequent, callback);
      collect_ident_refs_in_expr(&n.alternate, callback);
    }
    Expression::CallExpression(n) => {
      collect_ident_refs_in_expr(&n.callee, callback);
      for arg in &n.arguments {
        if let Some(e) = arg.as_expression() {
          collect_ident_refs_in_expr(e, callback);
        }
      }
    }
    Expression::StaticMemberExpression(n) => {
      collect_ident_refs_in_expr(&n.object, callback);
    }
    Expression::ComputedMemberExpression(n) => {
      collect_ident_refs_in_expr(&n.object, callback);
      collect_ident_refs_in_expr(&n.expression, callback);
    }
    Expression::AssignmentExpression(n) => {
      collect_ident_refs_in_expr(&n.right, callback);
    }
    Expression::ParenthesizedExpression(n) => {
      collect_ident_refs_in_expr(&n.expression, callback);
    }
    Expression::AwaitExpression(n) => {
      collect_ident_refs_in_expr(&n.argument, callback);
    }
    Expression::TSAsExpression(n) => {
      collect_ident_refs_in_expr(&n.expression, callback);
    }
    Expression::TSTypeAssertion(n) => {
      collect_ident_refs_in_expr(&n.expression, callback);
    }
    Expression::TSNonNullExpression(n) => {
      collect_ident_refs_in_expr(&n.expression, callback);
    }
    Expression::ArrowFunctionExpression(n) => {
      for stmt in &n.body.statements {
        if let Statement::ExpressionStatement(e) = stmt {
          collect_ident_refs_in_expr(&e.expression, callback);
        }
      }
    }
    Expression::SequenceExpression(n) => {
      for expr in &n.expressions {
        collect_ident_refs_in_expr(expr, callback);
      }
    }
    _ => {}
  }
}

// Statement, ClassElement, PropertyKey, BindingPattern, etc. all implement GetSpan
// from the oxc_ast crate, which provides .span() methods.

// ImportDeclarationSpecifier, ExportSpecifier, ClassElement all implement
// GetSpan from the oxc_ast crate (imported via deno_ast::oxc::span::GetSpan).

fn get_innermost_module_block<'a, 'b>(
  body: &'b mut TSModuleDeclarationBody<'a>,
) -> &'b mut TSModuleBlock<'a> {
  match body {
    TSModuleDeclarationBody::TSModuleBlock(block) => block.as_mut(),
    TSModuleDeclarationBody::TSModuleDeclaration(decl) => {
      if let Some(inner_body) = &mut decl.body {
        get_innermost_module_block(inner_body)
      } else {
        unreachable!("TSModuleDeclaration without body")
      }
    }
  }
}
