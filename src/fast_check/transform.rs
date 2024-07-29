// Copyright 2018-2024 the Deno authors. MIT license.

// for span methods, which actually make sense to use here in the transforms
#![allow(clippy::disallowed_methods)]
#![allow(clippy::disallowed_types)]

use std::collections::HashSet;
use std::sync::Arc;

use deno_ast::emit;
use deno_ast::swc::ast::*;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::common::comments::SingleThreadedComments;
use deno_ast::swc::common::comments::SingleThreadedCommentsMapInner;
use deno_ast::swc::common::Spanned;
use deno_ast::swc::common::SyntaxContext;
use deno_ast::swc::common::DUMMY_SP;
use deno_ast::swc::visit::VisitWith;
use deno_ast::EmitError;
use deno_ast::EmitOptions;
use deno_ast::ModuleSpecifier;
use deno_ast::MultiThreadedComments;
use deno_ast::ParsedSource;
use deno_ast::SourceMap;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use indexmap::IndexMap;

use crate::swc_helpers::analyze_return_stmts_in_function_body;
use crate::swc_helpers::FunctionKind;
use crate::swc_helpers::ReturnStatementAnalysis;
use crate::symbols::EsModuleInfo;
use crate::symbols::ExpandoPropertyRef;
use crate::symbols::Symbol;
use crate::ModuleGraph;
use crate::ModuleInfo;
use crate::ParserModuleAnalyzer;
use crate::WorkspaceMember;

use super::range_finder::ModulePublicRanges;
use super::swc_helpers::any_type_ann;
use super::swc_helpers::ident;
use super::swc_helpers::is_void_type;
use super::swc_helpers::maybe_lit_to_ts_type;
use super::swc_helpers::ts_keyword_type;
use super::transform_dts::FastCheckDtsDiagnostic;
use super::transform_dts::FastCheckDtsTransformer;
use super::FastCheckDiagnostic;
use super::FastCheckDiagnosticRange;

pub struct CommentsMut {
  leading: SingleThreadedCommentsMapInner,
  trailing: SingleThreadedCommentsMapInner,
}

impl CommentsMut {
  pub fn new(single_threaded: SingleThreadedComments) -> Self {
    fn prune_comments(comments: &mut SingleThreadedCommentsMapInner) {
      comments.retain(|_key, value| {
        value.retain(|c| {
          match c.kind {
            // only keep js docs and @ts-* comments
            CommentKind::Line => c.text.trim_start().starts_with("@ts-"),
            CommentKind::Block => c.text.starts_with('*'),
          }
        });
        !value.is_empty()
      });
    }

    let (leading, trailing) = single_threaded.take_all();
    let mut leading = leading.take();
    let mut trailing = trailing.take();
    prune_comments(&mut leading);
    prune_comments(&mut trailing);
    Self { leading, trailing }
  }

  pub fn remove_leading(&mut self, start: deno_ast::SourcePos) {
    self.leading.remove(&start.as_byte_pos());
  }

  pub fn into_multi_threaded(self) -> MultiThreadedComments {
    MultiThreadedComments::from_leading_and_trailing(
      self.leading,
      self.trailing,
    )
  }
}

#[derive(Debug, Clone)]
pub struct FastCheckDtsModule {
  pub program: Program,
  pub comments: MultiThreadedComments,
  pub diagnostics: Vec<FastCheckDtsDiagnostic>,
}

#[derive(Debug)]
pub struct FastCheckModule {
  pub module_info: Arc<ModuleInfo>,
  pub text: Arc<str>,
  pub source_map: Arc<[u8]>,
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
  let mut transformer = FastCheckTransformer::new(
    graph,
    es_module_info,
    public_ranges,
    options.should_error_on_first_diagnostic,
  );
  let (module, comments) = transformer.transform()?;
  if !transformer.diagnostics.is_empty() {
    return Err(transformer.diagnostics);
  }
  let parsed_source = es_module_info.source();
  let specifier = es_module_info.specifier();
  let module_info = ParserModuleAnalyzer::module_info_from_swc(
    parsed_source.media_type(),
    &module,
    parsed_source.text_info_lazy(),
    &comments,
  );

  // swc will modify the comment collection internally when emitting,
  // so if we're emitting with dts, make a copy of the comments for
  // each emit
  let (fast_check_comments, dts_comments) = if options.dts {
    (comments.as_single_threaded(), Some(comments))
  } else {
    (comments.into_single_threaded(), None)
  };

  // now emit
  let source_map =
    SourceMap::single(specifier.clone(), parsed_source.text().to_string());
  let program = Program::Module(module);
  let emitted_source = emit(
    &program,
    &fast_check_comments,
    &source_map,
    &EmitOptions {
      remove_comments: false,
      source_map: deno_ast::SourceMapOption::Separate,
      source_map_file: None,
      inline_sources: false,
    },
  )
  .map_err(|e| {
    vec![FastCheckDiagnostic::Emit {
      specifier: specifier.clone(),
      inner: Arc::new(e),
    }]
  })?;
  let emitted_text = String::from_utf8(emitted_source.source).map_err(|e| {
    vec![FastCheckDiagnostic::Emit {
      specifier: specifier.clone(),
      inner: Arc::new(EmitError::SwcEmit(e.into())),
    }]
  })?;

  let dts = if let Some(dts_comments) = dts_comments {
    let mut dts_transformer = FastCheckDtsTransformer::new(
      parsed_source.text_info_lazy(),
      public_ranges,
      specifier,
    );

    let module = dts_transformer.transform(program.expect_module())?;

    Some(FastCheckDtsModule {
      program: Program::Module(module),
      comments: dts_comments,
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
    if retain {
      Self::Retain
    } else {
      Self::Remove
    }
  }
}

struct FastCheckTransformer<'a> {
  graph: &'a ModuleGraph,
  specifier: &'a ModuleSpecifier,
  es_module_info: &'a EsModuleInfo,
  public_ranges: &'a ModulePublicRanges,
  parsed_source: &'a ParsedSource,
  should_error_on_first_diagnostic: bool,
  diagnostics: Vec<FastCheckDiagnostic>,
  expando_namespaces: IndexMap<Id, Vec<VarDeclarator>>,
}

impl<'a> FastCheckTransformer<'a> {
  pub fn new(
    graph: &'a ModuleGraph,
    es_module_info: &'a EsModuleInfo,
    public_ranges: &'a ModulePublicRanges,
    should_error_on_first_diagnostic: bool,
  ) -> Self {
    Self {
      graph,
      specifier: es_module_info.specifier(),
      es_module_info,
      public_ranges,
      parsed_source: es_module_info.source(),
      should_error_on_first_diagnostic,
      diagnostics: Default::default(),
      expando_namespaces: Default::default(),
    }
  }

  pub fn transform(
    &mut self,
  ) -> Result<
    (deno_ast::swc::ast::Module, MultiThreadedComments),
    Vec<FastCheckDiagnostic>,
  > {
    let is_ambient = self.parsed_source.media_type().is_declaration();
    let mut module = self.parsed_source.module().clone();
    let mut comments =
      CommentsMut::new(self.parsed_source.comments().as_single_threaded());
    module.body = self.transform_module_body(
      std::mem::take(&mut module.body),
      &mut comments,
      is_ambient,
    )?;
    Ok((module, comments.into_multi_threaded()))
  }

  fn transform_module_body(
    &mut self,
    body: Vec<ModuleItem>,
    comments: &mut CommentsMut,
    is_ambient: bool,
  ) -> Result<Vec<ModuleItem>, Vec<FastCheckDiagnostic>> {
    let parent_expando_namespaces =
      std::mem::take(&mut self.expando_namespaces);
    let mut final_body = Vec::with_capacity(body.len());
    for mut item in body {
      let result = self.transform_item(&mut item, comments, is_ambient)?;
      match result {
        TransformItemResult::Retain => final_body.push(item),
        TransformItemResult::Remove => {
          comments.remove_leading(item.start());
        }
      }
    }

    // Add accumulated namespaces
    final_body.reserve(self.expando_namespaces.len());
    for (swc_id, var_decls) in
      std::mem::take(&mut self.expando_namespaces).drain(..)
    {
      let symbol = self.es_module_info.symbol_from_swc(&swc_id).unwrap();
      for decl in &var_decls {
        self.check_expando_property_diagnostics(decl, &swc_id, symbol)?;
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
      let module_decl = Decl::TsModule(Box::new(TsModuleDecl {
        span: DUMMY_SP,
        declare: false,
        global: false,
        id: TsModuleName::Ident(Ident::new(swc_id.0, DUMMY_SP)),
        body: Some(TsNamespaceBody::TsModuleBlock(TsModuleBlock {
          span: DUMMY_SP,
          body: vec![ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(
            ExportDecl {
              span: DUMMY_SP,
              decl: Decl::Var(Box::new(VarDecl {
                span: DUMMY_SP,
                kind: VarDeclKind::Var,
                declare: false,
                decls: var_decls,
              })),
            },
          ))],
        })),
      }));
      final_body.push(if has_export_keyword {
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
          span: DUMMY_SP,
          decl: module_decl,
        }))
      } else {
        ModuleItem::Stmt(Stmt::Decl(module_decl))
      });
    }

    self.expando_namespaces = parent_expando_namespaces;

    Ok(final_body)
  }

  fn check_expando_property_diagnostics(
    &mut self,
    decl: &VarDeclarator,
    parent_id: &Id,
    parent_symbol: &Symbol,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    struct VisitExpandoPropInits<'a> {
      symbol: &'a Symbol,
      parent_context: SyntaxContext,
      diagnostics: IndexMap<String, SourceRange>,
    }

    impl deno_ast::swc::visit::Visit for VisitExpandoPropInits<'_> {
      fn visit_ident(&mut self, ident: &Ident) {
        let (name, context) = ident.to_id();
        if context == self.parent_context && self.symbol.export(&name).is_some()
        {
          self.diagnostics.insert(name.to_string(), ident.range());
        }
      }
    }

    let mut inits = VisitExpandoPropInits {
      symbol: parent_symbol,
      parent_context: parent_id.1,
      diagnostics: Default::default(),
    };
    decl.init.visit_with(&mut inits);
    for (reference_name, range) in inits.diagnostics {
      self.mark_diagnostic(
        FastCheckDiagnostic::UnsupportedExpandoProperty {
          object_name: parent_id.0.to_string(),
          reference_name,
          range: self.source_range_to_range(range),
        },
      )?;
    }
    Ok(())
  }

  fn transform_module_specifier(&mut self, src: &mut Str) {
    // only do this for relative specifiers (specifiers to specifiers within the package)
    if !src.value.starts_with('.') {
      return;
    }
    let Some(resolved_specifier) =
      self
        .graph
        .resolve_dependency(&src.value, self.specifier, true)
    else {
      return;
    };
    if let Some(relative) = self.specifier.make_relative(&resolved_specifier) {
      if !relative.starts_with("../") {
        src.value = format!("./{}", relative).into();
      } else {
        src.value = relative.into();
      }
      src.raw = None;
    }
  }

  fn transform_item(
    &mut self,
    item: &mut ModuleItem,
    comments: &mut CommentsMut,
    is_ambient: bool,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    match item {
      ModuleItem::ModuleDecl(decl) => match decl {
        ModuleDecl::Import(n) => {
          n.specifiers
            .retain(|s| self.public_ranges.contains(&s.range()));
          let retain = !n.specifiers.is_empty();
          if retain {
            self.transform_module_specifier(&mut n.src);
          }
          Ok(TransformItemResult::from_retain(retain))
        }
        ModuleDecl::ExportNamed(n) => {
          n.specifiers
            .retain(|s| self.public_ranges.contains(&s.range()));
          let retain = !n.specifiers.is_empty();
          if retain {
            if let Some(src) = &mut n.src {
              self.transform_module_specifier(src);
            }
          }
          Ok(TransformItemResult::from_retain(retain))
        }
        ModuleDecl::ExportAll(n) => {
          let retain = self.public_ranges.contains(&n.range());
          if retain {
            self.transform_module_specifier(&mut n.src);
          }
          Ok(TransformItemResult::from_retain(retain))
        }
        ModuleDecl::ExportDefaultExpr(n) => {
          // todo: investigate why both these checks are needed
          if !self.public_ranges.contains(&n.range())
            && !self.public_ranges.contains(&n.expr.range())
          {
            return Ok(TransformItemResult::Remove);
          }

          if self.maybe_transform_expr_if_leavable(&mut n.expr, None)?
            || is_expr_ident_or_member_idents(&n.expr)
          {
            Ok(TransformItemResult::Retain)
          } else {
            self.mark_diagnostic(
              FastCheckDiagnostic::UnsupportedDefaultExportExpr {
                range: self.source_range_to_range(n.range()),
              },
            )?;
            Ok(TransformItemResult::Remove)
          }
        }
        ModuleDecl::ExportDefaultDecl(n) => {
          if !self.public_ranges.contains(&n.range()) {
            return Ok(TransformItemResult::Remove);
          }

          let node_range = n.range();
          self.transform_default_decl(
            &mut n.decl,
            comments,
            node_range,
            is_ambient,
          )
        }
        ModuleDecl::ExportDecl(n) => {
          let export_decl_range = n.range();
          self.transform_decl(
            &mut n.decl,
            comments,
            Some(export_decl_range),
            is_ambient,
          )
        }
        ModuleDecl::TsImportEquals(n) => match &n.module_ref {
          TsModuleRef::TsEntityName(_) => {
            let retain = self.public_ranges.contains(&n.range());
            Ok(TransformItemResult::from_retain(retain))
          }
          TsModuleRef::TsExternalModuleRef(_) => {
            self.mark_diagnostic(FastCheckDiagnostic::UnsupportedRequire {
              range: self.source_range_to_range(n.range()),
            })?;
            Ok(TransformItemResult::Remove)
          }
        },
        ModuleDecl::TsExportAssignment(n) => {
          self.mark_diagnostic(
            FastCheckDiagnostic::UnsupportedTsExportAssignment {
              range: self.source_range_to_range(n.range()),
            },
          )?;
          Ok(TransformItemResult::Remove)
        }
        ModuleDecl::TsNamespaceExport(n) => {
          self.mark_diagnostic(
            FastCheckDiagnostic::UnsupportedTsNamespaceExport {
              range: self.source_range_to_range(n.range()),
            },
          )?;
          Ok(TransformItemResult::Remove)
        }
      },
      ModuleItem::Stmt(stmt) => match stmt {
        Stmt::Decl(n) => self.transform_decl(n, comments, None, is_ambient),
        Stmt::Expr(n) => match &mut *n.expr {
          Expr::Assign(assign_expr) => self.transform_assign_expr(assign_expr),
          _ => Ok(TransformItemResult::Remove),
        },
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
        | Stmt::ForOf(_) => Ok(TransformItemResult::Remove),
      },
    }
  }

  fn transform_default_decl(
    &mut self,
    default_decl: &mut DefaultDecl,
    comments: &mut CommentsMut,
    parent_range: SourceRange,
    is_ambient: bool,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    match default_decl {
      DefaultDecl::Class(n) => {
        (self.transform_class(
          &mut n.class,
          comments,
          /* has declare keyword */ false,
        )?);
        Ok(TransformItemResult::Retain)
      }
      DefaultDecl::Fn(n) => {
        self.transform_fn(
          &mut n.function,
          n.ident.as_ref().map(|i| i.range()),
          FunctionKind::DeclarationLike,
          self.public_ranges.is_impl_with_overloads(&parent_range),
          is_ambient,
        )?;
        Ok(TransformItemResult::Retain)
      }
      DefaultDecl::TsInterfaceDecl(_) => Ok(TransformItemResult::Retain),
    }
  }

  fn transform_decl(
    &mut self,
    decl: &mut Decl,
    comments: &mut CommentsMut,
    parent_range: Option<SourceRange>,
    is_ambient: bool,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    let public_range = parent_range.unwrap_or_else(|| decl.range());
    match decl {
      Decl::Class(n) => {
        if !self.public_ranges.contains(&public_range) {
          return Ok(TransformItemResult::Remove);
        }
        self.transform_class(
          &mut n.class,
          comments,
          is_ambient || n.declare,
        )?;
        Ok(TransformItemResult::Retain)
      }
      Decl::Fn(n) => {
        if !self.public_ranges.contains(&public_range) {
          return Ok(TransformItemResult::Remove);
        }

        let is_overload =
          self.public_ranges.is_impl_with_overloads(&public_range);
        self.transform_fn(
          &mut n.function,
          Some(n.ident.range()),
          FunctionKind::DeclarationLike,
          is_overload,
          is_ambient,
        )?;
        Ok(TransformItemResult::Retain)
      }
      Decl::Var(n) => self.transform_var(n, is_ambient || n.declare),
      Decl::TsInterface(_) => Ok(TransformItemResult::from_retain(
        self.public_ranges.contains(&public_range),
      )),
      Decl::TsTypeAlias(_) => Ok(TransformItemResult::from_retain(
        self.public_ranges.contains(&public_range),
      )),
      Decl::TsEnum(_) => Ok(TransformItemResult::from_retain(
        self.public_ranges.contains(&public_range),
      )),
      Decl::TsModule(m) => self.transform_ts_module(
        m,
        &public_range,
        comments,
        is_ambient || m.declare || m.global,
      ),
      Decl::Using(n) => {
        if self.public_ranges.contains(&public_range)
          || n
            .decls
            .iter()
            .any(|d| self.public_ranges.contains(&d.range()))
        {
          self.mark_diagnostic(FastCheckDiagnostic::UnsupportedUsing {
            range: self.source_range_to_range(
              n.decls
                .first()
                .map(|n| n.range())
                .unwrap_or_else(|| n.range()),
            ),
          })?;
        }
        Ok(TransformItemResult::Remove)
      }
    }
  }

  fn transform_class(
    &mut self,
    n: &mut Class,
    comments: &mut CommentsMut,
    is_ambient: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    if is_ambient {
      // ignore private computed members
      n.body.retain(|m| !is_ts_private_computed_class_member(m));
      return Ok(());
    }

    let mut members = Vec::with_capacity(n.body.len());
    let mut had_private = false;
    if let Some(super_class) = &n.super_class {
      if !is_expr_ident_or_member_idents(super_class) {
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedSuperClassExpr {
            range: self.source_range_to_range(n.super_class.range()),
          },
        )?;
      }
    }
    let mut insert_members = Vec::new();
    let mut had_private_constructor = false;
    let mut seen_ts_private_methods = HashSet::new();
    for mut member in std::mem::take(&mut n.body) {
      had_private = had_private
        || matches!(
          member,
          ClassMember::PrivateMethod(_)
            | ClassMember::PrivateProp(_)
            | ClassMember::AutoAccessor(AutoAccessor {
              key: Key::Private(_),
              ..
            })
        );

      let mut retain = !is_ts_private_computed_class_member(&member);
      if retain {
        // do some extra checks to see whether it should be removed
        if let ClassMember::Constructor(ctor) = &member {
          if ctor.accessibility == Some(Accessibility::Private) {
            if had_private_constructor {
              retain = false;
            } else {
              had_private_constructor = true;
            }
          }
        } else if let ClassMember::Method(method) = &member {
          if method.accessibility == Some(Accessibility::Private) {
            let key = match &method.key {
              PropName::Ident(i) => Some(i.sym.to_string()),
              PropName::Str(s) => Some(s.value.to_string()),
              PropName::Num(n) => Some(
                n.raw
                  .as_ref()
                  .map(|r| r.to_string())
                  .unwrap_or_else(|| n.value.to_string()),
              ),

              PropName::Computed(_) => None,
              PropName::BigInt(n) => Some(
                n.raw
                  .as_ref()
                  .map(|r| r.to_string())
                  .unwrap_or_else(|| n.value.to_string()),
              ),
            };
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
        comments.remove_leading(member.start());
      }
    }

    if had_private {
      insert_members.insert(
        0,
        ClassMember::PrivateProp(PrivateProp {
          span: DUMMY_SP,
          key: PrivateName {
            span: DUMMY_SP,
            id: ident("private".to_string()),
          },
          value: None,
          type_ann: Some(Box::new(TsTypeAnn {
            span: DUMMY_SP,
            type_ann: Box::new(ts_keyword_type(
              TsKeywordTypeKind::TsUnknownKeyword,
            )),
          })),
          is_static: false,
          decorators: Default::default(),
          accessibility: Default::default(),
          is_optional: false,
          is_override: false,
          readonly: false,
          definite: true,
        }),
      )
    }

    n.body = insert_members.into_iter().chain(members).collect();
    n.decorators.clear();
    Ok(())
  }

  fn transform_class_member(
    &mut self,
    member: &mut ClassMember,
    insert_members: &mut Vec<ClassMember>,
    is_ambient: bool,
  ) -> Result<bool, Vec<FastCheckDiagnostic>> {
    match member {
      ClassMember::Constructor(n) => {
        if let Some(body) = &mut n.body {
          body.stmts.retain_mut(|stmt| match stmt {
            Stmt::Expr(e) => match &mut *e.expr {
              Expr::Call(c) => {
                if !matches!(c.callee, Callee::Super(_)) {
                  return false;
                }
                for arg in c.args.iter_mut() {
                  arg.expr = if arg.spread.is_some() {
                    paren_expr(array_as_never_array_expr())
                  } else {
                    obj_as_never_expr()
                  };
                }
                true
              }
              _ => false,
            },
            _ => false,
          });
        }

        for param in &mut n.params {
          match param {
            ParamOrTsParamProp::Param(_) => {
              // ignore
            }
            ParamOrTsParamProp::TsParamProp(prop) => {
              let is_optional = match &prop.param {
                TsParamPropParam::Ident(ident) => ident.optional,
                TsParamPropParam::Assign(_) => false,
              };
              insert_members.push(ClassMember::ClassProp(ClassProp {
                span: DUMMY_SP,
                key: match &prop.param {
                  TsParamPropParam::Ident(ident) => {
                    PropName::Ident(ident.id.clone())
                  }
                  TsParamPropParam::Assign(assign) => match &*assign.left {
                    Pat::Ident(ident) => PropName::Ident(ident.id.clone()),
                    Pat::Array(_)
                    | Pat::Rest(_)
                    | Pat::Object(_)
                    | Pat::Assign(_)
                    | Pat::Invalid(_)
                    | Pat::Expr(_) => {
                      self.mark_diagnostic(
                        FastCheckDiagnostic::UnsupportedDestructuring {
                          range: self
                            .source_range_to_range(assign.left.range()),
                        },
                      )?;
                      return Ok(false);
                    }
                  },
                },
                value: None,
                type_ann: if prop.accessibility == Some(Accessibility::Private)
                {
                  Some(any_type_ann())
                } else {
                  match &prop.param {
                    TsParamPropParam::Ident(ident) => ident.type_ann.clone(),
                    TsParamPropParam::Assign(assign) => {
                      let explicit_type_ann = match &*assign.left {
                        Pat::Ident(binding_ident) => {
                          binding_ident.type_ann.clone()
                        }
                        _ => None,
                      };
                      explicit_type_ann.or_else(|| {
                        self.maybe_infer_type_from_expr(&assign.right).map(
                          |type_ann| {
                            Box::new(TsTypeAnn {
                              span: DUMMY_SP,
                              type_ann: Box::new(type_ann),
                            })
                          },
                        )
                      })
                    }
                  }
                },
                is_static: false,
                decorators: Vec::new(),
                accessibility: match prop.accessibility {
                  Some(Accessibility::Public) | None => None,
                  Some(accessibility) => Some(accessibility),
                },
                is_abstract: false,
                is_optional,
                is_override: prop.is_override,
                readonly: prop.readonly,
                // declare is not valid with override
                declare: !prop.is_override,
                definite: prop.is_override,
              }));
              *param = ParamOrTsParamProp::Param(Param {
                span: prop.span,
                decorators: vec![],
                pat: match prop.param.clone() {
                  TsParamPropParam::Ident(ident) => Pat::Ident(ident),
                  TsParamPropParam::Assign(pat) => Pat::Assign(pat),
                },
              });
            }
          }
        }

        if n.accessibility == Some(Accessibility::Private) {
          n.params.clear();
          return Ok(true);
        }
        let is_overload = self.public_ranges.is_impl_with_overloads(&n.range());
        if is_overload {
          for (i, param) in n.params.iter_mut().enumerate() {
            if param.as_param().map(|p| p.pat.is_rest()).unwrap_or(false) {
              *param = ParamOrTsParamProp::Param(Param {
                span: DUMMY_SP,
                decorators: Vec::new(),
                pat: Pat::Rest(RestPat {
                  span: DUMMY_SP,
                  dot3_token: DUMMY_SP,
                  type_ann: Some(any_type_ann()),
                  arg: Box::new(Pat::Ident(BindingIdent {
                    id: Ident {
                      span: DUMMY_SP,
                      sym: format!("param{}", i).into(),
                      optional: false,
                    },
                    type_ann: None,
                  })),
                }),
              });
            } else {
              *param = ParamOrTsParamProp::Param(Param {
                span: DUMMY_SP,
                decorators: Vec::new(),
                pat: Pat::Ident(BindingIdent {
                  id: Ident {
                    span: DUMMY_SP,
                    sym: format!("param{}", i).into(),
                    optional: true,
                  },
                  type_ann: Some(any_type_ann()),
                }),
              });
            }
          }
        }

        let optional_start_index =
          ParamsOptionalStartIndex::build(n.params.iter().map(|p| match p {
            ParamOrTsParamProp::Param(p) => &p.pat,
            // should have been converted to a param
            ParamOrTsParamProp::TsParamProp(_) => unreachable!(),
          }));
        for (i, param) in n.params.iter_mut().enumerate() {
          match param {
            ParamOrTsParamProp::Param(param) => {
              self.handle_param_pat(
                &mut param.pat,
                optional_start_index.is_optional_at_index(i),
              )?;
              param.decorators.clear();
            }
            ParamOrTsParamProp::TsParamProp(_) => {
              // should have been converted to a param
              unreachable!();
            }
          }
        }

        Ok(true)
      }
      ClassMember::Method(n) => {
        if n.accessibility == Some(Accessibility::Private) {
          *member = ClassMember::ClassProp(ClassProp {
            span: DUMMY_SP,
            key: n.key.clone(),
            value: None,
            type_ann: Some(any_type_ann()),
            is_static: n.is_static,
            decorators: Vec::new(),
            accessibility: Some(Accessibility::Private),
            is_abstract: n.is_abstract,
            is_optional: n.is_optional,
            is_override: n.is_override,
            readonly: false,
            // delcare is not valid with override
            declare: !n.is_override,
            definite: n.is_override,
          });
          return Ok(true);
        }
        let is_overload = self.public_ranges.is_impl_with_overloads(&n.range());
        self.transform_fn(
          &mut n.function,
          Some(n.key.range()),
          match n.kind {
            MethodKind::Method => FunctionKind::DeclarationLike,
            MethodKind::Getter => FunctionKind::Getter,
            MethodKind::Setter => FunctionKind::Setter,
          },
          is_overload,
          is_ambient,
        )?;
        Ok(true)
      }
      ClassMember::ClassProp(n) => {
        if n.accessibility == Some(Accessibility::Private) {
          n.type_ann = Some(any_type_ann());
          // it doesn't make sense for a private property to have the override
          // keyword, but might as well make this consistent with elsewhere
          n.declare = !n.is_override;
          n.definite = n.is_override;
          n.value = None;
          return Ok(true);
        }
        if n.type_ann.is_none() {
          let inferred_type = n
            .value
            .as_ref()
            .and_then(|e| self.maybe_infer_type_from_expr(e));
          match inferred_type {
            Some(t) => {
              n.type_ann = Some(Box::new(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::new(t),
              }));
              n.value = None;
            }
            None => {
              let is_value_leavable = match n.value.as_mut() {
                Some(init) => self.maybe_transform_expr_if_leavable(
                  init,
                  Some(n.key.range()),
                )?,
                None => false,
              };
              if !is_value_leavable {
                self.mark_diagnostic(
                  FastCheckDiagnostic::MissingExplicitType {
                    range: self.source_range_to_range(n.key.range()),
                  },
                )?;
              }
            }
          }
        } else {
          n.value = None;
        }
        n.declare = n.value.is_none() && !n.is_override;
        n.definite = n.value.is_none() && n.is_override;
        n.decorators.clear();
        Ok(true)
      }
      ClassMember::AutoAccessor(n) => {
        let key = match &n.key {
          Key::Private(_) => {
            return Ok(false);
          }
          Key::Public(key) => key,
        };
        let type_ann = if n.accessibility == Some(Accessibility::Private) {
          any_type_ann()
        } else if let Some(type_ann) = n.type_ann.clone() {
          type_ann
        } else {
          let inferred_type = n
            .value
            .as_ref()
            .and_then(|e| self.maybe_infer_type_from_expr(e));
          match inferred_type {
            Some(t) => Box::new(TsTypeAnn {
              span: DUMMY_SP,
              type_ann: Box::new(t),
            }),
            None => {
              self.mark_diagnostic(
                FastCheckDiagnostic::MissingExplicitType {
                  range: self.source_range_to_range(n.key.range()),
                },
              )?;
              any_type_ann()
            }
          }
        };
        *member = ClassMember::ClassProp(ClassProp {
          span: n.span,
          key: key.clone(),
          value: None,
          type_ann: Some(type_ann),
          is_static: n.is_static,
          decorators: Vec::new(),
          accessibility: n.accessibility,
          // todo(dsherret): ensure abstract auto-accessors work
          // once this pr lands: https://github.com/swc-project/swc/pull/8736
          is_abstract: false,
          is_optional: false,
          is_override: n.is_override,
          readonly: false,
          declare: !n.is_override,
          definite: n.is_override,
        });
        Ok(true)
      }
      ClassMember::TsIndexSignature(_) => {
        // ok, as-is
        Ok(true)
      }
      ClassMember::PrivateMethod(_)
      | ClassMember::PrivateProp(_)
      | ClassMember::Empty(_)
      | ClassMember::StaticBlock(_) => Ok(false),
    }
  }

  fn transform_fn(
    &mut self,
    n: &mut Function,
    parent_id_range: Option<SourceRange>,
    function_kind: FunctionKind,
    is_overload: bool,
    is_ambient: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    if is_ambient {
      return Ok(()); // no need to do anything
    }
    if is_overload {
      for (i, param) in n.params.iter_mut().enumerate() {
        if param.pat.is_rest() {
          *param = Param {
            span: DUMMY_SP,
            decorators: Vec::new(),
            pat: Pat::Rest(RestPat {
              span: DUMMY_SP,
              dot3_token: DUMMY_SP,
              type_ann: Some(any_type_ann()),
              arg: Box::new(Pat::Ident(BindingIdent {
                id: Ident {
                  span: DUMMY_SP,
                  sym: format!("param{}", i).into(),
                  optional: false,
                },
                type_ann: None,
              })),
            }),
          };
        } else {
          *param = Param {
            span: DUMMY_SP,
            decorators: Vec::new(),
            pat: Pat::Ident(BindingIdent {
              id: Ident {
                span: DUMMY_SP,
                sym: format!("param{}", i).into(),
                optional: true,
              },
              type_ann: Some(any_type_ann()),
            }),
          };
        }
      }
      n.return_type = Some(any_type_ann());
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
      let range = parent_id_range.unwrap_or_else(|| n.range());
      if n.is_generator {
        self.mark_diagnostic(
          FastCheckDiagnostic::MissingExplicitReturnType {
            range: self.source_range_to_range(range),
            is_definitely_void_or_never: false,
            is_async: n.is_async,
          },
        )?;
      } else if let Some(body) = &mut n.body {
        self.transform_function_body_block_stmt(
          range,
          &mut n.return_type,
          body,
          function_kind,
          n.is_async,
        )?;
      }
    }

    if let Some(body) = &mut n.body {
      body.stmts.clear();

      // push a return stmt to suppress type errors, if the function is not a
      // void function
      let return_expr = n
        .return_type
        .as_ref()
        .and_then(|t| replacement_return_value(&t.type_ann));
      if let Some(return_expr) = return_expr {
        body.stmts.push(Stmt::Return(ReturnStmt {
          span: DUMMY_SP,
          arg: Some(return_expr),
        }));
      }
    }

    let optional_start_index =
      ParamsOptionalStartIndex::build(n.params.iter().map(|p| &p.pat));
    for (i, param) in n.params.iter_mut().enumerate() {
      self.handle_param_pat(
        &mut param.pat,
        optional_start_index.is_optional_at_index(i),
      )?;
      param.decorators.clear();
    }

    n.is_async = false;
    n.is_generator = false;
    n.decorators.clear();

    Ok(())
  }

  fn transform_arrow(
    &mut self,
    n: &mut ArrowExpr,
    parent_id_range: Option<SourceRange>,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    let range = parent_id_range.unwrap_or_else(|| n.range());

    if n.return_type.is_none() {
      // arrow functions can not be generators, let's ignore

      match &mut *n.body {
        BlockStmtOrExpr::BlockStmt(body) => {
          self.transform_function_body_block_stmt(
            range,
            &mut n.return_type,
            body,
            FunctionKind::ExpressionLike,
            n.is_async,
          )?;
        }
        BlockStmtOrExpr::Expr(expr) => {
          let inferred_type = self.maybe_infer_type_from_expr(expr);
          match inferred_type {
            Some(t) => {
              let mut return_type = Box::new(t);
              if n.is_async {
                return_type = self.promise_wrap_type(return_type)
              }
              n.return_type = Some(Box::new(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: return_type,
              }));
            }
            None => {
              let is_expr_leavable =
                self.maybe_transform_expr_if_leavable(expr, None)?;
              if !is_expr_leavable {
                self.mark_diagnostic(
                  FastCheckDiagnostic::MissingExplicitReturnType {
                    range: self.source_range_to_range(range),
                    is_definitely_void_or_never: false,
                    is_async: n.is_async,
                  },
                )?;
              }
            }
          }
        }
      }
    }

    if let Some(t) = &n.return_type {
      // there is an explicit return type, so we can clear the body
      let return_expr = replacement_return_value(&t.type_ann);
      *n.body = return_expr
        .map(|e| BlockStmtOrExpr::Expr(paren_expr(e)))
        .unwrap_or_else(|| {
          BlockStmtOrExpr::BlockStmt(BlockStmt {
            span: DUMMY_SP,
            stmts: vec![],
          })
        });

      // Only reset is_async if there is an explicit return type
      n.is_async = false;
    }

    let optional_start_index = ParamsOptionalStartIndex::build(n.params.iter());
    for (i, pat) in n.params.iter_mut().enumerate() {
      self
        .handle_param_pat(pat, optional_start_index.is_optional_at_index(i))?;
    }

    Ok(())
  }

  fn transform_function_body_block_stmt(
    &mut self,
    range: SourceRange,
    return_type: &mut Option<Box<TsTypeAnn>>,
    body: &mut BlockStmt,
    function_kind: FunctionKind,
    is_async: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    let analysis = analyze_return_stmts_in_function_body(body);
    match (analysis, function_kind) {
      (_, FunctionKind::Setter) => unreachable!(),
      (ReturnStatementAnalysis::None, FunctionKind::DeclarationLike)
      | (ReturnStatementAnalysis::Void, FunctionKind::DeclarationLike)
      | (ReturnStatementAnalysis::Void, FunctionKind::ExpressionLike) => {
        *return_type = Some(Box::new(TsTypeAnn {
          span: DUMMY_SP,
          type_ann: void_or_promise_void(is_async),
        }));
      }
      (ReturnStatementAnalysis::None, FunctionKind::ExpressionLike) => {
        self.mark_diagnostic(
          FastCheckDiagnostic::MissingExplicitReturnType {
            range: self.source_range_to_range(range),
            is_definitely_void_or_never: true,
            is_async,
          },
        )?;
      }
      (ReturnStatementAnalysis::Single(_), _) => {
        // TODO: infer return type based on return type
        self.mark_diagnostic(
          FastCheckDiagnostic::MissingExplicitReturnType {
            range: self.source_range_to_range(range),
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
            range: self.source_range_to_range(range),
            is_definitely_void_or_never: false,
            is_async,
          },
        )?;
      }
    }

    Ok(())
  }

  fn handle_param_pat(
    &mut self,
    pat: &mut Pat,
    is_optional: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    match pat {
      Pat::Ident(ident) => {
        if ident.type_ann.is_none() {
          self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
            range: self.source_range_to_range(pat.range()),
          })?;
        }
      }
      Pat::Assign(assign) => match &mut *assign.left {
        Pat::Ident(ident) => {
          if ident.type_ann.is_none() {
            let inferred_type = self.maybe_infer_type_from_expr(&assign.right);
            match inferred_type {
              Some(t) => {
                ident.type_ann = Some(Box::new(TsTypeAnn {
                  span: DUMMY_SP,
                  type_ann: Box::new(t),
                }));
                if !is_optional {
                  convert_optional_ident_to_nullable_type(ident);
                } else {
                  ident.id.optional = true;
                }
                *pat = Pat::Ident((*ident).clone());
              }
              None => {
                let is_expr_leavable = self.maybe_transform_expr_if_leavable(
                  &mut assign.right,
                  Some(ident.range()),
                )?;
                if !is_expr_leavable {
                  self.mark_diagnostic(
                    FastCheckDiagnostic::MissingExplicitType {
                      range: self.source_range_to_range(ident.range()),
                    },
                  )?;
                }
              }
            }
          } else {
            if !is_optional {
              convert_optional_ident_to_nullable_type(ident);
            } else {
              ident.id.optional = true;
            }
            *pat = Pat::Ident((*ident).clone());
          }
        }
        Pat::Array(p) => {
          if p.type_ann.is_none() {
            if !self
              .maybe_transform_expr_if_leavable(&mut assign.right, None)?
            {
              self.mark_diagnostic(
                FastCheckDiagnostic::MissingExplicitType {
                  range: self.source_range_to_range(p.range()),
                },
              )?;
            }
          } else {
            assign.right = array_as_never_expr();
          }
          p.elems.clear();
        }
        Pat::Object(p) => {
          if p.type_ann.is_none() {
            if !self
              .maybe_transform_expr_if_leavable(&mut assign.right, None)?
            {
              self.mark_diagnostic(
                FastCheckDiagnostic::MissingExplicitType {
                  range: self.source_range_to_range(p.range()),
                },
              )?;
            }
          } else {
            assign.right = obj_as_never_expr();
          }
          p.props.clear();
        }
        Pat::Assign(_) | Pat::Invalid(_) | Pat::Rest(_) | Pat::Expr(_) => {
          self.mark_diagnostic(
            FastCheckDiagnostic::UnsupportedDestructuring {
              range: self.source_range_to_range(pat.range()),
            },
          )?;
        }
      },
      Pat::Rest(p) => {
        if p.type_ann.is_none() {
          self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
            range: self.source_range_to_range(p.range()),
          })?;
        }
      }
      Pat::Array(p) => {
        if p.type_ann.is_none() {
          self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
            range: self.source_range_to_range(p.range()),
          })?;
        }
        p.elems.clear();
      }
      Pat::Object(p) => {
        if p.type_ann.is_none() {
          self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
            range: self.source_range_to_range(p.range()),
          })?;
        }
        p.props.clear();
      }
      Pat::Invalid(_) | Pat::Expr(_) => {
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedDestructuring {
            range: self.source_range_to_range(pat.range()),
          },
        )?;
      }
    }
    Ok(())
  }

  fn transform_var(
    &mut self,
    n: &mut VarDecl,
    is_ambient: bool,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    n.decls.retain(|n| self.public_ranges.contains(&n.range()));

    // don't need to do anything for ambient decls
    if !is_ambient {
      for decl in &mut n.decls {
        self.transform_var_declarator(decl)?;
      }
    }

    Ok(TransformItemResult::from_retain(!n.decls.is_empty()))
  }

  fn transform_var_declarator(
    &mut self,
    n: &mut VarDeclarator,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    match &mut n.name {
      Pat::Ident(ident) => {
        if ident.type_ann.is_none() {
          let inferred_type = n
            .init
            .as_ref()
            .and_then(|e| self.maybe_infer_type_from_expr(e));
          match inferred_type {
            Some(t) => {
              ident.type_ann = Some(Box::new(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::new(t),
              }));
              n.init = Some(obj_as_never_expr());
            }
            None => {
              let is_init_leavable = match n.init.as_mut() {
                Some(init) => self.maybe_transform_expr_if_leavable(
                  init,
                  Some(ident.id.range()),
                )?,
                None => false,
              };
              if !is_init_leavable {
                self.mark_diagnostic(
                  FastCheckDiagnostic::MissingExplicitType {
                    range: self.source_range_to_range(ident.range()),
                  },
                )?;
              }
            }
          }
        } else {
          n.init = Some(obj_as_never_expr());
        }
      }
      Pat::Array(_)
      | Pat::Rest(_)
      | Pat::Object(_)
      | Pat::Assign(_)
      | Pat::Invalid(_)
      | Pat::Expr(_) => {
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedDestructuring {
            range: self.source_range_to_range(n.name.range()),
          },
        )?;
      }
    }

    Ok(())
  }

  fn transform_ts_module(
    &mut self,
    n: &mut TsModuleDecl,
    public_range: &SourceRange,
    comments: &mut CommentsMut,
    is_ambient: bool,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    if n.global {
      self.mark_diagnostic(FastCheckDiagnostic::UnsupportedGlobalModule {
        range: self.source_range_to_range(n.range()),
      })?;
      return Ok(TransformItemResult::Remove);
    }

    match &n.id {
      TsModuleName::Ident(_) => {
        // ok
      }
      TsModuleName::Str(_) => {
        // not ok
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedAmbientModule {
            range: self.source_range_to_range(n.id.range()),
          },
        )?;
        return Ok(TransformItemResult::Remove);
      }
    }

    let ts_module_block = match &mut n.body {
      Some(body) => match body {
        TsNamespaceBody::TsModuleBlock(block) => block,
        TsNamespaceBody::TsNamespaceDecl(decl) => {
          let mut body = &mut *decl.body;
          loop {
            match body {
              TsNamespaceBody::TsModuleBlock(block) => {
                break block;
              }
              TsNamespaceBody::TsNamespaceDecl(decl) => {
                body = &mut decl.body;
              }
            }
          }
        }
      },
      None => {
        self.mark_diagnostic(
          FastCheckDiagnostic::UnsupportedAmbientModule {
            range: self.source_range_to_range(n.id.range()),
          },
        )?;
        return Ok(TransformItemResult::Remove);
      }
    };

    // allow the above diagnostics to error before checking for a public range
    if !self.public_ranges.contains(public_range) {
      return Ok(TransformItemResult::Remove);
    }

    let body = std::mem::take(&mut ts_module_block.body);
    ts_module_block.body =
      self.transform_module_body(body, comments, is_ambient)?;

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
    n: &mut AssignExpr,
  ) -> Result<TransformItemResult, Vec<FastCheckDiagnostic>> {
    if !self.public_ranges.contains(&n.range()) {
      return Ok(TransformItemResult::Remove);
    }

    // it will be an expando property at this point, but we still need n.right
    // to be mutable, so transform before getting the expando property
    let right_range = n.right.range();
    let is_init_leavable =
      self.maybe_transform_expr_if_leavable(&mut n.right, Some(right_range))?;
    let Some(expando_prop) = ExpandoPropertyRef::maybe_new(n) else {
      return Ok(TransformItemResult::Remove);
    };
    let mem_expr = expando_prop.member_expr();
    if !is_init_leavable {
      self.mark_diagnostic(FastCheckDiagnostic::MissingExplicitType {
        range: self.source_range_to_range(mem_expr.range()),
      })?;
    } else {
      let var_decls = self
        .expando_namespaces
        .entry(expando_prop.obj_ident().to_id())
        .or_default();
      var_decls.push(VarDeclarator {
        span: DUMMY_SP,
        name: Pat::Ident(BindingIdent {
          // this property name is guaranteed to be a valid identifier
          id: Ident::new(
            expando_prop.prop_name().clone(),
            expando_prop.prop_name_range().into(),
          ),
          type_ann: None,
        }),
        init: Some(n.right.clone()),
        definite: false,
      });
    }
    Ok(TransformItemResult::Remove)
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

  fn source_range_to_range(
    &self,
    range: SourceRange,
  ) -> FastCheckDiagnosticRange {
    FastCheckDiagnosticRange {
      specifier: self.specifier.clone(),
      text_info: self.parsed_source.text_info_lazy().clone(),
      range,
    }
  }

  fn maybe_transform_expr_if_leavable(
    &mut self,
    expr: &mut Expr,
    parent_id_range: Option<SourceRange>,
  ) -> Result<bool, Vec<FastCheckDiagnostic>> {
    let mut recurse =
      |expr: &mut Expr| self.maybe_transform_expr_if_leavable(expr, None);

    let is_leavable = match expr {
      Expr::This(_) => true,
      Expr::Array(n) => {
        let mut is_leavable = true;
        for elem in &mut n.elems {
          is_leavable = match elem {
            Some(elem) => recurse(&mut elem.expr)?,
            None => true,
          };
          if !is_leavable {
            break
          }
        }
        is_leavable
    },
      Expr::Object(n) =>  {
        let mut is_leavable = true;
        for prop in &mut n.props {
          is_leavable = match prop {
            PropOrSpread::Prop(prop) => match &mut **prop {
              Prop::Shorthand(_) => true,
              Prop::KeyValue(prop) => match &mut prop.key {
                PropName::Ident(_) => recurse(&mut prop.value)?,
                PropName::Str(_) => recurse(&mut prop.value)?,
                PropName::Num(_) => recurse(&mut prop.value)?,
                PropName::Computed(c) => {
                  recurse(&mut c.expr)? && recurse(&mut prop.value)?
                }
                PropName::BigInt(_) => recurse(&mut prop.value)?,
              },
              Prop::Assign(n) => recurse(&mut n.value)?,
              Prop::Getter(_) | Prop::Setter(_) | Prop::Method(_) => false,
            },
            PropOrSpread::Spread(n) => {
              recurse(&mut n.expr)?
            },
          };
          if !is_leavable {
            break
          }
        }
        is_leavable
      }
      Expr::Unary(n) => recurse(&mut n.arg)?,
      Expr::Update(n) => recurse(&mut n.arg)?,
      Expr::Bin(n) => recurse(&mut n.left)? && recurse(&mut n.right)?,
      Expr::Assign(_) | Expr::SuperProp(_) => false,
      Expr::Cond(n) => {
        recurse(&mut n.test)?
          && recurse(&mut n.cons)?
          && recurse(&mut n.alt)?
      }
      Expr::Member(n) => {
        recurse(&mut n.obj)? && match &mut n.prop {
          MemberProp::Ident(_) => true,
          MemberProp::PrivateName(_) => false,
          MemberProp::Computed(n) => {
            self.maybe_transform_expr_if_leavable(&mut n.expr, None)?
          }
        }
      }
      Expr::Ident(_) => true,
      Expr::Call(_) | Expr::New(_) | Expr::Seq(_)  => false,
      Expr::Lit(n) => match n {
        Lit::Str(_)
        | Lit::Bool(_)
        | Lit::Null(_)
        | Lit::Num(_)
        | Lit::BigInt(_)
        | Lit::Regex(_) => true,
        Lit::JSXText(_) => false,
      },
      Expr::Await(n) => recurse(&mut n.arg)?,
      Expr::Paren(n) => recurse(&mut n.expr)?,
      Expr::TsTypeAssertion(assertion) => {
        assertion.expr = paren_expr(obj_as_never_expr());
        true
      }
      Expr::TsAs(assertion) => {
        assertion.expr = paren_expr(obj_as_never_expr());
        true
      }
      Expr::TsConstAssertion(n) => recurse(&mut n.expr)?,
      Expr::TsNonNull(n) => recurse(&mut n.expr)?,
      Expr::Fn(n) => {
        self.transform_fn(&mut n.function, parent_id_range, FunctionKind::ExpressionLike, /* is overload */ false, /* is ambient */ false)?;
        true
      }
      Expr::Arrow(n) => {
        self.transform_arrow(n, parent_id_range)?;
        true
      }
      Expr::Tpl(_)
      | Expr::TaggedTpl(_)
      | Expr::Class(_)
      | Expr::Yield(_)
      | Expr::MetaProp(_)
      | Expr::JSXMember(_)
      | Expr::JSXNamespacedName(_)
      | Expr::JSXEmpty(_)
      | Expr::JSXElement(_)
      | Expr::JSXFragment(_)
      | Expr::TsInstantiation(_)
      | Expr::TsSatisfies(_)
      | Expr::PrivateName(_)
      // todo(dsherret): probably could analyze this more
      | Expr::OptChain(_)
      | Expr::Invalid(_) => false,
    };
    Ok(is_leavable)
  }

  fn maybe_infer_type_from_expr(&self, expr: &Expr) -> Option<TsType> {
    match expr {
      Expr::TsTypeAssertion(n) => infer_simple_type_from_type(&n.type_ann),
      Expr::TsAs(n) => infer_simple_type_from_type(&n.type_ann),
      Expr::Lit(lit) => maybe_lit_to_ts_type(lit),
      Expr::Call(call_expr) => {
        if self.is_call_expr_symbol_create(call_expr) {
          Some(TsType::TsTypeOperator(TsTypeOperator {
            span: DUMMY_SP,
            op: TsTypeOperatorOp::Unique,
            type_ann: Box::new(TsType::TsTypeRef(TsTypeRef {
              span: DUMMY_SP,
              type_name: TsEntityName::Ident(ident("symbol".to_string())),
              type_params: None,
            })),
          }))
        } else {
          None
        }
      }
      Expr::Paren(n) => self.maybe_infer_type_from_expr(&n.expr),
      Expr::This(_)
      | Expr::Array(_)
      | Expr::Object(_)
      | Expr::Fn(_)
      | Expr::Unary(_)
      | Expr::Update(_)
      | Expr::Bin(_)
      | Expr::Assign(_)
      | Expr::Member(_)
      | Expr::SuperProp(_)
      | Expr::Cond(_)
      | Expr::New(_)
      | Expr::Seq(_)
      | Expr::Ident(_)
      | Expr::Tpl(_)
      | Expr::TaggedTpl(_)
      | Expr::Arrow(_)
      | Expr::Class(_)
      | Expr::Yield(_)
      | Expr::MetaProp(_)
      | Expr::Await(_)
      | Expr::JSXMember(_)
      | Expr::JSXNamespacedName(_)
      | Expr::JSXEmpty(_)
      | Expr::JSXElement(_)
      | Expr::JSXFragment(_)
      | Expr::TsConstAssertion(_)
      | Expr::TsNonNull(_)
      | Expr::TsInstantiation(_)
      | Expr::TsSatisfies(_)
      | Expr::PrivateName(_)
      | Expr::OptChain(_)
      | Expr::Invalid(_) => None,
    }
  }

  /// Looks if the call expr is `Symbol("example")` or `Symbol.for("example")`
  fn is_call_expr_symbol_create(&self, call_expr: &CallExpr) -> bool {
    let Some(expr) = call_expr.callee.as_expr() else {
      return false;
    };
    let expr_ident = match &**expr {
      Expr::Ident(ident) => ident,
      Expr::Member(member_expr) => {
        let Some(ident) = member_expr.obj.as_ident() else {
          return false;
        };
        let Some(prop_ident) = member_expr.prop.as_ident() else {
          return false;
        };
        if prop_ident.sym != "for" {
          return false;
        }
        ident
      }
      _ => return false,
    };

    let is_symbol_global = expr_ident.sym == "Symbol"
      && expr_ident.to_id().1 == self.parsed_source.unresolved_context();
    if !is_symbol_global || call_expr.args.len() != 1 {
      return false;
    }
    let Some(arg_lit) = call_expr.args.first().and_then(|a| a.expr.as_lit())
    else {
      return false;
    };
    matches!(arg_lit, Lit::Str(_))
  }

  fn promise_wrap_type(&self, ty: Box<TsType>) -> Box<TsType> {
    match ty.as_ref() {
      TsType::TsTypeRef(TsTypeRef {
        type_name: TsEntityName::Ident(ident),
        type_params: Some(type_params),
        ..
      }) if type_params.params.len() == 1
        && ident.sym == "Promise"
        && ident.to_id().1 == self.parsed_source.unresolved_context() =>
      {
        ty
      }
      _ => Box::new(TsType::TsTypeRef(TsTypeRef {
        span: DUMMY_SP,
        type_name: TsEntityName::Ident(ident("Promise".into())),
        type_params: Some(Box::new(TsTypeParamInstantiation {
          span: DUMMY_SP,
          params: vec![ty],
        })),
      })),
    }
  }
}

fn is_ts_private_computed_class_member(m: &ClassMember) -> bool {
  match m {
    ClassMember::Method(m) => {
      if m.accessibility == Some(Accessibility::Private) {
        is_computed_prop_name(&m.key)
      } else {
        false
      }
    }
    ClassMember::AutoAccessor(m) => {
      if m.accessibility == Some(Accessibility::Private) {
        match &m.key {
          Key::Private(_) => false,
          Key::Public(k) => is_computed_prop_name(k),
        }
      } else {
        false
      }
    }
    ClassMember::ClassProp(p) => {
      if p.accessibility == Some(Accessibility::Private) {
        is_computed_prop_name(&p.key)
      } else {
        false
      }
    }
    _ => false,
  }
}

fn is_computed_prop_name(prop_name: &PropName) -> bool {
  match prop_name {
    PropName::Ident(_)
    | PropName::Str(_)
    | PropName::Num(_)
    | PropName::BigInt(_) => false,
    PropName::Computed(_) => true,
  }
}

fn void_or_promise_void(is_async: bool) -> Box<TsType> {
  let void_type = Box::new(ts_keyword_type(TsKeywordTypeKind::TsVoidKeyword));
  if is_async {
    Box::new(TsType::TsTypeRef(TsTypeRef {
      span: DUMMY_SP,
      type_name: TsEntityName::Ident(Ident::new("Promise".into(), DUMMY_SP)),
      type_params: Some(Box::new(TsTypeParamInstantiation {
        span: DUMMY_SP,
        params: vec![void_type],
      })),
    }))
  } else {
    void_type
  }
}

fn replacement_return_value(ty: &TsType) -> Option<Box<Expr>> {
  if is_void_type(ty) {
    None
  } else {
    Some(obj_as_never_expr())
  }
}

// Converts `ident?: string` to `ident: string | undefined`
fn convert_optional_ident_to_nullable_type(ident: &mut BindingIdent) {
  ident.optional = false;
  if let Some(type_ann) = ident.type_ann.take() {
    ident.type_ann = Some(Box::new(TsTypeAnn {
      span: type_ann.span,
      type_ann: Box::new(TsType::TsUnionOrIntersectionType(
        TsUnionOrIntersectionType::TsUnionType(TsUnionType {
          span: DUMMY_SP,
          types: vec![
            type_ann.type_ann,
            Box::new(ts_keyword_type(TsKeywordTypeKind::TsUndefinedKeyword)),
          ],
        }),
      )),
    }));
  }
}

fn infer_simple_type_from_type(t: &TsType) -> Option<TsType> {
  match t {
    TsType::TsKeywordType(_) => Some(t.clone()),
    TsType::TsThisType(_) => Some(t.clone()),
    TsType::TsFnOrConstructorType(_) => None,
    TsType::TsTypeRef(type_ref) => Some(TsType::TsTypeRef(TsTypeRef {
      span: type_ref.span,
      type_name: type_ref.type_name.clone(),
      type_params: match type_ref.type_params.as_deref() {
        Some(type_params) => Some(Box::new(TsTypeParamInstantiation {
          span: type_params.span,
          params: {
            let mut params = Vec::with_capacity(type_params.params.len());
            for param in &type_params.params {
              params.push(Box::new(infer_simple_type_from_type(param)?));
            }
            params
          },
        })),
        None => None,
      },
    })),
    TsType::TsTypeQuery(_) => None,
    TsType::TsTypeLit(t) => Some(TsType::TsTypeLit(t.clone())),
    TsType::TsTupleType(t) => {
      let mut elems = Vec::with_capacity(t.elem_types.len());
      for elem_type in &t.elem_types {
        let inferred_type = infer_simple_type_from_type(&elem_type.ty)?;
        elems.push(TsTupleElement {
          span: elem_type.span,
          label: elem_type.label.clone(),
          ty: Box::new(inferred_type),
        });
      }
      Some(TsType::TsTupleType(TsTupleType {
        span: t.span(),
        elem_types: elems,
      }))
    }
    TsType::TsArrayType(t) => {
      infer_simple_type_from_type(&t.elem_type).map(|inner| {
        TsType::TsArrayType(TsArrayType {
          span: t.span(),
          elem_type: Box::new(inner),
        })
      })
    }
    TsType::TsOptionalType(t) => {
      infer_simple_type_from_type(&t.type_ann).map(|inner| {
        TsType::TsOptionalType(TsOptionalType {
          span: t.span(),
          type_ann: Box::new(inner),
        })
      })
    }
    TsType::TsRestType(t) => {
      infer_simple_type_from_type(&t.type_ann).map(|inner| {
        TsType::TsRestType(TsRestType {
          span: t.span(),
          type_ann: Box::new(inner),
        })
      })
    }
    TsType::TsUnionOrIntersectionType(t) => match t {
      TsUnionOrIntersectionType::TsUnionType(t) => {
        let mut types = Vec::with_capacity(t.types.len());
        for ty in &t.types {
          let inferred_type = infer_simple_type_from_type(ty)?;
          types.push(Box::new(inferred_type));
        }
        Some(TsType::TsUnionOrIntersectionType(
          TsUnionOrIntersectionType::TsUnionType(TsUnionType {
            span: t.span(),
            types,
          }),
        ))
      }
      TsUnionOrIntersectionType::TsIntersectionType(t) => {
        let mut types = Vec::with_capacity(t.types.len());
        for ty in &t.types {
          let inferred_type = infer_simple_type_from_type(ty)?;
          types.push(Box::new(inferred_type));
        }
        Some(TsType::TsUnionOrIntersectionType(
          TsUnionOrIntersectionType::TsIntersectionType(TsIntersectionType {
            span: t.span(),
            types,
          }),
        ))
      }
    },
    TsType::TsConditionalType(_) => None,
    TsType::TsInferType(_) => None,
    TsType::TsParenthesizedType(t) => infer_simple_type_from_type(&t.type_ann)
      .map(|inner| {
        TsType::TsParenthesizedType(TsParenthesizedType {
          span: t.span(),
          type_ann: Box::new(inner),
        })
      }),
    TsType::TsTypeOperator(t) => {
      infer_simple_type_from_type(&t.type_ann).map(|inner| {
        TsType::TsTypeOperator(TsTypeOperator {
          span: t.span(),
          op: t.op,
          type_ann: Box::new(inner),
        })
      })
    }
    TsType::TsIndexedAccessType(_) => None,
    TsType::TsMappedType(_) => None,
    TsType::TsLitType(t) => match &t.lit {
      TsLit::Number(_) | TsLit::Str(_) | TsLit::Bool(_) | TsLit::BigInt(_) => {
        Some(TsType::TsLitType(t.clone()))
      }
      TsLit::Tpl(_) => None,
    },
    TsType::TsTypePredicate(_) | TsType::TsImportType(_) => None,
  }
}

/// Holds when the first optional parameter occurs.
struct ParamsOptionalStartIndex(Option<usize>);

impl ParamsOptionalStartIndex {
  pub fn build<'a>(param_pats: impl Iterator<Item = &'a Pat>) -> Self {
    fn is_param_pat_optional(pat: &Pat) -> bool {
      match pat {
        Pat::Ident(ident) => ident.optional,
        Pat::Array(a) => a.optional,
        Pat::Object(o) => o.optional,
        Pat::Assign(_) | Pat::Rest(_) => true,
        Pat::Invalid(_) | Pat::Expr(_) => false,
      }
    }

    let mut optional_start_index = None;
    for (i, pat) in param_pats.enumerate() {
      if is_param_pat_optional(pat) {
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

fn is_expr_ident_or_member_idents(expr: &Expr) -> bool {
  match expr {
    Expr::Ident(_) => true,
    Expr::Member(n) => {
      is_expr_ident_or_member_idents(&n.obj)
        && match &n.prop {
          MemberProp::Ident(_) => true,
          MemberProp::PrivateName(_) => false,
          MemberProp::Computed(p) => is_expr_ident_or_member_idents(&p.expr),
        }
    }
    _ => false,
  }
}

fn array_as_never_expr() -> Box<Expr> {
  expr_as_keyword_expr(
    empty_array_lit_expr(),
    TsKeywordTypeKind::TsNeverKeyword,
  )
}

fn obj_as_never_expr() -> Box<Expr> {
  expr_as_keyword_expr(empty_obj_lit_expr(), TsKeywordTypeKind::TsNeverKeyword)
}

fn array_as_never_array_expr() -> Box<Expr> {
  as_expr(
    empty_array_lit_expr(),
    Box::new(TsType::TsArrayType(TsArrayType {
      span: DUMMY_SP,
      elem_type: Box::new(ts_keyword_type(TsKeywordTypeKind::TsNeverKeyword)),
    })),
  )
}

fn empty_array_lit_expr() -> Box<Expr> {
  Box::new(Expr::Array(ArrayLit {
    span: DUMMY_SP,
    elems: Default::default(),
  }))
}

fn empty_obj_lit_expr() -> Box<Expr> {
  Box::new(Expr::Object(ObjectLit {
    span: DUMMY_SP,
    props: Default::default(),
  }))
}

fn expr_as_keyword_expr(
  expr: Box<Expr>,
  keyword: TsKeywordTypeKind,
) -> Box<Expr> {
  as_expr(expr, keyword_type(keyword))
}

fn keyword_type(keyword: TsKeywordTypeKind) -> Box<TsType> {
  Box::new(TsType::TsKeywordType(TsKeywordType {
    span: DUMMY_SP,
    kind: keyword,
  }))
}

fn as_expr(expr: Box<Expr>, type_ann: Box<TsType>) -> Box<Expr> {
  Box::new(Expr::TsAs(TsAsExpr {
    span: DUMMY_SP,
    expr,
    type_ann,
  }))
}

fn paren_expr(expr: Box<Expr>) -> Box<Expr> {
  Box::new(Expr::Paren(ParenExpr {
    span: DUMMY_SP,
    expr,
  }))
}
