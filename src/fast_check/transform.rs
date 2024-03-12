// Copyright 2018-2024 the Deno authors. MIT license.

// for span methods, which actually make sense to use here in the transforms
#![allow(clippy::disallowed_methods)]
#![allow(clippy::disallowed_types)]

use std::collections::HashSet;
use std::sync::Arc;

use deno_ast::swc::ast::*;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::common::comments::SingleThreadedComments;
use deno_ast::swc::common::comments::SingleThreadedCommentsMapInner;
use deno_ast::swc::common::Spanned;
use deno_ast::swc::common::DUMMY_SP;
use deno_ast::ModuleSpecifier;
use deno_ast::MultiThreadedComments;
use deno_ast::ParsedSource;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;

use crate::DefaultModuleAnalyzer;
use crate::ModuleGraph;
use crate::ModuleInfo;
use crate::WorkspaceMember;

use super::range_finder::ModulePublicRanges;
use super::swc_helpers::any_type_ann;
use super::swc_helpers::emit;
use super::swc_helpers::get_return_stmts_with_arg_from_function_body;
use super::swc_helpers::ident;
use super::swc_helpers::is_never_type;
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
  pub text: String,
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
  specifier: &ModuleSpecifier,
  public_ranges: &ModulePublicRanges,
  parsed_source: &ParsedSource,
  options: &TransformOptions,
) -> Result<FastCheckModule, Vec<FastCheckDiagnostic>> {
  let mut transformer = FastCheckTransformer::new(
    graph,
    specifier,
    public_ranges,
    parsed_source,
    options.should_error_on_first_diagnostic,
  );
  let (module, comments) = transformer.transform()?;
  if !transformer.diagnostics.is_empty() {
    return Err(transformer.diagnostics);
  }
  let module_info = DefaultModuleAnalyzer::module_info_from_swc(
    parsed_source.media_type(),
    &module,
    parsed_source.text_info(),
    &comments,
  );

  // swc will modify the comment collection internally when emitting,
  // so if we're emitting with dts, make a copy of the comments for
  // each emit
  let (fast_check_comments, dts_comments) = if options.dts {
    (
      comments.as_single_threaded(),
      Some(comments.into_single_threaded()),
    )
  } else {
    (comments.into_single_threaded(), None)
  };

  // now emit
  let (text, source_map) = emit(
    specifier,
    &fast_check_comments,
    parsed_source.text_info(),
    &module,
  )
  .map_err(|e| {
    vec![FastCheckDiagnostic::Emit {
      specifier: specifier.clone(),
      inner: Arc::new(e),
    }]
  })?;

  let dts = if let Some(dts_comments) = dts_comments {
    let mut dts_transformer =
      FastCheckDtsTransformer::new(parsed_source.text_info(), specifier);

    let module = dts_transformer.transform(module)?;
    let (text, _source_map) =
      emit(specifier, &dts_comments, parsed_source.text_info(), &module)
        .map_err(|e| {
          vec![FastCheckDiagnostic::Emit {
            specifier: specifier.clone(),
            inner: Arc::new(e),
          }]
        })?;

    Some(FastCheckDtsModule {
      text,
      diagnostics: dts_transformer.diagnostics,
    })
  } else {
    None
  };

  Ok(FastCheckModule {
    module_info: module_info.into(),
    text: text.into(),
    dts,
    source_map: source_map.into(),
  })
}

struct FastCheckTransformer<'a> {
  graph: &'a ModuleGraph,
  specifier: &'a ModuleSpecifier,
  public_ranges: &'a ModulePublicRanges,
  parsed_source: &'a ParsedSource,
  should_error_on_first_diagnostic: bool,
  diagnostics: Vec<FastCheckDiagnostic>,
}

impl<'a> FastCheckTransformer<'a> {
  pub fn new(
    graph: &'a ModuleGraph,
    specifier: &'a ModuleSpecifier,
    public_ranges: &'a ModulePublicRanges,
    parsed_source: &'a ParsedSource,
    should_error_on_first_diagnostic: bool,
  ) -> Self {
    Self {
      graph,
      specifier,
      public_ranges,
      parsed_source,
      should_error_on_first_diagnostic,
      diagnostics: Default::default(),
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
    let mut final_body = Vec::with_capacity(body.len());
    for mut item in body {
      let retain = self.transform_item(&mut item, comments, is_ambient)?;
      if retain {
        final_body.push(item);
      } else {
        comments.remove_leading(item.start());
      }
    }
    Ok(final_body)
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
  ) -> Result<bool, Vec<FastCheckDiagnostic>> {
    match item {
      ModuleItem::ModuleDecl(decl) => match decl {
        ModuleDecl::Import(n) => {
          n.specifiers
            .retain(|s| self.public_ranges.contains(&s.range()));
          let retain = !n.specifiers.is_empty();
          if retain {
            self.transform_module_specifier(&mut n.src);
          }
          Ok(retain)
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
          Ok(retain)
        }
        ModuleDecl::ExportAll(n) => {
          let retain = self.public_ranges.contains(&n.range());
          if retain {
            self.transform_module_specifier(&mut n.src);
          }
          Ok(retain)
        }
        ModuleDecl::ExportDefaultExpr(n) => {
          // todo: investigate why both these checks are needed
          if !self.public_ranges.contains(&n.range())
            && !self.public_ranges.contains(&n.expr.range())
          {
            return Ok(false);
          }

          if self.maybe_transform_expr_if_leavable(&mut n.expr, None)?
            || is_expr_ident_or_member_idents(&n.expr)
          {
            Ok(true)
          } else {
            self.mark_diagnostic(
              FastCheckDiagnostic::UnsupportedDefaultExportExpr {
                range: self.source_range_to_range(n.range()),
              },
            )?;
            Ok(false)
          }
        }
        ModuleDecl::ExportDefaultDecl(n) => {
          if !self.public_ranges.contains(&n.range()) {
            return Ok(false);
          }

          let node_range = n.range();
          self.transform_default_decl(
            &mut n.decl,
            comments,
            node_range,
            is_ambient,
          )?;
          Ok(true)
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
          TsModuleRef::TsEntityName(n) => {
            Ok(self.public_ranges.contains(&n.range()))
          }
          TsModuleRef::TsExternalModuleRef(_) => {
            self.mark_diagnostic(FastCheckDiagnostic::UnsupportedRequire {
              range: self.source_range_to_range(n.range()),
            })?;
            Ok(false)
          }
        },
        ModuleDecl::TsExportAssignment(n) => {
          self.mark_diagnostic(
            FastCheckDiagnostic::UnsupportedTsExportAssignment {
              range: self.source_range_to_range(n.range()),
            },
          )?;
          Ok(false)
        }
        ModuleDecl::TsNamespaceExport(n) => {
          self.mark_diagnostic(
            FastCheckDiagnostic::UnsupportedTsNamespaceExport {
              range: self.source_range_to_range(n.range()),
            },
          )?;
          Ok(false)
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
        | Stmt::Expr(_) => Ok(false),
        Stmt::Decl(n) => self.transform_decl(n, comments, None, is_ambient),
      },
    }
  }

  fn transform_default_decl(
    &mut self,
    default_decl: &mut DefaultDecl,
    comments: &mut CommentsMut,
    parent_range: SourceRange,
    is_ambient: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    match default_decl {
      DefaultDecl::Class(n) => self.transform_class(
        &mut n.class,
        comments,
        /* has declare keyword */ false,
      ),
      DefaultDecl::Fn(n) => self.transform_fn(
        &mut n.function,
        n.ident.as_ref().map(|i| i.range()),
        self.public_ranges.is_impl_with_overloads(&parent_range),
        /* is setter */ false,
        is_ambient,
      ),
      DefaultDecl::TsInterfaceDecl(_) => Ok(()),
    }
  }

  fn transform_decl(
    &mut self,
    decl: &mut Decl,
    comments: &mut CommentsMut,
    parent_range: Option<SourceRange>,
    is_ambient: bool,
  ) -> Result<bool, Vec<FastCheckDiagnostic>> {
    let public_range = parent_range.unwrap_or_else(|| decl.range());
    match decl {
      Decl::Class(n) => {
        if !self.public_ranges.contains(&public_range) {
          return Ok(false);
        }
        self.transform_class(
          &mut n.class,
          comments,
          is_ambient || n.declare,
        )?;
        Ok(true)
      }
      Decl::Fn(n) => {
        if !self.public_ranges.contains(&public_range) {
          return Ok(false);
        }
        let is_overload =
          self.public_ranges.is_impl_with_overloads(&public_range);
        self.transform_fn(
          &mut n.function,
          Some(n.ident.range()),
          is_overload,
          /* is setter */ false,
          is_ambient,
        )?;
        Ok(true)
      }
      Decl::Var(n) => self.transform_var(n, is_ambient),
      Decl::TsInterface(_) => Ok(self.public_ranges.contains(&public_range)),
      Decl::TsTypeAlias(_) => Ok(self.public_ranges.contains(&public_range)),
      Decl::TsEnum(_) => Ok(self.public_ranges.contains(&public_range)),
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
        Ok(false)
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
                    Box::new(paren_expr(obj_as_any_expr()))
                  } else {
                    obj_as_any_expr()
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
                // delcare is not valid with override
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

        for param in &mut n.params {
          match param {
            ParamOrTsParamProp::Param(param) => {
              self.handle_param_pat(&mut param.pat)?;
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
          is_overload,
          /* is setter */ n.kind == MethodKind::Setter,
          is_ambient,
        )?;
        Ok(true)
      }
      ClassMember::ClassProp(n) => {
        if n.accessibility == Some(Accessibility::Private) {
          n.type_ann = Some(any_type_ann());
          n.declare = !n.is_override;
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
            .and_then(|e| self.maybe_infer_type_from_expr(&*e));
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
    is_overload: bool,
    is_set_accessor: bool,
    is_ambient: bool,
  ) -> Result<(), Vec<FastCheckDiagnostic>> {
    if is_ambient {
      return Ok(()); // no need to do anything
    }
    if is_overload {
      for (i, param) in n.params.iter_mut().enumerate() {
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
      n.return_type = Some(any_type_ann());
    }
    if is_set_accessor {
      // suppress any unused param errors
      for param in n.params.iter_mut() {
        prefix_idents_in_pat(&mut param.pat, "_");
      }
    }

    if !is_set_accessor && n.return_type.is_none() {
      let range = parent_id_range.unwrap_or_else(|| n.range());
      if n.is_generator {
        self.mark_diagnostic(
          FastCheckDiagnostic::MissingExplicitReturnType {
            range: self.source_range_to_range(range),
          },
        )?;
      }

      if let Some(body) = &mut n.body {
        let return_stmts = get_return_stmts_with_arg_from_function_body(body);
        if return_stmts.is_empty() {
          n.return_type = Some(Box::new(TsTypeAnn {
            span: DUMMY_SP,
            type_ann: void_or_promise_void(n.is_async),
          }));
        } else {
          self.mark_diagnostic(
            FastCheckDiagnostic::MissingExplicitReturnType {
              range: self.source_range_to_range(range),
            },
          )?;
        }
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

    for param in &mut n.params {
      self.handle_param_pat(&mut param.pat)?;
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
          let return_stmts = get_return_stmts_with_arg_from_function_body(body);
          if return_stmts.is_empty() {
            n.return_type = Some(Box::new(TsTypeAnn {
              span: DUMMY_SP,
              type_ann: void_or_promise_void(n.is_async),
            }));
          } else {
            self.mark_diagnostic(
              FastCheckDiagnostic::MissingExplicitReturnType {
                range: self.source_range_to_range(range),
              },
            )?;
          }
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
        .map(|e| BlockStmtOrExpr::Expr(Box::new(paren_expr(e))))
        .unwrap_or_else(|| {
          BlockStmtOrExpr::BlockStmt(BlockStmt {
            span: DUMMY_SP,
            stmts: vec![],
          })
        });

      // Only reset is_async if there is an explicit return type
      n.is_async = false;
    }

    for pat in &mut n.params {
      self.handle_param_pat(pat)?;
    }

    Ok(())
  }

  fn handle_param_pat(
    &mut self,
    pat: &mut Pat,
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
                ident.id.optional = true;
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
            ident.id.optional = true;
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
            assign.right = array_as_any_expr();
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
            assign.right = obj_as_any_expr();
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
  ) -> Result<bool, Vec<FastCheckDiagnostic>> {
    n.decls.retain(|n| self.public_ranges.contains(&n.range()));

    // don't need to do anything for these in a declaration file
    if !is_ambient {
      for decl in &mut n.decls {
        self.transform_var_declarator(decl)?;
      }
    }

    Ok(!n.decls.is_empty())
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
              n.init = Some(obj_as_any_expr());
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
          n.init = Some(obj_as_any_expr());
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
  ) -> Result<bool, Vec<FastCheckDiagnostic>> {
    if n.global {
      self.mark_diagnostic(FastCheckDiagnostic::UnsupportedGlobalModule {
        range: self.source_range_to_range(n.range()),
      })?;
      return Ok(false);
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
        return Ok(false);
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
        return Ok(false);
      }
    };

    // allow the above diagnostics to error before checking for a public range
    if !self.public_ranges.contains(public_range) {
      return Ok(false);
    }

    let body = std::mem::take(&mut ts_module_block.body);
    ts_module_block.body =
      self.transform_module_body(body, comments, is_ambient)?;

    Ok(true)
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
      text_info: self.parsed_source.text_info().clone(),
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
      Expr::TsTypeAssertion(_) | Expr::TsAs(_) => false,
      Expr::TsConstAssertion(n) => recurse(&mut n.expr)?,
      Expr::TsNonNull(n) => recurse(&mut n.expr)?,
      Expr::Fn(n) => {
        self.transform_fn(&mut n.function, parent_id_range, /* is overload */ false, /* is setter */ false, /* is ambient */ false)?;
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
          Key::Public(k) => is_computed_prop_name(&k),
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
  } else if is_never_type(ty) {
    Some(obj_as_never_expr())
  } else {
    Some(obj_as_any_expr())
  }
}

fn prefix_ident(ident: &mut Ident, prefix: &str) {
  ident.sym = format!("{}{}", prefix, ident.sym).into();
}

fn prefix_idents_in_pat(pat: &mut Pat, prefix: &str) {
  match pat {
    Pat::Ident(ident) => {
      prefix_ident(&mut ident.id, prefix);
    }
    Pat::Array(array) => {
      for pat in array.elems.iter_mut().flatten() {
        prefix_idents_in_pat(pat, prefix);
      }
    }
    Pat::Rest(rest) => {
      prefix_idents_in_pat(&mut rest.arg, prefix);
    }
    Pat::Object(o) => {
      for prop in &mut o.props {
        match prop {
          ObjectPatProp::KeyValue(n) => match &mut n.key {
            PropName::Ident(ident) => {
              prefix_ident(ident, prefix);
            }
            PropName::Str(str) => {
              str.value = format!("{}{}", prefix, str.value).into();
            }
            PropName::Num(_) | PropName::Computed(_) | PropName::BigInt(_) => {
              // ignore
            }
          },
          ObjectPatProp::Assign(n) => prefix_ident(&mut n.key, prefix),
          ObjectPatProp::Rest(n) => prefix_idents_in_pat(&mut n.arg, prefix),
        }
      }
    }
    Pat::Assign(a) => prefix_idents_in_pat(&mut a.left, prefix),
    Pat::Expr(_) | Pat::Invalid(_) => {
      // ignore
    }
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

fn array_as_any_expr() -> Box<Expr> {
  expr_as_keyword_expr(
    Expr::Array(ArrayLit {
      span: DUMMY_SP,
      elems: Default::default(),
    }),
    TsKeywordTypeKind::TsAnyKeyword,
  )
}

fn obj_as_any_expr() -> Box<Expr> {
  expr_as_keyword_expr(
    Expr::Object(ObjectLit {
      span: DUMMY_SP,
      props: Default::default(),
    }),
    TsKeywordTypeKind::TsAnyKeyword,
  )
}

fn obj_as_never_expr() -> Box<Expr> {
  expr_as_keyword_expr(
    Expr::Object(ObjectLit {
      span: DUMMY_SP,
      props: Default::default(),
    }),
    TsKeywordTypeKind::TsNeverKeyword,
  )
}

fn expr_as_keyword_expr(expr: Expr, keyword: TsKeywordTypeKind) -> Box<Expr> {
  Box::new(Expr::TsAs(TsAsExpr {
    span: DUMMY_SP,
    expr: Box::new(expr),
    type_ann: Box::new(TsType::TsKeywordType(TsKeywordType {
      span: DUMMY_SP,
      kind: keyword,
    })),
  }))
}

fn paren_expr(expr: Box<Expr>) -> Expr {
  Expr::Paren(ParenExpr {
    span: DUMMY_SP,
    expr,
  })
}
