use std::collections::HashSet;
use std::rc::Rc;

use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::ast::BindingIdent;
use deno_ast::swc::ast::Class;
use deno_ast::swc::ast::ClassMember;
use deno_ast::swc::ast::ClassProp;
use deno_ast::swc::ast::Decl;
use deno_ast::swc::ast::DefaultDecl;
use deno_ast::swc::ast::ExportDecl;
use deno_ast::swc::ast::Expr;
use deno_ast::swc::ast::Function;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::Lit;
use deno_ast::swc::ast::ModuleDecl;
use deno_ast::swc::ast::ModuleItem;
use deno_ast::swc::ast::ObjectLit;
use deno_ast::swc::ast::Param;
use deno_ast::swc::ast::ParamOrTsParamProp;
use deno_ast::swc::ast::Pat;
use deno_ast::swc::ast::PrivateName;
use deno_ast::swc::ast::PrivateProp;
use deno_ast::swc::ast::PropName;
use deno_ast::swc::ast::ReturnStmt;
use deno_ast::swc::ast::Stmt;
use deno_ast::swc::ast::TsArrayType;
use deno_ast::swc::ast::TsAsExpr;
use deno_ast::swc::ast::TsEntityName;
use deno_ast::swc::ast::TsIntersectionType;
use deno_ast::swc::ast::TsKeywordType;
use deno_ast::swc::ast::TsKeywordTypeKind;
use deno_ast::swc::ast::TsLit;
use deno_ast::swc::ast::TsOptionalType;
use deno_ast::swc::ast::TsParamPropParam;
use deno_ast::swc::ast::TsParenthesizedType;
use deno_ast::swc::ast::TsRestType;
use deno_ast::swc::ast::TsTupleElement;
use deno_ast::swc::ast::TsTupleType;
use deno_ast::swc::ast::TsType;
use deno_ast::swc::ast::TsTypeAnn;
use deno_ast::swc::ast::TsTypeOperator;
use deno_ast::swc::ast::TsTypeParamInstantiation;
use deno_ast::swc::ast::TsTypeRef;
use deno_ast::swc::ast::TsUnionOrIntersectionType;
use deno_ast::swc::ast::TsUnionType;
use deno_ast::swc::ast::VarDecl;
use deno_ast::swc::codegen::text_writer::JsWriter;
use deno_ast::swc::codegen::Node;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::common::comments::SingleThreadedComments;
use deno_ast::swc::common::comments::SingleThreadedCommentsMapInner;
use deno_ast::swc::common::FileName;
use deno_ast::swc::common::SourceMap;
use deno_ast::swc::common::Spanned;
use deno_ast::swc::common::DUMMY_SP;
use deno_ast::swc_codegen_config;
use deno_ast::ModuleSpecifier;
use deno_ast::MultiThreadedComments;
use deno_ast::ParsedSource;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfo;

use crate::DefaultModuleAnalyzer;
use crate::ModuleInfo;

use super::range_finder::ModulePublicRanges;
use super::swc_helpers::get_return_stmts_with_arg_from_function;
use super::swc_helpers::ident;
use super::swc_helpers::ts_keyword_type;

struct CommentsMut {
  leading: SingleThreadedCommentsMapInner,
  trailing: SingleThreadedCommentsMapInner,
}

impl CommentsMut {
  pub fn new(single_threaded: SingleThreadedComments) -> Self {
    fn prune_comments(comments: &mut SingleThreadedCommentsMapInner) {
      comments.retain(|_key, value| {
        value.retain(|c| {
          match c.kind {
            // only keep js docs and @deno-types comments
            CommentKind::Line => c.text.starts_with("@deno-types"),
            CommentKind::Block => c.text.starts_with("*"),
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

#[derive(Debug, thiserror::Error)]
pub enum TransformError {
  #[error("Destructuring is not supported in the public API.")]
  UnsupportedDestructuring { range: SourceRange },
  #[error("Missing explicit type in the public API.")]
  MissingExplicitType { range: SourceRange },
  #[error("Missing explicit return type in the public API.")]
  MissingExplicitReturnType { range: SourceRange },
  #[error("Failed to emit low res module: {0:#}")]
  Emit(anyhow::Error),
}

pub struct LowResModule {
  pub specifier: ModuleSpecifier,
  pub module_info: ModuleInfo,
  pub text: String,
  pub source_map: String,
}

pub fn transform(
  specifier: &ModuleSpecifier,
  public_ranges: &ModulePublicRanges,
  parsed_source: &ParsedSource,
) -> Result<LowResModule, TransformError> {
  let mut module = parsed_source.module().clone();
  let mut comments =
    CommentsMut::new(parsed_source.comments().as_single_threaded());
  let mut final_body = Vec::with_capacity(module.body.len());
  for mut item in std::mem::take(&mut module.body) {
    let retain = transform_item(&mut item, &mut comments, public_ranges)?;
    if retain {
      final_body.push(item);
    } else {
      comments.remove_leading(item.start());
    }
  }
  module.body = final_body;
  let comments = comments.into_multi_threaded();
  let module_info = DefaultModuleAnalyzer::module_info_from_swc(
    parsed_source.media_type(),
    &module,
    parsed_source.text_info(),
    &comments,
  );

  // now emit
  let comments = comments.into_single_threaded();
  let (text, source_map) =
    emit(specifier, &comments, parsed_source.text_info(), &module)
      .map_err(TransformError::Emit)?;

  Ok(LowResModule {
    specifier: specifier.clone(),
    module_info,
    text,
    source_map,
  })
}

fn emit(
  specifier: &ModuleSpecifier,
  comments: &SingleThreadedComments,
  text_info: &SourceTextInfo,
  module: &deno_ast::swc::ast::Module,
) -> Result<(String, String), anyhow::Error> {
  let source_map = Rc::new(SourceMap::default());
  let file_name = FileName::Url(specifier.clone());
  source_map.new_source_file(file_name, text_info.text_str().to_string());

  let mut src_map_buf = vec![];
  let mut buf = vec![];
  {
    let mut writer = Box::new(JsWriter::new(
      source_map.clone(),
      "\n",
      &mut buf,
      Some(&mut src_map_buf),
    ));
    writer.set_indent_str("  "); // two spaces

    let mut emitter = deno_ast::swc::codegen::Emitter {
      cfg: swc_codegen_config(),
      comments: Some(comments),
      cm: source_map.clone(),
      wr: writer,
    };
    module.emit_with(&mut emitter)?;
  }
  let src = String::from_utf8(buf)?;
  let map = {
    let mut buf = Vec::new();
    source_map
      .build_source_map_from(&src_map_buf, None)
      .to_writer(&mut buf)?;
    String::from_utf8(buf)?
  };

  Ok((src, map))
}

fn transform_item(
  item: &mut ModuleItem,
  comments: &mut CommentsMut,
  public_ranges: &ModulePublicRanges,
) -> Result<bool, TransformError> {
  match item {
    ModuleItem::ModuleDecl(decl) => match decl {
      ModuleDecl::Import(n) => {
        n.specifiers.retain(|s| public_ranges.contains(&s.range()));
        Ok(!n.specifiers.is_empty())
      }
      ModuleDecl::ExportNamed(n) => {
        n.specifiers.retain(|s| public_ranges.contains(&s.range()));
        Ok(!n.specifiers.is_empty())
      }
      ModuleDecl::ExportAll(n) => Ok(public_ranges.contains(&n.range())),
      ModuleDecl::ExportDefaultExpr(n) => {
        if !public_ranges.contains(&n.range()) {
          return Ok(false);
        }

        todo!("Handle default export expr")
      }
      ModuleDecl::ExportDefaultDecl(n) => {
        if !public_ranges.contains(&n.range()) {
          return Ok(false);
        }

        let node_range = n.range();
        transform_default_decl(
          &mut n.decl,
          comments,
          public_ranges,
          node_range,
        )?;
        Ok(true)
      }
      ModuleDecl::ExportDecl(n) => {
        let export_decl_range = n.range();
        transform_decl(
          &mut n.decl,
          comments,
          public_ranges,
          Some(export_decl_range),
        )
      }
      ModuleDecl::TsImportEquals(_)
      | ModuleDecl::TsExportAssignment(_)
      | ModuleDecl::TsNamespaceExport(_) => {
        todo!("should exit as non-analyzable");
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
      Stmt::Decl(n) => transform_decl(n, comments, public_ranges, None),
    },
  }
}

fn transform_default_decl(
  default_decl: &mut DefaultDecl,
  comments: &mut CommentsMut,
  public_ranges: &ModulePublicRanges,
  parent_range: SourceRange,
) -> Result<(), TransformError> {
  match default_decl {
    DefaultDecl::Class(n) => {
      transform_class(&mut n.class, comments, public_ranges)
    }
    DefaultDecl::Fn(n) => transform_fn(
      &mut n.function,
      n.ident.as_ref().map(|i| i.range()),
      public_ranges.is_impl_with_overloads(&parent_range),
    ),
    DefaultDecl::TsInterfaceDecl(_) => Ok(()),
  }
}

fn transform_decl(
  decl: &mut Decl,
  comments: &mut CommentsMut,
  public_ranges: &ModulePublicRanges,
  parent_range: Option<SourceRange>,
) -> Result<bool, TransformError> {
  let public_range = parent_range.unwrap_or_else(|| decl.range());
  match decl {
    Decl::Class(n) => {
      if !public_ranges.contains(&public_range) {
        return Ok(false);
      }
      transform_class(&mut n.class, comments, public_ranges)?;
      Ok(true)
    }
    Decl::Fn(n) => {
      if !public_ranges.contains(&public_range) {
        return Ok(false);
      }
      let is_overload = public_ranges.is_impl_with_overloads(&public_range);
      transform_fn(&mut n.function, Some(n.ident.range()), is_overload)?;
      Ok(true)
    }
    Decl::Var(n) => transform_var(n, public_ranges),
    Decl::TsInterface(_) => Ok(public_ranges.contains(&public_range)),
    Decl::TsTypeAlias(_) => Ok(public_ranges.contains(&public_range)),
    Decl::TsEnum(_) => Ok(public_ranges.contains(&public_range)),
    Decl::TsModule(_) => todo!(),
    Decl::Using(_) => todo!("not implemented using"),
  }
}

fn transform_class(
  n: &mut Class,
  comments: &mut CommentsMut,
  public_ranges: &ModulePublicRanges,
) -> Result<(), TransformError> {
  let mut members = Vec::with_capacity(n.body.len());
  let mut had_private = false;
  if n.super_class.is_some() {
    todo!("Handle class extends");
  }
  let mut insert_members = Vec::new();
  for mut member in std::mem::take(&mut n.body) {
    had_private = had_private
      || matches!(
        member,
        ClassMember::PrivateMethod(_) | ClassMember::PrivateProp(_)
      );

    let retain =
      transform_class_member(&mut member, public_ranges, &mut insert_members)?;
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

  n.body = insert_members
    .into_iter()
    .chain(members.into_iter())
    .collect();
  n.decorators.clear();
  Ok(())
}

fn transform_class_member(
  member: &mut ClassMember,
  public_ranges: &ModulePublicRanges,
  insert_members: &mut Vec<ClassMember>,
) -> Result<bool, TransformError> {
  match member {
    ClassMember::Constructor(n) => {
      if let Some(body) = &mut n.body {
        body.stmts.clear();
      }

      for param in &mut n.params {
        match param {
          ParamOrTsParamProp::Param(_) => {
            // ignore
          }
          ParamOrTsParamProp::TsParamProp(prop) => {
            insert_members.push(ClassMember::ClassProp(ClassProp {
              span: DUMMY_SP,
              key: match &prop.param {
                TsParamPropParam::Ident(ident) => {
                  PropName::Ident(ident.id.clone())
                }
                TsParamPropParam::Assign(assign) => match &*assign.left {
                  Pat::Ident(ident) => PropName::Ident(ident.id.clone()),
                  Pat::Array(_) => todo!(),
                  Pat::Rest(_) => todo!(),
                  Pat::Object(_) => todo!(),
                  Pat::Assign(_) => todo!(),
                  Pat::Invalid(_) => todo!(),
                  Pat::Expr(_) => todo!(),
                },
              },
              value: None,
              type_ann: if prop.accessibility == Some(Accessibility::Private) {
                Some(unknown_type_ann())
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
                      maybe_infer_type_from_expr(&*assign.right).map(
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
              is_optional: false,
              is_override: prop.is_override,
              readonly: prop.readonly,
              declare: false,
              definite: false,
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
        if n.body.is_none() {
          return Ok(false);
        }
        n.params.clear();
        return Ok(true);
      }
      let is_overload = public_ranges.is_impl_with_overloads(&n.range());
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
            handle_param_pat(&mut param.pat)?;
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
          type_ann: Some(unknown_type_ann()),
          is_static: n.is_static,
          decorators: Vec::new(),
          accessibility: Some(Accessibility::Private),
          is_abstract: n.is_abstract,
          is_optional: n.is_optional,
          is_override: n.is_override,
          readonly: false,
          declare: false,
          definite: !n.is_optional,
        });
        return Ok(true);
      }
      let is_overload = public_ranges.is_impl_with_overloads(&n.range());
      transform_fn(&mut n.function, Some(n.key.range()), is_overload)?;
      Ok(true)
    }
    ClassMember::ClassProp(n) => {
      if n.accessibility == Some(Accessibility::Private) {
        n.type_ann = Some(unknown_type_ann());
        if !n.is_optional {
          n.definite = true;
        }
        return Ok(true);
      }
      if n.type_ann.is_none() {
        let inferred_type = n
          .value
          .as_ref()
          .and_then(|e| maybe_infer_type_from_expr(&*e));
        match inferred_type {
          Some(t) => {
            n.type_ann = Some(Box::new(TsTypeAnn {
              span: DUMMY_SP,
              type_ann: Box::new(t),
            }));
          }
          None => {
            return Err(TransformError::MissingExplicitType {
              range: n.key.range(),
            });
          }
        }
      }
      n.definite = !n.is_optional;
      n.decorators.clear();
      n.value = None;
      Ok(true)
    }
    ClassMember::TsIndexSignature(_) => {
      // ok, as-is
      Ok(true)
    }
    ClassMember::AutoAccessor(_) => todo!(),
    ClassMember::PrivateMethod(_)
    | ClassMember::PrivateProp(_)
    | ClassMember::Empty(_)
    | ClassMember::StaticBlock(_) => Ok(false),
  }
}

fn transform_fn(
  n: &mut Function,
  parent_id_range: Option<SourceRange>,
  is_overload: bool,
) -> Result<(), TransformError> {
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

  if n.return_type.is_none() {
    if n.is_generator {
      return Err(TransformError::MissingExplicitReturnType {
        range: parent_id_range.unwrap_or_else(|| n.range()),
      });
    }

    let return_stmts = get_return_stmts_with_arg_from_function(n);
    if return_stmts.is_empty() {
      let void_type =
        Box::new(ts_keyword_type(TsKeywordTypeKind::TsVoidKeyword));
      n.return_type = Some(Box::new(TsTypeAnn {
        span: DUMMY_SP,
        type_ann: if n.is_async {
          Box::new(TsType::TsTypeRef(TsTypeRef {
            span: DUMMY_SP,
            type_name: TsEntityName::Ident(Ident::new(
              "Promise".into(),
              DUMMY_SP,
            )),
            type_params: Some(Box::new(TsTypeParamInstantiation {
              span: DUMMY_SP,
              params: vec![void_type],
            })),
          }))
        } else {
          void_type
        },
      }));
    } else {
      return Err(TransformError::MissingExplicitReturnType {
        range: parent_id_range.unwrap_or_else(|| n.range()),
      });
    }
  }

  if let Some(body) = &mut n.body {
    body.stmts.clear();
    // push a return stmt to suppress type errors
    body.stmts.push(Stmt::Return(ReturnStmt {
      span: DUMMY_SP,
      arg: Some(obj_as_any_expr()),
    }))
  }

  for param in &mut n.params {
    handle_param_pat(&mut param.pat)?;
    param.decorators.clear();
  }

  n.is_async = false;
  n.is_generator = false;
  n.decorators.clear();

  Ok(())
}

fn handle_param_pat(pat: &mut Pat) -> Result<(), TransformError> {
  match pat {
    Pat::Ident(ident) => {
      if ident.type_ann.is_none() {
        return Err(TransformError::MissingExplicitType { range: pat.range() });
      }
    }
    Pat::Assign(assign) => match &mut *assign.left {
      Pat::Ident(ident) => {
        if ident.type_ann.is_none() {
          let inferred_type = maybe_infer_type_from_expr(&*assign.right);
          match inferred_type {
            Some(t) => {
              ident.type_ann = Some(Box::new(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::new(t),
              }));
            }
            None => {
              return Err(TransformError::MissingExplicitType {
                range: pat.range(),
              });
            }
          }
        }
        ident.id.optional = true;
        *pat = Pat::Ident((*ident).clone());
      }
      Pat::Array(_)
      | Pat::Rest(_)
      | Pat::Object(_)
      | Pat::Assign(_)
      | Pat::Invalid(_)
      | Pat::Expr(_) => {
        return Err(TransformError::UnsupportedDestructuring {
          range: pat.range(),
        });
      }
    },
    Pat::Array(_)
    | Pat::Rest(_)
    | Pat::Object(_)
    | Pat::Invalid(_)
    | Pat::Expr(_) => {
      return Err(TransformError::UnsupportedDestructuring {
        range: pat.range(),
      });
    }
  }
  Ok(())
}

fn transform_var(
  n: &mut VarDecl,
  public_ranges: &ModulePublicRanges,
) -> Result<bool, TransformError> {
  n.decls.retain(|n| public_ranges.contains(&n.range()));

  for decl in &mut n.decls {
    match &mut decl.name {
      Pat::Ident(ident) => {
        if ident.type_ann.is_none() {
          let inferred_type = decl
            .init
            .as_ref()
            .and_then(|e| maybe_infer_type_from_expr(&*e));
          match inferred_type {
            Some(t) => {
              ident.type_ann = Some(Box::new(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::new(t),
              }));
            }
            None => {
              return Err(TransformError::MissingExplicitType {
                range: ident.range(),
              });
            }
          }
        }
      }
      Pat::Array(_)
      | Pat::Rest(_)
      | Pat::Object(_)
      | Pat::Assign(_)
      | Pat::Invalid(_)
      | Pat::Expr(_) => {
        return Err(TransformError::UnsupportedDestructuring {
          range: decl.name.range(),
        })
      }
    }
    decl.init = Some(obj_as_any_expr());
    decl.definite = true;
  }

  Ok(!n.decls.is_empty())
}

fn maybe_infer_type_from_expr(expr: &Expr) -> Option<TsType> {
  match expr {
    Expr::TsTypeAssertion(n) => infer_simple_type_from_type(&n.type_ann),
    Expr::TsAs(n) => infer_simple_type_from_type(&n.type_ann),
    Expr::Lit(lit) => match lit {
      Lit::Str(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsStringKeyword)),
      Lit::Bool(_) => {
        Some(ts_keyword_type(TsKeywordTypeKind::TsBooleanKeyword))
      }
      Lit::Null(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsNullKeyword)),
      Lit::Num(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsNumberKeyword)),
      Lit::BigInt(_) => {
        Some(ts_keyword_type(TsKeywordTypeKind::TsBigIntKeyword))
      }
      Lit::Regex(_) => Some(TsType::TsTypeRef(TsTypeRef {
        span: DUMMY_SP,
        type_name: TsEntityName::Ident(Ident::new("RegExp".into(), DUMMY_SP)),
        type_params: None,
      })),
      Lit::JSXText(_) => None,
    },
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
    | Expr::Call(_)
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
    | Expr::Paren(_)
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

fn infer_simple_type_from_type(t: &TsType) -> Option<TsType> {
  match t {
    TsType::TsKeywordType(_) => Some(t.clone()),
    TsType::TsThisType(_) => Some(t.clone()),
    TsType::TsFnOrConstructorType(_) => None,
    TsType::TsTypeRef(_) => None,
    TsType::TsTypeQuery(_) => None,
    TsType::TsTypeLit(_) => None,
    TsType::TsTupleType(t) => {
      let mut elems = Vec::with_capacity(t.elem_types.len());
      for elem_type in &t.elem_types {
        let inferred_type = infer_simple_type_from_type(&elem_type.ty)?;
        elems.push(TsTupleElement {
          span: elem_type.span.clone(),
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
          let inferred_type = infer_simple_type_from_type(&ty)?;
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
          let inferred_type = infer_simple_type_from_type(&ty)?;
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
          op: t.op.clone(),
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

fn obj_as_any_expr() -> Box<Expr> {
  Box::new(Expr::TsAs(TsAsExpr {
    span: DUMMY_SP,
    expr: Box::new(Expr::Object(ObjectLit {
      span: DUMMY_SP,
      props: Default::default(),
    })),
    type_ann: Box::new(TsType::TsKeywordType(TsKeywordType {
      span: DUMMY_SP,
      kind: TsKeywordTypeKind::TsAnyKeyword,
    })),
  }))
}

fn any_type_ann() -> Box<TsTypeAnn> {
  Box::new(TsTypeAnn {
    span: DUMMY_SP,
    type_ann: Box::new(ts_keyword_type(TsKeywordTypeKind::TsAnyKeyword)),
  })
}

fn unknown_type_ann() -> Box<TsTypeAnn> {
  Box::new(TsTypeAnn {
    span: DUMMY_SP,
    type_ann: Box::new(ts_keyword_type(TsKeywordTypeKind::TsUnknownKeyword)),
  })
}
