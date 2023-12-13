use std::collections::HashSet;
use std::rc::Rc;

use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::ast::Class;
use deno_ast::swc::ast::ClassMember;
use deno_ast::swc::ast::Decl;
use deno_ast::swc::ast::DefaultDecl;
use deno_ast::swc::ast::Expr;
use deno_ast::swc::ast::Function;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::Lit;
use deno_ast::swc::ast::ModuleDecl;
use deno_ast::swc::ast::ModuleItem;
use deno_ast::swc::ast::ObjectLit;
use deno_ast::swc::ast::Pat;
use deno_ast::swc::ast::PrivateName;
use deno_ast::swc::ast::PrivateProp;
use deno_ast::swc::ast::ReturnStmt;
use deno_ast::swc::ast::Stmt;
use deno_ast::swc::ast::TsAsExpr;
use deno_ast::swc::ast::TsImportEqualsDecl;
use deno_ast::swc::ast::TsKeywordType;
use deno_ast::swc::ast::TsKeywordTypeKind;
use deno_ast::swc::ast::TsType;
use deno_ast::swc::ast::TsTypeAnn;
use deno_ast::swc::ast::VarDecl;
use deno_ast::swc::codegen::text_writer::JsWriter;
use deno_ast::swc::codegen::Node;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::common::comments::SingleThreadedComments;
use deno_ast::swc::common::comments::SingleThreadedCommentsMapInner;
use deno_ast::swc::common::FileName;
use deno_ast::swc::common::SourceMap;
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

use super::swc_helpers::get_return_stmts_with_arg_from_function;

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

pub enum TransformError {
  UnsupportedDestructuring { range: SourceRange },
  MissingExplicitType { range: SourceRange },
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
  public_ranges: &HashSet<SourceRange>,
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
  public_ranges: &HashSet<SourceRange>,
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
      ModuleDecl::ExportAll(_) => {
        todo!("retain if the referenced module is public");
      }
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

        transform_default_decl(&mut n.decl, comments)?;
        Ok(true)
      }
      ModuleDecl::ExportDecl(n) => {
        if !public_ranges.contains(&n.range()) {
          return Ok(false);
        }

        transform_decl(&mut n.decl, comments, public_ranges)?;
        Ok(true)
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
      Stmt::Decl(n) => {
        if !public_ranges.contains(&n.range()) {
          return Ok(false);
        }

        transform_decl(n, comments, public_ranges)?;
        Ok(true)
      }
    },
  }
}

fn transform_default_decl(
  default_decl: &mut DefaultDecl,
  comments: &mut CommentsMut,
) -> Result<(), TransformError> {
  match default_decl {
    DefaultDecl::Class(n) => transform_class(&mut n.class, comments),
    DefaultDecl::Fn(n) => transform_fn(&mut n.function),
    DefaultDecl::TsInterfaceDecl(_) => Ok(()),
  }
}

fn transform_decl(
  decl: &mut Decl,
  comments: &mut CommentsMut,
  public_ranges: &HashSet<SourceRange>,
) -> Result<(), TransformError> {
  match decl {
    Decl::Class(n) => transform_class(&mut n.class, comments),
    Decl::Fn(n) => transform_fn(&mut n.function),
    Decl::Var(n) => transform_var(n, public_ranges),
    Decl::TsInterface(_) => Ok(()),
    Decl::TsTypeAlias(_) => Ok(()),
    Decl::TsEnum(_) => Ok(()),
    Decl::TsModule(_) => todo!(),
    Decl::Using(_) => todo!("not implemented using"),
  }
}

fn transform_class(
  n: &mut Class,
  comments: &mut CommentsMut,
) -> Result<(), TransformError> {
  let mut members = Vec::with_capacity(n.body.len());
  let mut had_private = false;
  if n.super_class.is_some() {
    todo!("Handle class extends");
  }
  for mut member in std::mem::take(&mut n.body) {
    had_private = had_private
      || matches!(
        member,
        ClassMember::PrivateMethod(_) | ClassMember::PrivateProp(_)
      );

    let retain = transform_class_member(&mut member)?;
    if retain {
      members.push(member);
    } else {
      comments.remove_leading(member.start());
    }
  }

  if had_private {
    members.insert(
      0,
      ClassMember::PrivateProp(PrivateProp {
        span: DUMMY_SP,
        key: PrivateName {
          span: DUMMY_SP,
          id: Ident {
            span: DUMMY_SP,
            sym: "private".into(),
            optional: false,
          },
        },
        value: None,
        type_ann: Some(Box::new(TsTypeAnn {
          span: DUMMY_SP,
          type_ann: Box::new(TsType::TsKeywordType(TsKeywordType {
            span: DUMMY_SP,
            kind: TsKeywordTypeKind::TsUnknownKeyword,
          })),
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

  n.body = members;
  Ok(())
}

fn transform_class_member(n: &mut ClassMember) -> Result<bool, TransformError> {
  match n {
    ClassMember::Constructor(n) => {
      if n.accessibility == Some(Accessibility::Private) {
        return Ok(false);
      }
      if let Some(body) = &mut n.body {
        body.stmts.clear();
      }
      Ok(true)
    }
    ClassMember::Method(n) => {
      if n.accessibility == Some(Accessibility::Private) {
        return Ok(false);
      }
      transform_fn(&mut n.function)?;
      Ok(true)
    }
    ClassMember::ClassProp(n) => {
      if n.accessibility == Some(Accessibility::Private) {
        return Ok(false);
      }
      if n.type_ann.is_none() {
        todo!("no type ann on property");
      }
      n.definite = true;
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

fn transform_fn(n: &mut Function) -> Result<(), TransformError> {
  if n.return_type.is_none() {
    let return_stmts = get_return_stmts_with_arg_from_function(n);
    if return_stmts.is_empty() {
      n.return_type = Some(Box::new(TsTypeAnn {
        span: DUMMY_SP,
        type_ann: Box::new(TsType::TsKeywordType(TsKeywordType {
          span: DUMMY_SP,
          kind: TsKeywordTypeKind::TsVoidKeyword,
        })),
      }));
    } else {
      todo!();
    }
  }

  if let Some(body) = &mut n.body {
    body.stmts.clear();
    // push a return stmt to suppress type errors
    body.stmts.push(Stmt::Return(ReturnStmt {
      span: DUMMY_SP,
      arg: Some(Box::new(Expr::TsAs(TsAsExpr {
        span: DUMMY_SP,
        expr: Box::new(Expr::Object(ObjectLit {
          span: DUMMY_SP,
          props: Default::default(),
        })),
        type_ann: Box::new(TsType::TsKeywordType(TsKeywordType {
          span: DUMMY_SP,
          kind: TsKeywordTypeKind::TsAnyKeyword,
        })),
      }))),
    }))
  }

  for param in &mut n.params {
    match &mut param.pat {
      Pat::Ident(ident) => {
        if ident.type_ann.is_none() {
          return Err(TransformError::MissingExplicitType {
            range: param.pat.range(),
          });
        }
      }
      Pat::Assign(assign) => match &mut *assign.left {
        Pat::Ident(ident) => {
          if ident.type_ann.is_none() {
            return Err(TransformError::MissingExplicitType {
              range: param.pat.range(),
            });
          }
        }
        Pat::Array(_)
        | Pat::Rest(_)
        | Pat::Object(_)
        | Pat::Assign(_)
        | Pat::Invalid(_)
        | Pat::Expr(_) => {
          return Err(TransformError::UnsupportedDestructuring {
            range: param.pat.range(),
          });
        }
      },
      Pat::Array(_)
      | Pat::Rest(_)
      | Pat::Object(_)
      | Pat::Invalid(_)
      | Pat::Expr(_) => {
        return Err(TransformError::UnsupportedDestructuring {
          range: param.pat.range(),
        });
      }
    }
    param.decorators.clear();
  }

  Ok(())
}

fn transform_var(
  n: &mut VarDecl,
  public_ranges: &HashSet<SourceRange>,
) -> Result<(), TransformError> {
  n.decls.retain(|n| public_ranges.contains(&n.range()));

  for decl in &mut n.decls {
    match &mut decl.name {
      Pat::Ident(ident) => {
        if ident.type_ann.is_none() {
          return Err(TransformError::MissingExplicitType {
            range: decl.name.range(),
          });
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
    decl.init = None;
    decl.definite = true;
  }

  Ok(())
}

fn maybe_infer_type_from_expr(expr: &Expr) -> Option<TsType> {
  match expr {
    Expr::TsTypeAssertion(n) => Some(*n.type_ann.clone()),
    Expr::TsAs(n) => Some(*n.type_ann.clone()),
    Expr::Lit(lit) => {
      let keyword = match lit {
        Lit::Str(_) => Some(TsKeywordTypeKind::TsStringKeyword),
        Lit::Bool(_) => Some(TsKeywordTypeKind::TsBooleanKeyword),
        Lit::Null(_) => Some(TsKeywordTypeKind::TsNullKeyword),
        Lit::Num(_) => Some(TsKeywordTypeKind::TsNumberKeyword),
        Lit::BigInt(_) => Some(TsKeywordTypeKind::TsBigIntKeyword),
        Lit::Regex(_) => None,
        Lit::JSXText(_) => None,
      };
      keyword.map(|kind| {
        TsType::TsKeywordType(TsKeywordType {
          span: DUMMY_SP,
          kind,
        })
      })
    }
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
