// Copyright 2018-2024 the Deno authors. MIT license.

use std::rc::Rc;

use deno_ast::swc::ast::*;
use deno_ast::swc::codegen::text_writer::JsWriter;
use deno_ast::swc::codegen::Node;
use deno_ast::swc::common::comments::SingleThreadedComments;
use deno_ast::swc::common::FileName;
use deno_ast::swc::common::SourceMap;
use deno_ast::swc::common::DUMMY_SP;
use deno_ast::swc_codegen_config;
use deno_ast::ModuleSpecifier;
use deno_ast::SourceTextInfo;

pub fn ident(name: String) -> Ident {
  Ident {
    span: DUMMY_SP,
    sym: name.clone().into(),
    optional: false,
  }
}

pub fn ts_keyword_type(kind: TsKeywordTypeKind) -> TsType {
  TsType::TsKeywordType(TsKeywordType {
    span: DUMMY_SP,
    kind,
  })
}

pub fn get_return_stmts_with_arg_from_function_body(
  body: &deno_ast::swc::ast::BlockStmt,
) -> Vec<&ReturnStmt> {
  let stmts = get_return_stmts_with_arg_from_stmts(&body.stmts);
  debug_assert!(stmts.iter().all(|stmt| stmt.arg.is_some()));
  stmts
}

fn get_return_stmts_with_arg_from_stmts(stmts: &[Stmt]) -> Vec<&ReturnStmt> {
  let mut result = Vec::new();
  for stmt in stmts {
    result.extend(get_return_stmts_with_arg_from_stmt(stmt))
  }
  result
}

fn get_return_stmts_with_arg_from_stmt(stmt: &Stmt) -> Vec<&ReturnStmt> {
  match stmt {
    Stmt::Block(n) => get_return_stmts_with_arg_from_stmts(&n.stmts),
    Stmt::With(n) => get_return_stmts_with_arg_from_stmt(&n.body),
    Stmt::Return(n) => {
      if n.arg.is_none() {
        Vec::new()
      } else {
        vec![n]
      }
    }
    Stmt::Labeled(n) => get_return_stmts_with_arg_from_stmt(&n.body),
    Stmt::If(n) => get_return_stmts_with_arg_from_stmt(&n.cons),
    Stmt::Switch(n) => n
      .cases
      .iter()
      .flat_map(|case| get_return_stmts_with_arg_from_stmts(&case.cons))
      .collect(),
    Stmt::Try(n) => {
      let mut result = Vec::new();
      result.extend(get_return_stmts_with_arg_from_stmts(&n.block.stmts));
      if let Some(n) = &n.handler {
        result.extend(get_return_stmts_with_arg_from_stmts(&n.body.stmts))
      }
      if let Some(n) = &n.finalizer {
        result.extend(get_return_stmts_with_arg_from_stmts(&n.stmts))
      }
      result
    }
    Stmt::While(n) => get_return_stmts_with_arg_from_stmt(&n.body),
    Stmt::DoWhile(n) => get_return_stmts_with_arg_from_stmt(&n.body),
    Stmt::For(n) => get_return_stmts_with_arg_from_stmt(&n.body),
    Stmt::ForIn(n) => get_return_stmts_with_arg_from_stmt(&n.body),
    Stmt::ForOf(n) => get_return_stmts_with_arg_from_stmt(&n.body),
    Stmt::Break(_)
    | Stmt::Continue(_)
    | Stmt::Throw(_)
    | Stmt::Debugger(_)
    | Stmt::Decl(_)
    | Stmt::Expr(_)
    | Stmt::Empty(_) => Vec::new(),
  }
}

pub fn is_void_type(return_type: &TsType) -> bool {
  is_keyword_type(return_type, TsKeywordTypeKind::TsVoidKeyword)
}

pub fn is_never_type(return_type: &TsType) -> bool {
  is_keyword_type(return_type, TsKeywordTypeKind::TsNeverKeyword)
}

fn is_keyword_type(return_type: &TsType, kind: TsKeywordTypeKind) -> bool {
  match return_type {
    TsType::TsKeywordType(TsKeywordType { kind: k, .. }) => k == &kind,
    _ => false,
  }
}

pub fn any_type_ann() -> Box<TsTypeAnn> {
  type_ann(ts_keyword_type(TsKeywordTypeKind::TsAnyKeyword))
}

pub fn ts_readonly(ann: TsType) -> TsType {
  TsType::TsTypeOperator(TsTypeOperator {
    span: DUMMY_SP,
    op: TsTypeOperatorOp::ReadOnly,
    type_ann: Box::new(ann),
  })
}

pub fn type_ann(ts_type: TsType) -> Box<TsTypeAnn> {
  Box::new(TsTypeAnn {
    span: DUMMY_SP,
    type_ann: Box::new(ts_type),
  })
}

pub fn type_ref(name: String) -> TsTypeRef {
  TsTypeRef {
    span: DUMMY_SP,
    type_name: TsEntityName::Ident(Ident::new(name.into(), DUMMY_SP)),
    type_params: None,
  }
}

pub fn ts_lit_type(lit: TsLit) -> TsType {
  TsType::TsLitType(TsLitType {
    lit,
    span: DUMMY_SP,
  })
}

pub fn regex_type() -> TsType {
  TsType::TsTypeRef(type_ref("RegExp".to_string()))
}

pub fn ts_tuple_element(ts_type: TsType) -> TsTupleElement {
  TsTupleElement {
    label: None,
    span: DUMMY_SP,
    ty: Box::new(ts_type),
  }
}

pub fn maybe_lit_to_ts_type_const(lit: &Lit) -> Option<TsType> {
  match lit {
    Lit::Str(lit_str) => Some(ts_lit_type(TsLit::Str(lit_str.clone()))),
    Lit::Bool(lit_bool) => Some(ts_lit_type(TsLit::Bool(*lit_bool))),
    Lit::Null(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsNullKeyword)),
    Lit::Num(lit_num) => Some(ts_lit_type(TsLit::Number(lit_num.clone()))),
    Lit::BigInt(lit_bigint) => {
      Some(ts_lit_type(TsLit::BigInt(lit_bigint.clone())))
    }
    Lit::Regex(_) => Some(regex_type()),
    Lit::JSXText(_) => None,
  }
}

pub fn maybe_lit_to_ts_type(lit: &Lit) -> Option<TsType> {
  match lit {
    Lit::Str(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsStringKeyword)),
    Lit::Bool(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsBooleanKeyword)),
    Lit::Null(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsNullKeyword)),
    Lit::Num(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsNumberKeyword)),
    Lit::BigInt(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsBigIntKeyword)),
    Lit::Regex(_) => Some(regex_type()),
    Lit::JSXText(_) => None,
  }
}

pub fn emit(
  specifier: &ModuleSpecifier,
  comments: &SingleThreadedComments,
  text_info: &SourceTextInfo,
  module: &deno_ast::swc::ast::Module,
) -> Result<(String, Vec<u8>), anyhow::Error> {
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
    buf
  };

  Ok((src, map))
}
