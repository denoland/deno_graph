// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::ast::Expr;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::Lit;
use deno_ast::swc::ast::MemberProp;
use deno_ast::swc::ast::Prop;
use deno_ast::swc::ast::PropName;
use deno_ast::swc::ast::PropOrSpread;
use deno_ast::swc::ast::ReturnStmt;
use deno_ast::swc::ast::Stmt;
use deno_ast::swc::ast::TsKeywordType;
use deno_ast::swc::ast::TsKeywordTypeKind;
use deno_ast::swc::ast::TsType;
use deno_ast::swc::common::DUMMY_SP;
use deno_ast::SourceRange;
use deno_ast::SourceTextInfo;

use crate::ModuleSpecifier;
use crate::PositionRange;
use crate::Range;

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

pub fn get_return_stmts_with_arg_from_function(
  func: &deno_ast::swc::ast::Function,
) -> Vec<&ReturnStmt> {
  let Some(body) = func.body.as_ref() else {
    return Vec::new();
  };
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

pub fn source_range_to_range(
  range: SourceRange,
  specifier: &ModuleSpecifier,
  text_info: &SourceTextInfo,
) -> Range {
  let position_range = PositionRange::from_source_range(range, text_info);
  Range::from_position_range(specifier.clone(), position_range)
}

pub fn is_expr_leavable(expr: &Expr) -> bool {
  fn is_member_prop_leavable(n: &MemberProp) -> bool {
    match n {
      MemberProp::Ident(_) => true,
      MemberProp::PrivateName(_) => false,
      MemberProp::Computed(n) => is_expr_leavable(&n.expr),
    }
  }

  match expr {
    Expr::This(_) => true,
    Expr::Array(n) => n.elems.iter().all(|elem| match elem {
      Some(elem) => is_expr_leavable(&elem.expr),
      None => true,
    }),
    Expr::Object(n) => n.props.iter().all(|prop| match prop {
      PropOrSpread::Prop(prop) => match &**prop {
        Prop::Shorthand(_) => true,
        Prop::KeyValue(prop) => match &prop.key {
          PropName::Ident(_) => is_expr_leavable(&prop.value),
          PropName::Str(_) => is_expr_leavable(&prop.value),
          PropName::Num(_) => is_expr_leavable(&prop.value),
          PropName::Computed(c) => {
            is_expr_leavable(&c.expr) && is_expr_leavable(&prop.value)
          }
          PropName::BigInt(_) => is_expr_leavable(&prop.value),
        },
        Prop::Assign(n) => is_expr_leavable(&n.value),
        Prop::Getter(_) | Prop::Setter(_) | Prop::Method(_) => false,
      },
      PropOrSpread::Spread(n) => {
        is_expr_leavable(&n.expr)
      },
    }),
    Expr::Unary(n) => is_expr_leavable(&n.arg),
    Expr::Update(n) => is_expr_leavable(&n.arg),
    Expr::Bin(n) => is_expr_leavable(&n.left) && is_expr_leavable(&n.right),
    Expr::Assign(_) | Expr::SuperProp(_) => false,
    Expr::Cond(n) => {
      is_expr_leavable(&n.test)
        && is_expr_leavable(&n.cons)
        && is_expr_leavable(&n.alt)
    }
    Expr::Member(n) => {
      is_expr_leavable(&n.obj) && is_member_prop_leavable(&n.prop)
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
    Expr::Await(n) => is_expr_leavable(&n.arg),
    Expr::Paren(n) => is_expr_leavable(&n.expr),
    Expr::TsTypeAssertion(_) | Expr::TsAs(_) => false,
    Expr::TsConstAssertion(n) => is_expr_leavable(&n.expr),
    Expr::TsNonNull(n) => is_expr_leavable(&n.expr),
    Expr::Tpl(_)
    | Expr::Fn(_)
    | Expr::TaggedTpl(_)
    | Expr::Arrow(_)
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
    // todo: probably could analyze this more
    | Expr::OptChain(_)
    | Expr::Invalid(_) => false,
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
