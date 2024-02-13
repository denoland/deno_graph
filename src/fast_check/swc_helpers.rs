// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::ast::ArrayLit;
use deno_ast::swc::ast::Expr;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::Lit;
use deno_ast::swc::ast::MemberProp;
use deno_ast::swc::ast::ObjectLit;
use deno_ast::swc::ast::ParenExpr;
use deno_ast::swc::ast::Prop;
use deno_ast::swc::ast::PropName;
use deno_ast::swc::ast::PropOrSpread;
use deno_ast::swc::ast::ReturnStmt;
use deno_ast::swc::ast::Stmt;
use deno_ast::swc::ast::TsAsExpr;
use deno_ast::swc::ast::TsEntityName;
use deno_ast::swc::ast::TsKeywordType;
use deno_ast::swc::ast::TsKeywordTypeKind;
use deno_ast::swc::ast::TsLit;
use deno_ast::swc::ast::TsLitType;
use deno_ast::swc::ast::TsTupleElement;
use deno_ast::swc::ast::TsType;
use deno_ast::swc::ast::TsTypeAnn;
use deno_ast::swc::ast::TsTypeOperator;
use deno_ast::swc::ast::TsTypeOperatorOp;
use deno_ast::swc::ast::TsTypeRef;
use deno_ast::swc::common::DUMMY_SP;

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

// KEEP IN SYNC with maybe_transform_expr_if_leavable
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
    // todo(dsherret): probably could analyze this more
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
