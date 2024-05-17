// Copyright 2018-2024 the Deno authors. MIT license.

use deno_ast::swc::ast::*;
use deno_ast::swc::common::SyntaxContext;
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

pub fn is_void_type(return_type: &TsType) -> bool {
  is_keyword_type(return_type, TsKeywordTypeKind::TsVoidKeyword)
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

pub fn lit_to_ts_type_const(lit: &Lit) -> TsType {
  match lit {
    Lit::Str(lit_str) => ts_lit_type(TsLit::Str(lit_str.clone())),
    Lit::Bool(lit_bool) => ts_lit_type(TsLit::Bool(*lit_bool)),
    Lit::Null(_) => ts_keyword_type(TsKeywordTypeKind::TsNullKeyword),
    Lit::Num(lit_num) => ts_lit_type(TsLit::Number(lit_num.clone())),
    Lit::BigInt(lit_bigint) => ts_lit_type(TsLit::BigInt(lit_bigint.clone())),
    Lit::Regex(_) => regex_type(),
    Lit::JSXText(_) => {
      unreachable!("jsx text can only happen inside of jsx elements")
    }
  }
}

pub fn lit_to_ts_type(lit: &Lit) -> TsType {
  match lit {
    Lit::Str(_) => ts_keyword_type(TsKeywordTypeKind::TsStringKeyword),
    Lit::Bool(_) => ts_keyword_type(TsKeywordTypeKind::TsBooleanKeyword),
    Lit::Null(_) => ts_keyword_type(TsKeywordTypeKind::TsNullKeyword),
    Lit::Num(_) => ts_keyword_type(TsKeywordTypeKind::TsNumberKeyword),
    Lit::BigInt(_) => ts_keyword_type(TsKeywordTypeKind::TsBigIntKeyword),
    Lit::Regex(_) => regex_type(),
    Lit::JSXText(_) => {
      unreachable!("jsx text can only happen inside of jsx elements")
    }
  }
}

pub fn prop_name_to_ts_key(
  prop_name: &PropName,
) -> (Box<Expr>, /* computed */ bool) {
  match prop_name {
    PropName::Ident(ident) => (Box::new(Expr::Ident(ident.clone())), false),
    PropName::Str(str) => match Ident::verify_symbol(str.value.as_str()) {
      Ok(_) => (
        Box::new(Expr::Ident(Ident::new(str.value.clone(), str.span))),
        false,
      ),
      Err(_) => (Box::new(Expr::Lit(Lit::Str(str.clone()))), false),
    },
    PropName::Num(num) => (Box::new(Expr::Lit(Lit::Num(num.clone()))), false),
    PropName::Computed(expr) => {
      // We leave the expression as is. See also https://github.com/microsoft/TypeScript/issues/58533
      (expr.expr.clone(), true)
    }
    PropName::BigInt(bigint) => {
      (Box::new(Expr::Lit(Lit::BigInt(bigint.clone()))), false)
    }
  }
}

pub fn void_or_promise_void(is_async: bool) -> Box<TsType> {
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

pub fn is_param_pat_optional(pat: &Pat) -> bool {
  match pat {
    Pat::Ident(ident) => ident.optional,
    Pat::Array(a) => a.optional,
    Pat::Object(o) => o.optional,
    Pat::Assign(_) | Pat::Rest(_) => true,
    Pat::Invalid(_) | Pat::Expr(_) => false,
  }
}

/// Looks if the call expr is `Symbol("example")` or `Symbol.for("example")`
pub fn is_call_expr_symbol_create(
  call_expr: &CallExpr,
  unresolved_context: SyntaxContext,
) -> bool {
  let Some(expr) = call_expr.callee.as_expr() else {
    return false;
  };
  let (expr_ident, is_for) = match &**expr {
    Expr::Ident(ident) => (ident, false),
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
      (ident, true)
    }
    _ => return false,
  };

  let is_symbol_global =
    expr_ident.sym == "Symbol" && expr_ident.to_id().1 == unresolved_context;
  if !is_symbol_global {
    return false;
  }
  if !is_for && call_expr.args.is_empty() {
    return true;
  }
  if call_expr.args.len() != 1 {
    return false;
  }
  let Some(arg_lit) = call_expr.args.first().and_then(|a| a.expr.as_lit())
  else {
    return false;
  };
  matches!(arg_lit, Lit::Str(_))
}
