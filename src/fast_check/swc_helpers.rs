// Copyright 2018-2024 the Deno authors. MIT license.

use std::ops::ControlFlow;

use deno_ast::swc::ast::*;
use deno_ast::swc::atoms::Atom;
use deno_ast::swc::common::DUMMY_SP;
use deno_ast::swc::common::SyntaxContext;

pub fn new_ident(name: Atom) -> Ident {
  Ident {
    span: DUMMY_SP,
    ctxt: Default::default(),
    sym: name,
    optional: false,
  }
}

pub fn ts_keyword_type(kind: TsKeywordTypeKind) -> TsType {
  TsType::TsKeywordType(TsKeywordType {
    span: DUMMY_SP,
    kind,
  })
}

#[derive(Debug)]
pub enum ReturnStatementAnalysis {
  /// There are no return statements in the function body.
  None,
  /// There are only return statements without arguments in the function body,
  /// or if the function body is empty.
  Void,
  /// There is only a single return statement in the function body, and it has
  /// an argument.
  Single,
  /// There are multiple return statements in the function body, and at least
  /// one of them has an argument.
  Multiple,
}

pub fn analyze_return_stmts_in_function_body(
  body: &deno_ast::swc::ast::BlockStmt,
) -> ReturnStatementAnalysis {
  if body.stmts.is_empty() {
    ReturnStatementAnalysis::Void
  } else {
    let mut analysis = ReturnStatementAnalysis::None;
    _ = analyze_return_stmts_from_stmts(&body.stmts, &mut analysis);
    analysis
  }
}

fn analyze_return_stmts_from_stmts(
  stmts: &[Stmt],
  analysis: &mut ReturnStatementAnalysis,
) -> ControlFlow<(), ()> {
  for stmt in stmts {
    analyze_return_stmts_from_stmt(stmt, analysis)?;
  }
  ControlFlow::Continue(())
}

fn analyze_return_stmts_from_stmt(
  stmt: &Stmt,
  analysis: &mut ReturnStatementAnalysis,
) -> ControlFlow<(), ()> {
  match stmt {
    Stmt::Block(n) => analyze_return_stmts_from_stmts(&n.stmts, analysis),
    Stmt::With(n) => analyze_return_stmts_from_stmt(&n.body, analysis),
    Stmt::Return(n) => {
      match (&n.arg, &*analysis) {
        (None, ReturnStatementAnalysis::None) => {
          *analysis = ReturnStatementAnalysis::Void;
        }
        (None, ReturnStatementAnalysis::Void) => {}
        (Some(_), ReturnStatementAnalysis::None)
        | (Some(_), ReturnStatementAnalysis::Void) => {
          *analysis = ReturnStatementAnalysis::Single;
        }
        (_, ReturnStatementAnalysis::Single) => {
          *analysis = ReturnStatementAnalysis::Multiple;
          return ControlFlow::Break(());
        }
        (_, ReturnStatementAnalysis::Multiple) => unreachable!(), // we break early when analysis is Multiple
      }
      ControlFlow::Continue(())
    }
    Stmt::Labeled(n) => analyze_return_stmts_from_stmt(&n.body, analysis),
    Stmt::If(n) => analyze_return_stmts_from_stmt(&n.cons, analysis),
    Stmt::Switch(n) => {
      for case in &n.cases {
        analyze_return_stmts_from_stmts(&case.cons, analysis)?;
      }
      ControlFlow::Continue(())
    }
    Stmt::Try(n) => {
      analyze_return_stmts_from_stmts(&n.block.stmts, analysis)?;
      if let Some(n) = &n.handler {
        analyze_return_stmts_from_stmts(&n.body.stmts, analysis)?;
      }
      if let Some(n) = &n.finalizer {
        analyze_return_stmts_from_stmts(&n.stmts, analysis)?;
      }
      ControlFlow::Continue(())
    }
    Stmt::While(n) => analyze_return_stmts_from_stmt(&n.body, analysis),
    Stmt::DoWhile(n) => analyze_return_stmts_from_stmt(&n.body, analysis),
    Stmt::For(n) => analyze_return_stmts_from_stmt(&n.body, analysis),
    Stmt::ForIn(n) => analyze_return_stmts_from_stmt(&n.body, analysis),
    Stmt::ForOf(n) => analyze_return_stmts_from_stmt(&n.body, analysis),
    Stmt::Break(_)
    | Stmt::Continue(_)
    | Stmt::Throw(_)
    | Stmt::Debugger(_)
    | Stmt::Decl(_)
    | Stmt::Expr(_)
    | Stmt::Empty(_) => ControlFlow::Continue(()),
  }
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

pub fn type_ref(name: Atom) -> TsTypeRef {
  TsTypeRef {
    span: DUMMY_SP,
    type_name: TsEntityName::Ident(Ident::new(
      name,
      DUMMY_SP,
      SyntaxContext::default(),
    )),
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
  TsType::TsTypeRef(type_ref("RegExp".into()))
}

pub fn ts_tuple_element(ts_type: TsType) -> TsTupleElement {
  TsTupleElement {
    label: None,
    span: DUMMY_SP,
    ty: Box::new(ts_type),
  }
}

pub enum DeclMutabilityKind {
  Const,
  Mutable,
}

pub fn maybe_lit_to_ts_type(
  lit: &Lit,
  decl_kind: DeclMutabilityKind,
) -> Option<TsType> {
  match decl_kind {
    DeclMutabilityKind::Const => match lit {
      Lit::Str(lit_str) => Some(ts_lit_type(TsLit::Str(lit_str.clone()))),
      Lit::Bool(lit_bool) => Some(ts_lit_type(TsLit::Bool(*lit_bool))),
      Lit::Null(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsNullKeyword)),
      Lit::Num(lit_num) => Some(ts_lit_type(TsLit::Number(lit_num.clone()))),
      Lit::BigInt(lit_bigint) => {
        Some(ts_lit_type(TsLit::BigInt(lit_bigint.clone())))
      }
      Lit::Regex(_) => Some(regex_type()),
      Lit::JSXText(_) => None,
    },
    DeclMutabilityKind::Mutable => match lit {
      Lit::Str(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsStringKeyword)),
      Lit::Bool(_) => {
        Some(ts_keyword_type(TsKeywordTypeKind::TsBooleanKeyword))
      }
      Lit::Null(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsNullKeyword)),
      Lit::Num(_) => Some(ts_keyword_type(TsKeywordTypeKind::TsNumberKeyword)),
      Lit::BigInt(_) => {
        Some(ts_keyword_type(TsKeywordTypeKind::TsBigIntKeyword))
      }
      Lit::Regex(_) => Some(regex_type()),
      Lit::JSXText(_) => None,
    },
  }
}
