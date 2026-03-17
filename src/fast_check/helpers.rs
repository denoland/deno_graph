// Copyright 2018-2024 the Deno authors. MIT license.

use std::ops::ControlFlow;

use deno_ast::oxc::allocator::Allocator;
use deno_ast::oxc::allocator::Box;
use deno_ast::oxc::allocator::CloneIn;
use deno_ast::oxc::ast::ast::*;
use deno_ast::oxc::span::Ident;
use deno_ast::oxc::span::SPAN;
use deno_ast::oxc::syntax::node::NodeId;
use std::cell::Cell;

pub fn new_ident<'a>(
  allocator: &'a Allocator,
  name: &str,
) -> BindingIdentifier<'a> {
  BindingIdentifier {
    node_id: Cell::new(NodeId::DUMMY),
    span: SPAN,
    name: Ident::from(allocator.alloc_str(name)),
    symbol_id: Cell::new(None),
  }
}

pub fn ts_keyword_type<'a>(
  allocator: &'a Allocator,
  kind: TSKeywordKind,
) -> TSType<'a> {
  match kind {
    TSKeywordKind::Any => TSType::TSAnyKeyword(Box::new_in(
      TSAnyKeyword {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
      },
      allocator,
    )),
    TSKeywordKind::Boolean => TSType::TSBooleanKeyword(Box::new_in(
      TSBooleanKeyword {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
      },
      allocator,
    )),
    TSKeywordKind::Number => TSType::TSNumberKeyword(Box::new_in(
      TSNumberKeyword {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
      },
      allocator,
    )),
    TSKeywordKind::String => TSType::TSStringKeyword(Box::new_in(
      TSStringKeyword {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
      },
      allocator,
    )),
    TSKeywordKind::Void => TSType::TSVoidKeyword(Box::new_in(
      TSVoidKeyword {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
      },
      allocator,
    )),
    TSKeywordKind::Null => TSType::TSNullKeyword(Box::new_in(
      TSNullKeyword {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
      },
      allocator,
    )),
    TSKeywordKind::BigInt => TSType::TSBigIntKeyword(Box::new_in(
      TSBigIntKeyword {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
      },
      allocator,
    )),
  }
}

/// Enum to represent the keyword types we use in the helpers,
/// since OXC doesn't have a unified TsKeywordTypeKind.
pub enum TSKeywordKind {
  Any,
  Boolean,
  Number,
  String,
  Void,
  Null,
  BigInt,
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
  body: &FunctionBody<'_>,
) -> ReturnStatementAnalysis {
  if body.statements.is_empty() {
    ReturnStatementAnalysis::Void
  } else {
    let mut analysis = ReturnStatementAnalysis::None;
    _ = analyze_return_stmts_from_stmts(&body.statements, &mut analysis);
    analysis
  }
}

fn analyze_return_stmts_from_stmts(
  stmts: &[Statement<'_>],
  analysis: &mut ReturnStatementAnalysis,
) -> ControlFlow<(), ()> {
  for stmt in stmts {
    analyze_return_stmts_from_stmt(stmt, analysis)?;
  }
  ControlFlow::Continue(())
}

fn analyze_return_stmts_from_stmt(
  stmt: &Statement<'_>,
  analysis: &mut ReturnStatementAnalysis,
) -> ControlFlow<(), ()> {
  match stmt {
    Statement::BlockStatement(n) => {
      analyze_return_stmts_from_stmts(&n.body, analysis)
    }
    Statement::WithStatement(n) => {
      analyze_return_stmts_from_stmt(&n.body, analysis)
    }
    Statement::ReturnStatement(n) => {
      match (&n.argument, &*analysis) {
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
        (_, ReturnStatementAnalysis::Multiple) => unreachable!(),
      }
      ControlFlow::Continue(())
    }
    Statement::LabeledStatement(n) => {
      analyze_return_stmts_from_stmt(&n.body, analysis)
    }
    Statement::IfStatement(n) => {
      analyze_return_stmts_from_stmt(&n.consequent, analysis)
    }
    Statement::SwitchStatement(n) => {
      for case in &n.cases {
        analyze_return_stmts_from_stmts(&case.consequent, analysis)?;
      }
      ControlFlow::Continue(())
    }
    Statement::TryStatement(n) => {
      analyze_return_stmts_from_stmts(&n.block.body, analysis)?;
      if let Some(n) = &n.handler {
        analyze_return_stmts_from_stmts(&n.body.body, analysis)?;
      }
      if let Some(n) = &n.finalizer {
        analyze_return_stmts_from_stmts(&n.body, analysis)?;
      }
      ControlFlow::Continue(())
    }
    Statement::WhileStatement(n) => {
      analyze_return_stmts_from_stmt(&n.body, analysis)
    }
    Statement::DoWhileStatement(n) => {
      analyze_return_stmts_from_stmt(&n.body, analysis)
    }
    Statement::ForStatement(n) => {
      analyze_return_stmts_from_stmt(&n.body, analysis)
    }
    Statement::ForInStatement(n) => {
      analyze_return_stmts_from_stmt(&n.body, analysis)
    }
    Statement::ForOfStatement(n) => {
      analyze_return_stmts_from_stmt(&n.body, analysis)
    }
    Statement::BreakStatement(_)
    | Statement::ContinueStatement(_)
    | Statement::ThrowStatement(_)
    | Statement::DebuggerStatement(_)
    | Statement::EmptyStatement(_)
    | Statement::ExpressionStatement(_) => ControlFlow::Continue(()),
    // Declaration and module declaration variants
    _ => ControlFlow::Continue(()),
  }
}

pub fn is_void_type(return_type: &TSType) -> bool {
  matches!(return_type, TSType::TSVoidKeyword(_))
}

pub fn any_type_ann<'a>(
  allocator: &'a Allocator,
) -> Box<'a, TSTypeAnnotation<'a>> {
  type_ann(allocator, ts_keyword_type(allocator, TSKeywordKind::Any))
}

pub fn ts_readonly<'a>(
  allocator: &'a Allocator,
  ann: TSType<'a>,
) -> TSType<'a> {
  TSType::TSTypeOperatorType(Box::new_in(
    TSTypeOperator {
      node_id: Cell::new(NodeId::DUMMY),
      span: SPAN,
      operator: TSTypeOperatorOperator::Readonly,
      type_annotation: ann,
    },
    allocator,
  ))
}

pub fn type_ann<'a>(
  allocator: &'a Allocator,
  ts_type: TSType<'a>,
) -> Box<'a, TSTypeAnnotation<'a>> {
  Box::new_in(
    TSTypeAnnotation {
      node_id: Cell::new(NodeId::DUMMY),
      span: SPAN,
      type_annotation: ts_type,
    },
    allocator,
  )
}

pub fn type_ref<'a>(
  allocator: &'a Allocator,
  name: &str,
) -> TSTypeReference<'a> {
  TSTypeReference {
    node_id: Cell::new(NodeId::DUMMY),
    span: SPAN,
    type_name: TSTypeName::IdentifierReference(Box::new_in(
      IdentifierReference {
        node_id: Cell::new(NodeId::DUMMY),
        span: SPAN,
        name: Ident::from(allocator.alloc_str(name)),
        reference_id: Cell::new(None),
      },
      allocator,
    )),
    type_arguments: None,
  }
}

pub fn ts_lit_type<'a>(
  allocator: &'a Allocator,
  lit: TSLiteral<'a>,
) -> TSType<'a> {
  TSType::TSLiteralType(Box::new_in(
    TSLiteralType {
      node_id: Cell::new(NodeId::DUMMY),
      span: SPAN,
      literal: lit,
    },
    allocator,
  ))
}

pub fn regex_type<'a>(allocator: &'a Allocator) -> TSType<'a> {
  TSType::TSTypeReference(Box::new_in(type_ref(allocator, "RegExp"), allocator))
}

/// Convert a TSType into a TSTupleElement.
/// TSTupleElement inherits all TSType variants via the @inherit macro.
pub fn ts_tuple_element<'a>(ts_type: TSType<'a>) -> TSTupleElement<'a> {
  match ts_type {
    TSType::TSAnyKeyword(t) => TSTupleElement::TSAnyKeyword(t),
    TSType::TSBigIntKeyword(t) => TSTupleElement::TSBigIntKeyword(t),
    TSType::TSBooleanKeyword(t) => TSTupleElement::TSBooleanKeyword(t),
    TSType::TSIntrinsicKeyword(t) => TSTupleElement::TSIntrinsicKeyword(t),
    TSType::TSNeverKeyword(t) => TSTupleElement::TSNeverKeyword(t),
    TSType::TSNullKeyword(t) => TSTupleElement::TSNullKeyword(t),
    TSType::TSNumberKeyword(t) => TSTupleElement::TSNumberKeyword(t),
    TSType::TSObjectKeyword(t) => TSTupleElement::TSObjectKeyword(t),
    TSType::TSStringKeyword(t) => TSTupleElement::TSStringKeyword(t),
    TSType::TSSymbolKeyword(t) => TSTupleElement::TSSymbolKeyword(t),
    TSType::TSUndefinedKeyword(t) => TSTupleElement::TSUndefinedKeyword(t),
    TSType::TSUnknownKeyword(t) => TSTupleElement::TSUnknownKeyword(t),
    TSType::TSVoidKeyword(t) => TSTupleElement::TSVoidKeyword(t),
    TSType::TSArrayType(t) => TSTupleElement::TSArrayType(t),
    TSType::TSConditionalType(t) => TSTupleElement::TSConditionalType(t),
    TSType::TSConstructorType(t) => TSTupleElement::TSConstructorType(t),
    TSType::TSFunctionType(t) => TSTupleElement::TSFunctionType(t),
    TSType::TSImportType(t) => TSTupleElement::TSImportType(t),
    TSType::TSIndexedAccessType(t) => TSTupleElement::TSIndexedAccessType(t),
    TSType::TSInferType(t) => TSTupleElement::TSInferType(t),
    TSType::TSIntersectionType(t) => TSTupleElement::TSIntersectionType(t),
    TSType::TSLiteralType(t) => TSTupleElement::TSLiteralType(t),
    TSType::TSMappedType(t) => TSTupleElement::TSMappedType(t),
    TSType::TSNamedTupleMember(t) => TSTupleElement::TSNamedTupleMember(t),
    TSType::TSTemplateLiteralType(t) => {
      TSTupleElement::TSTemplateLiteralType(t)
    }
    TSType::TSThisType(t) => TSTupleElement::TSThisType(t),
    TSType::TSTupleType(t) => TSTupleElement::TSTupleType(t),
    TSType::TSTypeLiteral(t) => TSTupleElement::TSTypeLiteral(t),
    TSType::TSTypeOperatorType(t) => TSTupleElement::TSTypeOperatorType(t),
    TSType::TSTypePredicate(t) => TSTupleElement::TSTypePredicate(t),
    TSType::TSTypeQuery(t) => TSTupleElement::TSTypeQuery(t),
    TSType::TSTypeReference(t) => TSTupleElement::TSTypeReference(t),
    TSType::TSUnionType(t) => TSTupleElement::TSUnionType(t),
    TSType::TSParenthesizedType(t) => TSTupleElement::TSParenthesizedType(t),
    TSType::JSDocNullableType(t) => TSTupleElement::JSDocNullableType(t),
    TSType::JSDocNonNullableType(t) => TSTupleElement::JSDocNonNullableType(t),
    TSType::JSDocUnknownType(t) => TSTupleElement::JSDocUnknownType(t),
  }
}

pub enum DeclMutabilityKind {
  Const,
  Mutable,
}

pub fn maybe_lit_to_ts_type<'a>(
  allocator: &'a Allocator,
  expr: &Expression<'a>,
  decl_kind: DeclMutabilityKind,
) -> Option<TSType<'a>> {
  match decl_kind {
    DeclMutabilityKind::Const => match expr {
      Expression::StringLiteral(lit_str) => Some(ts_lit_type(
        allocator,
        TSLiteral::StringLiteral(Box::new_in(
          lit_str.as_ref().clone_in(allocator),
          allocator,
        )),
      )),
      Expression::BooleanLiteral(lit_bool) => Some(ts_lit_type(
        allocator,
        TSLiteral::BooleanLiteral(Box::new_in(
          lit_bool.as_ref().clone_in(allocator),
          allocator,
        )),
      )),
      Expression::NullLiteral(_) => {
        Some(ts_keyword_type(allocator, TSKeywordKind::Null))
      }
      Expression::NumericLiteral(lit_num) => Some(ts_lit_type(
        allocator,
        TSLiteral::NumericLiteral(Box::new_in(
          lit_num.as_ref().clone_in(allocator),
          allocator,
        )),
      )),
      Expression::BigIntLiteral(lit_bigint) => Some(ts_lit_type(
        allocator,
        TSLiteral::BigIntLiteral(Box::new_in(
          lit_bigint.as_ref().clone_in(allocator),
          allocator,
        )),
      )),
      Expression::RegExpLiteral(_) => Some(regex_type(allocator)),
      _ => None,
    },
    DeclMutabilityKind::Mutable => match expr {
      Expression::StringLiteral(_) => {
        Some(ts_keyword_type(allocator, TSKeywordKind::String))
      }
      Expression::BooleanLiteral(_) => {
        Some(ts_keyword_type(allocator, TSKeywordKind::Boolean))
      }
      Expression::NullLiteral(_) => {
        Some(ts_keyword_type(allocator, TSKeywordKind::Null))
      }
      Expression::NumericLiteral(_) => {
        Some(ts_keyword_type(allocator, TSKeywordKind::Number))
      }
      Expression::BigIntLiteral(_) => {
        Some(ts_keyword_type(allocator, TSKeywordKind::BigInt))
      }
      Expression::RegExpLiteral(_) => Some(regex_type(allocator)),
      _ => None,
    },
  }
}
