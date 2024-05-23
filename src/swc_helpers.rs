// Copyright 2018-2024 the Deno authors. MIT license.

use std::ops::ControlFlow;

use deno_ast::swc::ast::ReturnStmt;
use deno_ast::swc::ast::Stmt;

pub enum FunctionKind {
  /// function declarations, class method declarations (both class decl and class expr)
  DeclarationLike,
  /// function expressions, arrow functions, object method shorthand properties
  ExpressionLike,
  /// getters, both on classes and object literals
  Getter,
  /// setters, both on classes and object literals
  Setter,
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
  Single(ReturnStmt),
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
    analyze_return_stmts_from_stmts(&body.stmts, &mut analysis);
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
          *analysis = ReturnStatementAnalysis::Single(n.clone());
        }
        (_, ReturnStatementAnalysis::Single(_)) => {
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
