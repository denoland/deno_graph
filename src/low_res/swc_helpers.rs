use deno_ast::swc::ast::ReturnStmt;
use deno_ast::swc::ast::Stmt;

pub fn get_return_stmt_with_arg_from_function(
  func: &deno_ast::swc::ast::Function,
) -> Option<&ReturnStmt> {
  let body = func.body.as_ref()?;
  let stmt = get_return_stmt_with_arg_from_stmts(&body.stmts)?;
  debug_assert!(stmt.arg.is_some());
  Some(stmt)
}

fn get_return_stmt_with_arg_from_stmts(stmts: &[Stmt]) -> Option<&ReturnStmt> {
  for stmt in stmts {
    if let Some(return_stmt) = get_return_stmt_with_arg_from_stmt(stmt) {
      return Some(return_stmt);
    }
  }

  None
}

fn get_return_stmt_with_arg_from_stmt(stmt: &Stmt) -> Option<&ReturnStmt> {
  match stmt {
    Stmt::Block(n) => get_return_stmt_with_arg_from_stmts(&n.stmts),
    Stmt::With(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::Return(n) => {
      if n.arg.is_none() {
        None
      } else {
        Some(n)
      }
    }
    Stmt::Labeled(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::If(n) => get_return_stmt_with_arg_from_stmt(&n.cons),
    Stmt::Switch(n) => n
      .cases
      .iter()
      .find_map(|case| get_return_stmt_with_arg_from_stmts(&case.cons)),
    Stmt::Try(n) => get_return_stmt_with_arg_from_stmts(&n.block.stmts)
      .or_else(|| {
        n.handler
          .as_ref()
          .and_then(|h| get_return_stmt_with_arg_from_stmts(&h.body.stmts))
      })
      .or_else(|| {
        n.finalizer
          .as_ref()
          .and_then(|f| get_return_stmt_with_arg_from_stmts(&f.stmts))
      }),
    Stmt::While(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::DoWhile(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::For(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::ForIn(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::ForOf(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::Break(_)
    | Stmt::Continue(_)
    | Stmt::Throw(_)
    | Stmt::Debugger(_)
    | Stmt::Decl(_)
    | Stmt::Expr(_)
    | Stmt::Empty(_) => None,
  }
}
