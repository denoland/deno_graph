use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::ReturnStmt;
use deno_ast::swc::ast::Stmt;
use deno_ast::swc::ast::TsKeywordType;
use deno_ast::swc::ast::TsKeywordTypeKind;
use deno_ast::swc::ast::TsType;
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
      .map(|case| get_return_stmts_with_arg_from_stmts(&case.cons))
      .flatten()
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
