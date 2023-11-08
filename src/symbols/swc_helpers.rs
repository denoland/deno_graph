// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::ast::Id;
use deno_ast::swc::ast::TsEntityName;
use deno_ast::swc::ast::TsQualifiedName;

pub fn ts_entity_name_to_parts(
  entity_name: &TsEntityName,
) -> (Id, Vec<String>) {
  match entity_name {
    TsEntityName::TsQualifiedName(qualified_name) => {
      ts_qualified_name_parts(qualified_name)
    }
    TsEntityName::Ident(ident) => (ident.to_id(), Vec::new()),
  }
}

pub fn ts_qualified_name_parts(
  mut qualified_name: &TsQualifiedName,
) -> (Id, Vec<String>) {
  let mut parts = Vec::new();
  loop {
    parts.push(qualified_name.right.sym.to_string());
    match &qualified_name.left {
      TsEntityName::TsQualifiedName(n) => {
        qualified_name = n;
      }
      TsEntityName::Ident(n) => {
        parts.reverse();
        return (n.to_id(), parts);
      }
    }
  }
}
