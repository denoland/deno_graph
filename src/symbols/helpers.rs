// Copyright 2018-2024 the Deno authors. MIT license.

use deno_ast::oxc::ast::ast::BindingIdentifier;
use deno_ast::oxc::ast::ast::IdentifierReference;
use deno_ast::oxc::ast::ast::IdentifierName;
use deno_ast::oxc::ast::ast::TSQualifiedName;
use deno_ast::oxc::ast::ast::TSTypeName;

/// In OXC, the equivalent of SWC's `Id` is `(String, usize)`.
pub type Id = (String, usize);

/// Trait to convert various identifier types to an `Id`.
pub trait ToId {
  fn to_id(&self) -> Id;
}

impl ToId for BindingIdentifier<'_> {
  fn to_id(&self) -> Id {
    (self.name.to_string(), 0)
  }
}

impl ToId for IdentifierReference<'_> {
  fn to_id(&self) -> Id {
    (self.name.to_string(), 0)
  }
}

impl ToId for IdentifierName<'_> {
  fn to_id(&self) -> Id {
    (self.name.to_string(), 0)
  }
}

pub fn ts_entity_name_to_parts(
  entity_name: &TSTypeName,
) -> (Id, Vec<String>) {
  match entity_name {
    TSTypeName::QualifiedName(qualified_name) => {
      ts_qualified_name_parts(qualified_name)
    }
    TSTypeName::IdentifierReference(ident) => {
      ((ident.name.to_string(), 0), Vec::new())
    }
    TSTypeName::ThisExpression(_) => {
      (("this".to_string(), 0), Vec::new())
    }
  }
}

pub fn ts_qualified_name_parts(
  mut qualified_name: &TSQualifiedName,
) -> (Id, Vec<String>) {
  let mut parts = Vec::new();
  loop {
    parts.push(qualified_name.right.name.to_string());
    match &qualified_name.left {
      TSTypeName::QualifiedName(n) => {
        qualified_name = n;
      }
      TSTypeName::IdentifierReference(n) => {
        parts.reverse();
        return ((n.name.to_string(), 0), parts);
      }
      TSTypeName::ThisExpression(_) => {
        parts.reverse();
        return (("this".to_string(), 0), parts);
      }
    }
  }
}
