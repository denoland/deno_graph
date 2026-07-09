// Copyright 2018-2024 the Deno authors. MIT license.

use deno_ast::oxc::ast::ast::BindingIdentifier;
use deno_ast::oxc::ast::ast::IdentifierName;
use deno_ast::oxc::ast::ast::IdentifierReference;
use deno_ast::oxc::ast::ast::TSQualifiedName;
use deno_ast::oxc::ast::ast::TSTypeName;
use deno_ast::oxc::semantic::Scoping;

/// In OXC, the equivalent of SWC's `Id` is `(String, usize)`. The second
/// component is a scope discriminator: SWC uses a `SyntaxContext`, and the
/// OXC equivalent is the resolved symbol id from semantic analysis. `0` means
/// "unresolved" (a global, or a context-free property/member name) and never
/// collides with a real symbol because real symbols are stored as
/// `symbol_id + 1`.
pub type Id = (String, usize);

/// Discriminator value used when an identifier has no resolved symbol.
const UNRESOLVED_CTXT: usize = 0;

fn symbol_ctxt(symbol_id: deno_ast::oxc::semantic::SymbolId) -> usize {
  symbol_id.index() + 1
}

/// Trait to convert various identifier types to an `Id`. `scoping` is the
/// semantic analysis result; when present it disambiguates same-named symbols
/// across scopes (the SWC `SyntaxContext` equivalent).
pub trait ToId {
  fn to_id(&self, scoping: Option<&Scoping>) -> Id;
}

impl ToId for BindingIdentifier<'_> {
  fn to_id(&self, _scoping: Option<&Scoping>) -> Id {
    // A binding's resolved symbol id is stored directly on the node by
    // semantic analysis.
    let ctxt = self
      .symbol_id
      .get()
      .map(symbol_ctxt)
      .unwrap_or(UNRESOLVED_CTXT);
    (self.name.to_string(), ctxt)
  }
}

impl ToId for IdentifierReference<'_> {
  fn to_id(&self, scoping: Option<&Scoping>) -> Id {
    // A reference resolves to a symbol via the scoping's reference table.
    let ctxt = self
      .reference_id
      .get()
      .zip(scoping)
      .and_then(|(reference_id, scoping)| {
        scoping.get_reference(reference_id).symbol_id()
      })
      .map(symbol_ctxt)
      .unwrap_or(UNRESOLVED_CTXT);
    (self.name.to_string(), ctxt)
  }
}

impl ToId for IdentifierName<'_> {
  fn to_id(&self, _scoping: Option<&Scoping>) -> Id {
    // A property/member name is resolved structurally, not by scope.
    (self.name.to_string(), UNRESOLVED_CTXT)
  }
}

pub fn ts_entity_name_to_parts(
  entity_name: &TSTypeName,
  scoping: Option<&Scoping>,
) -> (Id, Vec<String>) {
  match entity_name {
    TSTypeName::QualifiedName(qualified_name) => {
      ts_qualified_name_parts(qualified_name, scoping)
    }
    TSTypeName::IdentifierReference(ident) => {
      (ident.to_id(scoping), Vec::new())
    }
    TSTypeName::ThisExpression(_) => {
      (("this".to_string(), UNRESOLVED_CTXT), Vec::new())
    }
  }
}

pub fn ts_qualified_name_parts(
  mut qualified_name: &TSQualifiedName,
  scoping: Option<&Scoping>,
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
        return (n.to_id(scoping), parts);
      }
      TSTypeName::ThisExpression(_) => {
        parts.reverse();
        return (("this".to_string(), UNRESOLVED_CTXT), parts);
      }
    }
  }
}
