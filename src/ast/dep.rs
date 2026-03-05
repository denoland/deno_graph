use std::collections::HashMap;

use deno_ast::oxc::ast::ast::*;
use deno_ast::oxc::ast_visit::Visit;
use deno_ast::oxc::ast_visit::walk;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Span;

use crate::analysis::DynamicDependencyKind;
use crate::analysis::ImportAttribute;
use crate::analysis::ImportAttributes;
use crate::analysis::StaticDependencyKind;

pub fn analyze_program_dependencies(
  program: &Program<'_>,
  source_text: &str,
) -> Vec<DependencyDescriptor> {
  let mut v = DependencyCollector {
    comments: &program.comments,
    source_text,
    items: vec![],
  };
  v.visit_program(program);
  v.items
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DependencyComment {
  pub kind: DependencyCommentKind,
  pub span: Span,
  /// The byte offset where the comment content starts (after `//` or `/*`).
  pub content_start: u32,
  pub text: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DependencyCommentKind {
  Line,
  Block,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DependencyDescriptor {
  Static(StaticDependencyDescriptor),
  Dynamic(DynamicDependencyDescriptor),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StaticDependencyDescriptor {
  pub kind: StaticDependencyKind,
  /// Any leading comments associated with the dependency. This is used for
  /// further processing of supported pragma that impact the dependency.
  pub leading_comments: Vec<DependencyComment>,
  /// The range of the import/export statement.
  pub span: Span,
  /// The text specifier associated with the import/export statement.
  pub specifier: String,
  /// The range of the specifier.
  pub specifier_span: Span,
  /// Import attributes for this dependency.
  pub import_attributes: ImportAttributes,
  /// If this is an import for side effects only (ex. `import './load.js';`)
  pub is_side_effect: bool,
}

impl From<StaticDependencyDescriptor> for DependencyDescriptor {
  fn from(descriptor: StaticDependencyDescriptor) -> Self {
    DependencyDescriptor::Static(descriptor)
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DynamicDependencyDescriptor {
  /// Kind of dynamic dependency.
  pub kind: DynamicDependencyKind,
  /// Any leading comments associated with the dependency. This is used for
  /// further processing of supported pragma that impact the dependency.
  pub leading_comments: Vec<DependencyComment>,
  /// The range of the import/export statement.
  pub span: Span,
  /// The argument associated with the dynamic import
  pub argument: DynamicArgument,
  /// The range of the specifier.
  pub argument_span: Span,
  /// Import attributes for this dependency.
  pub import_attributes: ImportAttributes,
}

impl From<DynamicDependencyDescriptor> for DependencyDescriptor {
  fn from(descriptor: DynamicDependencyDescriptor) -> Self {
    DependencyDescriptor::Dynamic(descriptor)
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DynamicArgument {
  String(String),
  Template(Vec<DynamicTemplatePart>),
  /// An expression that could not be analyzed.
  Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DynamicTemplatePart {
  String(String),
  /// An expression that could not be analyzed.
  Expr,
}

struct DependencyCollector<'a, 'b> {
  comments: &'b deno_ast::oxc::allocator::Vec<'a, Comment>,
  source_text: &'b str,
  pub items: Vec<DependencyDescriptor>,
}

impl<'a, 'b> DependencyCollector<'a, 'b> {
  fn get_leading_comments(&self, span_start: u32) -> Vec<DependencyComment> {
    self
      .comments
      .iter()
      .filter(|c| c.is_leading() && c.attached_to == span_start)
      .map(|c| {
        let content_span = c.content_span();
        let text = self.source_text
          [content_span.start as usize..content_span.end as usize]
          .to_string();
        DependencyComment {
          kind: if c.is_line() {
            DependencyCommentKind::Line
          } else {
            DependencyCommentKind::Block
          },
          span: c.span,
          content_start: content_span.start,
          text,
        }
      })
      .collect()
  }

  fn is_require(&self, callee: &Expression<'a>) -> bool {
    matches!(callee, Expression::Identifier(ident) if ident.name == "require")
  }
}

impl<'a, 'b> Visit<'a> for DependencyCollector<'a, 'b> {
  fn visit_import_declaration(&mut self, node: &ImportDeclaration<'a>) {
    let leading_comments = self.get_leading_comments(node.span.start);
    let kind = if node.import_kind == ImportOrExportKind::Type {
      StaticDependencyKind::ImportType
    } else if node.phase == Some(ImportPhase::Source) {
      StaticDependencyKind::ImportSource
    } else if node.phase == Some(ImportPhase::Defer) {
      return;
    } else {
      StaticDependencyKind::Import
    };
    let is_side_effect = node
      .specifiers
      .as_ref()
      .map(|s| s.is_empty())
      .unwrap_or(true);
    self.items.push(
      StaticDependencyDescriptor {
        kind,
        leading_comments,
        span: node.span,
        specifier: node.source.value.to_string(),
        specifier_span: node.source.span,
        import_attributes: parse_import_attributes(node.with_clause.as_ref().map(|b| b.as_ref())),
        is_side_effect,
      }
      .into(),
    );
  }

  fn visit_export_named_declaration(&mut self, node: &ExportNamedDeclaration<'a>) {
    let Some(src) = &node.source else {
      // Check for `export import X = require("...")` pattern
      if let Some(Declaration::TSImportEqualsDeclaration(import_eq)) = &node.declaration {
        if let TSModuleReference::ExternalModuleReference(module) = &import_eq.module_reference {
          let leading_comments = self.get_leading_comments(node.span.start);
          let expr = &module.expression;
          let kind = if import_eq.import_kind == ImportOrExportKind::Type {
            StaticDependencyKind::ImportType
          } else {
            StaticDependencyKind::ExportEquals
          };
          self.items.push(
            StaticDependencyDescriptor {
              kind,
              leading_comments,
              span: node.span,
              specifier: expr.value.to_string(),
              specifier_span: expr.span,
              import_attributes: Default::default(),
              is_side_effect: false,
            }
            .into(),
          );
          return;
        }
      }
      walk::walk_export_named_declaration(self, node);
      return;
    };
    let leading_comments = self.get_leading_comments(node.span.start);
    let kind = if node.export_kind == ImportOrExportKind::Type {
      StaticDependencyKind::ExportType
    } else {
      StaticDependencyKind::Export
    };
    self.items.push(
      StaticDependencyDescriptor {
        kind,
        leading_comments,
        span: node.span,
        specifier: src.value.to_string(),
        specifier_span: src.span,
        import_attributes: parse_import_attributes(node.with_clause.as_ref().map(|b| b.as_ref())),
        is_side_effect: false,
      }
      .into(),
    );
  }

  fn visit_export_all_declaration(&mut self, node: &ExportAllDeclaration<'a>) {
    let leading_comments = self.get_leading_comments(node.span.start);
    let kind = if node.export_kind == ImportOrExportKind::Type {
      StaticDependencyKind::ExportType
    } else {
      StaticDependencyKind::Export
    };
    self.items.push(
      StaticDependencyDescriptor {
        kind,
        leading_comments,
        span: node.span,
        specifier: node.source.value.to_string(),
        specifier_span: node.source.span,
        import_attributes: parse_import_attributes(node.with_clause.as_ref().map(|b| b.as_ref())),
        is_side_effect: false,
      }
      .into(),
    );
  }

  fn visit_ts_import_type(&mut self, node: &TSImportType<'a>) {
    let leading_comments = self.get_leading_comments(node.span.start);
    self.items.push(
      StaticDependencyDescriptor {
        kind: StaticDependencyKind::ImportType,
        leading_comments,
        span: node.span,
        specifier: node.source.value.to_string(),
        specifier_span: node.source.span,
        import_attributes: node
          .options
          .as_ref()
          .map(|opts| parse_ts_import_type_options(opts))
          .unwrap_or_default(),
        is_side_effect: false,
      }
      .into(),
    );
    walk::walk_ts_import_type(self, node);
  }

  fn visit_import_expression(&mut self, node: &ImportExpression<'a>) {
    walk::walk_import_expression(self, node);

    let (argument, argument_span) = match &node.source {
      Expression::StringLiteral(s) => {
        (DynamicArgument::String(s.value.to_string()), s.span)
      }
      Expression::TemplateLiteral(tpl) => {
        if tpl.quasis.len() == 1 && tpl.expressions.is_empty() {
          let cooked = tpl.quasis[0]
            .value
            .cooked
            .as_ref()
            .map(|s| s.to_string())
            .unwrap_or_default();
          (DynamicArgument::String(cooked), tpl.span)
        } else {
          let mut parts =
            Vec::with_capacity(tpl.quasis.len() + tpl.expressions.len());
          for i in 0..tpl.quasis.len() {
            let cooked = tpl.quasis[i]
              .value
              .cooked
              .as_ref()
              .map(|s| s.to_string())
              .unwrap_or_default();
            if !cooked.is_empty() {
              parts.push(DynamicTemplatePart::String(cooked));
            }
            if tpl.expressions.get(i).is_some() {
              parts.push(DynamicTemplatePart::Expr);
            }
          }
          (DynamicArgument::Template(parts), tpl.span)
        }
      }
      Expression::BinaryExpression(bin) => {
        let mut parts = Vec::with_capacity(2);

        fn visit_bin(
          parts: &mut Vec<DynamicTemplatePart>,
          bin: &BinaryExpression<'_>,
        ) -> Result<(), ()> {
          if bin.operator != BinaryOperator::Addition {
            return Err(());
          }

          match &bin.left {
            Expression::BinaryExpression(left) => {
              visit_bin(parts, left)?;
            }
            Expression::StringLiteral(s) => {
              parts.push(DynamicTemplatePart::String(s.value.to_string()));
            }
            _ => {
              if parts.is_empty() {
                return Err(());
              }
              parts.push(DynamicTemplatePart::Expr);
            }
          };

          if let Expression::StringLiteral(s) = &bin.right {
            parts.push(DynamicTemplatePart::String(s.value.to_string()));
          } else {
            parts.push(DynamicTemplatePart::Expr);
          }

          Ok(())
        }

        if visit_bin(&mut parts, bin).is_ok() {
          (DynamicArgument::Template(parts), node.source.span())
        } else {
          (DynamicArgument::Expr, node.source.span())
        }
      }
      other => (DynamicArgument::Expr, other.span()),
    };

    let import_attributes = match &node.options {
      Some(expr) => parse_dynamic_import_attributes_from_expr(expr),
      None => ImportAttributes::None,
    };

    let kind = if node.phase == Some(ImportPhase::Source) {
      DynamicDependencyKind::ImportSource
    } else {
      DynamicDependencyKind::Import
    };

    let leading_comments = self.get_leading_comments(node.span.start);
    self.items.push(
      DynamicDependencyDescriptor {
        kind,
        leading_comments,
        span: node.span,
        argument,
        argument_span,
        import_attributes,
      }
      .into(),
    );
  }

  fn visit_call_expression(&mut self, node: &CallExpression<'a>) {
    walk::walk_call_expression(self, node);

    if !self.is_require(&node.callee) {
      return;
    }
    let Some(arg) = node.arguments.first() else {
      return;
    };
    if arg.is_spread() {
      return;
    }
    let arg_expr = &arg;

    let (argument, argument_span) = match arg_expr.as_expression().unwrap() {
      Expression::StringLiteral(s) => {
        (DynamicArgument::String(s.value.to_string()), s.span)
      }
      other => (DynamicArgument::Expr, other.span()),
    };
    let leading_comments = self.get_leading_comments(node.span.start);
    self.items.push(
      DynamicDependencyDescriptor {
        kind: DynamicDependencyKind::Require,
        leading_comments,
        span: node.span,
        argument,
        argument_span,
        import_attributes: Default::default(),
      }
      .into(),
    );
  }

  fn visit_ts_import_equals_declaration(
    &mut self,
    node: &TSImportEqualsDeclaration<'a>,
  ) {
    if let TSModuleReference::ExternalModuleReference(module) =
      &node.module_reference
    {
      let leading_comments = self.get_leading_comments(node.span.start);
      let expr = &module.expression;

      let kind = if node.import_kind == ImportOrExportKind::Type {
        StaticDependencyKind::ImportType
      } else {
        StaticDependencyKind::ImportEquals
      };

      self.items.push(
        StaticDependencyDescriptor {
          kind,
          leading_comments,
          span: node.span,
          specifier: expr.value.to_string(),
          specifier_span: expr.span,
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
      );
    }
  }

  fn visit_ts_module_declaration(&mut self, node: &TSModuleDeclaration<'a>) {
    if let TSModuleDeclarationName::StringLiteral(id_str) = &node.id {
      let value_str = id_str.value.as_str();
      if !value_str.contains('*')
        || value_str.starts_with("./")
        || value_str.starts_with("../")
        || value_str.starts_with('/')
      {
        let leading_comments = self.get_leading_comments(node.span.start);
        self.items.push(
          StaticDependencyDescriptor {
            kind: StaticDependencyKind::MaybeTsModuleAugmentation,
            leading_comments,
            span: id_str.span,
            specifier: id_str.value.to_string(),
            specifier_span: id_str.span,
            import_attributes: Default::default(),
            is_side_effect: false,
          }
          .into(),
        );
      }
    }
    walk::walk_ts_module_declaration(self, node);
  }
}

/// Parses import attributes from a WithClause.
fn parse_import_attributes(
  maybe_with: Option<&WithClause<'_>>,
) -> ImportAttributes {
  let Some(with) = maybe_with else {
    return ImportAttributes::None;
  };
  let mut import_attributes = HashMap::new();
  for attr in &with.with_entries {
    let key = match &attr.key {
      ImportAttributeKey::Identifier(ident) => ident.name.to_string(),
      ImportAttributeKey::StringLiteral(s) => s.value.to_string(),
    };
    let value_str = attr.value.value.as_str();
    import_attributes.insert(key, ImportAttribute::Known(value_str.to_string()));
  }
  ImportAttributes::Known(import_attributes)
}

/// Parses import attributes from a TSImportType options object.
/// The options object has the shape `{ with: { key: value, ... } }`.
fn parse_ts_import_type_options(
  object_lit: &ObjectExpression<'_>,
) -> ImportAttributes {
  let mut attributes_map = HashMap::new();
  let mut had_with_key = false;

  for prop in object_lit.properties.iter() {
    let ObjectPropertyKind::ObjectProperty(prop) = prop else {
      return ImportAttributes::Unknown;
    };
    if prop.kind != PropertyKind::Init || prop.computed {
      return ImportAttributes::Unknown;
    }
    let key = match &prop.key {
      PropertyKey::StringLiteral(s) => s.value.to_string(),
      PropertyKey::StaticIdentifier(ident) => ident.name.to_string(),
      _ => return ImportAttributes::Unknown,
    };
    if key == "with" || key == "assert" && !had_with_key {
      had_with_key = key == "with";
      let Expression::ObjectExpression(attrs_lit) = &prop.value else {
        return ImportAttributes::Unknown;
      };
      match parse_import_attributes_from_object_expr(attrs_lit) {
        ImportAttributes::Known(hash_map) => {
          attributes_map = hash_map;
        }
        value => return value,
      }
    }
  }

  if had_with_key || !attributes_map.is_empty() {
    ImportAttributes::Known(attributes_map)
  } else {
    ImportAttributes::None
  }
}

/// Parses import attributes from the options expression of a dynamic import.
fn parse_dynamic_import_attributes_from_expr(
  expr: &Expression<'_>,
) -> ImportAttributes {
  let Expression::ObjectExpression(object_lit) = expr else {
    return ImportAttributes::Unknown;
  };
  let mut attributes_map = HashMap::new();
  let mut had_attributes_key = false;
  let mut had_with_key = false;

  for prop in object_lit.properties.iter() {
    let ObjectPropertyKind::ObjectProperty(prop) = prop else {
      return ImportAttributes::Unknown;
    };
    if prop.kind != PropertyKind::Init || prop.computed {
      return ImportAttributes::Unknown;
    }
    let key = match &prop.key {
      PropertyKey::StringLiteral(s) => s.value.to_string(),
      PropertyKey::StaticIdentifier(ident) => ident.name.to_string(),
      _ => return ImportAttributes::Unknown,
    };
    if key == "with" || key == "assert" && !had_with_key {
      had_attributes_key = true;
      had_with_key = key == "with";
      let Expression::ObjectExpression(attrs_lit) = &prop.value else {
        return ImportAttributes::Unknown;
      };
      match parse_import_attributes_from_object_expr(attrs_lit) {
        ImportAttributes::Known(hash_map) => {
          attributes_map = hash_map;
        }
        value => return value,
      }
    }
  }

  if had_attributes_key {
    ImportAttributes::Known(attributes_map)
  } else {
    ImportAttributes::None
  }
}

fn parse_import_attributes_from_object_expr(
  attributes_lit: &ObjectExpression<'_>,
) -> ImportAttributes {
  let mut attributes_map =
    HashMap::with_capacity(attributes_lit.properties.len());

  for prop in attributes_lit.properties.iter() {
    let ObjectPropertyKind::ObjectProperty(prop) = prop else {
      return ImportAttributes::Unknown;
    };
    if prop.kind != PropertyKind::Init || prop.computed {
      return ImportAttributes::Unknown;
    }
    let key = match &prop.key {
      PropertyKey::StringLiteral(s) => s.value.to_string(),
      PropertyKey::StaticIdentifier(ident) => ident.name.to_string(),
      _ => return ImportAttributes::Unknown,
    };
    if let Expression::StringLiteral(s) = &prop.value {
      attributes_map.insert(
        key,
        ImportAttribute::Known(s.value.to_string()),
      );
    } else {
      attributes_map.insert(key, ImportAttribute::Unknown);
    }
  }
  ImportAttributes::Known(attributes_map)
}

#[cfg(test)]
mod tests {
  use deno_ast::oxc::span::Span;

  use pretty_assertions::assert_eq;

  use crate::ModuleSpecifier;

  use super::*;

  fn span(start: u32, end: u32) -> Span {
    Span::new(start, end)
  }

  fn helper(
    specifier: &str,
    source: &str,
  ) -> Vec<DependencyDescriptor> {
    let allocator = deno_ast::oxc::allocator::Allocator::default();
    let source = deno_ast::parse_module(
      &allocator,
      deno_ast::ParseParams {
        specifier: ModuleSpecifier::parse(specifier).unwrap(),
        text: source.into(),
        media_type: crate::MediaType::Tsx,
        capture_tokens: false,
        scope_analysis: false,
        maybe_source_type: None,
      },
    )
    .unwrap();
    analyze_program_dependencies(source.program(), source.text())
  }

  #[test]
  fn test_parsed_module_get_dependencies() {
    let source = r#"import * as bar from "./test.ts";
/** JSDoc */
import type { Foo } from "./foo.d.ts";
/// <reference foo="bar" />
export * as Buzz from "./buzz.ts";
// @some-pragma
/**
 * Foo
 */
export type { Fizz } from "./fizz.d.ts";
const { join } = require("path");
// dynamic
await import("./foo1.ts");
try {
    const foo = await import("./foo.ts");
} catch (e) {
    // pass
}
try {
    const foo = require("some_package");
} catch (e) {
    // pass
}
import foo2 = require("some_package_foo");
import type FooType = require('some_package_foo_type');
export import bar2 = require("some_package_bar");
const foo3 = require.resolve("some_package_resolve");
try {
    const foo4 = require.resolve("some_package_resolve_foo");
} catch (e) {
    // pass
}
      "#;
    let dependencies = helper("file:///test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(0, 33),
          specifier: "./test.ts".to_string(),
          specifier_span: span(21, 32),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: vec![DependencyComment {
            kind: DependencyCommentKind::Block,
            text: r#"* JSDoc "#.to_string(),
            span: span(34, 46),
            content_start: 36,
          }],
          span: span(47, 85),
          specifier: "./foo.d.ts".to_string(),
          specifier_span: span(72, 84),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Export,
          leading_comments: vec![DependencyComment {
            kind: DependencyCommentKind::Line,
            text: r#"/ <reference foo="bar" />"#.to_string(),
            span: span(86, 113),
            content_start: 88,
          }],
          span: span(114, 148),
          specifier: "./buzz.ts".to_string(),
          specifier_span: span(136, 147),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ExportType,
          leading_comments: vec![
            DependencyComment {
              kind: DependencyCommentKind::Line,
              text: r#" @some-pragma"#.to_string(),
              span: span(149, 164),
              content_start: 151,
            },
            DependencyComment {
              kind: DependencyCommentKind::Block,
              text: "*\n * Foo\n ".to_string(),
              span: span(165, 179),
              content_start: 167,
            }
          ],
          span: span(180, 220),
          specifier: "./fizz.d.ts".to_string(),
          specifier_span: span(206, 219),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          span: span(238, 253),
          argument: DynamicArgument::String("path".to_string()),
          argument_span: span(246, 252),
          import_attributes: Default::default(),
          kind: DynamicDependencyKind::Require,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(272, 291),
          argument: DynamicArgument::String("./foo1.ts".to_string()),
          argument_span: span(279, 290),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(321, 339),
          argument: DynamicArgument::String("./foo.ts".to_string()),
          argument_span: span(328, 338),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Require,
          leading_comments: Vec::new(),
          span: span(391, 414),
          argument: DynamicArgument::String("some_package".to_string()),
          argument_span: span(399, 413),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportEquals,
          leading_comments: Vec::new(),
          span: span(444, 486),
          specifier: "some_package_foo".to_string(),
          specifier_span: span(466, 484),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: Vec::new(),
          span: span(487, 542),
          specifier: "some_package_foo_type".to_string(),
          specifier_span: span(517, 540),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ExportEquals,
          leading_comments: Vec::new(),
          span: span(543, 592),
          specifier: "some_package_bar".to_string(),
          specifier_span: span(572, 590),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
      ]
    );
  }

  #[test]
  fn test_import_attributes() {
    let source = r#"import * as bar from "./test.ts" with { "type": "typescript" };
export * from "./test.ts" with { "type": "typescript" };
export { bar } from "./test.json" with { "type": "json" };
import foo from "./foo.json" with { type: "json" };
const fizz = await import("./fizz.json", { "with": { type: "json" } });
const buzz = await import("./buzz.json", { with: { "type": "json" } });
const d1 = await import("./d1.json");
const d2 = await import("./d2.json", {});
const d3 = await import("./d3.json", bar);
const d4 = await import("./d4.json", { with: {} });
const d5 = await import("./d5.json", { with: bar });
const d6 = await import("./d6.json", { with: {}, ...bar });
const d7 = await import("./d7.json", { with: {}, ["assert"]: "bad" });
const d8 = await import("./d8.json", { with: { type: bar } });
const d9 = await import("./d9.json", { with: { type: "json", ...bar } });
const d10 = await import("./d10.json", { with: { type: "json", ["type"]: "bad" } });
      "#;
    let dependencies = helper("file:///test.ts", source);
    let expected_attributes1 = ImportAttributes::Known({
      let mut map = HashMap::new();
      map.insert(
        "type".to_string(),
        ImportAttribute::Known("typescript".to_string()),
      );
      map
    });
    let expected_attributes2 = ImportAttributes::Known({
      let mut map = HashMap::new();
      map.insert(
        "type".to_string(),
        ImportAttribute::Known("json".to_string()),
      );
      map
    });
    let dynamic_expected_attributes2 = ImportAttributes::Known({
      let mut map = HashMap::new();
      map.insert(
        "type".to_string(),
        ImportAttribute::Known("json".to_string()),
      );
      map
    });
    assert_eq!(
      dependencies,
      vec![
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(0, 63),
          specifier: "./test.ts".to_string(),
          specifier_span: span(21, 32),
          import_attributes: expected_attributes1.clone(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Export,
          leading_comments: Vec::new(),
          span: span(64, 120),
          specifier: "./test.ts".to_string(),
          specifier_span: span(78, 89),
          import_attributes: expected_attributes1,
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Export,
          leading_comments: Vec::new(),
          span: span(121, 179),
          specifier: "./test.json".to_string(),
          specifier_span: span(141, 154),
          import_attributes: expected_attributes2.clone(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(180, 231),
          specifier: "./foo.json".to_string(),
          specifier_span: span(196, 208),
          import_attributes: expected_attributes2,
          is_side_effect: false,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(251, 302),
          argument: DynamicArgument::String("./fizz.json".to_string()),
          argument_span: span(258, 271),
          import_attributes: dynamic_expected_attributes2.clone(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(323, 374),
          argument: DynamicArgument::String("./buzz.json".to_string()),
          argument_span: span(330, 343),
          import_attributes: dynamic_expected_attributes2,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(393, 412),
          argument: DynamicArgument::String("./d1.json".to_string()),
          argument_span: span(400, 411),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(431, 454),
          argument: DynamicArgument::String("./d2.json".to_string()),
          argument_span: span(438, 449),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(473, 497),
          argument: DynamicArgument::String("./d3.json".to_string()),
          argument_span: span(480, 491),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(516, 549),
          argument: DynamicArgument::String("./d4.json".to_string()),
          argument_span: span(523, 534),
          import_attributes: ImportAttributes::Known(HashMap::new()),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(568, 602),
          argument: DynamicArgument::String("./d5.json".to_string()),
          argument_span: span(575, 586),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(621, 662),
          argument: DynamicArgument::String("./d6.json".to_string()),
          argument_span: span(628, 639),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(681, 733),
          argument: DynamicArgument::String("./d7.json".to_string()),
          argument_span: span(688, 699),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(752, 796),
          argument: DynamicArgument::String("./d8.json".to_string()),
          argument_span: span(759, 770),
          import_attributes: ImportAttributes::Known({
            let mut map = HashMap::new();
            map.insert("type".to_string(), ImportAttribute::Unknown);
            map
          }),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(815, 870),
          argument: DynamicArgument::String("./d9.json".to_string()),
          argument_span: span(822, 833),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(890, 955),
          argument: DynamicArgument::String("./d10.json".to_string()),
          argument_span: span(897, 909),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
      ]
    );
  }

  #[test]
  fn test_dynamic_imports() {
    let source = r#"const d1 = await import(`./d1.json`);
const d2 = await import(`${value}`);
const d3 = await import(`./test/${value}`);
const d4 = await import(`${value}/test`);
const d5 = await import(`${value}${value2}`);
const d6 = await import(`${value}/test/${value2}`);
const d7 = await import(`./${value}/test/${value2}/`);
const d8 = await import("./foo/" + value);
const d9 = await import("./foo/" + value + ".ts");
const d10 = await import(value + ".ts");
const d11 = await import("./foo/" - value);
const d12 = await import(expr);
"#;
    let dependencies = helper("file:///test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(17, 36),
          argument: DynamicArgument::String("./d1.json".to_string()),
          argument_span: span(24, 35),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(55, 73),
          argument: DynamicArgument::Template(vec![DynamicTemplatePart::Expr]),
          argument_span: span(62, 72),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(92, 117),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String("./test/".to_string()),
            DynamicTemplatePart::Expr,
          ]),
          argument_span: span(99, 116),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(136, 159),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String("/test".to_string()),
          ]),
          argument_span: span(143, 158),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(178, 205),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::Expr,
          ]),
          argument_span: span(185, 204),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(224, 257),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String("/test/".to_string()),
            DynamicTemplatePart::Expr,
          ]),
          argument_span: span(231, 256),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(276, 312),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String("./".to_string()),
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String("/test/".to_string()),
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String("/".to_string()),
          ]),
          argument_span: span(283, 311),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(331, 355),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String("./foo/".to_string()),
            DynamicTemplatePart::Expr,
          ]),
          argument_span: span(338, 354),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(374, 406),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String("./foo/".to_string()),
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String(".ts".to_string()),
          ]),
          argument_span: span(381, 405),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(426, 447),
          argument: DynamicArgument::Expr,
          argument_span: span(433, 446),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(467, 491),
          argument: DynamicArgument::Expr,
          argument_span: span(474, 490),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: span(511, 523),
          argument: DynamicArgument::Expr,
          argument_span: span(518, 522),
          import_attributes: ImportAttributes::None,
        }
        .into(),
      ]
    );
  }

  #[test]
  fn ts_import_object_lit_property() {
    let source = r#"
export declare const SomeValue: typeof Core & import("./a.d.ts").Constructor<{
    paginate: import("./b.d.ts").PaginateInterface;
} & import("./c.d.ts", { with: { "resolution-mode": "import" } }).RestEndpointMethods>;
"#;
    let dependencies = helper("file:///test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: Vec::new(),
          span: span(47, 218),
          specifier: "./a.d.ts".to_string(),
          specifier_span: span(54, 64),
          import_attributes: ImportAttributes::None,
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: Vec::new(),
          span: span(94, 130),
          specifier: "./b.d.ts".to_string(),
          specifier_span: span(101, 111),
          import_attributes: ImportAttributes::None,
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: Vec::new(),
          span: span(136, 217),
          specifier: "./c.d.ts".to_string(),
          specifier_span: span(143, 153),
          import_attributes: ImportAttributes::Known(HashMap::from([(
            "resolution-mode".to_string(),
            ImportAttribute::Known("import".to_string())
          )])),
          is_side_effect: false,
        }
        .into()
      ]
    );
  }

  #[test]
  fn test_declare_module() {
    let source = r#"
      declare module "*.css" {}
      declare module "./*.css" {} // not a wildcard
      declare module "../*.css" {} // not a wildcard
      declare module "/*.css" {} // not a wildcard
      // @some-pragma
      declare module "foo" {}
    "#;
    let dependencies = helper("file:///test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::MaybeTsModuleAugmentation,
          leading_comments: Vec::new(),
          span: span(54, 63),
          specifier: "./*.css".to_string(),
          specifier_span: span(54, 63),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::MaybeTsModuleAugmentation,
          leading_comments: Vec::new(),
          span: span(106, 116),
          specifier: "../*.css".to_string(),
          specifier_span: span(106, 116),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::MaybeTsModuleAugmentation,
          leading_comments: Vec::new(),
          span: span(159, 167),
          specifier: "/*.css".to_string(),
          specifier_span: span(159, 167),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::MaybeTsModuleAugmentation,
          leading_comments: vec![DependencyComment {
            kind: DependencyCommentKind::Line,
            text: r#" @some-pragma"#.to_string(),
            span: span(195, 210),
            content_start: 197,
          },],
          span: span(232, 237),
          specifier: "foo".to_string(),
          specifier_span: span(232, 237),
          import_attributes: Default::default(),
          is_side_effect: false,
        }
        .into(),
      ],
    );
  }

}
