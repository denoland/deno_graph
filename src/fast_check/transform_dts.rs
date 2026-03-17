use deno_ast::ModuleSpecifier;
use deno_ast::SourceTextInfo;
use deno_ast::oxc::allocator::Allocator;
use deno_ast::oxc::allocator::Box;
use deno_ast::oxc::allocator::CloneIn;
use deno_ast::oxc::allocator::Vec;
use deno_ast::oxc::ast::ast::*;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Ident;
use deno_ast::oxc::span::SPAN;
use deno_ast::oxc::span::Span;
use deno_ast::oxc::syntax::node::NodeId;
use std::cell::Cell;

use super::FastCheckDiagnosticRange;
use super::range_finder::ModulePublicRanges;
use super::helpers::DeclMutabilityKind;
use super::helpers::any_type_ann;
use super::helpers::maybe_lit_to_ts_type;
use super::helpers::new_ident;
use super::helpers::ts_keyword_type;
use super::helpers::TSKeywordKind;
use super::helpers::ts_readonly;
use super::helpers::ts_tuple_element;
use super::helpers::type_ann;


#[derive(Debug, Clone, thiserror::Error)]
pub enum FastCheckDtsDiagnostic {
  #[error("unable to infer type from expression or declaration")]
  UnableToInferType { range: FastCheckDiagnosticRange },
  #[error("unable to infer type, falling back to any type")]
  UnableToInferTypeFallbackAny { range: FastCheckDiagnosticRange },
  #[error("unable to infer type from object property, skipping")]
  UnableToInferTypeFromProp { range: FastCheckDiagnosticRange },
  #[error("unable to infer type from spread, skipping")]
  UnableToInferTypeFromSpread { range: FastCheckDiagnosticRange },
  #[error("cannot infer type from using, skipping")]
  UnsupportedUsing { range: FastCheckDiagnosticRange },
}

impl FastCheckDtsDiagnostic {
  pub fn specifier(&self) -> &ModuleSpecifier {
    match self {
      FastCheckDtsDiagnostic::UnableToInferType { range } => &range.specifier,
      FastCheckDtsDiagnostic::UnableToInferTypeFallbackAny { range } => {
        &range.specifier
      }
      FastCheckDtsDiagnostic::UnableToInferTypeFromProp { range } => {
        &range.specifier
      }
      FastCheckDtsDiagnostic::UnableToInferTypeFromSpread { range } => {
        &range.specifier
      }
      FastCheckDtsDiagnostic::UnsupportedUsing { range } => &range.specifier,
    }
  }

  pub fn range(&self) -> &FastCheckDiagnosticRange {
    match self {
      FastCheckDtsDiagnostic::UnableToInferType { range } => range,
      FastCheckDtsDiagnostic::UnableToInferTypeFallbackAny { range } => range,
      FastCheckDtsDiagnostic::UnableToInferTypeFromProp { range } => range,
      FastCheckDtsDiagnostic::UnableToInferTypeFromSpread { range } => range,
      FastCheckDtsDiagnostic::UnsupportedUsing { range } => range,
    }
  }
}

pub struct FastCheckDtsTransformer<'a, 'b> {
  allocator: &'a Allocator,
  id_counter: usize,
  text_info: &'b SourceTextInfo,
  public_ranges: &'b ModulePublicRanges,
  pub diagnostics: std::vec::Vec<FastCheckDtsDiagnostic>,
  specifier: &'b ModuleSpecifier,
  is_top_level: bool,
}

impl<'a, 'b> FastCheckDtsTransformer<'a, 'b> {
  pub fn new(
    allocator: &'a Allocator,
    text_info: &'b SourceTextInfo,
    public_ranges: &'b ModulePublicRanges,
    specifier: &'b ModuleSpecifier,
  ) -> Self {
    Self {
      allocator,
      id_counter: 0,
      text_info,
      specifier,
      public_ranges,
      diagnostics: vec![],
      is_top_level: true,
    }
  }

  fn gen_unique_name(&mut self) -> String {
    self.id_counter += 1;
    format!("_dts_{}", self.id_counter)
  }

  fn mark_diagnostic(&mut self, diagnostic: FastCheckDtsDiagnostic) {
    self.diagnostics.push(diagnostic)
  }

  fn source_range_to_range(
    &self,
    range: Span,
  ) -> FastCheckDiagnosticRange {
    FastCheckDiagnosticRange {
      specifier: self.specifier.clone(),
      text_info: self.text_info.clone(),
      range,
    }
  }

  fn mark_diagnostic_unable_to_infer(&mut self, range: Span) {
    self.mark_diagnostic(FastCheckDtsDiagnostic::UnableToInferType {
      range: self.source_range_to_range(range),
    })
  }

  fn mark_diagnostic_any_fallback(&mut self, range: Span) {
    self.mark_diagnostic(FastCheckDtsDiagnostic::UnableToInferTypeFallbackAny {
      range: self.source_range_to_range(range),
    })
  }

  fn mark_diagnostic_unsupported_prop(&mut self, range: Span) {
    self.mark_diagnostic(FastCheckDtsDiagnostic::UnableToInferTypeFromProp {
      range: self.source_range_to_range(range),
    })
  }

  pub fn transform(&mut self, mut program: Program<'a>) -> Program<'a> {
    self.is_top_level = true;
    let body = std::mem::replace(&mut program.body, Vec::new_in(self.allocator));
    program.body = self.transform_stmts(body);
    program
  }

  fn transform_stmts(
    &mut self,
    body: Vec<'a, Statement<'a>>,
  ) -> Vec<'a, Statement<'a>> {
    let mut new_items: Vec<'a, Statement<'a>> = Vec::new_in(self.allocator);

    for item in body {
      match item {
        Statement::ImportDeclaration(_) => {
          new_items.push(item);
        }
        Statement::ExportNamedDeclaration(export_decl) => {
          if let Some(ref decl) = export_decl.declaration {
            if let Declaration::FunctionDeclaration(_) = decl {
              if self
                .public_ranges
                .is_impl_with_overloads(&export_decl.span)
              {
                continue; // skip implementation signature
              }
            }
          }

          if let Some(ref decl) = export_decl.declaration {
            match self.decl_to_type_decl(decl.clone_in(self.allocator)) {
              Some(new_decl) => {
                new_items.push(Statement::ExportNamedDeclaration(
                  Box::new_in(
                    ExportNamedDeclaration {
                      node_id: Cell::new(NodeId::DUMMY),
                      declaration: Some(new_decl),
                      span: export_decl.span,
                      specifiers: Vec::new_in(self.allocator),
                      source: None,
                      export_kind: ImportOrExportKind::Value,
                      with_clause: None,
                    },
                    self.allocator,
                  ),
                ));
              }
              _ => self.mark_diagnostic(
                FastCheckDtsDiagnostic::UnableToInferType {
                  range: self.source_range_to_range(export_decl.span),
                },
              ),
            }
          } else {
            // Re-export like `export { foo } from "bar"` - keep as is
            new_items.push(Statement::ExportNamedDeclaration(export_decl));
          }
        }
        Statement::ExportDefaultDeclaration(mut export_decl) => {
          match &export_decl.declaration {
            ExportDefaultDeclarationKind::ClassDeclaration(_) => {
              let mut cloned = export_decl.clone_in(self.allocator);
              if let ExportDefaultDeclarationKind::ClassDeclaration(ref mut class_expr) = cloned.declaration {
                class_expr.body = self.class_body_to_type(&class_expr.body);
              }
              new_items.push(Statement::ExportDefaultDeclaration(cloned));
            }
            ExportDefaultDeclarationKind::FunctionDeclaration(_) => {
              if self
                .public_ranges
                .is_impl_with_overloads(&export_decl.span)
              {
                continue; // skip implementation signature
              }

              let mut cloned = export_decl.clone_in(self.allocator);
              if let ExportDefaultDeclarationKind::FunctionDeclaration(ref mut fn_expr) = cloned.declaration {
                fn_expr.body = None;
              }
              new_items.push(Statement::ExportDefaultDeclaration(cloned));
            }
            ExportDefaultDeclarationKind::TSInterfaceDeclaration(_) => {
              new_items.push(Statement::ExportDefaultDeclaration(export_decl));
            }
            _ => {
              // Expression variant - handle like ExportDefaultExpr
              let expr_span = export_decl.declaration.span();
              let decl_cloned = export_decl.declaration.clone_in(self.allocator);
              let expr = match decl_cloned {
                ExportDefaultDeclarationKind::BooleanLiteral(_)
                | ExportDefaultDeclarationKind::NullLiteral(_)
                | ExportDefaultDeclarationKind::NumericLiteral(_)
                | ExportDefaultDeclarationKind::BigIntLiteral(_)
                | ExportDefaultDeclarationKind::RegExpLiteral(_)
                | ExportDefaultDeclarationKind::StringLiteral(_)
                | ExportDefaultDeclarationKind::TemplateLiteral(_)
                | ExportDefaultDeclarationKind::Identifier(_)
                | ExportDefaultDeclarationKind::MetaProperty(_)
                | ExportDefaultDeclarationKind::Super(_)
                | ExportDefaultDeclarationKind::ArrayExpression(_)
                | ExportDefaultDeclarationKind::ArrowFunctionExpression(_)
                | ExportDefaultDeclarationKind::AssignmentExpression(_)
                | ExportDefaultDeclarationKind::AwaitExpression(_)
                | ExportDefaultDeclarationKind::BinaryExpression(_)
                | ExportDefaultDeclarationKind::CallExpression(_)
                | ExportDefaultDeclarationKind::ChainExpression(_)
                | ExportDefaultDeclarationKind::ClassExpression(_)
                | ExportDefaultDeclarationKind::ConditionalExpression(_)
                | ExportDefaultDeclarationKind::FunctionExpression(_)
                | ExportDefaultDeclarationKind::ImportExpression(_)
                | ExportDefaultDeclarationKind::LogicalExpression(_)
                | ExportDefaultDeclarationKind::NewExpression(_)
                | ExportDefaultDeclarationKind::ObjectExpression(_)
                | ExportDefaultDeclarationKind::ParenthesizedExpression(_)
                | ExportDefaultDeclarationKind::SequenceExpression(_)
                | ExportDefaultDeclarationKind::TaggedTemplateExpression(_)
                | ExportDefaultDeclarationKind::ThisExpression(_)
                | ExportDefaultDeclarationKind::UnaryExpression(_)
                | ExportDefaultDeclarationKind::UpdateExpression(_)
                | ExportDefaultDeclarationKind::YieldExpression(_)
                | ExportDefaultDeclarationKind::PrivateInExpression(_)
                | ExportDefaultDeclarationKind::JSXElement(_)
                | ExportDefaultDeclarationKind::JSXFragment(_)
                | ExportDefaultDeclarationKind::TSAsExpression(_)
                | ExportDefaultDeclarationKind::TSSatisfiesExpression(_)
                | ExportDefaultDeclarationKind::TSTypeAssertion(_)
                | ExportDefaultDeclarationKind::TSNonNullExpression(_)
                | ExportDefaultDeclarationKind::TSInstantiationExpression(_)
                | ExportDefaultDeclarationKind::V8IntrinsicExpression(_)
                | ExportDefaultDeclarationKind::ComputedMemberExpression(_)
                | ExportDefaultDeclarationKind::StaticMemberExpression(_)
                | ExportDefaultDeclarationKind::PrivateFieldExpression(_) => {
                  // Convert ExportDefaultDeclarationKind to Expression
                  // We need to extract the expression
                  export_default_decl_kind_to_expr(self.allocator, decl_cloned)
                }
                _ => unreachable!(),
              };

              if let Some(expr) = expr {
                let name = self.gen_unique_name();
                let name_ident = new_ident(self.allocator, &name);
                let type_ann_opt = self
                  .expr_to_ts_type(&expr, false, true)
                  .map(|t| type_ann(self.allocator, t));

                if let Some(ta) = type_ann_opt {
                  // Create: declare const _dts_N: Type;
                  let mut declarations = Vec::new_in(self.allocator);
                  declarations.push(VariableDeclarator {
                    node_id: Cell::new(NodeId::DUMMY),
                    span: SPAN,
                    kind: VariableDeclarationKind::Const,
                    id: BindingPattern::BindingIdentifier(Box::new_in(
                      name_ident.clone_in(self.allocator),
                      self.allocator,
                    )),
                    type_annotation: Some(ta),
                    init: None,
                    definite: false,
                  });
                  new_items.push(Statement::VariableDeclaration(
                    Box::new_in(
                      VariableDeclaration {
                        node_id: Cell::new(NodeId::DUMMY),
                        span: SPAN,
                        kind: VariableDeclarationKind::Const,
                        declarations,
                        declare: true,
                      },
                      self.allocator,
                    ),
                  ));

                  // Create: export default _dts_N;
                  new_items.push(Statement::ExportDefaultDeclaration(
                    Box::new_in(
                      ExportDefaultDeclaration {
                        node_id: Cell::new(NodeId::DUMMY),
                        span: export_decl.span,
                        declaration: ExportDefaultDeclarationKind::Identifier(
                          Box::new_in(
                            IdentifierReference {
                              node_id: Cell::new(NodeId::DUMMY),
                              span: SPAN,
                              name: Ident::from(
                                self.allocator.alloc_str(&name),
                              ),
                              reference_id: Cell::new(None),
                            },
                            self.allocator,
                          ),
                        ),
                      },
                      self.allocator,
                    ),
                  ));
                } else {
                  // Re-emit with original expression
                  let kind = expr_to_export_default_decl_kind(self.allocator, expr);
                  new_items.push(Statement::ExportDefaultDeclaration(
                    Box::new_in(
                      ExportDefaultDeclaration {
                        node_id: Cell::new(NodeId::DUMMY),
                        span: export_decl.span,
                        declaration: kind,
                      },
                      self.allocator,
                    ),
                  ));
                }
              }
            }
          }
        }
        // Keep all these
        Statement::TSImportEqualsDeclaration(_)
        | Statement::TSNamespaceExportDeclaration(_)
        | Statement::TSExportAssignment(_)
        | Statement::ExportAllDeclaration(_) => {
          new_items.push(item);
        }
        _ => {
          // Regular statements and declarations
          if let Some(new_stmt) = self.transform_module_stmt(item) {
            new_items.push(new_stmt);
          }
        }
      }
    }

    new_items
  }

  fn transform_module_stmt(
    &mut self,
    stmt: Statement<'a>,
  ) -> Option<Statement<'a>> {
    // Extract the declaration from a statement
    let decl = match stmt {
      Statement::VariableDeclaration(d) => {
        Declaration::VariableDeclaration(d)
      }
      Statement::FunctionDeclaration(d) => {
        Declaration::FunctionDeclaration(d)
      }
      Statement::ClassDeclaration(d) => {
        Declaration::ClassDeclaration(d)
      }
      Statement::TSTypeAliasDeclaration(d) => {
        Declaration::TSTypeAliasDeclaration(d)
      }
      Statement::TSInterfaceDeclaration(d) => {
        Declaration::TSInterfaceDeclaration(d)
      }
      Statement::TSEnumDeclaration(d) => {
        Declaration::TSEnumDeclaration(d)
      }
      Statement::TSModuleDeclaration(d) => {
        Declaration::TSModuleDeclaration(d)
      }
      Statement::TSGlobalDeclaration(d) => {
        Declaration::TSGlobalDeclaration(d)
      }
      Statement::TSImportEqualsDeclaration(d) => {
        Declaration::TSImportEqualsDeclaration(d)
      }
      _ => return None,
    };

    if let Declaration::FunctionDeclaration(_) = &decl {
      if self.public_ranges.is_impl_with_overloads(&decl.span()) {
        return None; // skip implementation signature
      }
    }

    match &decl {
      Declaration::TSEnumDeclaration(_)
      | Declaration::ClassDeclaration(_)
      | Declaration::FunctionDeclaration(_)
      | Declaration::VariableDeclaration(_)
      | Declaration::TSModuleDeclaration(_) => {
        match self.decl_to_type_decl(decl.clone_in(self.allocator)) {
          Some(new_decl) => Some(decl_to_stmt(self.allocator, new_decl)),
          _ => {
            self.mark_diagnostic_unable_to_infer(decl.span());
            None
          }
        }
      }
      Declaration::TSInterfaceDeclaration(_)
      | Declaration::TSTypeAliasDeclaration(_)
      | Declaration::TSGlobalDeclaration(_)
      | Declaration::TSImportEqualsDeclaration(_) => {
        Some(decl_to_stmt(self.allocator, decl))
      }
      Declaration::VariableDeclaration(var_decl) => {
        if matches!(
          var_decl.kind,
          VariableDeclarationKind::Using
            | VariableDeclarationKind::AwaitUsing
        ) {
          Some(decl_to_stmt(self.allocator, decl))
        } else {
          match self.decl_to_type_decl(decl.clone_in(self.allocator)) {
            Some(new_decl) => Some(decl_to_stmt(self.allocator, new_decl)),
            _ => {
              self.mark_diagnostic_unable_to_infer(decl.span());
              None
            }
          }
        }
      }
    }
  }

  fn expr_to_ts_type(
    &mut self,
    expr: &Expression<'a>,
    as_const: bool,
    as_readonly: bool,
  ) -> Option<TSType<'a>> {
    match expr {
      Expression::ArrayExpression(arr) => {
        let mut elem_types: std::vec::Vec<TSTupleElement<'a>> = vec![];

        for elem in &arr.elements {
          match elem {
            ArrayExpressionElement::Elision(_) => {
              // TypeScript converts holey arrays to any
              elem_types.push(ts_tuple_element(
                ts_keyword_type(self.allocator, TSKeywordKind::Any),
              ));
            }
            ArrayExpressionElement::SpreadElement(spread) => {
              match self.expr_to_ts_type(
                &spread.argument,
                as_const,
                as_readonly,
              ) {
                Some(ts_expr) => {
                  elem_types.push(ts_tuple_element(ts_expr));
                }
                _ => {
                  self.mark_diagnostic_unable_to_infer(spread.span);
                }
              }
            }
            _ => {
              // Expression variants
              let expr_inner = array_expression_element_to_expr(elem);
              if let Some(e) = expr_inner {
                match self.expr_to_ts_type(e, as_const, as_readonly) {
                  Some(ts_expr) => {
                    elem_types.push(ts_tuple_element(ts_expr));
                  }
                  _ => {
                    self.mark_diagnostic_unable_to_infer(e.span());
                  }
                }
              }
            }
          }
        }

        let mut oxc_elem_types = Vec::new_in(self.allocator);
        for et in elem_types {
          oxc_elem_types.push(et);
        }

        let mut result = TSType::TSTupleType(Box::new_in(
          TSTupleType {
            node_id: Cell::new(NodeId::DUMMY),
            span: arr.span,
            element_types: oxc_elem_types,
          },
          self.allocator,
        ));

        if as_readonly {
          result = ts_readonly(self.allocator, result);
        }
        Some(result)
      }
      Expression::ObjectExpression(obj) => {
        let mut members: std::vec::Vec<TSSignature<'a>> = vec![];

        for item in &obj.properties {
          match item {
            ObjectPropertyKind::ObjectProperty(prop) => {
              if prop.kind != PropertyKind::Init
                || prop.method
                || prop.shorthand
              {
                // getter/setter/method/shorthand
                self.mark_diagnostic_unsupported_prop(prop.span);
                continue;
              }

              let (key, computed) =
                self.prop_key_to_expr_key(&prop.key, prop.computed);

              let init_type = self
                .expr_to_ts_type(&prop.value, as_const, as_readonly)
                .map(|t| type_ann(self.allocator, t));

              members.push(TSSignature::TSPropertySignature(
                Box::new_in(
                  TSPropertySignature {
                    node_id: Cell::new(NodeId::DUMMY),
                    span: SPAN,
                    readonly: as_readonly,
                    key,
                    computed,
                    optional: false,
                    type_annotation: init_type,
                  },
                  self.allocator,
                ),
              ));
            }
            ObjectPropertyKind::SpreadProperty(spread) => {
              self.mark_diagnostic(
                FastCheckDtsDiagnostic::UnableToInferTypeFromSpread {
                  range: self.source_range_to_range(spread.span),
                },
              );
            }
          }
        }

        let mut oxc_members = Vec::new_in(self.allocator);
        for m in members {
          oxc_members.push(m);
        }

        Some(TSType::TSTypeLiteral(Box::new_in(
          TSTypeLiteral {
            node_id: Cell::new(NodeId::DUMMY),
            span: obj.span,
            members: oxc_members,
          },
          self.allocator,
        )))
      }
      Expression::BooleanLiteral(_)
      | Expression::NullLiteral(_)
      | Expression::NumericLiteral(_)
      | Expression::BigIntLiteral(_)
      | Expression::RegExpLiteral(_)
      | Expression::StringLiteral(_) => maybe_lit_to_ts_type(
        self.allocator,
        expr,
        match as_const {
          true => DeclMutabilityKind::Const,
          false => DeclMutabilityKind::Mutable,
        },
      ),
      Expression::TSAsExpression(ts_as) => {
        // Check if this is `as const` - OXC represents it as TSAsExpression
        // with type_annotation being TSTypeReference("const")
        if is_ts_const_type(&ts_as.type_annotation) {
          return self.expr_to_ts_type(&ts_as.expression, true, true);
        }
        Some(ts_as.type_annotation.clone_in(self.allocator))
      }
      Expression::TSSatisfiesExpression(satisfies) => {
        self.expr_to_ts_type(&satisfies.expression, as_const, as_readonly)
      }
      Expression::TSTypeAssertion(ts_assertion) => {
        // TsConstAssertion in SWC is TSTypeAssertion in OXC when
        // the type is "const". Check if it's `as const`.
        // Actually, OXC doesn't have a separate TsConstAssertion;
        // `as const` is represented as TSAsExpression with type TSTypeReference("const").
        // Let's just treat this as a type assertion and return the type.
        Some(ts_assertion.type_annotation.clone_in(self.allocator))
      }
      Expression::FunctionExpression(fn_expr) => {
        let return_type = fn_expr
          .return_type
          .as_ref()
          .map(|val| val.clone_in(self.allocator))
          .unwrap_or_else(|| any_type_ann(self.allocator));

        let params = self.fn_params_to_ts_fn_params(&fn_expr.params);

        Some(TSType::TSFunctionType(Box::new_in(
          TSFunctionType {
            node_id: Cell::new(NodeId::DUMMY),
            span: fn_expr.span,
            params,
            return_type,
            type_parameters: fn_expr
              .type_parameters
              .clone_in(self.allocator),
            this_param: None,
            scope_id: Cell::new(None),
          },
          self.allocator,
        )))
      }
      Expression::ArrowFunctionExpression(arrow_expr) => {
        let return_type = arrow_expr
          .return_type
          .as_ref()
          .map(|val| val.clone_in(self.allocator))
          .unwrap_or_else(|| any_type_ann(self.allocator));

        let params = self.fn_params_to_ts_fn_params(&arrow_expr.params);

        Some(TSType::TSFunctionType(Box::new_in(
          TSFunctionType {
            node_id: Cell::new(NodeId::DUMMY),
            span: arrow_expr.span,
            params,
            return_type,
            type_parameters: arrow_expr
              .type_parameters
              .clone_in(self.allocator),
            this_param: None,
            scope_id: Cell::new(None),
          },
          self.allocator,
        )))
      }
      // Since fast check requires explicit type annotations these
      // can be dropped as they are not part of an export declaration
      Expression::ThisExpression(_)
      | Expression::UnaryExpression(_)
      | Expression::UpdateExpression(_)
      | Expression::BinaryExpression(_)
      | Expression::AssignmentExpression(_)
      | Expression::ConditionalExpression(_)
      | Expression::CallExpression(_)
      | Expression::NewExpression(_)
      | Expression::SequenceExpression(_)
      | Expression::Identifier(_)
      | Expression::TemplateLiteral(_)
      | Expression::TaggedTemplateExpression(_)
      | Expression::ClassExpression(_)
      | Expression::YieldExpression(_)
      | Expression::MetaProperty(_)
      | Expression::AwaitExpression(_)
      | Expression::ParenthesizedExpression(_)
      | Expression::JSXElement(_)
      | Expression::JSXFragment(_)
      | Expression::TSNonNullExpression(_)
      | Expression::TSInstantiationExpression(_)
      | Expression::PrivateInExpression(_)
      | Expression::Super(_)
      | Expression::LogicalExpression(_)
      | Expression::ImportExpression(_)
      | Expression::ChainExpression(_)
      | Expression::V8IntrinsicExpression(_)
      | Expression::ComputedMemberExpression(_)
      | Expression::StaticMemberExpression(_)
      | Expression::PrivateFieldExpression(_) => None,
    }
  }

  fn prop_key_to_expr_key(
    &self,
    key: &PropertyKey<'a>,
    computed: bool,
  ) -> (PropertyKey<'a>, bool) {
    match key {
      PropertyKey::StaticIdentifier(_)
      | PropertyKey::PrivateIdentifier(_) => {
        (key.clone_in(self.allocator), computed)
      }
      _ => {
        // Expression variant - keep as computed
        (key.clone_in(self.allocator), true)
      }
    }
  }

  fn decl_to_type_decl(
    &mut self,
    decl: Declaration<'a>,
  ) -> Option<Declaration<'a>> {
    let is_declare = self.is_top_level;
    match decl {
      Declaration::ClassDeclaration(mut class_decl) => {
        class_decl.body = self.class_body_to_type(&class_decl.body);
        class_decl.declare = is_declare;
        Some(Declaration::ClassDeclaration(class_decl))
      }
      Declaration::FunctionDeclaration(mut fn_decl) => {
        fn_decl.body = None;
        fn_decl.declare = is_declare;

        self.handle_func_params(&mut fn_decl.params);
        Some(Declaration::FunctionDeclaration(fn_decl))
      }
      Declaration::VariableDeclaration(mut var_decl) => {
        if matches!(
          var_decl.kind,
          VariableDeclarationKind::Using
            | VariableDeclarationKind::AwaitUsing
        ) {
          self.mark_diagnostic(FastCheckDtsDiagnostic::UnsupportedUsing {
            range: self.source_range_to_range(var_decl.span),
          });
          return None;
        }

        var_decl.declare = is_declare;

        for decl in var_decl.declarations.iter_mut() {
          if let BindingPattern::BindingIdentifier(ident) = &mut decl.id {
            if decl.type_annotation.is_some() {
              decl.init = None;
              continue;
            }

            let ts_type = decl
              .init
              .as_ref()
              .and_then(|init| {
                self.expr_to_ts_type(init, false, true)
              })
              .map(|t| type_ann(self.allocator, t))
              .or_else(|| {
                self.mark_diagnostic_any_fallback(ident.span);
                Some(any_type_ann(self.allocator))
              });
            decl.type_annotation = ts_type;
          } else {
            self.mark_diagnostic_unable_to_infer(decl.span);
          }

          decl.init = None;
        }

        Some(Declaration::VariableDeclaration(var_decl))
      }
      Declaration::TSEnumDeclaration(mut ts_enum) => {
        ts_enum.declare = is_declare;

        for member in ts_enum.body.members.iter_mut() {
          if let Some(init) = &member.initializer {
            // Support for expressions is limited in enums
            member.initializer =
              if self.valid_enum_init_expr(init) {
                Some(init.clone_in(self.allocator))
              } else {
                None
              };
          }
        }

        Some(Declaration::TSEnumDeclaration(ts_enum))
      }
      Declaration::TSModuleDeclaration(mut ts_module) => {
        ts_module.declare = is_declare;

        if let Some(body) = ts_module.body.take() {
          ts_module.body =
            Some(self.transform_ts_ns_body(body));
        }

        Some(Declaration::TSModuleDeclaration(ts_module))
      }
      Declaration::TSInterfaceDeclaration(_)
      | Declaration::TSTypeAliasDeclaration(_) => Some(decl),
      Declaration::TSGlobalDeclaration(_)
      | Declaration::TSImportEqualsDeclaration(_) => Some(decl),
    }
  }

  fn transform_ts_ns_body(
    &mut self,
    ns: TSModuleDeclarationBody<'a>,
  ) -> TSModuleDeclarationBody<'a> {
    let original_is_top_level = self.is_top_level;
    self.is_top_level = false;
    let body = match ns {
      TSModuleDeclarationBody::TSModuleBlock(mut ts_module_block) => {
        let stmts =
          std::mem::replace(&mut ts_module_block.body, Vec::new_in(self.allocator));
        ts_module_block.body = self.transform_stmts(stmts);
        TSModuleDeclarationBody::TSModuleBlock(ts_module_block)
      }
      TSModuleDeclarationBody::TSModuleDeclaration(mut ts_ns) => {
        if let Some(inner_body) = ts_ns.body.take() {
          self.transform_ts_ns_body(inner_body)
        } else {
          TSModuleDeclarationBody::TSModuleDeclaration(ts_ns)
        }
      }
    };
    self.is_top_level = original_is_top_level;
    body
  }

  // Support for expressions is limited in enums,
  // see https://www.typescriptlang.org/docs/handbook/enums.html
  fn valid_enum_init_expr(&mut self, expr: &Expression<'a>) -> bool {
    match expr {
      Expression::BinaryExpression(bin_expr) => {
        if !self.valid_enum_init_expr(&bin_expr.left) {
          false
        } else {
          self.valid_enum_init_expr(&bin_expr.right)
        }
      }
      Expression::StaticMemberExpression(member_expr) => {
        self.valid_enum_init_expr(&member_expr.object)
      }
      Expression::ComputedMemberExpression(member_expr) => {
        self.valid_enum_init_expr(&member_expr.object)
      }
      Expression::ChainExpression(chain_expr) => match &chain_expr.expression {
        ChainElement::StaticMemberExpression(member_expr) => {
          self.valid_enum_init_expr(&member_expr.object)
        }
        ChainElement::ComputedMemberExpression(member_expr) => {
          self.valid_enum_init_expr(&member_expr.object)
        }
        ChainElement::PrivateFieldExpression(member_expr) => {
          self.valid_enum_init_expr(&member_expr.object)
        }
        ChainElement::CallExpression(_)
        | ChainElement::TSNonNullExpression(_) => false,
      },
      // TS does infer the type of identifiers
      Expression::Identifier(_) => true,
      Expression::NumericLiteral(_) | Expression::StringLiteral(_) => true,
      Expression::BooleanLiteral(_)
      | Expression::NullLiteral(_)
      | Expression::BigIntLiteral(_)
      | Expression::RegExpLiteral(_) => false,
      Expression::TemplateLiteral(tpl_expr) => {
        for expr in &tpl_expr.expressions {
          if !self.valid_enum_init_expr(expr) {
            return false;
          }
        }
        true
      }
      Expression::ParenthesizedExpression(paren_expr) => {
        self.valid_enum_init_expr(&paren_expr.expression)
      }
      Expression::TSTypeAssertion(ts_ass) => {
        // Only assertions to number are allowed for computed
        // enum members.
        self.valid_enum_ts_type(&ts_ass.type_annotation)
      }
      Expression::TSAsExpression(ts_as) => {
        self.valid_enum_ts_type(&ts_as.type_annotation)
      }

      // These are not valid as enum member initializer
      Expression::TSInstantiationExpression(_)
      | Expression::CallExpression(_)
      | Expression::UpdateExpression(_)
      | Expression::PrivateInExpression(_)
      | Expression::TSSatisfiesExpression(_)
      | Expression::TSNonNullExpression(_)
      | Expression::ConditionalExpression(_)
      | Expression::SequenceExpression(_)
      | Expression::TaggedTemplateExpression(_)
      | Expression::ObjectExpression(_)
      | Expression::ArrayExpression(_)
      | Expression::ArrowFunctionExpression(_)
      | Expression::ClassExpression(_)
      | Expression::AwaitExpression(_)
      | Expression::MetaProperty(_)
      | Expression::NewExpression(_)
      | Expression::JSXElement(_)
      | Expression::JSXFragment(_)
      | Expression::UnaryExpression(_)
      | Expression::AssignmentExpression(_)
      | Expression::YieldExpression(_)
      | Expression::Super(_)
      | Expression::FunctionExpression(_)
      | Expression::ThisExpression(_)
      | Expression::LogicalExpression(_)
      | Expression::ImportExpression(_)
      | Expression::V8IntrinsicExpression(_)
      | Expression::PrivateFieldExpression(_) => false,
    }
  }

  fn valid_enum_ts_type(&mut self, ts_type: &TSType<'a>) -> bool {
    match ts_type {
      TSType::TSLiteralType(ts_lit) => match &ts_lit.literal {
        TSLiteral::NumericLiteral(_) => true,
        TSLiteral::StringLiteral(_)
        | TSLiteral::BooleanLiteral(_)
        | TSLiteral::BigIntLiteral(_)
        | TSLiteral::TemplateLiteral(_)
        | TSLiteral::UnaryExpression(_) => false,
      },
      _ => false,
    }
  }

  fn infer_expr_fallback_any(
    &mut self,
    expr: &Expression<'a>,
    as_const: bool,
    as_readonly: bool,
  ) -> Box<'a, TSTypeAnnotation<'a>> {
    match self.expr_to_ts_type(expr, as_const, as_readonly) {
      Some(ts_type) => type_ann(self.allocator, ts_type),
      _ => {
        self.mark_diagnostic_any_fallback(expr.span());
        any_type_ann(self.allocator)
      }
    }
  }

  fn class_body_to_type(
    &mut self,
    body: &Box<'a, ClassBody<'a>>,
  ) -> Box<'a, ClassBody<'a>> {
    let filtered: std::vec::Vec<ClassElement<'a>> = body
      .body
      .clone_in(self.allocator)
      .into_iter()
      .filter(|member| match member {
        ClassElement::MethodDefinition(method) => {
          if method.kind == MethodDefinitionKind::Constructor {
            !self
              .public_ranges
              .is_impl_with_overloads(&method.span)
          } else {
            !self.public_ranges.is_impl_with_overloads(&method.span)
          }
        }
        ClassElement::TSIndexSignature(_)
        | ClassElement::PropertyDefinition(_)
        | ClassElement::StaticBlock(_)
        | ClassElement::AccessorProperty(_) => true,
      })
      .filter_map(|member| match member {
        ClassElement::MethodDefinition(mut method) => {
          // Skip private methods (PrivateIdentifier / #name)
          if let PropertyKey::PrivateIdentifier(_) = &method.key {
            return None;
          }

          if method.kind == MethodDefinitionKind::Constructor {
            // Constructor
            method.value.body = None;
            self.handle_func_params(&mut method.value.params);
            Some(ClassElement::MethodDefinition(method))
          } else {
            // Regular method / getter / setter
            match valid_prop_name(self.allocator, &method.key) {
              Some(new_prop_name) => {
                method.key = new_prop_name;
              }
              _ => {
                return None;
              }
            }

            method.value.body = None;
            if method.kind == MethodDefinitionKind::Set {
              method.value.return_type = None;
            }
            self.handle_func_params(&mut method.value.params);
            Some(ClassElement::MethodDefinition(method))
          }
        }
        ClassElement::PropertyDefinition(mut prop) => {
          // Skip private properties (PrivateIdentifier)
          if let PropertyKey::PrivateIdentifier(_) = &prop.key {
            return None;
          }

          match valid_prop_name(self.allocator, &prop.key) {
            Some(new_prop_name) => {
              prop.key = new_prop_name;
            }
            _ => {
              return None;
            }
          }
          if prop.type_annotation.is_none() {
            if let Some(ref value) = prop.value {
              prop.type_annotation = self
                .expr_to_ts_type(value, false, false)
                .map(|t| type_ann(self.allocator, t))
                .or_else(|| Some(any_type_ann(self.allocator)));
            }
          }
          prop.value = None;
          prop.definite = false;
          prop.declare = false;

          Some(ClassElement::PropertyDefinition(prop))
        }
        ClassElement::TSIndexSignature(index_sig) => {
          Some(ClassElement::TSIndexSignature(index_sig))
        }

        // These can be removed as they are not relevant for types
        ClassElement::StaticBlock(_)
        | ClassElement::AccessorProperty(_) => None,
      })
      .collect();

    let mut oxc_body = Vec::new_in(self.allocator);
    for el in filtered {
      oxc_body.push(el);
    }

    Box::new_in(ClassBody {
      node_id: Cell::new(NodeId::DUMMY),
      span: body.span,
      body: oxc_body,
    }, self.allocator)
  }

  fn handle_func_params(
    &mut self,
    params: &mut Box<'a, FormalParameters<'a>>,
  ) {
    for param in params.items.iter_mut() {
      self.handle_func_param(param);
    }
  }

  fn handle_func_param(&mut self, param: &mut FormalParameter<'a>) {
    // In OXC, parameter defaults are stored in FormalParameter::initializer
    // (not as AssignmentPattern in the pattern field).
    if let Some(init) = param.initializer.take() {
      // Has a default value: infer type if missing, make optional, remove default
      if param.type_annotation.is_none() {
        param.type_annotation = Some(self.infer_expr_fallback_any(
          &init,
          false,
          false,
        ));
      }
      param.optional = true;
      // initializer already taken via take()
      return;
    }

    match &mut param.pattern {
      BindingPattern::BindingIdentifier(ident) => {
        if param.type_annotation.is_none() {
          self.mark_diagnostic_any_fallback(ident.span);
          param.type_annotation = Some(any_type_ann(self.allocator));
        }
      }
      BindingPattern::AssignmentPattern(assign_pat) => {
        if let Some(new_result) =
          self.handle_func_param_assign(assign_pat, &mut param.type_annotation)
        {
          param.pattern = new_result;
          param.optional = true;
        }
      }
      BindingPattern::ArrayPattern(_)
      | BindingPattern::ObjectPattern(_) => {}
    }
  }

  fn handle_func_param_assign(
    &mut self,
    assign_pat: &mut AssignmentPattern<'a>,
    param_type_annotation: &mut Option<Box<'a, TSTypeAnnotation<'a>>>,
  ) -> Option<BindingPattern<'a>> {
    match &mut assign_pat.left {
      BindingPattern::BindingIdentifier(ident) => {
        if param_type_annotation.is_none() {
          *param_type_annotation = Some(self.infer_expr_fallback_any(
            &assign_pat.right,
            false,
            false,
          ));
        }

        Some(BindingPattern::BindingIdentifier(
          ident.clone_in(self.allocator),
        ))
      }
      BindingPattern::ArrayPattern(arr_pat) => {
        if param_type_annotation.is_none() {
          *param_type_annotation = Some(self.infer_expr_fallback_any(
            &assign_pat.right,
            false,
            false,
          ));
        }

        Some(BindingPattern::ArrayPattern(
          arr_pat.clone_in(self.allocator),
        ))
      }
      BindingPattern::ObjectPattern(obj_pat) => {
        if param_type_annotation.is_none() {
          *param_type_annotation = Some(self.infer_expr_fallback_any(
            &assign_pat.right,
            false,
            false,
          ));
        }

        Some(BindingPattern::ObjectPattern(
          obj_pat.clone_in(self.allocator),
        ))
      }
      BindingPattern::AssignmentPattern(_) => None,
    }
  }

  fn fn_params_to_ts_fn_params(
    &mut self,
    params: &FormalParameters<'a>,
  ) -> Box<'a, FormalParameters<'a>> {
    let mut items = Vec::new_in(self.allocator);
    for param in &params.items {
      if let Some(fp) = self.param_to_ts_fn_param(param) {
        items.push(fp);
      }
    }
    let rest = params.rest.clone_in(self.allocator);
    Box::new_in(
      FormalParameters {
        node_id: Cell::new(NodeId::DUMMY),
        span: params.span,
        kind: FormalParameterKind::Signature,
        items,
        rest,
      },
      self.allocator,
    )
  }

  fn param_to_ts_fn_param(
    &mut self,
    param: &FormalParameter<'a>,
  ) -> Option<FormalParameter<'a>> {
    // In OXC, parameter defaults are in FormalParameter::initializer
    if let Some(init) = &param.initializer {
      let ta = param.type_annotation.clone_in(self.allocator)
        .or_else(|| {
          self.expr_to_ts_type(init, false, false)
            .map(|t| type_ann(self.allocator, t))
        });
      return Some(FormalParameter {
        node_id: Cell::new(NodeId::DUMMY),
        span: param.span,
        decorators: Vec::new_in(self.allocator),
        pattern: param.pattern.clone_in(self.allocator),
        type_annotation: ta,
        initializer: None,
        optional: false,
        accessibility: param.accessibility,
        readonly: param.readonly,
        r#override: param.r#override,
      });
    }

    match &param.pattern {
      BindingPattern::BindingIdentifier(binding_id) => {
        Some(FormalParameter {
          node_id: Cell::new(NodeId::DUMMY),
          span: param.span,
          decorators: Vec::new_in(self.allocator),
          pattern: BindingPattern::BindingIdentifier(
            binding_id.clone_in(self.allocator),
          ),
          type_annotation: param.type_annotation.clone_in(self.allocator),
          initializer: None,
          optional: param.optional,
          accessibility: param.accessibility,
          readonly: param.readonly,
          r#override: param.r#override,
        })
      }
      BindingPattern::ArrayPattern(_) => {
        Some(param.clone_in(self.allocator))
      }
      BindingPattern::ObjectPattern(_) => {
        Some(param.clone_in(self.allocator))
      }
      BindingPattern::AssignmentPattern(assign_pat) => {
        self
          .expr_to_ts_type(&assign_pat.right, false, false)
          .map(|ts_type| {
            let name = if let BindingPattern::BindingIdentifier(ident) =
              &assign_pat.left
            {
              ident.name.clone()
            } else {
              Ident::from(self.allocator.alloc_str(&self.gen_unique_name()))
            };

            FormalParameter {
              node_id: Cell::new(NodeId::DUMMY),
              span: assign_pat.span,
              decorators: Vec::new_in(self.allocator),
              pattern: BindingPattern::BindingIdentifier(Box::new_in(
                BindingIdentifier {
                  node_id: Cell::new(NodeId::DUMMY),
                  span: assign_pat.span,
                  name,
                  symbol_id: Cell::new(None),
                },
                self.allocator,
              )),
              type_annotation: Some(type_ann(self.allocator, ts_type)),
              initializer: None,
              optional: false,
              accessibility: None,
              readonly: false,
              r#override: false,
            }
          })
      }
    }
  }
}

/// Check if a TSType is the `const` type reference used in `as const`.
fn is_ts_const_type(ty: &TSType) -> bool {
  if let TSType::TSTypeReference(type_ref) = ty {
    if let TSTypeName::IdentifierReference(ident) = &type_ref.type_name {
      return ident.name.as_str() == "const";
    }
  }
  false
}

fn decl_to_stmt<'a>(
  allocator: &'a Allocator,
  decl: Declaration<'a>,
) -> Statement<'a> {
  match decl {
    Declaration::VariableDeclaration(d) => Statement::VariableDeclaration(d),
    Declaration::FunctionDeclaration(d) => Statement::FunctionDeclaration(d),
    Declaration::ClassDeclaration(d) => Statement::ClassDeclaration(d),
    Declaration::TSTypeAliasDeclaration(d) => {
      Statement::TSTypeAliasDeclaration(d)
    }
    Declaration::TSInterfaceDeclaration(d) => {
      Statement::TSInterfaceDeclaration(d)
    }
    Declaration::TSEnumDeclaration(d) => Statement::TSEnumDeclaration(d),
    Declaration::TSModuleDeclaration(d) => Statement::TSModuleDeclaration(d),
    Declaration::TSGlobalDeclaration(d) => Statement::TSGlobalDeclaration(d),
    Declaration::TSImportEqualsDeclaration(d) => {
      Statement::TSImportEqualsDeclaration(d)
    }
  }
}

/// Convert an ArrayExpressionElement (expression variant) to an Expression reference.
/// ArrayExpressionElement inherits all Expression variants via @inherit,
/// so OXC provides an `as_expression()` method for the conversion.
fn array_expression_element_to_expr<'a, 'b>(
  elem: &'b ArrayExpressionElement<'a>,
) -> Option<&'b Expression<'a>> {
  match elem {
    ArrayExpressionElement::SpreadElement(_) | ArrayExpressionElement::Elision(_) => None,
    _ => elem.as_expression(),
  }
}

fn export_default_decl_kind_to_expr<'a>(
  _allocator: &'a Allocator,
  kind: ExportDefaultDeclarationKind<'a>,
) -> Option<Expression<'a>> {
  // ExportDefaultDeclarationKind inherits Expression variants
  // We need to convert back to Expression
  match kind {
    ExportDefaultDeclarationKind::BooleanLiteral(e) => {
      Some(Expression::BooleanLiteral(e))
    }
    ExportDefaultDeclarationKind::NullLiteral(e) => {
      Some(Expression::NullLiteral(e))
    }
    ExportDefaultDeclarationKind::NumericLiteral(e) => {
      Some(Expression::NumericLiteral(e))
    }
    ExportDefaultDeclarationKind::BigIntLiteral(e) => {
      Some(Expression::BigIntLiteral(e))
    }
    ExportDefaultDeclarationKind::RegExpLiteral(e) => {
      Some(Expression::RegExpLiteral(e))
    }
    ExportDefaultDeclarationKind::StringLiteral(e) => {
      Some(Expression::StringLiteral(e))
    }
    ExportDefaultDeclarationKind::TemplateLiteral(e) => {
      Some(Expression::TemplateLiteral(e))
    }
    ExportDefaultDeclarationKind::Identifier(e) => {
      Some(Expression::Identifier(e))
    }
    ExportDefaultDeclarationKind::MetaProperty(e) => {
      Some(Expression::MetaProperty(e))
    }
    ExportDefaultDeclarationKind::Super(e) => Some(Expression::Super(e)),
    ExportDefaultDeclarationKind::ArrayExpression(e) => {
      Some(Expression::ArrayExpression(e))
    }
    ExportDefaultDeclarationKind::ArrowFunctionExpression(e) => {
      Some(Expression::ArrowFunctionExpression(e))
    }
    ExportDefaultDeclarationKind::AssignmentExpression(e) => {
      Some(Expression::AssignmentExpression(e))
    }
    ExportDefaultDeclarationKind::AwaitExpression(e) => {
      Some(Expression::AwaitExpression(e))
    }
    ExportDefaultDeclarationKind::BinaryExpression(e) => {
      Some(Expression::BinaryExpression(e))
    }
    ExportDefaultDeclarationKind::CallExpression(e) => {
      Some(Expression::CallExpression(e))
    }
    ExportDefaultDeclarationKind::ChainExpression(e) => {
      Some(Expression::ChainExpression(e))
    }
    ExportDefaultDeclarationKind::ClassExpression(e) => {
      Some(Expression::ClassExpression(e))
    }
    ExportDefaultDeclarationKind::ConditionalExpression(e) => {
      Some(Expression::ConditionalExpression(e))
    }
    ExportDefaultDeclarationKind::FunctionExpression(e) => {
      Some(Expression::FunctionExpression(e))
    }
    ExportDefaultDeclarationKind::ImportExpression(e) => {
      Some(Expression::ImportExpression(e))
    }
    ExportDefaultDeclarationKind::LogicalExpression(e) => {
      Some(Expression::LogicalExpression(e))
    }
    ExportDefaultDeclarationKind::NewExpression(e) => {
      Some(Expression::NewExpression(e))
    }
    ExportDefaultDeclarationKind::ObjectExpression(e) => {
      Some(Expression::ObjectExpression(e))
    }
    ExportDefaultDeclarationKind::ParenthesizedExpression(e) => {
      Some(Expression::ParenthesizedExpression(e))
    }
    ExportDefaultDeclarationKind::SequenceExpression(e) => {
      Some(Expression::SequenceExpression(e))
    }
    ExportDefaultDeclarationKind::TaggedTemplateExpression(e) => {
      Some(Expression::TaggedTemplateExpression(e))
    }
    ExportDefaultDeclarationKind::ThisExpression(e) => {
      Some(Expression::ThisExpression(e))
    }
    ExportDefaultDeclarationKind::UnaryExpression(e) => {
      Some(Expression::UnaryExpression(e))
    }
    ExportDefaultDeclarationKind::UpdateExpression(e) => {
      Some(Expression::UpdateExpression(e))
    }
    ExportDefaultDeclarationKind::YieldExpression(e) => {
      Some(Expression::YieldExpression(e))
    }
    ExportDefaultDeclarationKind::PrivateInExpression(e) => {
      Some(Expression::PrivateInExpression(e))
    }
    ExportDefaultDeclarationKind::JSXElement(e) => {
      Some(Expression::JSXElement(e))
    }
    ExportDefaultDeclarationKind::JSXFragment(e) => {
      Some(Expression::JSXFragment(e))
    }
    ExportDefaultDeclarationKind::TSAsExpression(e) => {
      Some(Expression::TSAsExpression(e))
    }
    ExportDefaultDeclarationKind::TSSatisfiesExpression(e) => {
      Some(Expression::TSSatisfiesExpression(e))
    }
    ExportDefaultDeclarationKind::TSTypeAssertion(e) => {
      Some(Expression::TSTypeAssertion(e))
    }
    ExportDefaultDeclarationKind::TSNonNullExpression(e) => {
      Some(Expression::TSNonNullExpression(e))
    }
    ExportDefaultDeclarationKind::TSInstantiationExpression(e) => {
      Some(Expression::TSInstantiationExpression(e))
    }
    ExportDefaultDeclarationKind::V8IntrinsicExpression(e) => {
      Some(Expression::V8IntrinsicExpression(e))
    }
    ExportDefaultDeclarationKind::ComputedMemberExpression(e) => {
      Some(Expression::ComputedMemberExpression(e))
    }
    ExportDefaultDeclarationKind::StaticMemberExpression(e) => {
      Some(Expression::StaticMemberExpression(e))
    }
    ExportDefaultDeclarationKind::PrivateFieldExpression(e) => {
      Some(Expression::PrivateFieldExpression(e))
    }
    _ => None,
  }
}

fn expr_to_export_default_decl_kind<'a>(
  _allocator: &'a Allocator,
  expr: Expression<'a>,
) -> ExportDefaultDeclarationKind<'a> {
  match expr {
    Expression::BooleanLiteral(e) => {
      ExportDefaultDeclarationKind::BooleanLiteral(e)
    }
    Expression::NullLiteral(e) => {
      ExportDefaultDeclarationKind::NullLiteral(e)
    }
    Expression::NumericLiteral(e) => {
      ExportDefaultDeclarationKind::NumericLiteral(e)
    }
    Expression::BigIntLiteral(e) => {
      ExportDefaultDeclarationKind::BigIntLiteral(e)
    }
    Expression::RegExpLiteral(e) => {
      ExportDefaultDeclarationKind::RegExpLiteral(e)
    }
    Expression::StringLiteral(e) => {
      ExportDefaultDeclarationKind::StringLiteral(e)
    }
    Expression::TemplateLiteral(e) => {
      ExportDefaultDeclarationKind::TemplateLiteral(e)
    }
    Expression::Identifier(e) => {
      ExportDefaultDeclarationKind::Identifier(e)
    }
    Expression::MetaProperty(e) => {
      ExportDefaultDeclarationKind::MetaProperty(e)
    }
    Expression::Super(e) => ExportDefaultDeclarationKind::Super(e),
    Expression::ArrayExpression(e) => {
      ExportDefaultDeclarationKind::ArrayExpression(e)
    }
    Expression::ArrowFunctionExpression(e) => {
      ExportDefaultDeclarationKind::ArrowFunctionExpression(e)
    }
    Expression::AssignmentExpression(e) => {
      ExportDefaultDeclarationKind::AssignmentExpression(e)
    }
    Expression::AwaitExpression(e) => {
      ExportDefaultDeclarationKind::AwaitExpression(e)
    }
    Expression::BinaryExpression(e) => {
      ExportDefaultDeclarationKind::BinaryExpression(e)
    }
    Expression::CallExpression(e) => {
      ExportDefaultDeclarationKind::CallExpression(e)
    }
    Expression::ChainExpression(e) => {
      ExportDefaultDeclarationKind::ChainExpression(e)
    }
    Expression::ClassExpression(e) => {
      ExportDefaultDeclarationKind::ClassExpression(e)
    }
    Expression::ConditionalExpression(e) => {
      ExportDefaultDeclarationKind::ConditionalExpression(e)
    }
    Expression::FunctionExpression(e) => {
      ExportDefaultDeclarationKind::FunctionExpression(e)
    }
    Expression::ImportExpression(e) => {
      ExportDefaultDeclarationKind::ImportExpression(e)
    }
    Expression::LogicalExpression(e) => {
      ExportDefaultDeclarationKind::LogicalExpression(e)
    }
    Expression::NewExpression(e) => {
      ExportDefaultDeclarationKind::NewExpression(e)
    }
    Expression::ObjectExpression(e) => {
      ExportDefaultDeclarationKind::ObjectExpression(e)
    }
    Expression::ParenthesizedExpression(e) => {
      ExportDefaultDeclarationKind::ParenthesizedExpression(e)
    }
    Expression::SequenceExpression(e) => {
      ExportDefaultDeclarationKind::SequenceExpression(e)
    }
    Expression::TaggedTemplateExpression(e) => {
      ExportDefaultDeclarationKind::TaggedTemplateExpression(e)
    }
    Expression::ThisExpression(e) => {
      ExportDefaultDeclarationKind::ThisExpression(e)
    }
    Expression::UnaryExpression(e) => {
      ExportDefaultDeclarationKind::UnaryExpression(e)
    }
    Expression::UpdateExpression(e) => {
      ExportDefaultDeclarationKind::UpdateExpression(e)
    }
    Expression::YieldExpression(e) => {
      ExportDefaultDeclarationKind::YieldExpression(e)
    }
    Expression::PrivateInExpression(e) => {
      ExportDefaultDeclarationKind::PrivateInExpression(e)
    }
    Expression::JSXElement(e) => {
      ExportDefaultDeclarationKind::JSXElement(e)
    }
    Expression::JSXFragment(e) => {
      ExportDefaultDeclarationKind::JSXFragment(e)
    }
    Expression::TSAsExpression(e) => {
      ExportDefaultDeclarationKind::TSAsExpression(e)
    }
    Expression::TSSatisfiesExpression(e) => {
      ExportDefaultDeclarationKind::TSSatisfiesExpression(e)
    }
    Expression::TSTypeAssertion(e) => {
      ExportDefaultDeclarationKind::TSTypeAssertion(e)
    }
    Expression::TSNonNullExpression(e) => {
      ExportDefaultDeclarationKind::TSNonNullExpression(e)
    }
    Expression::TSInstantiationExpression(e) => {
      ExportDefaultDeclarationKind::TSInstantiationExpression(e)
    }
    Expression::V8IntrinsicExpression(e) => {
      ExportDefaultDeclarationKind::V8IntrinsicExpression(e)
    }
    Expression::ComputedMemberExpression(e) => {
      ExportDefaultDeclarationKind::ComputedMemberExpression(e)
    }
    Expression::StaticMemberExpression(e) => {
      ExportDefaultDeclarationKind::StaticMemberExpression(e)
    }
    Expression::PrivateFieldExpression(e) => {
      ExportDefaultDeclarationKind::PrivateFieldExpression(e)
    }
  }
}

fn valid_prop_name<'a>(
  allocator: &'a Allocator,
  prop_key: &PropertyKey<'a>,
) -> Option<PropertyKey<'a>> {
  fn prop_name_from_property_key<'a>(
    allocator: &'a Allocator,
    key: &PropertyKey<'a>,
  ) -> Option<PropertyKey<'a>> {
    match key {
      PropertyKey::StaticIdentifier(_) => Some(key.clone_in(allocator)),
      PropertyKey::PrivateIdentifier(_) => Some(key.clone_in(allocator)),
      // Expression variants
      PropertyKey::StringLiteral(_)
      | PropertyKey::NumericLiteral(_)
      | PropertyKey::BigIntLiteral(_) => Some(key.clone_in(allocator)),
      PropertyKey::TemplateLiteral(tpl) => {
        if tpl.quasis.is_empty() && tpl.expressions.len() == 1 {
          prop_name_from_expr(allocator, &tpl.expressions[0])
        } else {
          None
        }
      }
      PropertyKey::ParenthesizedExpression(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      PropertyKey::TSTypeAssertion(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      PropertyKey::TSNonNullExpression(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      PropertyKey::TSAsExpression(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      PropertyKey::TSSatisfiesExpression(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      PropertyKey::Identifier(ident) => {
        // Computed identifier - keep as computed property key
        Some(key.clone_in(allocator))
      }
      _ => None,
    }
  }

  fn prop_name_from_expr<'a>(
    allocator: &'a Allocator,
    expr: &Expression<'a>,
  ) -> Option<PropertyKey<'a>> {
    match expr {
      Expression::StringLiteral(s) => Some(PropertyKey::StringLiteral(
        s.clone_in(allocator),
      )),
      Expression::NumericLiteral(n) => Some(PropertyKey::NumericLiteral(
        n.clone_in(allocator),
      )),
      Expression::BigIntLiteral(b) => Some(PropertyKey::BigIntLiteral(
        b.clone_in(allocator),
      )),
      Expression::TemplateLiteral(e) => {
        if e.quasis.is_empty() && e.expressions.len() == 1 {
          prop_name_from_expr(allocator, &e.expressions[0])
        } else {
          None
        }
      }
      Expression::ParenthesizedExpression(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      Expression::TSTypeAssertion(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      Expression::TSNonNullExpression(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      Expression::TSAsExpression(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      Expression::TSSatisfiesExpression(e) => {
        prop_name_from_expr(allocator, &e.expression)
      }
      Expression::Identifier(_) => {
        // Computed identifier reference
        Some(PropertyKey::Identifier(Box::new_in(
          IdentifierReference {
            node_id: Cell::new(NodeId::DUMMY),
            span: expr.span(),
            name: match expr {
              Expression::Identifier(id) => id.name.clone(),
              _ => unreachable!(),
            },
            reference_id: Cell::new(None),
          },
          allocator,
        )))
      }
      _ => None,
    }
  }

  prop_name_from_property_key(allocator, prop_key)
}

#[cfg(test)]
mod tests {
  use std::collections::VecDeque;

  use crate::BuildOptions;
  use crate::GraphKind;
  use crate::ModuleGraph;
  use crate::WorkspaceMember;
  use crate::ast::CapturingModuleAnalyzer;
  use crate::ast::EsParser;
  use crate::fast_check::range_finder::find_public_ranges;
  use crate::fast_check::transform_dts::FastCheckDtsTransformer;
  use crate::source::MemoryLoader;
  use crate::source::Source;
  use crate::symbols::RootSymbol;
  use deno_ast::EmitOptions;
  use deno_ast::EmittedSourceText;
  use deno_ast::ModuleSpecifier;
  use deno_ast::emit;
  use deno_ast::oxc::allocator::CloneIn;
  use deno_semver::package::PackageNv;
  use indexmap::IndexMap;
  use url::Url;

  async fn transform_dts_test(source: &str, expected: &str) {
    let specifier = Url::parse("file:///mod.ts").unwrap();

    let loader = MemoryLoader::new(
      vec![(
        specifier.to_string(),
        Source::Module {
          specifier: specifier.to_string(),
          maybe_headers: None,
          content: source.to_string(),
        },
      )],
      vec![],
    );
    let analyzer = CapturingModuleAnalyzer::default();
    let allocator = deno_ast::oxc::allocator::Allocator::default();
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![specifier.clone()],
        Default::default(),
        &loader,
        BuildOptions {
          module_analyzer: &analyzer,
          ..Default::default()
        },
      )
      .await;

    let root_sym = RootSymbol::new(&graph, &analyzer, &allocator);

    let module_info = root_sym
      .module_from_specifier(&specifier)
      .unwrap()
      .esm()
      .unwrap();

    let parsed_source = analyzer.parse_program(
      &allocator,
      crate::ast::ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: deno_ast::MediaType::TypeScript,
        scope_analysis: false,
      },
    ).unwrap();
    let program = parsed_source.program().clone_in(&allocator);
    let nv = PackageNv::from_str("package@1.0.0").unwrap();
    let public_ranges = find_public_ranges(
      None,
      Default::default(),
      &graph,
      &root_sym,
      &[WorkspaceMember {
        base: ModuleSpecifier::parse("file:///").unwrap(),
        name: nv.name.clone(),
        version: Some(nv.version.clone()),
        exports: IndexMap::from([(".".to_string(), "./mod.ts".to_string())]),
      }],
      VecDeque::from([nv.clone()]),
    )
    .remove(&nv)
    .unwrap()
    .module_ranges
    .shift_remove(&specifier)
    .unwrap();

    let mut transformer = FastCheckDtsTransformer::new(
      &allocator,
      parsed_source.text_info_lazy(),
      &public_ranges,
      &specifier,
    );
    let program = transformer.transform(program);

    let EmittedSourceText { text: actual, .. } = emit(
      &program,
      parsed_source.text(),
      &specifier.to_string(),
      &EmitOptions {
        remove_comments: false,
        source_map: deno_ast::SourceMapOption::None,
        source_map_base: None,
        source_map_file: None,
        inline_sources: false,
        ascii_only: false,
      },
    )
    .unwrap();

    assert_eq!(actual.trim(), expected.trim());
  }

  #[tokio::test]
  async fn dts_function_test() {
    transform_dts_test(
      r#"export function foo(a: number): number {
  return {};
}"#,
      "export declare function foo(a: number): number;",
    )
    .await;
    transform_dts_test(
      r#"export function foo(a: string): number;
export function foo(a: any): number {
  return {};
}"#,
      r#"export declare function foo(a: string): number;"#,
    )
    .await;
    transform_dts_test(
      r#"export function foo(a = 2): number {
  return 2;
}"#,
      r#"export declare function foo(a?: number): number;"#,
    )
    .await;
    transform_dts_test(
      r#"export function foo(a: string = 2): number {
  return 2;
}"#,
      r#"export declare function foo(a?: string): number;"#,
    )
    .await;
    transform_dts_test(
      r#"export function foo([a, b] = [1, 2]): number {
  return 2;
}"#,
      r#"export declare function foo([a, b]?: [number, number]): number;"#,
    )
    .await;
    transform_dts_test(
      r#"export function foo({a, b} = { a: 1, b: 2 }): number {
  return 2;
}"#,
      r#"export declare function foo({ a, b }?: {
  a: number;
  b: number;
}): number;"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_class_decl_test() {
    transform_dts_test(
      r#"export class Foo {
  a: number = 2;
  static b: number = 1;
  #b: number = 3;
  constructor(value: string) {
    return 42;
  }
  foo(): string {
    return "abc";
  }
  #bar(): number {
    return 2
  }
  get asdf(): number {

  }
  set asdf(value: number) {

  }

  static {

  }
}"#,
      r#"export declare class Foo {
  a: number;
  static b: number;
  constructor(value: string);
  foo(): string;
  get asdf(): number;
  set asdf(value: number);
}"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_class_decl_rest_test() {
    transform_dts_test(
      r#"export class Foo {
  constructor(...args: string[]) {}
}"#,
      r#"export declare class Foo {
  constructor(...args: string[]);
}"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_class_decl_overloads_test() {
    transform_dts_test(
      r#"export class Foo {
  constructor(arg: string);
  constructor(arg: number);
  constructor(arg: any) {}
}"#,
      r#"export declare class Foo {
  constructor(arg: string);
  constructor(arg: number);
}"#,
    )
    .await;

    transform_dts_test(
      r#"export class Foo {
  foo(arg: string);
  foo(arg: number);
  foo(arg: any) {}
}"#,
      r#"export declare class Foo {
  foo(arg: string);
  foo(arg: number);
}"#,
    )
    .await;

    transform_dts_test(
      r#"export class Foo {
  constructor(arg: string);
  constructor(arg: number);
  constructor(arg: any) {}

  bar(arg: number): number {
    return 2
  }

  foo(arg: string);
  foo(arg: number);
  foo(arg: any) {}
}"#,
      r#"export declare class Foo {
  constructor(arg: string);
  constructor(arg: number);
  bar(arg: number): number;
  foo(arg: string);
  foo(arg: number);
}"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_class_decl_prop_test() {
    transform_dts_test(
      r#"export class Foo { private a!: string }"#,
      r#"export declare class Foo {
  private a: string;
}"#,
    )
    .await;

    transform_dts_test(
      r#"export class Foo { declare a: string }"#,
      r#"export declare class Foo {
  a: string;
}"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_class_decl_prop_infer_test() {
    transform_dts_test(
      r#"export class Foo { foo = (a: string): string => ({} as any) }"#,
      r#"export declare class Foo {
  foo: (a: string) => string;
}"#,
    )
    .await;
    transform_dts_test(
      r#"export class Foo { foo = function(a: string): void {} }"#,
      r#"export declare class Foo {
  foo: (a: string) => void;
}"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_var_decl_test() {
    transform_dts_test(
      r#"export const foo: number = 42;"#,
      "export declare const foo: number;",
    )
    .await;

    transform_dts_test(
      r#"export var foo: number = 42;"#,
      "export declare var foo: number;",
    )
    .await;

    transform_dts_test(
      r#"export let foo: number = 42;"#,
      "export declare let foo: number;",
    )
    .await;

    // Default to any if it cannot be determined
    transform_dts_test(
      r#"export const foo = adsf.b;"#,
      "export declare const foo: any;",
    )
    .await;
    transform_dts_test(r#"export let foo;"#, "export declare let foo: any;")
      .await;
  }

  #[tokio::test]
  async fn dts_global_declare() {
    transform_dts_test(
      r#"declare global {
  interface String {
    fancyFormat(opts: StringFormatOptions): string;
  }
}"#,
      r#"declare global {
  interface String {
    fancyFormat(opts: StringFormatOptions): string;
  }
}"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_inference() {
    transform_dts_test(
      r#"export const foo = null as string as number;"#,
      "export declare const foo: number;",
    )
    .await;
  }

  #[tokio::test]
  async fn dts_as_const() {
    transform_dts_test(
      r#"export const foo = [1, 2] as const;"#,
      "export declare const foo: readonly [1, 2];",
    )
    .await;
    transform_dts_test(
      r#"export const foo = [1, ,2] as const;"#,
      "export declare const foo: readonly [1, any, 2];",
    )
    .await;

    transform_dts_test(
      r#"export const foo = { str: "bar", bool: true, bool2: false, num: 42,   nullish: null } as const;"#,
      r#"export declare const foo: {
  readonly str: "bar";
  readonly bool: true;
  readonly bool2: false;
  readonly num: 42;
  readonly nullish: null;
};"#,
    ).await;

    transform_dts_test(
      r#"export const foo = { str: [1, 2] as const } as const;"#,
      r#"export declare const foo: {
  readonly str: readonly [1, 2];
};"#,
    )
    .await;

    // TODO: Requires type resolving
    transform_dts_test(
      r#"export const foo = { bar } as const;"#,
      r#"export declare const foo: {};"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_literal_inference_ann() {
    transform_dts_test(
      r#"export const foo: number = "abc";"#,
      "export declare const foo: number;",
    )
    .await;
    transform_dts_test(
      r#"export let foo: number = "abc";"#,
      "export declare let foo: number;",
    )
    .await;
    transform_dts_test(
      r#"export var foo: number = "abc";"#,
      "export declare var foo: number;",
    )
    .await;
  }

  #[tokio::test]
  async fn dts_literal_inference() {
    transform_dts_test(
      r#"export const foo = 42;"#,
      "export declare const foo: number;",
    )
    .await;
    transform_dts_test(
      r#"export const foo = "foo";"#,
      "export declare const foo: string;",
    )
    .await;
    transform_dts_test(
      r#"export const foo = true;"#,
      "export declare const foo: boolean;",
    )
    .await;
    transform_dts_test(
      r#"export const foo = false;"#,
      "export declare const foo: boolean;",
    )
    .await;
    transform_dts_test(
      r#"export const foo = null;"#,
      "export declare const foo: null;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = undefined;"#,
      "export declare let foo: any;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = 10n;"#,
      "export declare let foo: bigint;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = /foo/;"#,
      "export declare let foo: RegExp;",
    )
    .await;
  }

  #[tokio::test]
  async fn dts_fn_expr() {
    transform_dts_test(
      r#"export let foo = function add(a: number, b: number): number {
  return a + b;
}"#,
      "export declare let foo: (a: number, b: number) => number;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = function add<T>([a, b]: T): void {}"#,
      "export declare let foo: <T>([a, b]: T) => void;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = function add<T>({a, b}: T): void {}"#,
      "export declare let foo: <T>({ a, b }: T) => void;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = function add(a = 2): void {}"#,
      "export declare let foo: (a: number) => void;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = function add(...params: any[]): void {}"#,
      "export declare let foo: (...params: any[]) => void;",
    )
    .await;
  }

  #[tokio::test]
  async fn dts_fn_arrow_expr() {
    transform_dts_test(
      r#"export let foo = (a: number, b: number): number => {
  return a + b;
}"#,
      "export declare let foo: (a: number, b: number) => number;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = <T>([a, b]: T): void => {}"#,
      "export declare let foo: <T>([a, b]: T) => void;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = <T>({a, b}: T): void => {}"#,
      "export declare let foo: <T>({ a, b }: T) => void;",
    )
    .await;
    transform_dts_test(
      r#"export let foo = (a = 2): void => {}"#,
      "export declare let foo: (a: number) => void;",
    )
    .await;

    transform_dts_test(
      r#"export let foo = (...params: any[]): void => {}"#,
      "export declare let foo: (...params: any[]) => void;",
    )
    .await;
  }

  #[tokio::test]
  async fn dts_type_export() {
    transform_dts_test(r#"interface Foo {}"#, "interface Foo {}").await;
    transform_dts_test(r#"type Foo = number;"#, "type Foo = number;").await;

    transform_dts_test(
      r#"export interface Foo {}"#,
      "export interface Foo {}",
    )
    .await;
    transform_dts_test(
      r#"export type Foo = number;"#,
      "export type Foo = number;",
    )
    .await;
  }

  #[tokio::test]
  async fn dts_enum_export() {
    transform_dts_test(
      r#"export enum Foo { A, B }"#,
      "export declare enum Foo {\n  A,\n  B\n}",
    )
    .await;
    transform_dts_test(
      r#"export const enum Foo { A, B }"#,
      "export declare const enum Foo {\n  A,\n  B\n}",
    )
    .await;

    transform_dts_test(
      r#"export enum Foo { A = "foo", B = "bar" }"#,
      r#"export declare enum Foo {
  A = "foo",
  B = "bar"
}"#,
    )
    .await;

    // TODO: Enum rules https://www.typescriptlang.org/docs/handbook/enums.html
  }

  #[tokio::test]
  async fn dts_default_export() {
    transform_dts_test(
      r#"export default function(a: number, b: number): number {};"#,
      "export default function(a: number, b: number): number;",
    )
    .await;
    transform_dts_test(
      r#"export default function(a: number, b: number): number;
export default function(a: number, b: number): any {
  return foo
};"#,
      "export default function(a: number, b: number): number;",
    )
    .await;
    transform_dts_test(
      r#"export default class {foo = 2};"#,
      r#"export default class {
  foo: number;
}"#,
    )
    .await;
    transform_dts_test(
      r#"export default 42;"#,
      r#"declare const _dts_1: number;
export default _dts_1;"#,
    )
    .await;
    // TODO
    //     transform_dts_test(
    //       r#"const a: number = 42; export default a;"#,
    //       r#"declare const a: number;
    // export default a;"#,
    //     )
    //     .await;
  }

  #[tokio::test]
  async fn dts_default_export_named() {
    transform_dts_test(
      r#"export { foo, bar } from "foo";"#,
      r#"export { foo, bar } from "foo";"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_default_export_all() {
    transform_dts_test(
      r#"export * as foo from "foo";"#,
      r#"export * as foo from "foo";"#,
    )
    .await;
  }

  #[tokio::test]
  async fn dts_imports() {
    transform_dts_test(
      r#"import { foo } from "bar";export const foobar = foo;"#,
      r#"import { foo } from "bar";
export declare const foobar: any;"#,
    )
    .await;
  }
}
