use deno_ast::swc::ast::*;
use deno_ast::swc::common::DUMMY_SP;
use deno_ast::ModuleSpecifier;
use deno_ast::ParsedSource;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;

use crate::FastCheckDiagnostic;
use crate::FastCheckDiagnosticRange;

use super::swc_helpers::any_type_ann;
use super::swc_helpers::maybe_lit_to_ts_type;
use super::swc_helpers::maybe_lit_to_ts_type_const;
use super::swc_helpers::ts_readonly;
use super::swc_helpers::ts_tuple_element;
use super::swc_helpers::type_ann;

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

  pub fn range(&self) -> Option<&FastCheckDiagnosticRange> {
    match self {
      FastCheckDtsDiagnostic::UnableToInferType { range } => Some(range),
      FastCheckDtsDiagnostic::UnableToInferTypeFallbackAny { range } => {
        Some(range)
      }
      FastCheckDtsDiagnostic::UnableToInferTypeFromProp { range } => {
        Some(range)
      }
      FastCheckDtsDiagnostic::UnableToInferTypeFromSpread { range } => {
        Some(range)
      }
      FastCheckDtsDiagnostic::UnsupportedUsing { range } => Some(range),
    }
  }
}

pub struct FastCheckDtsTransformer<'a> {
  id_counter: usize,
  parsed_source: &'a ParsedSource,
  pub diagnostics: Vec<FastCheckDtsDiagnostic>,
  specifier: &'a ModuleSpecifier,
}

impl<'a> FastCheckDtsTransformer<'a> {
  pub fn new(
    parsed_source: &'a ParsedSource,
    specifier: &'a ModuleSpecifier,
  ) -> Self {
    Self {
      id_counter: 0,
      parsed_source,
      specifier,
      diagnostics: vec![],
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
    range: SourceRange,
  ) -> FastCheckDiagnosticRange {
    FastCheckDiagnosticRange {
      specifier: self.specifier.clone(),
      text_info: self.parsed_source.text_info().clone(),
      range,
    }
  }

  fn mark_diagnostic_unable_to_infer(&mut self, range: SourceRange) {
    self.mark_diagnostic(FastCheckDtsDiagnostic::UnableToInferType {
      range: self.source_range_to_range(range),
    })
  }

  fn mark_diagnostic_any_fallback(&mut self, range: SourceRange) {
    self.mark_diagnostic(FastCheckDtsDiagnostic::UnableToInferTypeFallbackAny {
      range: self.source_range_to_range(range),
    })
  }

  fn mark_diagnostic_unsupported_prop(&mut self, range: SourceRange) {
    self.mark_diagnostic(FastCheckDtsDiagnostic::UnableToInferTypeFromProp {
      range: self.source_range_to_range(range),
    })
  }

  pub fn transform(
    &mut self,
    mut module: Module,
  ) -> Result<Module, Vec<FastCheckDiagnostic>> {
    let body = module.body;

    module.body = self.transform_module_items(body);
    Ok(module)
  }

  fn transform_module_items(
    &mut self,
    body: Vec<ModuleItem>,
  ) -> Vec<ModuleItem> {
    let mut new_items: Vec<ModuleItem> = vec![];
    let mut prev_is_overload = false;

    for item in body {
      match item {
        ModuleItem::ModuleDecl(module_decl) => match module_decl {
          ModuleDecl::Import(_) => {
            prev_is_overload = false;
            new_items.push(ModuleItem::ModuleDecl(module_decl));
          }
          ModuleDecl::ExportDecl(export_decl) => {
            let is_overload = if let Decl::Fn(fn_decl) = &export_decl.decl {
              fn_decl.function.body.is_none()
            } else {
              false
            };

            let should_keep = prev_is_overload && !is_overload;
            prev_is_overload = is_overload;
            if should_keep {
              continue;
            }

            if let Some(decl) = self.decl_to_type_decl(export_decl.decl.clone())
            {
              new_items.push(ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(
                ExportDecl {
                  decl,
                  span: export_decl.span,
                },
              )));
            } else {
              self.mark_diagnostic(FastCheckDtsDiagnostic::UnableToInferType {
                range: self.source_range_to_range(export_decl.range()),
              })
            }
          }
          ModuleDecl::ExportDefaultDecl(export_decl) => {
            let mut is_overload = false;

            let value = match export_decl.decl {
              DefaultDecl::Class(mut class_expr) => {
                class_expr.class.body =
                  self.class_body_to_type(class_expr.class.body);
                ExportDefaultDecl {
                  span: export_decl.span,
                  decl: DefaultDecl::Class(class_expr),
                }
              }
              DefaultDecl::Fn(mut fn_expr) => {
                is_overload = fn_expr.function.body.is_none();

                fn_expr.function.body = None;
                ExportDefaultDecl {
                  span: export_decl.span,
                  decl: DefaultDecl::Fn(fn_expr),
                }
              }
              DefaultDecl::TsInterfaceDecl(_) => export_decl,
            };

            let should_keep = prev_is_overload && !is_overload;
            prev_is_overload = is_overload;
            if should_keep {
              continue;
            }

            new_items.push(ModuleItem::ModuleDecl(
              ModuleDecl::ExportDefaultDecl(value),
            ))
          }
          ModuleDecl::ExportDefaultExpr(export_default_expr) => {
            let is_overload =
              if let Expr::Fn(fn_expr) = &*export_default_expr.expr {
                fn_expr.function.body.is_none()
              } else {
                false
              };
            let should_keep = prev_is_overload && !is_overload;
            prev_is_overload = is_overload;
            if should_keep {
              continue;
            }

            let name = self.gen_unique_name();
            let name_ident = Ident::new(name.into(), DUMMY_SP);
            let type_ann = self
              .expr_to_ts_type(*export_default_expr.expr.clone(), false, true)
              .map(type_ann);

            if let Some(type_ann) = type_ann {
              new_items.push(ModuleItem::Stmt(Stmt::Decl(Decl::Var(
                Box::new(VarDecl {
                  span: DUMMY_SP,
                  kind: VarDeclKind::Const,
                  declare: true,
                  decls: vec![VarDeclarator {
                    span: DUMMY_SP,
                    name: Pat::Ident(BindingIdent {
                      id: name_ident.clone(),
                      type_ann: Some(type_ann),
                    }),
                    init: None,
                    definite: false,
                  }],
                }),
              ))));

              new_items.push(ModuleItem::ModuleDecl(
                ModuleDecl::ExportDefaultExpr(ExportDefaultExpr {
                  span: export_default_expr.span,
                  expr: Box::new(Expr::Ident(name_ident)),
                }),
              ))
            } else {
              new_items.push(ModuleItem::ModuleDecl(
                ModuleDecl::ExportDefaultExpr(ExportDefaultExpr {
                  span: export_default_expr.span,
                  expr: export_default_expr.expr,
                }),
              ))
            }
          }
          // Keep all these
          ModuleDecl::TsImportEquals(_)
          | ModuleDecl::TsNamespaceExport(_)
          | ModuleDecl::TsExportAssignment(_)
          | ModuleDecl::ExportNamed(_)
          | ModuleDecl::ExportAll(_) => {
            prev_is_overload = false;
            new_items.push(ModuleItem::ModuleDecl(module_decl));
          }
        },
        ModuleItem::Stmt(stmt) => {
          prev_is_overload = false;
          if let Stmt::Decl(decl) = stmt {
            match decl {
              Decl::TsEnum(_)
              | Decl::Class(_)
              | Decl::Fn(_)
              | Decl::Var(_)
              | Decl::TsModule(_) => {
                if let Some(decl) = self.decl_to_type_decl(decl.clone()) {
                  new_items.push(ModuleItem::Stmt(Stmt::Decl(decl)));
                } else {
                  self.mark_diagnostic_unable_to_infer(decl.range())
                }
              }

              Decl::TsInterface(_) | Decl::TsTypeAlias(_) | Decl::Using(_) => {
                new_items.push(ModuleItem::Stmt(Stmt::Decl(decl)));
              }
            }
          }
        }
      }
    }

    new_items
  }

  fn expr_to_ts_type(
    &mut self,
    expr: Expr,
    as_const: bool,
    as_readonly: bool,
  ) -> Option<TsType> {
    match expr {
      Expr::Array(arr) => {
        let mut elem_types: Vec<TsTupleElement> = vec![];

        for elems in arr.elems {
          if let Some(expr_or_spread) = elems {
            if let Some(ts_expr) = self.expr_to_ts_type(
              *expr_or_spread.expr.clone(),
              as_const,
              as_readonly,
            ) {
              elem_types.push(ts_tuple_element(ts_expr));
            } else {
              self.mark_diagnostic_unable_to_infer(expr_or_spread.range());
            }
          } else {
            // TypeScript converts holey arrays to any
            // Example: const a = [,,] -> const a = [any, any, any]
            elem_types.push(ts_tuple_element(TsType::TsKeywordType(
              TsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                span: DUMMY_SP,
              },
            )))
          }
        }

        let mut result = TsType::TsTupleType(TsTupleType {
          span: arr.span,
          elem_types,
        });

        if as_readonly {
          result = ts_readonly(result);
        }
        Some(result)
      }
      Expr::Object(obj) => {
        let mut members: Vec<TsTypeElement> = vec![];

        // TODO: Prescan all object properties to know which ones
        // have a getter or a setter. This allows us to apply
        // TypeScript's `readonly` keyword accordingly.

        for item in obj.props {
          match item {
            PropOrSpread::Prop(prop_box) => {
              let prop = *prop_box;
              match prop {
                Prop::KeyValue(key_value) => {
                  let (key, computed) = match key_value.key {
                    PropName::Ident(ident) => (Expr::Ident(ident), false),
                    PropName::Str(str_prop) => {
                      (Expr::Lit(Lit::Str(str_prop)), false)
                    }
                    PropName::Num(num) => (Expr::Lit(Lit::Num(num)), true),
                    PropName::Computed(computed) => (*computed.expr, true),
                    PropName::BigInt(big_int) => {
                      (Expr::Lit(Lit::BigInt(big_int)), true)
                    }
                  };

                  let init_type = self
                    .expr_to_ts_type(*key_value.value, as_const, as_readonly)
                    .map(type_ann);

                  members.push(TsTypeElement::TsPropertySignature(
                    TsPropertySignature {
                      span: DUMMY_SP,
                      readonly: as_readonly,
                      key: Box::new(key),
                      computed,
                      optional: false,
                      init: None,
                      params: vec![],
                      type_ann: init_type,
                      type_params: None,
                    },
                  ));
                }
                Prop::Shorthand(_)
                | Prop::Assign(_)
                | Prop::Getter(_)
                | Prop::Setter(_)
                | Prop::Method(_) => {
                  self.mark_diagnostic_unsupported_prop(prop.range());
                }
              }
            }
            PropOrSpread::Spread(_) => self.mark_diagnostic(
              FastCheckDtsDiagnostic::UnableToInferTypeFromSpread {
                range: self.source_range_to_range(item.range()),
              },
            ),
          }
        }

        Some(TsType::TsTypeLit(TsTypeLit {
          span: obj.span,
          members,
        }))
      }
      Expr::Lit(lit) => {
        if as_const {
          maybe_lit_to_ts_type_const(&lit)
        } else {
          maybe_lit_to_ts_type(&lit)
        }
      }
      Expr::TsConstAssertion(ts_const) => {
        self.expr_to_ts_type(*ts_const.expr, true, true)
      }
      Expr::TsSatisfies(satisifies) => {
        self.expr_to_ts_type(*satisifies.expr, as_const, as_readonly)
      }
      Expr::TsAs(ts_as) => Some(*ts_as.type_ann),
      Expr::Fn(fn_expr) => {
        let return_type = fn_expr
          .function
          .return_type
          .map_or(any_type_ann(), |val| val);

        let params: Vec<TsFnParam> = fn_expr
          .function
          .params
          .into_iter()
          .filter_map(|param| self.pat_to_ts_fn_param(param.pat))
          .collect();

        Some(TsType::TsFnOrConstructorType(
          TsFnOrConstructorType::TsFnType(TsFnType {
            span: fn_expr.function.span,
            params,
            type_ann: return_type,
            type_params: fn_expr.function.type_params,
          }),
        ))
      }
      Expr::Arrow(arrow_expr) => {
        let return_type =
          arrow_expr.return_type.map_or(any_type_ann(), |val| val);

        let params = arrow_expr
          .params
          .into_iter()
          .filter_map(|pat| self.pat_to_ts_fn_param(pat))
          .collect();

        Some(TsType::TsFnOrConstructorType(
          TsFnOrConstructorType::TsFnType(TsFnType {
            span: arrow_expr.span,
            params,
            type_ann: return_type,
            type_params: arrow_expr.type_params,
          }),
        ))
      }
      // Since fast check requires explicit type annotations these
      // can be dropped as they are not part of an export declaration
      Expr::This(_)
      | Expr::Unary(_)
      | Expr::Update(_)
      | Expr::Bin(_)
      | Expr::Assign(_)
      | Expr::Member(_)
      | Expr::SuperProp(_)
      | Expr::Cond(_)
      | Expr::Call(_)
      | Expr::New(_)
      | Expr::Seq(_)
      | Expr::Ident(_)
      | Expr::Tpl(_)
      | Expr::TaggedTpl(_)
      | Expr::Class(_)
      | Expr::Yield(_)
      | Expr::MetaProp(_)
      | Expr::Await(_)
      | Expr::Paren(_)
      | Expr::JSXMember(_)
      | Expr::JSXNamespacedName(_)
      | Expr::JSXEmpty(_)
      | Expr::JSXElement(_)
      | Expr::JSXFragment(_)
      | Expr::TsTypeAssertion(_)
      | Expr::TsNonNull(_)
      | Expr::TsInstantiation(_)
      | Expr::PrivateName(_)
      | Expr::OptChain(_)
      | Expr::Invalid(_) => None,
    }
  }

  fn decl_to_type_decl(&mut self, decl: Decl) -> Option<Decl> {
    match decl {
      Decl::Class(mut class_decl) => {
        class_decl.class.body = self.class_body_to_type(class_decl.class.body);
        class_decl.declare = true;
        Some(Decl::Class(class_decl))
      }
      Decl::Fn(mut fn_decl) => {
        fn_decl.function.body = None;

        for param in &mut fn_decl.function.params {
          match &mut param.pat {
            Pat::Ident(ident) => {
              if ident.type_ann.is_none() {
                self.mark_diagnostic_any_fallback(ident.range());
                ident.type_ann = Some(any_type_ann());
              }
            }
            Pat::Assign(assign_pat) => {
              match &mut *assign_pat.left {
                Pat::Ident(ident) => {
                  if ident.type_ann.is_none() {
                    ident.type_ann = self.infer_expr_fallback_any(
                      *assign_pat.right.clone(),
                      false,
                      false,
                    );
                  }

                  ident.optional = true;
                  param.pat = Pat::Ident(ident.clone());
                }
                Pat::Array(arr_pat) => {
                  if arr_pat.type_ann.is_none() {
                    arr_pat.type_ann = self.infer_expr_fallback_any(
                      *assign_pat.right.clone(),
                      false,
                      false,
                    );
                  }

                  arr_pat.optional = true;
                  param.pat = Pat::Array(arr_pat.clone());
                }
                Pat::Object(obj_pat) => {
                  if obj_pat.type_ann.is_none() {
                    obj_pat.type_ann = self.infer_expr_fallback_any(
                      *assign_pat.right.clone(),
                      false,
                      false,
                    );
                  }

                  obj_pat.optional = true;
                  param.pat = Pat::Object(obj_pat.clone());
                }
                Pat::Rest(_)
                | Pat::Assign(_)
                | Pat::Expr(_)
                | Pat::Invalid(_) => {}
              };
            }
            Pat::Array(_)
            | Pat::Rest(_)
            | Pat::Object(_)
            | Pat::Invalid(_)
            | Pat::Expr(_) => {}
          }
        }

        Some(Decl::Fn(fn_decl))
      }
      Decl::Var(mut var_decl) => {
        var_decl.declare = true;

        for decl in &mut var_decl.decls {
          if let Pat::Ident(ident) = &mut decl.name {
            if ident.type_ann.is_some() {
              decl.init = None;
              continue;
            }

            let ts_type = decl
              .init
              .as_ref()
              .and_then(|init_box| {
                let init = *init_box.clone();
                self.expr_to_ts_type(init, false, true)
              })
              .map(type_ann)
              .or_else(|| {
                self.mark_diagnostic_any_fallback(ident.range());
                Some(any_type_ann())
              });
            ident.type_ann = ts_type;
          } else {
            self.mark_diagnostic_unable_to_infer(decl.range());
          }

          decl.init = None;
        }

        Some(Decl::Var(var_decl))
      }
      Decl::TsEnum(mut ts_enum) => {
        ts_enum.declare = true;

        for member in &mut ts_enum.members {
          if let Some(init) = &member.init {
            // Support for expressions is limited in enums,
            // see https://www.typescriptlang.org/docs/handbook/enums.html
            member.init = if self.valid_enum_init_expr(*init.clone()) {
              Some(init.clone())
            } else {
              None
            };
          }
        }

        Some(Decl::TsEnum(ts_enum))
      }
      Decl::TsModule(mut ts_module) => {
        ts_module.declare = true;

        if let Some(body) = ts_module.body.clone() {
          ts_module.body = Some(self.transform_ts_ns_body(body));

          Some(Decl::TsModule(ts_module))
        } else {
          Some(Decl::TsModule(ts_module))
        }
      }
      Decl::TsInterface(_) | Decl::TsTypeAlias(_) => Some(decl),
      Decl::Using(_) => {
        self.mark_diagnostic(FastCheckDtsDiagnostic::UnsupportedUsing {
          range: self.source_range_to_range(decl.range()),
        });
        None
      }
    }
  }

  fn transform_ts_ns_body(&mut self, ns: TsNamespaceBody) -> TsNamespaceBody {
    match ns {
      TsNamespaceBody::TsModuleBlock(mut ts_module_block) => {
        ts_module_block.body =
          self.transform_module_items(ts_module_block.body);
        TsNamespaceBody::TsModuleBlock(ts_module_block)
      }
      TsNamespaceBody::TsNamespaceDecl(ts_ns) => {
        self.transform_ts_ns_body(*ts_ns.body)
      }
    }
  }

  // Support for expressions is limited in enums,
  // see https://www.typescriptlang.org/docs/handbook/enums.html
  fn valid_enum_init_expr(&mut self, expr: Expr) -> bool {
    match expr {
      Expr::Bin(bin_expr) => {
        if !self.valid_enum_init_expr(*bin_expr.left) {
          false
        } else {
          self.valid_enum_init_expr(*bin_expr.right)
        }
      }

      Expr::Member(member_expr) => self.valid_enum_init_expr(*member_expr.obj),
      Expr::OptChain(opt_expr) => match *opt_expr.base {
        OptChainBase::Member(member_expr) => {
          self.valid_enum_init_expr(Expr::Member(member_expr))
        }
        OptChainBase::Call(_) => false,
      },
      // TS does infer the type of identifiers
      Expr::Ident(_) => true,
      Expr::Lit(lit) => match lit {
        Lit::Num(_) | Lit::Str(_) => true,
        Lit::Bool(_)
        | Lit::Null(_)
        | Lit::BigInt(_)
        | Lit::Regex(_)
        | Lit::JSXText(_) => false,
      },
      Expr::Tpl(tpl_expr) => {
        for expr in tpl_expr.exprs {
          if !self.valid_enum_init_expr(*expr) {
            return false;
          }
        }
        true
      }

      Expr::Paren(paren_expr) => self.valid_enum_init_expr(*paren_expr.expr),

      Expr::TsTypeAssertion(ts_ass) => {
        // Only assertions to number are allowed for computed
        // enum members.
        match *ts_ass.type_ann {
          TsType::TsLitType(ts_lit) => match ts_lit.lit {
            TsLit::Number(_) => true,
            TsLit::Str(_)
            | TsLit::Bool(_)
            | TsLit::BigInt(_)
            | TsLit::Tpl(_) => false,
          },
          TsType::TsKeywordType(_)
          | TsType::TsThisType(_)
          | TsType::TsFnOrConstructorType(_)
          | TsType::TsTypeRef(_)
          | TsType::TsTypeQuery(_)
          | TsType::TsTypeLit(_)
          | TsType::TsArrayType(_)
          | TsType::TsTupleType(_)
          | TsType::TsOptionalType(_)
          | TsType::TsRestType(_)
          | TsType::TsUnionOrIntersectionType(_)
          | TsType::TsConditionalType(_)
          | TsType::TsInferType(_)
          | TsType::TsParenthesizedType(_)
          | TsType::TsTypeOperator(_)
          | TsType::TsIndexedAccessType(_)
          | TsType::TsMappedType(_)
          | TsType::TsTypePredicate(_)
          | TsType::TsImportType(_) => false,
        }
      }

      Expr::TsAs(ts_as) => self.valid_enum_ts_type(*ts_as.type_ann),

      // These are not valid as enum member initializer and
      // TS will throw a type error. For declaration generation
      // they will be dropped in TS so we do that too.
      Expr::TsInstantiation(_)
      | Expr::Call(_)
      | Expr::Update(_)
      | Expr::PrivateName(_)
      | Expr::TsSatisfies(_)
      | Expr::TsNonNull(_)
      | Expr::TsConstAssertion(_)
      | Expr::Cond(_)
      | Expr::Seq(_)
      | Expr::TaggedTpl(_)
      | Expr::Object(_)
      | Expr::Array(_)
      | Expr::Arrow(_)
      | Expr::Class(_)
      | Expr::Await(_)
      | Expr::MetaProp(_)
      | Expr::New(_)
      | Expr::JSXMember(_)
      | Expr::JSXNamespacedName(_)
      | Expr::JSXEmpty(_)
      | Expr::JSXElement(_)
      | Expr::JSXFragment(_)
      | Expr::Unary(_)
      | Expr::Assign(_)
      | Expr::Yield(_)
      | Expr::SuperProp(_)
      | Expr::Fn(_)
      | Expr::This(_)
      | Expr::Invalid(_) => false,
    }
  }

  fn valid_enum_ts_type(&mut self, ts_type: TsType) -> bool {
    match ts_type {
      TsType::TsLitType(ts_lit) => match ts_lit.lit {
        TsLit::Number(_) => true,
        TsLit::Str(_) | TsLit::Bool(_) | TsLit::BigInt(_) | TsLit::Tpl(_) => {
          false
        }
      },
      TsType::TsKeywordType(_)
      | TsType::TsThisType(_)
      | TsType::TsFnOrConstructorType(_)
      | TsType::TsTypeRef(_)
      | TsType::TsTypeQuery(_)
      | TsType::TsTypeLit(_)
      | TsType::TsArrayType(_)
      | TsType::TsTupleType(_)
      | TsType::TsOptionalType(_)
      | TsType::TsRestType(_)
      | TsType::TsUnionOrIntersectionType(_)
      | TsType::TsConditionalType(_)
      | TsType::TsInferType(_)
      | TsType::TsParenthesizedType(_)
      | TsType::TsTypeOperator(_)
      | TsType::TsIndexedAccessType(_)
      | TsType::TsMappedType(_)
      | TsType::TsTypePredicate(_)
      | TsType::TsImportType(_) => false,
    }
  }

  fn infer_expr_fallback_any(
    &mut self,
    expr: Expr,
    as_const: bool,
    as_readonly: bool,
  ) -> Option<Box<TsTypeAnn>> {
    if let Some(ts_type) =
      self.expr_to_ts_type(expr.clone(), as_const, as_readonly)
    {
      Some(type_ann(ts_type))
    } else {
      self.mark_diagnostic_any_fallback(expr.range());
      Some(any_type_ann())
    }
  }

  fn class_body_to_type(&mut self, body: Vec<ClassMember>) -> Vec<ClassMember> {
    // Track if the previous member was an overload signature or not.
    // When overloads are present the last item has the implementation
    // body. For declaration files the implementation always needs to
    // be dropped. Needs to be unique for each class because another
    // class could be created inside a class method.
    let mut prev_is_overload = false;

    body
      .into_iter()
      .filter(|member| match member {
        ClassMember::Constructor(class_constructor) => {
          let is_overload = class_constructor.body.is_none();
          if !prev_is_overload || is_overload {
            prev_is_overload = is_overload;
            true
          } else {
            prev_is_overload = false;
            false
          }
        }
        ClassMember::Method(method) => {
          let is_overload = method.function.body.is_none();
          if !prev_is_overload || is_overload {
            prev_is_overload = is_overload;
            true
          } else {
            prev_is_overload = false;
            false
          }
        }
        ClassMember::TsIndexSignature(_)
        | ClassMember::ClassProp(_)
        | ClassMember::PrivateProp(_)
        | ClassMember::Empty(_)
        | ClassMember::StaticBlock(_)
        | ClassMember::AutoAccessor(_)
        | ClassMember::PrivateMethod(_) => {
          prev_is_overload = false;
          true
        }
      })
      .filter_map(|member| match member {
        ClassMember::Constructor(mut class_constructor) => {
          class_constructor.body = None;
          Some(ClassMember::Constructor(class_constructor))
        }
        ClassMember::Method(mut method) => {
          method.function.body = None;
          if method.kind == MethodKind::Setter {
            method.function.return_type = None;
          }
          Some(ClassMember::Method(method))
        }
        ClassMember::ClassProp(mut prop) => {
          if prop.type_ann.is_none() {
            if let Some(value) = prop.value {
              prop.type_ann = self
                .expr_to_ts_type(*value, false, false)
                .map(type_ann)
                .or_else(|| Some(any_type_ann()));
            }
          }
          prop.value = None;
          prop.definite = false;
          prop.declare = false;

          Some(ClassMember::ClassProp(prop))
        }
        ClassMember::TsIndexSignature(index_sig) => {
          Some(ClassMember::TsIndexSignature(index_sig))
        }

        // These can be removed as they are not relevant for types
        ClassMember::PrivateMethod(_)
        | ClassMember::PrivateProp(_)
        | ClassMember::Empty(_)
        | ClassMember::StaticBlock(_)
        | ClassMember::AutoAccessor(_) => None,
      })
      .collect()
  }

  fn pat_to_ts_fn_param(&mut self, pat: Pat) -> Option<TsFnParam> {
    match pat {
      Pat::Ident(binding_id) => Some(TsFnParam::Ident(binding_id)),
      Pat::Array(arr_pat) => Some(TsFnParam::Array(arr_pat)),
      Pat::Rest(rest_pat) => Some(TsFnParam::Rest(rest_pat)),
      Pat::Object(obj) => Some(TsFnParam::Object(obj)),
      Pat::Assign(assign_pat) => self
        .expr_to_ts_type(*assign_pat.right, false, false)
        .map(|param| {
          let name = if let Pat::Ident(ident) = *assign_pat.left {
            ident.id.sym.as_str().to_string()
          } else {
            self.gen_unique_name()
          };

          TsFnParam::Ident(BindingIdent {
            id: Ident::new(name.into(), assign_pat.span),
            type_ann: Some(type_ann(param)),
          })
        }),
      Pat::Expr(expr) => {
        self.mark_diagnostic_unable_to_infer(expr.range());
        None
      }
      // Invalid code is invalid, not sure why SWC doesn't throw
      // a parse error here.
      Pat::Invalid(_) => None,
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::fast_check::swc_helpers::emit;
  use crate::fast_check::transform_dts::FastCheckDtsTransformer;
  use crate::source::MemoryLoader;
  use crate::source::Source;
  use crate::symbols::RootSymbol;
  use crate::BuildOptions;
  use crate::DefaultModuleParser;
  use crate::GraphKind;
  use crate::ModuleGraph;
  use url::Url;

  async fn transform_dts_test(source: &str, expected: &str) {
    let specifier = Url::parse("file:///mod.ts").unwrap();

    let mut loader = MemoryLoader::new(
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
    let parser = DefaultModuleParser {};
    let mut graph = ModuleGraph::new(GraphKind::All);
    graph
      .build(
        vec![specifier.clone()],
        &mut loader,
        BuildOptions {
          module_parser: Some(&parser),
          ..Default::default()
        },
      )
      .await;

    let root_sym = RootSymbol::new(&graph, &parser);

    let module_info = root_sym
      .module_from_specifier(&specifier)
      .unwrap()
      .esm()
      .unwrap();

    let parsed_source = module_info.source();
    let module = parsed_source.module().to_owned();

    let mut transformer =
      FastCheckDtsTransformer::new(parsed_source, &specifier);
    let module = transformer.transform(module).unwrap();

    let comments = parsed_source.comments().as_single_threaded();

    let (actual, _) =
      emit(&specifier, &comments, parsed_source.text_info(), &module).unwrap();

    assert_eq!(actual.trim(), expected.trim());
  }

  #[tokio::test]
  async fn dts_function_test() {
    transform_dts_test(
      r#"export function foo(a: number): number {
  return {};
}"#,
      "export function foo(a: number): number;",
    )
    .await;
    transform_dts_test(
      r#"export function foo(a: string): number;
export function foo(a: any): number {
  return {};
}"#,
      r#"export function foo(a: string): number;"#,
    )
    .await;
    transform_dts_test(
      r#"export function foo(a = 2): number {
  return 2;
}"#,
      r#"export function foo(a?: number): number;"#,
    )
    .await;
    transform_dts_test(
      r#"export function foo(a: string = 2): number {
  return 2;
}"#,
      r#"export function foo(a?: string): number;"#,
    )
    .await;
    transform_dts_test(
      r#"export function foo([a, b] = [1, 2]): number {
  return 2;
}"#,
      r#"export function foo([a, b]?: [number, number]): number;"#,
    )
    .await;
    transform_dts_test(
      r#"export function foo({a, b} = { a: 1, b: 2 }): number {
  return 2;
}"#,
      r#"export function foo({ a, b }?: {
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
      r#"export declare const foo: {
};"#,
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
    transform_dts_test(r#"interface Foo {}"#, "interface Foo {\n}").await;
    transform_dts_test(r#"type Foo = number;"#, "type Foo = number;").await;

    transform_dts_test(
      r#"export interface Foo {}"#,
      "export interface Foo {\n}",
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
