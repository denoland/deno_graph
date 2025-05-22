use deno_ast::swc::ast::*;
use deno_ast::swc::common::SyntaxContext;
use deno_ast::swc::common::DUMMY_SP;
use deno_ast::ModuleSpecifier;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfo;

use crate::FastCheckDiagnosticRange;

use super::range_finder::ModulePublicRanges;
use super::swc_helpers::any_type_ann;
use super::swc_helpers::maybe_lit_to_ts_type;
use super::swc_helpers::ts_readonly;
use super::swc_helpers::ts_tuple_element;
use super::swc_helpers::type_ann;
use super::swc_helpers::DeclMutabilityKind;

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

pub struct FastCheckDtsTransformer<'a> {
  id_counter: usize,
  text_info: &'a SourceTextInfo,
  public_ranges: &'a ModulePublicRanges,
  pub diagnostics: Vec<FastCheckDtsDiagnostic>,
  specifier: &'a ModuleSpecifier,
  is_top_level: bool,
}

impl<'a> FastCheckDtsTransformer<'a> {
  pub fn new(
    text_info: &'a SourceTextInfo,
    public_ranges: &'a ModulePublicRanges,
    specifier: &'a ModuleSpecifier,
  ) -> Self {
    Self {
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
    range: SourceRange,
  ) -> FastCheckDiagnosticRange {
    FastCheckDiagnosticRange {
      specifier: self.specifier.clone(),
      text_info: self.text_info.clone(),
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

  pub fn transform(&mut self, program: Program) -> Program {
    self.is_top_level = true;
    match program {
      Program::Module(mut module) => {
        let body = module.body;
        module.body = self.transform_module_items(body);
        Program::Module(module)
      }
      Program::Script(mut script) => {
        script.body = script
          .body
          .into_iter()
          .filter_map(|stmt| {
            let new_stmt = self.transform_module_stmt(stmt)?;
            Some(new_stmt)
          })
          .collect();
        Program::Script(script)
      }
    }
  }

  fn transform_module_items(
    &mut self,
    body: Vec<ModuleItem>,
  ) -> Vec<ModuleItem> {
    let mut new_items: Vec<ModuleItem> = vec![];

    for item in body {
      match item {
        ModuleItem::ModuleDecl(module_decl) => match module_decl {
          ModuleDecl::Import(_) => {
            new_items.push(ModuleItem::ModuleDecl(module_decl));
          }
          ModuleDecl::ExportDecl(export_decl) => {
            if let Decl::Fn(_) = &export_decl.decl {
              if self
                .public_ranges
                .is_impl_with_overloads(&export_decl.range())
              {
                continue; // skip implementation signature
              }
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
                if self
                  .public_ranges
                  .is_impl_with_overloads(&export_decl.span.range())
                {
                  continue; // skip implementation signature
                }

                fn_expr.function.body = None;
                ExportDefaultDecl {
                  span: export_decl.span,
                  decl: DefaultDecl::Fn(fn_expr),
                }
              }
              DefaultDecl::TsInterfaceDecl(_) => export_decl,
            };

            new_items.push(ModuleItem::ModuleDecl(
              ModuleDecl::ExportDefaultDecl(value),
            ))
          }
          ModuleDecl::ExportDefaultExpr(export_default_expr) => {
            let name = self.gen_unique_name();
            let name_ident =
              Ident::new(name.into(), DUMMY_SP, SyntaxContext::default());
            let type_ann = self
              .expr_to_ts_type(*export_default_expr.expr.clone(), false, true)
              .map(type_ann);

            if let Some(type_ann) = type_ann {
              new_items.push(ModuleItem::Stmt(Stmt::Decl(Decl::Var(
                Box::new(VarDecl {
                  span: DUMMY_SP,
                  ctxt: Default::default(),
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
            new_items.push(ModuleItem::ModuleDecl(module_decl));
          }
        },
        ModuleItem::Stmt(stmt) => {
          if let Some(new_stmt) = self.transform_module_stmt(stmt) {
            new_items.push(ModuleItem::Stmt(new_stmt));
          }
        }
      }
    }

    new_items
  }

  fn transform_module_stmt(&mut self, stmt: Stmt) -> Option<Stmt> {
    let Stmt::Decl(decl) = stmt else {
      return None;
    };
    if let Decl::Fn(_) = &decl {
      if self.public_ranges.is_impl_with_overloads(&decl.range()) {
        return None; // skip implementation signature
      }
    }
    match decl {
      Decl::TsEnum(_)
      | Decl::Class(_)
      | Decl::Fn(_)
      | Decl::Var(_)
      | Decl::TsModule(_) => {
        if let Some(decl) = self.decl_to_type_decl(decl.clone()) {
          Some(Stmt::Decl(decl))
        } else {
          self.mark_diagnostic_unable_to_infer(decl.range());
          None
        }
      }
      Decl::TsInterface(_) | Decl::TsTypeAlias(_) | Decl::Using(_) => {
        Some(Stmt::Decl(decl))
      }
    }
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
                    PropName::Ident(ident_name) => (
                      Expr::Ident(Ident {
                        span: ident_name.span,
                        ctxt: SyntaxContext::default(),
                        sym: ident_name.sym,
                        optional: false,
                      }),
                      false,
                    ),
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
                      type_ann: init_type,
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
      Expr::Lit(lit) => maybe_lit_to_ts_type(
        &lit,
        match as_const {
          true => DeclMutabilityKind::Const,
          false => DeclMutabilityKind::Mutable,
        },
      ),
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
    let is_declare = self.is_top_level;
    match decl {
      Decl::Class(mut class_decl) => {
        class_decl.class.body = self.class_body_to_type(class_decl.class.body);
        class_decl.declare = is_declare;
        Some(Decl::Class(class_decl))
      }
      Decl::Fn(mut fn_decl) => {
        fn_decl.function.body = None;
        fn_decl.declare = is_declare;

        self.handle_func_params(&mut fn_decl.function.params);
        Some(Decl::Fn(fn_decl))
      }
      Decl::Var(mut var_decl) => {
        var_decl.declare = is_declare;

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
        ts_enum.declare = is_declare;

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
        ts_module.declare = is_declare;

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
    let original_is_top_level = self.is_top_level;
    self.is_top_level = false;
    let body = match ns {
      TsNamespaceBody::TsModuleBlock(mut ts_module_block) => {
        ts_module_block.body =
          self.transform_module_items(ts_module_block.body);
        TsNamespaceBody::TsModuleBlock(ts_module_block)
      }
      TsNamespaceBody::TsNamespaceDecl(ts_ns) => {
        self.transform_ts_ns_body(*ts_ns.body)
      }
    };
    self.is_top_level = original_is_top_level;
    body
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
  ) -> Box<TsTypeAnn> {
    if let Some(ts_type) =
      self.expr_to_ts_type(expr.clone(), as_const, as_readonly)
    {
      type_ann(ts_type)
    } else {
      self.mark_diagnostic_any_fallback(expr.range());
      any_type_ann()
    }
  }

  fn class_body_to_type(&mut self, body: Vec<ClassMember>) -> Vec<ClassMember> {
    body
      .into_iter()
      .filter(|member| match member {
        ClassMember::Constructor(constructor) => !self
          .public_ranges
          .is_impl_with_overloads(&constructor.range()),
        ClassMember::Method(method) => {
          !self.public_ranges.is_impl_with_overloads(&method.range())
        }
        ClassMember::TsIndexSignature(_)
        | ClassMember::ClassProp(_)
        | ClassMember::PrivateProp(_)
        | ClassMember::Empty(_)
        | ClassMember::StaticBlock(_)
        | ClassMember::AutoAccessor(_)
        | ClassMember::PrivateMethod(_) => true,
      })
      .filter_map(|member| match member {
        ClassMember::Constructor(mut class_constructor) => {
          class_constructor.body = None;
          self.handle_ts_param_props(&mut class_constructor.params);
          Some(ClassMember::Constructor(class_constructor))
        }
        ClassMember::Method(mut method) => {
          if let Some(new_prop_name) = valid_prop_name(&method.key) {
            method.key = new_prop_name;
          } else {
            return None;
          }

          method.function.body = None;
          if method.kind == MethodKind::Setter {
            method.function.return_type = None;
          }
          self.handle_func_params(&mut method.function.params);
          Some(ClassMember::Method(method))
        }
        ClassMember::ClassProp(mut prop) => {
          if let Some(new_prop_name) = valid_prop_name(&prop.key) {
            prop.key = new_prop_name;
          } else {
            return None;
          }
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

  fn handle_ts_param_props(
    &mut self,
    param_props: &mut Vec<ParamOrTsParamProp>,
  ) {
    for param in param_props {
      match param {
        ParamOrTsParamProp::TsParamProp(param) => {
          match &mut param.param {
            TsParamPropParam::Ident(ident) => {
              self.handle_func_param_ident(ident);
            }
            TsParamPropParam::Assign(assign) => {
              if let Some(new_pat) = self.handle_func_param_assign(assign) {
                match new_pat {
                  Pat::Ident(new_ident) => {
                    param.param = TsParamPropParam::Ident(new_ident)
                  }
                  Pat::Assign(new_assign) => {
                    param.param = TsParamPropParam::Assign(new_assign)
                  }
                  Pat::Rest(_)
                  | Pat::Object(_)
                  | Pat::Array(_)
                  | Pat::Invalid(_)
                  | Pat::Expr(_) => {
                    // should never happen for parameter properties
                    unreachable!();
                  }
                }
              }
            }
          }
        }
        ParamOrTsParamProp::Param(param) => self.handle_func_param(param),
      }
    }
  }

  fn handle_func_params(&mut self, params: &mut Vec<Param>) {
    for param in params {
      self.handle_func_param(param);
    }
  }

  fn handle_func_param(&mut self, param: &mut Param) {
    match &mut param.pat {
      Pat::Ident(ident) => {
        self.handle_func_param_ident(ident);
      }
      Pat::Assign(assign_pat) => {
        if let Some(new_pat) = self.handle_func_param_assign(assign_pat) {
          param.pat = new_pat;
        }
      }
      Pat::Array(_)
      | Pat::Rest(_)
      | Pat::Object(_)
      | Pat::Invalid(_)
      | Pat::Expr(_) => {}
    }
  }

  fn handle_func_param_ident(&mut self, ident: &mut BindingIdent) {
    if ident.type_ann.is_none() {
      self.mark_diagnostic_any_fallback(ident.range());
      ident.type_ann = Some(any_type_ann());
    }
  }

  fn handle_func_param_assign(
    &mut self,
    assign_pat: &mut AssignPat,
  ) -> Option<Pat> {
    match &mut *assign_pat.left {
      Pat::Ident(ident) => {
        if ident.type_ann.is_none() {
          ident.type_ann = Some(self.infer_expr_fallback_any(
            *assign_pat.right.clone(),
            false,
            false,
          ));
        }

        ident.optional = true;
        Some(Pat::Ident(ident.clone()))
      }
      Pat::Array(arr_pat) => {
        if arr_pat.type_ann.is_none() {
          arr_pat.type_ann = Some(self.infer_expr_fallback_any(
            *assign_pat.right.clone(),
            false,
            false,
          ));
        }

        arr_pat.optional = true;
        Some(Pat::Array(arr_pat.clone()))
      }
      Pat::Object(obj_pat) => {
        if obj_pat.type_ann.is_none() {
          obj_pat.type_ann = Some(self.infer_expr_fallback_any(
            *assign_pat.right.clone(),
            false,
            false,
          ));
        }

        obj_pat.optional = true;
        Some(Pat::Object(obj_pat.clone()))
      }
      Pat::Rest(_) | Pat::Assign(_) | Pat::Expr(_) | Pat::Invalid(_) => None,
    }
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
            ident.id.sym.clone()
          } else {
            self.gen_unique_name().into()
          };

          TsFnParam::Ident(BindingIdent {
            id: Ident::new(name, assign_pat.span, Default::default()),
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

fn valid_prop_name(prop_name: &PropName) -> Option<PropName> {
  fn prop_name_from_expr(expr: &Expr) -> Option<PropName> {
    match expr {
      Expr::Lit(e) => match &e {
        Lit::Str(e) => Some(PropName::Str(e.clone())),
        Lit::Num(e) => Some(PropName::Num(e.clone())),
        Lit::BigInt(e) => Some(PropName::BigInt(e.clone())),
        Lit::Bool(_) | Lit::Null(_) | Lit::Regex(_) | Lit::JSXText(_) => None,
      },
      Expr::Tpl(e) => {
        if e.quasis.is_empty() && e.exprs.len() == 1 {
          prop_name_from_expr(&e.exprs[0])
        } else {
          None
        }
      }
      Expr::Paren(e) => prop_name_from_expr(&e.expr),
      Expr::TsTypeAssertion(e) => prop_name_from_expr(&e.expr),
      Expr::TsConstAssertion(e) => prop_name_from_expr(&e.expr),
      Expr::TsNonNull(e) => prop_name_from_expr(&e.expr),
      Expr::TsAs(e) => prop_name_from_expr(&e.expr),
      Expr::TsSatisfies(e) => prop_name_from_expr(&e.expr),
      Expr::Ident(_) => Some(PropName::Computed(ComputedPropName {
        #[allow(clippy::disallowed_methods)]
        span: deno_ast::swc::common::Spanned::span(&expr),
        expr: Box::new(expr.clone()),
      })),
      Expr::TaggedTpl(_)
      | Expr::This(_)
      | Expr::Array(_)
      | Expr::Object(_)
      | Expr::Fn(_)
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
      | Expr::Arrow(_)
      | Expr::Class(_)
      | Expr::Yield(_)
      | Expr::Await(_)
      | Expr::MetaProp(_)
      | Expr::JSXMember(_)
      | Expr::JSXNamespacedName(_)
      | Expr::JSXEmpty(_)
      | Expr::JSXElement(_)
      | Expr::JSXFragment(_)
      | Expr::TsInstantiation(_)
      | Expr::PrivateName(_)
      | Expr::OptChain(_)
      | Expr::Invalid(_) => None,
    }
  }

  match prop_name {
    PropName::Ident(_)
    | PropName::Str(_)
    | PropName::Num(_)
    | PropName::BigInt(_) => Some(prop_name.clone()),
    PropName::Computed(computed) => prop_name_from_expr(&computed.expr),
  }
}

#[cfg(test)]
mod tests {
  use std::collections::VecDeque;

  use crate::fast_check::range_finder::find_public_ranges;
  use crate::fast_check::transform_dts::FastCheckDtsTransformer;
  use crate::source::MemoryLoader;
  use crate::source::Source;
  use crate::symbols::RootSymbol;
  use crate::BuildOptions;
  use crate::CapturingModuleAnalyzer;
  use crate::GraphKind;
  use crate::ModuleGraph;
  use crate::WorkspaceMember;
  use deno_ast::emit;
  use deno_ast::EmitOptions;
  use deno_ast::EmittedSourceText;
  use deno_ast::ModuleSpecifier;
  use deno_ast::SourceMap;
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

    let root_sym = RootSymbol::new(&graph, &analyzer);

    let module_info = root_sym
      .module_from_specifier(&specifier)
      .unwrap()
      .esm()
      .unwrap();

    let parsed_source = module_info.source();
    let program = parsed_source.program_ref().to_owned();
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
      parsed_source.text_info_lazy(),
      &public_ranges,
      &specifier,
    );
    let program = transformer.transform(program);

    let comments = parsed_source.comments().as_single_threaded();

    let source_map =
      SourceMap::single(specifier, parsed_source.text().to_string());

    let EmittedSourceText { text: actual, .. } = emit(
      (&program).into(),
      &comments,
      &source_map,
      &EmitOptions {
        remove_comments: false,
        source_map: deno_ast::SourceMapOption::None,
        source_map_base: None,
        source_map_file: None,
        inline_sources: false,
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
