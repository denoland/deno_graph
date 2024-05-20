use std::borrow::Cow;

use deno_ast::diagnostics::DiagnosticSnippetHighlight;
use deno_ast::diagnostics::DiagnosticSnippetHighlightStyle;
use deno_ast::swc::ast::ArrayLit;
use deno_ast::swc::ast::ArrowExpr;
use deno_ast::swc::ast::BindingIdent;
use deno_ast::swc::ast::BlockStmt;
use deno_ast::swc::ast::BlockStmtOrExpr;
use deno_ast::swc::ast::Expr;
use deno_ast::swc::ast::Function;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::ObjectLit;
use deno_ast::swc::ast::ObjectPatProp;
use deno_ast::swc::ast::Pat;
use deno_ast::swc::ast::Prop;
use deno_ast::swc::ast::PropOrSpread;
use deno_ast::swc::ast::Str;
use deno_ast::swc::ast::Tpl;
use deno_ast::swc::ast::TsEntityName;
use deno_ast::swc::ast::TsFnOrConstructorType;
use deno_ast::swc::ast::TsFnParam;
use deno_ast::swc::ast::TsFnType;
use deno_ast::swc::ast::TsGetterSignature;
use deno_ast::swc::ast::TsKeywordTypeKind;
use deno_ast::swc::ast::TsLit;
use deno_ast::swc::ast::TsMethodSignature;
use deno_ast::swc::ast::TsPropertySignature;
use deno_ast::swc::ast::TsSetterSignature;
use deno_ast::swc::ast::TsTupleElement;
use deno_ast::swc::ast::TsTupleType;
use deno_ast::swc::ast::TsType;
use deno_ast::swc::ast::TsTypeAnn;
use deno_ast::swc::ast::TsTypeElement;
use deno_ast::swc::ast::TsTypeLit;
use deno_ast::swc::ast::TsTypeOperator;
use deno_ast::swc::ast::TsTypeOperatorOp;
use deno_ast::swc::common::Spanned;
use deno_ast::swc::common::DUMMY_SP;
use deno_ast::swc::visit::Visit;
use deno_ast::swc::visit::VisitWith;
use deno_ast::ModuleSpecifier;
use deno_ast::ParsedSource;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;

use crate::swc_helpers::analyze_return_stmts_in_function_body;
use crate::swc_helpers::FunctionKind;
use crate::swc_helpers::ts_entity_name_to_parts;
use crate::swc_helpers::ReturnStatementAnalysis;
use crate::symbols::DefinitionPathNode;
use crate::symbols::EsModuleInfo;
use crate::symbols::ModuleInfoRef;
use crate::symbols::ResolvedSymbolDepEntry;
use crate::symbols::RootSymbol;
use crate::symbols::SymbolNodeDep;
use crate::FastCheckDiagnosticRange;

use super::swc_helpers::is_call_expr_symbol_create;
use super::swc_helpers::is_param_pat_optional;
use super::swc_helpers::lit_to_ts_type;
use super::swc_helpers::lit_to_ts_type_const;
use super::swc_helpers::prop_name_to_ts_key;
use super::swc_helpers::ts_keyword_type;
use super::swc_helpers::ts_lit_type;
use super::swc_helpers::void_or_promise_void;

#[derive(Debug, Clone)]
pub enum ExprInferFailCause {
  ObjectLitSpread {
    dot3_token: FastCheckDiagnosticRange,
  },
  ObjectLitShorthand {
    key: FastCheckDiagnosticRange,
  },
  ArrayWithoutAsConst {
    expr: FastCheckDiagnosticRange,
  },
  ArraySpread {
    dot3_token: FastCheckDiagnosticRange,
  },
  ExprInTemplateLiteral {
    expr: FastCheckDiagnosticRange,
  },

  ObjectGetter {
    key: FastCheckDiagnosticRange,
    cause: Box<FunctionInferFailCause>,
  },
  ObjectSetter {
    key: FastCheckDiagnosticRange,
    cause: Box<FunctionInferFailCause>,
  },
  ObjectMethod {
    key: FastCheckDiagnosticRange,
    cause: Box<FunctionInferFailCause>,
  },

  FunctionExpression {
    name: FastCheckDiagnosticRange,
    cause: Box<FunctionInferFailCause>,
  },
  ArrowExpression {
    name: FastCheckDiagnosticRange,
    cause: Box<FunctionInferFailCause>,
  },

  LocalReference {
    ident: FastCheckDiagnosticRange,
  },

  Other {
    expr: FastCheckDiagnosticRange,
  },
}

impl ExprInferFailCause {
  pub fn highlights(&self, highlights: &mut Vec<DiagnosticSnippetHighlight>) {
    // this is always prefixed with
    // "this function's return type could not be inferred because the value returned here"
    // "this function's return type could not be inferred because the returned value"
    // "this variable's type could not be inferred because its initializer"
    match self {
      ExprInferFailCause::ObjectLitSpread { dot3_token } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: dot3_token.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(
            "contains this spread property, which can not be inferred",
          )),
        })
      }
      ExprInferFailCause::ObjectLitShorthand { key } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: key.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(
            "contains this shorthand property, which can not be inferred",
          )),
        })
      }
      ExprInferFailCause::ArrayWithoutAsConst { expr } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: expr.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(
            "contains this array literal, which can not be inferred unless marked 'as const'",
          )),
        })
      }
      ExprInferFailCause::ArraySpread { dot3_token } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: dot3_token.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(
            "contains this spread element, which can not be inferred",
          )),
        })
      }
      ExprInferFailCause::ExprInTemplateLiteral { expr } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: expr.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(
            "contains this expression in a template literal, which can not be inferred",
          )),
        })
      },
      ExprInferFailCause::ObjectGetter { key, cause } => {
        cause.highlights(highlights, key, "contains this object getter", true);
      },
      ExprInferFailCause::ObjectSetter { key, cause } => {
        cause.highlights(highlights, key, "contains this object setter", true);
      },
      ExprInferFailCause::ObjectMethod { key, cause } => {
        cause.highlights(highlights, key, "contains this object method", true);
      },
      ExprInferFailCause::FunctionExpression { name, cause } => {
        cause.highlights(highlights, name, "contains this function expression", true);
      },
      ExprInferFailCause::ArrowExpression { name, cause } => {
        cause.highlights(highlights, name, "contains this arrow expression", true);
      },
      ExprInferFailCause::LocalReference { ident } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: ident.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(
            "contains this reference to a local variable or type",
          )),
        })
      },
      ExprInferFailCause::Other { expr } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: expr.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(
            "contains this expression, of which the type could not be inferred",
          )),
        })
      }
    }
  }

  pub fn hint(&self, location: Cow<'_, str>) -> Option<String> {
    match self {
      ExprInferFailCause::ObjectLitSpread { .. } => None,
      ExprInferFailCause::ObjectLitShorthand { .. } => None,
      ExprInferFailCause::ArrayWithoutAsConst { .. } => None,
      ExprInferFailCause::ArraySpread { .. } => None,
      ExprInferFailCause::ExprInTemplateLiteral { .. } => None,
      ExprInferFailCause::ObjectGetter { cause, .. } => {
        cause.hint("object getter", location)
      }
      ExprInferFailCause::ObjectSetter { cause, .. } => {
        cause.hint("object setter", location)
      }
      ExprInferFailCause::ObjectMethod { cause, .. } => {
        cause.hint("object method", location)
      }
      ExprInferFailCause::FunctionExpression { cause, .. } => {
        cause.hint("function expression", location)
      }
      ExprInferFailCause::ArrowExpression { cause, .. } => {
        cause.hint("arrow expression", location)
      }
      ExprInferFailCause::LocalReference { .. } => None,
      ExprInferFailCause::Other { .. } => None,
    }
  }

  pub fn info(&self, infos: &mut Vec<Cow<'static, str>>) {
    match self {
      ExprInferFailCause::ObjectLitSpread { .. } => {
        infos.push(Cow::Borrowed("spread properties can not be inferred because the type of the resulting object can not be narrowed without a type checker"))
      }
      ExprInferFailCause::ObjectLitShorthand { .. } => {
        infos.push(Cow::Borrowed("shorthand properties can not be inferred because the type of the value referred to by the shorthand property is not known without a type checker"))
      }
      ExprInferFailCause::ArrayWithoutAsConst { .. } => {
        infos.push(Cow::Borrowed("array literals without 'as const' can not be inferred because the type of the array can not be narrowed without a type checker"));
      }
      ExprInferFailCause::ArraySpread { .. } => {
        infos.push(Cow::Borrowed("spread elements can not be inferred because the type of the resulting array can not be narrowed without a type checker"));
      }
      ExprInferFailCause::ExprInTemplateLiteral { .. } => {
        infos.push(Cow::Borrowed("expressions in template literals can not be inferred because the type of the resulting string can not be narrowed without a type checker"));
      },
      ExprInferFailCause::ObjectGetter { cause, .. } => {
        cause.info(infos, "an object getter");
      }
      ExprInferFailCause::ObjectSetter { cause, ..} => {
        cause.info(infos, "an object setter");
      },
      ExprInferFailCause::ObjectMethod { cause, .. } => {
        cause.info(infos, "an object method");
      },
      ExprInferFailCause::FunctionExpression { cause, .. } => {
        cause.info(infos, "a function expression");
      },
      ExprInferFailCause::ArrowExpression { cause, .. } => {
        cause.info(infos, "an arrow expression");
      },
      ExprInferFailCause::LocalReference { .. } => {
        infos.push(Cow::Borrowed("local variables or types can not be referenced in the public API because they are not visible at the top level of the module"));
      },
      ExprInferFailCause::Other { .. } => infos.push(Cow::Borrowed("the type of arbitrary expressions can not be inferred without a type checker")),
    }
  }
}

#[derive(Debug, Clone)]
pub enum FunctionInferFailCause {
  ParamType {
    pat: FastCheckDiagnosticRange,
    cause: Option<Box<ExprInferFailCause>>,
  },
  RequiredParamAfterOptional {
    pat: FastCheckDiagnosticRange,
  },
  ReturnType {
    cause: Box<ReturnTypeInferFailCause>,
  },
}

impl FunctionInferFailCause {
  fn highlights(
    &self,
    highlights: &mut Vec<DiagnosticSnippetHighlight>,
    ident: &FastCheckDiagnosticRange,
    prefix: &'static str,
    which: bool,
  ) {
    match self {
      FunctionInferFailCause::ParamType { pat, cause } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: ident.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(prefix)),
        });
        let description = match (which, cause.is_some()) {
          (false, false) => "has this parameter, the type of which could not be inferred because the default value",
          (false, true) => "has this parameter, which is missing an explicit type annotation",
          (true, false) => "which has this parameter, the type of which could not be inferred from the default value",
          (true, true) => "which has this parameter, which is missing an explicit type annotation",
        };
        highlights.push(DiagnosticSnippetHighlight {
          range: pat.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(description)),
        });
        if let Some(cause) = cause {
          cause.highlights(highlights);
        }
      }
      FunctionInferFailCause::RequiredParamAfterOptional { pat } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: ident.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(prefix)),
        });
        let description = if which {
          "which has this required parameter following an optional parameter or a parameter with a default value"
        } else {
          "has this required parameter following an optional parameter or a parameter with a default value"
        };
        highlights.push(DiagnosticSnippetHighlight {
          range: pat.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed(description)),
        });
      }
      FunctionInferFailCause::ReturnType { cause } => {
        cause.highlights(highlights, ident, prefix, which);
      }
    }
  }

  fn hint(
    &self,
    subject: &'static str,
    location: Cow<'_, str>,
  ) -> Option<String> {
    match self {
      FunctionInferFailCause::ParamType { cause, .. } => {
        let hint = cause.as_ref().and_then(|cause| cause.hint(format!(" in the {subject} parameter{location}").into()))
          .unwrap_or_else(|| format!("add an explicit type annotation to the {subject} parameter{location}"));
        Some(hint)
      }
      FunctionInferFailCause::RequiredParamAfterOptional { .. } => {
        Some(format!(
          "make the {subject} parameter{location} optional or provide a default value"
        ).into())
      },
      FunctionInferFailCause::ReturnType { cause } => {
        let hint = cause.hint(subject, &location)
          .unwrap_or_else(|| format!("add an explicit return type annotation to the {subject}{location}").into());
        Some(hint)
      },
    }
  }

  fn info(&self, infos: &mut Vec<Cow<'static, str>>, subject: &str) {
    match self {
      FunctionInferFailCause::ParamType { cause, .. } => {
        if let Some(cause) = cause {
          cause.info(infos)
        } else {
          infos.push(Cow::Owned(format!("all parameters of {subject} must have an explicit type annotation or an inferrable default value")));
        }
      }
      FunctionInferFailCause::RequiredParamAfterOptional { .. } => {
        infos.push(Cow::Owned(format!("all required parameters of {subject} must precede optional parameters or parameters with default values")));
        infos.push(Cow::Borrowed("this is because to compute the type of a optional parameter that is followed by a required parameter, a type checker is needed"));
      }
      FunctionInferFailCause::ReturnType { cause } => {
        cause.info(infos, subject);
      }
    }
  }
}

#[derive(Debug, Clone)]
pub enum ReturnTypeInferFailCause {
  IsGenerator,
  NoReturnStmt {
    is_async: bool,
  },
  NoValueReturnStmt, // getter only
  ReturnStmtValue {
    return_keyword: FastCheckDiagnosticRange,
    cause: Box<ExprInferFailCause>,
  },
  ArrowExpressionValue {
    cause: Box<ExprInferFailCause>,
  },
  MultipleReturnStmts,
}

impl ReturnTypeInferFailCause {
  pub fn highlights(
    &self,
    highlights: &mut Vec<DiagnosticSnippetHighlight>,
    ident: &FastCheckDiagnosticRange,
    prefix: &'static str,
    which: bool,
  ) {
    match self {
      ReturnTypeInferFailCause::IsGenerator
      | ReturnTypeInferFailCause::NoReturnStmt { .. }
      | ReturnTypeInferFailCause::NoValueReturnStmt
      | ReturnTypeInferFailCause::MultipleReturnStmts => {
        highlights.push(DiagnosticSnippetHighlight {
          range: ident.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Owned(format!(
            "{prefix}{} is missing an explicit return type annotation",
            if which { ", which" } else { "" }
          ))),
        });
      }
      ReturnTypeInferFailCause::ReturnStmtValue {
        return_keyword,
        cause,
      } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: ident.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Owned(format!(
            "{prefix}{} return type could not be inferred",
            if which { ", of which the" } else { "'s" }
          ))),
        });
        highlights.push(DiagnosticSnippetHighlight {
          range: return_keyword.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Borrowed("because the value returned here")),
        });
        cause.highlights(highlights);
      }
      ReturnTypeInferFailCause::ArrowExpressionValue { cause } => {
        highlights.push(DiagnosticSnippetHighlight {
          range: ident.into_diagnostic_range(),
          style: DiagnosticSnippetHighlightStyle::Hint,
          description: Some(Cow::Owned(format!(
            "{prefix}{} return type could not be inferred from the returned value",
            if which { ", of which the" } else { "'s" }
          ))),
        });
        cause.highlights(highlights);
      }
    };
  }

  pub fn hint(&self, subject: &'static str, location: &str) -> Option<String> {
    match self {
      ReturnTypeInferFailCause::IsGenerator
      | ReturnTypeInferFailCause::NoReturnStmt { .. }
      | ReturnTypeInferFailCause::NoValueReturnStmt
      | ReturnTypeInferFailCause::MultipleReturnStmts => None,
      ReturnTypeInferFailCause::ReturnStmtValue { cause, .. }
      | ReturnTypeInferFailCause::ArrowExpressionValue { cause } => {
        cause.hint(format!(" in the {subject} return value{location}").into())
      }
    }
  }

  pub fn info(&self, infos: &mut Vec<Cow<'static, str>>, subject: &str) {
    match self {
      ReturnTypeInferFailCause::IsGenerator => {
        infos.push(Cow::Owned(format!("{subject} that is a generator must have an explicit return type annotation because the return type can not be inferred without a type checker")));
      }
      ReturnTypeInferFailCause::NoReturnStmt { is_async } => {
        infos.push(Cow::Owned(format!("the return type of {subject} can not be inferred if it has no return statements")));
        if *is_async {
          infos.push(Cow::Owned(format!("this is because async {subject}s without a return statement can either return 'Promise<void>' or 'Promise<never>', and the specific type can not be determined without a type checker")));
        } else {
          infos.push(Cow::Owned(format!("this is because {subject}s without a return statement can either return 'void' or 'never', and the specific type can not be determined without a type checker")));
        }
      }
      ReturnTypeInferFailCause::NoValueReturnStmt => {
        infos.push(Cow::Owned(format!("{subject} does not have a return statement with a value, which is required to infer the return type")));
        infos.push(Cow::Borrowed(
          "this is because getters must have a return statement with a value",
        ));
      }
      ReturnTypeInferFailCause::ReturnStmtValue { cause, .. } => {
        cause.info(infos);
      }
      ReturnTypeInferFailCause::ArrowExpressionValue { cause } => {
        cause.info(infos);
      }
      ReturnTypeInferFailCause::MultipleReturnStmts => {
        infos.push(Cow::Owned(format!("{subject} has multiple return statements with values, so a return type can not be inferred without a type checker")));
      }
    }
  }
}

pub struct TypeInferrer<'a> {
  specifier: &'a ModuleSpecifier,
  parsed_source: &'a ParsedSource,
  module_info: &'a EsModuleInfo,
  root_symbol: &'a RootSymbol<'a>,
}

impl<'a> TypeInferrer<'a> {
  pub fn new(
    specifier: &'a ModuleSpecifier,
    parsed_source: &'a ParsedSource,
    module_info: &'a EsModuleInfo,
    root_symbol: &'a RootSymbol<'a>,
  ) -> Self {
    Self {
      specifier,
      parsed_source,
      module_info,
      root_symbol,
    }
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

  /// Infer a type from an expression in a non `const x = ...` context.
  pub fn infer_expr(
    &self,
    expr: &Expr,
    as_const: bool,
    ident_range: Option<SourceRange>,
  ) -> Result<Box<TsType>, ExprInferFailCause> {
    let err_other = || {
      Err(ExprInferFailCause::Other {
        expr: self.source_range_to_range(expr.range()),
      })
    };

    match expr {
      Expr::Paren(n) => self.infer_expr(&n.expr, as_const, ident_range),
      Expr::TsSatisfies(n) => self.infer_expr(&n.expr, as_const, ident_range),
      Expr::Seq(n) => {
        self.infer_expr(n.exprs.last().unwrap(), as_const, ident_range)
      }
      Expr::Assign(n) => self.infer_expr(&n.right, as_const, ident_range),

      Expr::Lit(lit) => {
        if as_const {
          Ok(Box::new(lit_to_ts_type_const(lit)))
        } else {
          Ok(Box::new(lit_to_ts_type(lit)))
        }
      }
      Expr::Tpl(tpl) => {
        if as_const {
          self.infer_tpl_const(tpl)
        } else {
          Ok(Box::new(ts_keyword_type(
            TsKeywordTypeKind::TsStringKeyword,
          )))
        }
      }

      Expr::Object(n) => self.infer_object_lit(n, as_const),
      Expr::Array(n) => self.infer_array_lit(n, as_const),

      Expr::Call(n) => {
        if is_call_expr_symbol_create(
          n,
          self.parsed_source.unresolved_context(),
        ) {
          Ok(Box::new(ts_keyword_type(
            TsKeywordTypeKind::TsSymbolKeyword,
          )))
        } else {
          // Can not infer because we don't know the return type of the function being called
          err_other()
        }
      }
      Expr::Ident(i) => {
        let is_symbol_global = i.sym == "undefined"
          && i.to_id().1 == self.parsed_source.unresolved_context();
        if is_symbol_global {
          Ok(Box::new(ts_keyword_type(
            TsKeywordTypeKind::TsUndefinedKeyword,
          )))
        } else {
          // Can not infer because we don't know the type of the identifier
          err_other()
        }
      }
      Expr::TsTypeAssertion(n) => {
        self.ensure_valid_type_ann(&n.type_ann)?;
        Ok(n.type_ann.clone())
      }
      Expr::TsAs(n) => {
        self.ensure_valid_type_ann(&n.type_ann)?;
        Ok(n.type_ann.clone())
      }
      Expr::TsConstAssertion(n) => self.infer_expr(&n.expr, true, ident_range),

      Expr::Fn(n) => {
        let ident_range = n
          .ident
          .as_ref()
          .map(|ident| ident.span.range())
          .or(ident_range)
          .unwrap_or_else(|| {
            let range = n.function.span.range();
            SourceRange::new(range.start, range.start + 1) // this could be improved
          });
        self.infer_fn(&n.function).map_err(|cause| {
          ExprInferFailCause::FunctionExpression {
            name: self.source_range_to_range(ident_range),
            cause: Box::new(cause),
          }
        })
      }
      Expr::Arrow(n) => {
        let ident_range = ident_range.unwrap_or_else(|| {
          let range = n.span.range();
          SourceRange::new(range.start, range.start + 1)
        });
        self.infer_arrow(&n).map_err(|cause| {
          ExprInferFailCause::ArrowExpression {
            name: self.source_range_to_range(ident_range),
            cause: Box::new(cause),
          }
        })
      }

      Expr::Unary(_) => {
        // TODO(@lucacasonato): maybe support `void`?
        err_other()
      }
      Expr::Bin(_) => {
        // TODO(@lucacasonato): maybe support inferring comparison operators and `in` and `instanceof`?
        err_other()
      }
      Expr::Class(_) => {
        // TODO(@lucacasonato): maybe support inferring class expressions
        err_other()
      }
      Expr::MetaProp(_) | Expr::Update(_) => err_other(), // Does not make sense to infer, usage not frequent for possible inferences
      Expr::This(_) => err_other(), // Can not infer because we don't know the type of this
      Expr::TsInstantiation(_) => err_other(), // Can not infer because we don't know the type being instantiated
      Expr::Member(_) | Expr::SuperProp(_) | Expr::OptChain(_) => err_other(), // Can not infer because we don't know the type of the object being accessed
      Expr::New(_) | Expr::TaggedTpl(_) => err_other(), // Can not infer because we don't know the type of the function being called
      Expr::Yield(_) => err_other(), // Can not infer because we don't know the type of the generators next value
      Expr::Cond(_) => err_other(), // Can not narrow the type of branches without type information
      Expr::JSXMember(_)
      | Expr::JSXNamespacedName(_)
      | Expr::JSXEmpty(_)
      | Expr::JSXElement(_)
      | Expr::JSXFragment(_) => err_other(), // Can not infer because we don't know the type of the JSX element
      Expr::Await(_) | Expr::TsNonNull(_) => err_other(), // Can not infer because we need type information to narrow the inner type
      Expr::PrivateName(_) => err_other(), // Unreachable in valid code
      Expr::Invalid(_) => err_other(),
    }
  }

  fn infer_array_lit(
    &self,
    n: &ArrayLit,
    as_const: bool,
  ) -> Result<Box<TsType>, ExprInferFailCause> {
    if !as_const {
      // Can not infer array types in non-const context, because narrowing would require type information
      Err(ExprInferFailCause::ArrayWithoutAsConst {
        expr: self.source_range_to_range(n.range()),
      })
    } else {
      let mut elem_types = vec![];

      for elem in &n.elems {
        match elem {
          Some(expr) => {
            if let Some(span) = expr.spread {
              return Err(ExprInferFailCause::ArraySpread {
                dot3_token: self.source_range_to_range(span.range()),
              });
            }
            let ty = self.infer_expr(&expr.expr, as_const, None)?;
            elem_types.push(TsTupleElement {
              span: expr.expr.span(),
              ty,
              label: None,
            });
          }
          None => elem_types.push(TsTupleElement {
            span: DUMMY_SP,
            ty: Box::new(ts_keyword_type(
              TsKeywordTypeKind::TsUndefinedKeyword,
            )),
            label: None,
          }),
        }
      }

      Ok(Box::new(TsType::TsTypeOperator(TsTypeOperator {
        span: DUMMY_SP,
        op: TsTypeOperatorOp::ReadOnly,
        type_ann: Box::new(TsType::TsTupleType(TsTupleType {
          span: n.span(),
          elem_types,
        })),
      })))
    }
  }

  // Infer a type from an expression in a `const x = ...` context.
  pub fn infer_expr_in_const_pos(
    &mut self,
    expr: &Expr,
    ident_range: Option<SourceRange>,
  ) -> Result<Box<TsType>, ExprInferFailCause> {
    match expr {
      Expr::Paren(n) => self.infer_expr_in_const_pos(&n.expr, ident_range),
      Expr::TsSatisfies(n) => {
        self.infer_expr_in_const_pos(&n.expr, ident_range)
      }
      Expr::Seq(n) => {
        self.infer_expr_in_const_pos(n.exprs.last().unwrap(), ident_range)
      }
      Expr::Assign(n) => self.infer_expr_in_const_pos(&n.right, ident_range),

      Expr::Lit(lit) => Ok(Box::new(lit_to_ts_type_const(lit))),
      Expr::Tpl(tpl) => self.infer_tpl_const(tpl),

      Expr::Call(n)
        if is_call_expr_symbol_create(
          n,
          self.parsed_source.unresolved_context(),
        ) =>
      {
        Ok(Box::new(TsType::TsTypeOperator(TsTypeOperator {
          span: DUMMY_SP,
          op: TsTypeOperatorOp::Unique,
          type_ann: Box::new(ts_keyword_type(
            TsKeywordTypeKind::TsSymbolKeyword,
          )),
        })))
      }

      _ => self.infer_expr(expr, false, ident_range),
    }
  }

  fn infer_tpl_const(
    &self,
    tpl: &Tpl,
  ) -> Result<Box<TsType>, ExprInferFailCause> {
    if tpl.exprs.is_empty() {
      let quasi = tpl.quasis.first().unwrap();
      Ok(Box::new(ts_lit_type(TsLit::Str(Str {
        span: quasi.span,
        value: quasi.cooked.clone().unwrap(),
        raw: None,
      }))))
    } else {
      Err(ExprInferFailCause::ExprInTemplateLiteral {
        expr: self.source_range_to_range(tpl.exprs.first().unwrap().range()),
      })
    }
  }

  fn infer_object_lit(
    &self,
    n: &ObjectLit,
    as_const: bool,
  ) -> Result<Box<TsType>, ExprInferFailCause> {
    let mut members = vec![];
    for prop in &n.props {
      match prop {
        PropOrSpread::Prop(prop) => match prop.as_ref() {
          Prop::KeyValue(kv) => {
            let (key, computed) = prop_name_to_ts_key(&kv.key);
            let type_ann =
              self.infer_expr(&kv.value, as_const, Some(key.range()))?;
            members.push(TsTypeElement::TsPropertySignature(
              TsPropertySignature {
                span: kv.span(),
                key,
                computed,
                type_ann: Some(Box::new(TsTypeAnn {
                  span: kv.value.span(),
                  type_ann,
                })),
                readonly: as_const,
                optional: false,
                // TODO: remove after https://github.com/swc-project/swc/pull/8955 lands
                init: None,
                params: vec![],
                type_params: None,
              },
            ));
          }
          Prop::Getter(getter) => {
            let (key, computed) = prop_name_to_ts_key(&getter.key);
            if getter.type_ann.is_none() {
              // TODO(lucacasonato): if we know the type of the setter, use that

              let body = getter.body.as_ref().unwrap();
              let res = self.infer_function_body_block_stmt(
                body,
                FunctionKind::Getter,
                /* is async */ false,
              );
              match res {
                Ok(ty) => members.push(TsTypeElement::TsGetterSignature(
                  TsGetterSignature {
                    span: getter.span(),
                    key,
                    computed,
                    type_ann: Some(Box::new(TsTypeAnn {
                      span: getter.span(),
                      type_ann: ty,
                    })),
                    readonly: false,
                    optional: false,
                  },
                )),
                Err(cause) => {
                  return Err(ExprInferFailCause::ObjectGetter {
                    key: self.source_range_to_range(getter.key.range()),
                    cause: Box::new(FunctionInferFailCause::ReturnType {
                      cause: Box::new(cause),
                    }),
                  })
                }
              }
            }
          }
          Prop::Setter(setter) => {
            let (key, computed) = prop_name_to_ts_key(&setter.key);
            // TODO(lucacasonato): if we know the type of the setter, use that

            let param =
              self
                .infer_fn_param(&*setter.param, false)
                .map_err(|cause| ExprInferFailCause::ObjectSetter {
                  key: self.source_range_to_range(setter.key.range()),
                  cause: Box::new(cause),
                })?;

            members.push(TsTypeElement::TsSetterSignature(TsSetterSignature {
              span: setter.span(),
              key,
              computed,
              param,
              readonly: false,
              optional: false,
            }));
          }
          Prop::Method(n) => {
            let (key, computed) = prop_name_to_ts_key(&n.key);

            let return_type =
              self.infer_fn_return_type(&n.function).map_err(|cause| {
                ExprInferFailCause::ObjectMethod {
                  key: self.source_range_to_range(n.key.range()),
                  cause: Box::new(FunctionInferFailCause::ReturnType {
                    cause: Box::new(cause),
                  }),
                }
              })?;
            let params = self
              .infer_fn_params(n.function.params.iter().map(|p| &p.pat))
              .map_err(|cause| ExprInferFailCause::ObjectMethod {
                key: self.source_range_to_range(n.key.range()),
                cause: Box::new(cause),
              })?;

            members.push(TsTypeElement::TsMethodSignature(TsMethodSignature {
              span: n.span(),
              key,
              computed,
              params,
              type_ann: Some(return_type),
              type_params: n.function.type_params.clone(),
              readonly: false,
              optional: false,
            }));
          }
          Prop::Shorthand(n) => {
            return Err(ExprInferFailCause::ObjectLitShorthand {
              key: self.source_range_to_range(n.range()),
            })
          }
          Prop::Assign(_) => unreachable!("not valid on object literal"),
        },
        PropOrSpread::Spread(n) => {
          return Err(ExprInferFailCause::ObjectLitSpread {
            dot3_token: self.source_range_to_range(n.dot3_token.range()),
          })
        }
      }
    }

    let obj_literal = Box::new(TsType::TsTypeLit(TsTypeLit {
      span: Default::default(),
      members,
    }));

    if as_const {
      Ok(Box::new(TsType::TsTypeOperator(TsTypeOperator {
        span: DUMMY_SP,
        op: TsTypeOperatorOp::ReadOnly,
        type_ann: obj_literal,
      })))
    } else {
      Ok(obj_literal)
    }
  }

  pub fn infer_function_body_block_stmt(
    &self,
    body: &BlockStmt,
    function_kind: FunctionKind,
    is_async: bool,
  ) -> Result<Box<TsType>, ReturnTypeInferFailCause> {
    let analysis = analyze_return_stmts_in_function_body(body);
    match (analysis, function_kind) {
      (_, FunctionKind::Setter) => unreachable!(),
      (ReturnStatementAnalysis::None, FunctionKind::DeclarationLike)
      | (ReturnStatementAnalysis::Void, FunctionKind::DeclarationLike)
      | (ReturnStatementAnalysis::Void, FunctionKind::ExpressionLike) => {
        Ok(void_or_promise_void(is_async))
      }
      (ReturnStatementAnalysis::None, FunctionKind::ExpressionLike) => {
        Err(ReturnTypeInferFailCause::NoReturnStmt { is_async })
      }
      (ReturnStatementAnalysis::Single(n), _) => {
        let expr = n.arg.as_ref().unwrap();
        match self.infer_expr(expr, /* as const */ false, None) {
          Ok(ty) => Ok(ty),
          Err(cause) => Err(ReturnTypeInferFailCause::ReturnStmtValue {
            return_keyword: self.source_range_to_range(SourceRange {
              start: n.span.start(),
              end: n.span.start() + 6,
            }),
            cause: Box::new(cause),
          }),
        }
      }
      (ReturnStatementAnalysis::None, FunctionKind::Getter)
      | (ReturnStatementAnalysis::Void, FunctionKind::Getter) => {
        Err(ReturnTypeInferFailCause::NoValueReturnStmt)
      }
      (ReturnStatementAnalysis::Multiple, _) => {
        Err(ReturnTypeInferFailCause::MultipleReturnStmts)
      }
    }
  }

  fn infer_fn(
    &self,
    function: &Function,
  ) -> Result<Box<TsType>, FunctionInferFailCause> {
    let return_type = self.infer_fn_return_type(function).map_err(|cause| {
      FunctionInferFailCause::ReturnType {
        cause: Box::new(cause),
      }
    })?;

    let param_pats = function.params.iter().map(|param| &param.pat);
    let params = self.infer_fn_params(param_pats)?;

    Ok(Box::new(TsType::TsFnOrConstructorType(
      TsFnOrConstructorType::TsFnType(TsFnType {
        span: function.span,
        params,
        type_ann: return_type,
        type_params: function.type_params.clone(),
      }),
    )))
  }

  fn infer_fn_return_type(
    &self,
    function: &Function,
  ) -> Result<Box<TsTypeAnn>, ReturnTypeInferFailCause> {
    let return_type = match &function.return_type {
      Some(ty) => ty.clone(),
      None => {
        if function.is_generator {
          return Err(ReturnTypeInferFailCause::IsGenerator);
        }

        let body = function.body.as_ref().unwrap();
        let type_ann = self.infer_function_body_block_stmt(
          body,
          FunctionKind::ExpressionLike,
          function.is_async,
        )?;

        Box::new(TsTypeAnn {
          span: DUMMY_SP,
          type_ann,
        })
      }
    };
    Ok(return_type)
  }

  fn infer_fn_params(
    &self,
    param_pats: impl Iterator<Item = &'a Pat>,
  ) -> Result<Vec<TsFnParam>, FunctionInferFailCause> {
    let mut is_previous_optional = false;
    let mut params = vec![];
    for pat in param_pats {
      let optional = is_param_pat_optional(pat);
      if !optional && is_previous_optional {
        return Err(FunctionInferFailCause::RequiredParamAfterOptional {
          pat: self.source_range_to_range(pat.range()),
        });
      }
      is_previous_optional = optional;
      let param = self.infer_fn_param(&pat, optional)?;
      params.push(param)
    }
    Ok(params)
  }

  fn infer_fn_param(
    &self,
    pat: &Pat,
    optional: bool,
  ) -> Result<TsFnParam, FunctionInferFailCause> {
    let (pat, default_value) = match &*pat {
      Pat::Assign(assign) => (&*assign.left, Some(&assign.right)),
      _ => (pat, None),
    };

    let range = pat.range();

    let mut type_ann = match pat {
      Pat::Ident(n) => n.type_ann.clone(),
      Pat::Array(n) => n.type_ann.clone(),
      Pat::Object(n) => n.type_ann.clone(),
      Pat::Rest(n) => n.type_ann.clone(),
      Pat::Assign(_) => unreachable!(),
      Pat::Expr(_) => unreachable!(),
      Pat::Invalid(_) => unreachable!(),
    };

    if type_ann.is_none() {
      if let Some(default_value) = default_value {
        match self.infer_expr(default_value, false, None) {
          Ok(ty) => {
            type_ann = Some(Box::new(TsTypeAnn {
              span: DUMMY_SP,
              type_ann: ty,
            }))
          }
          Err(cause) => {
            return Err(FunctionInferFailCause::ParamType {
              pat: self.source_range_to_range(range),
              cause: Some(Box::new(cause)),
            });
          }
        };
      } else {
        return Err(FunctionInferFailCause::ParamType {
          pat: self.source_range_to_range(range),
          cause: None,
        });
      }
    }

    Ok(pat_to_ts_fn_param(pat, type_ann, optional))
  }

  fn infer_arrow(
    &self,
    arrow: &ArrowExpr,
  ) -> Result<Box<TsType>, FunctionInferFailCause> {
    let params = self.infer_fn_params(arrow.params.iter())?;

    let return_type = match &arrow.return_type {
      Some(ty) => ty.clone(),
      None => {
        if arrow.is_generator {
          return Err(FunctionInferFailCause::ReturnType {
            cause: Box::new(ReturnTypeInferFailCause::IsGenerator),
          });
        }

        let type_ann = match &*arrow.body {
          BlockStmtOrExpr::BlockStmt(body) => self
            .infer_function_body_block_stmt(
              body,
              FunctionKind::ExpressionLike,
              arrow.is_async,
            )
            .map_err(|cause| FunctionInferFailCause::ReturnType {
              cause: Box::new(cause),
            })?,
          BlockStmtOrExpr::Expr(expr) => self
            .infer_expr(expr, false, None)
            .map_err(|cause| FunctionInferFailCause::ReturnType {
              cause: Box::new(ReturnTypeInferFailCause::ArrowExpressionValue {
                cause: Box::new(cause),
              }),
            })?,
        };

        Box::new(TsTypeAnn {
          span: DUMMY_SP,
          type_ann,
        })
      }
    };

    Ok(Box::new(TsType::TsFnOrConstructorType(
      TsFnOrConstructorType::TsFnType(TsFnType {
        span: arrow.span,
        params,
        type_ann: return_type,
        type_params: arrow.type_params.clone(),
      }),
    )))
  }

  pub fn ensure_valid_type_ann(
    &self,
    ty: &TsType,
  ) -> Result<(), ExprInferFailCause> {
    struct Visitor<'a> {
      root_symbol: &'a RootSymbol<'a>,
      es_module_info: &'a EsModuleInfo,
      invalid_range: Option<SourceRange>,
    }

    impl Visit for Visitor<'_> {
      fn visit_ts_entity_name(&mut self, n: &TsEntityName) {
        let (id, parts) = ts_entity_name_to_parts(n);

        let dep = SymbolNodeDep::QualifiedId(id, parts);

        let entries = self
          .root_symbol
          .resolve_symbol_dep(ModuleInfoRef::Esm(self.es_module_info), &dep);

        for entry in entries {
          match entry {
            ResolvedSymbolDepEntry::Path(DefinitionPathNode::Resolved(_))
            | ResolvedSymbolDepEntry::ImportType(_) => {
              // valid
            }
            ResolvedSymbolDepEntry::Path(DefinitionPathNode::Unresolved(_)) => {
              self.invalid_range = Some(n.range());
            }
          }
        }
      }
    }

    let mut visitor = Visitor {
      es_module_info: self.module_info,
      root_symbol: self.root_symbol,
      invalid_range: None,
    };
    ty.visit_with(&mut visitor);
    match visitor.invalid_range {
      Some(range) => Err(ExprInferFailCause::LocalReference {
        ident: self.source_range_to_range(range),
      }),
      None => Ok(()),
    }
  }
}

fn pat_to_ts_fn_param(
  pat: &Pat,
  type_ann: Option<Box<TsTypeAnn>>,
  optional: bool,
) -> TsFnParam {
  match pat {
    Pat::Ident(n) => TsFnParam::Ident(BindingIdent {
      id: Ident {
        span: n.span,
        sym: n.sym.clone(),
        optional,
      },
      type_ann,
    }),
    pat @ Pat::Array(_) => {
      let mut new_pat = pat.clone();
      clean_pat_for_ts_fn_param(&mut new_pat);
      let Pat::Array(mut n) = new_pat else {
        unreachable!()
      };
      n.optional = optional;
      n.type_ann = type_ann;
      TsFnParam::Array(n)
    }
    pat @ Pat::Object(_) => {
      let mut new_pat = pat.clone();
      clean_pat_for_ts_fn_param(&mut new_pat);
      let Pat::Object(mut n) = new_pat else {
        unreachable!()
      };
      n.optional = optional;
      n.type_ann = type_ann;
      TsFnParam::Object(n)
    }
    pat @ Pat::Rest(_) => {
      let mut new_pat = pat.clone();
      clean_pat_for_ts_fn_param(&mut new_pat);
      let Pat::Rest(mut n) = new_pat else {
        unreachable!()
      };
      n.type_ann = type_ann;
      TsFnParam::Rest(n)
    }
    Pat::Assign(p) => pat_to_ts_fn_param(&p.left, type_ann, optional),
    Pat::Invalid(_) => unreachable!(),
    Pat::Expr(_) => unreachable!(),
  }
}

fn clean_pat_for_ts_fn_param(p: &mut Pat) {
  match &mut *p {
    Pat::Assign(n) => {
      *p = *n.left.clone();
      clean_pat_for_ts_fn_param(p);
    }
    Pat::Array(n) => {
      n.optional = false;
      n.type_ann = None;
      for elem in &mut n.elems {
        if let Some(elem) = elem {
          clean_pat_for_ts_fn_param(elem);
        }
      }
    }
    Pat::Object(n) => {
      n.optional = false;
      n.type_ann = None;
      for prop in &mut n.props {
        match prop {
          ObjectPatProp::KeyValue(p) => clean_pat_for_ts_fn_param(&mut p.value),
          ObjectPatProp::Assign(p) => p.value = None,
          ObjectPatProp::Rest(p) => clean_pat_for_ts_fn_param(&mut p.arg),
        }
      }
    }
    Pat::Rest(n) => {
      n.type_ann = None;
      clean_pat_for_ts_fn_param(&mut n.arg)
    }
    Pat::Ident(n) => {
      n.id.optional = false;
      n.type_ann = None;
    }
    Pat::Invalid(_) | Pat::Expr(_) => unreachable!(),
  }
}
