// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::ast::Class;
use deno_ast::swc::ast::DefaultDecl;
use deno_ast::swc::ast::Expr;
use deno_ast::swc::ast::Function;
use deno_ast::swc::ast::Id;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::Lit;
use deno_ast::swc::ast::MemberProp;
use deno_ast::swc::ast::Param;
use deno_ast::swc::ast::ParamOrTsParamProp;
use deno_ast::swc::ast::Pat;
use deno_ast::swc::ast::PropName;
use deno_ast::swc::ast::TsEnumDecl;
use deno_ast::swc::ast::TsExprWithTypeArgs;
use deno_ast::swc::ast::TsFnParam;
use deno_ast::swc::ast::TsImportType;
use deno_ast::swc::ast::TsInterfaceDecl;
use deno_ast::swc::ast::TsParamProp;
use deno_ast::swc::ast::TsParamPropParam;
use deno_ast::swc::ast::TsQualifiedName;
use deno_ast::swc::ast::TsType;
use deno_ast::swc::ast::TsTypeAliasDecl;
use deno_ast::swc::ast::TsTypeAnn;
use deno_ast::swc::ast::TsTypeParam;
use deno_ast::swc::ast::TsTypeParamDecl;
use deno_ast::swc::ast::TsTypeParamInstantiation;
use deno_ast::swc::ast::VarDeclarator;
use deno_ast::swc::visit::Visit;
use deno_ast::swc::visit::VisitWith;

use super::swc_helpers::ts_entity_name_to_parts;
use super::swc_helpers::ts_qualified_name_parts;
use super::ExportDeclRef;
use super::SymbolNodeRef;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolNodeDep {
  Id(Id),
  QualifiedId(Id, Vec<String>),
  ImportType(String, Vec<String>),
}

impl From<Id> for SymbolNodeDep {
  fn from(value: Id) -> Self {
    Self::Id(value)
  }
}

pub fn resolve_deps(node_ref: SymbolNodeRef) -> Vec<SymbolNodeDep> {
  let mut result = Vec::new();
  let deps = &mut result;
  match node_ref {
    SymbolNodeRef::Module(_) | SymbolNodeRef::TsNamespace(_) => {
      // no deps, as this has children
    }
    SymbolNodeRef::ClassDecl(n) => {
      fill_class(deps, &n.class);
    }
    SymbolNodeRef::ExportDecl(_, n) => match n {
      ExportDeclRef::Class(n) => fill_class(deps, &n.class),
      ExportDeclRef::Fn(n) => fill_function_decl(deps, &n.function),
      ExportDeclRef::Var(_, n, _) => {
        fill_var_declarator(deps, n);
      }
      ExportDeclRef::TsEnum(n) => fill_enum(deps, n),
      ExportDeclRef::TsInterface(n) => fill_interface(deps, n),
      ExportDeclRef::TsModule(_) => {
        // no deps, as this has children
      }
      ExportDeclRef::TsTypeAlias(n) => fill_type_alias(deps, n),
    },
    SymbolNodeRef::ExportDefaultDecl(n) => match &n.decl {
      DefaultDecl::Class(n) => fill_class(deps, &n.class),
      DefaultDecl::Fn(n) => {
        fill_function_decl(deps, &n.function);
      }
      DefaultDecl::TsInterfaceDecl(n) => {
        fill_interface(deps, n);
      }
    },
    SymbolNodeRef::ExportDefaultExprLit(n, _) => {
      fill_expr(deps, &n.expr);
    }
    SymbolNodeRef::FnDecl(n) => fill_function_decl(deps, &n.function),
    SymbolNodeRef::TsEnum(n) => {
      fill_enum(deps, n);
    }
    SymbolNodeRef::TsInterface(n) => fill_interface(deps, n),
    SymbolNodeRef::TsTypeAlias(n) => {
      fill_type_alias(deps, n);
    }
    SymbolNodeRef::Var(_, n, _) => {
      fill_var_declarator(deps, n);
    }
    SymbolNodeRef::AutoAccessor(n) => {
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann)
      }
    }
    SymbolNodeRef::ClassMethod(n) => {
      fill_prop_name(deps, &n.key);
      if let Some(type_params) = &n.function.type_params {
        fill_ts_type_param_decl(deps, type_params)
      }
      for param in &n.function.params {
        fill_param(deps, param)
      }
      if let Some(return_type) = &n.function.return_type {
        fill_ts_type_ann(deps, return_type)
      }
    }
    SymbolNodeRef::ClassProp(n) => {
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann)
      }
    }
    SymbolNodeRef::Constructor(n) => {
      for param in &n.params {
        match param {
          ParamOrTsParamProp::TsParamProp(param) => {
            fill_ts_param_prop(deps, param)
          }
          ParamOrTsParamProp::Param(param) => fill_param(deps, param),
        }
      }
    }
    SymbolNodeRef::TsIndexSignature(n) => {
      for param in &n.params {
        fill_ts_fn_param(deps, param)
      }
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann)
      }
    }
    SymbolNodeRef::TsCallSignatureDecl(n) => {
      if let Some(type_params) = &n.type_params {
        fill_ts_type_param_decl(deps, type_params);
      }
      for param in &n.params {
        fill_ts_fn_param(deps, param);
      }
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann)
      }
    }
    SymbolNodeRef::TsConstructSignatureDecl(n) => {
      if let Some(type_params) = &n.type_params {
        fill_ts_type_param_decl(deps, type_params);
      }
      for param in &n.params {
        fill_ts_fn_param(deps, param);
      }
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann)
      }
    }
    SymbolNodeRef::TsPropertySignature(n) => {
      if let Some(init) = &n.init {
        fill_expr(deps, init);
      }
      if let Some(type_params) = &n.type_params {
        fill_ts_type_param_decl(deps, type_params);
      }
      for param in &n.params {
        fill_ts_fn_param(deps, param);
      }
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann)
      }
    }
    SymbolNodeRef::TsGetterSignature(n) => {
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann)
      }
    }
    SymbolNodeRef::TsSetterSignature(n) => {
      fill_ts_fn_param(deps, &n.param);
    }
    SymbolNodeRef::TsMethodSignature(n) => {
      if let Some(type_params) = &n.type_params {
        fill_ts_type_param_decl(deps, type_params);
      }
      for param in &n.params {
        fill_ts_fn_param(deps, param)
      }
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann)
      }
    }
  }
  result
}

fn fill_class(deps: &mut Vec<SymbolNodeDep>, n: &Class) {
  if let Some(type_params) = &n.type_params {
    fill_ts_type_param_decl(deps, type_params);
  }
  if let Some(expr) = &n.super_class {
    fill_expr(deps, expr);
  }
  if let Some(type_params) = &n.super_type_params {
    fill_ts_type_param_instantiation(deps, type_params)
  }
  for expr in &n.implements {
    fill_ts_expr_with_type_args(deps, expr);
  }
}

fn fill_enum(deps: &mut Vec<SymbolNodeDep>, n: &TsEnumDecl) {
  for member in &n.members {
    if let Some(init) = &member.init {
      fill_expr(deps, init);
    }
  }
}

fn fill_function_decl(deps: &mut Vec<SymbolNodeDep>, n: &Function) {
  if let Some(type_params) = &n.type_params {
    fill_ts_type_param_decl(deps, type_params);
  }
  for param in &n.params {
    fill_param(deps, param);
  }
  if let Some(return_type) = &n.return_type {
    fill_ts_type_ann(deps, return_type);
  }
}

fn fill_interface(deps: &mut Vec<SymbolNodeDep>, n: &TsInterfaceDecl) {
  if let Some(type_params) = &n.type_params {
    fill_ts_type_param_decl(deps, type_params);
  }
  for extends in &n.extends {
    fill_ts_expr_with_type_args(deps, extends);
  }
}

fn fill_type_alias(deps: &mut Vec<SymbolNodeDep>, n: &TsTypeAliasDecl) {
  if let Some(type_params) = &n.type_params {
    fill_ts_type_param_decl(deps, type_params);
  }
  fill_ts_type(deps, &n.type_ann)
}

fn fill_var_declarator(deps: &mut Vec<SymbolNodeDep>, n: &VarDeclarator) {
  fill_pat(deps, &n.name);
}

fn fill_prop_name(deps: &mut Vec<SymbolNodeDep>, key: &PropName) {
  match key {
    PropName::Computed(name) => {
      fill_expr(deps, &name.expr);
    }
    PropName::Ident(_)
    | PropName::Str(_)
    | PropName::Num(_)
    | PropName::BigInt(_) => {
      // ignore
    }
  }
}

fn fill_ts_expr_with_type_args(
  deps: &mut Vec<SymbolNodeDep>,
  n: &TsExprWithTypeArgs,
) {
  if let Some(type_args) = &n.type_args {
    fill_ts_type_param_instantiation(deps, type_args);
  }
  fill_expr(deps, &n.expr);
}

fn fill_ts_fn_param(deps: &mut Vec<SymbolNodeDep>, param: &TsFnParam) {
  let mut visitor = SymbolDepFillVisitor { deps };
  param.visit_with(&mut visitor);
}

fn fill_ts_type_param_decl(
  deps: &mut Vec<SymbolNodeDep>,
  type_params: &TsTypeParamDecl,
) {
  for param in &type_params.params {
    fill_ts_type_param(deps, param);
  }
}

fn fill_ts_type_param(deps: &mut Vec<SymbolNodeDep>, param: &TsTypeParam) {
  if let Some(constraint) = &param.constraint {
    fill_ts_type(deps, constraint);
  }
  if let Some(default) = &param.default {
    fill_ts_type(deps, default);
  }
}

fn fill_ts_type_param_instantiation(
  deps: &mut Vec<SymbolNodeDep>,
  type_params: &TsTypeParamInstantiation,
) {
  for param in &type_params.params {
    fill_ts_type(deps, param);
  }
}

fn fill_ts_param_prop(deps: &mut Vec<SymbolNodeDep>, param: &TsParamProp) {
  match &param.param {
    TsParamPropParam::Ident(ident) => {
      if let Some(type_ann) = &ident.type_ann {
        fill_ts_type_ann(deps, type_ann)
      }
    }
    TsParamPropParam::Assign(_) => {
      // nothing to fill
    }
  }
}

fn fill_param(deps: &mut Vec<SymbolNodeDep>, param: &Param) {
  fill_pat(deps, &param.pat);
}

fn fill_pat(deps: &mut Vec<SymbolNodeDep>, pat: &Pat) {
  match pat {
    Pat::Ident(n) => {
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann);
      }
    }
    Pat::Array(n) => {
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann);
      }
    }
    Pat::Rest(n) => {
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann);
      }
    }
    Pat::Object(n) => {
      if let Some(type_ann) = &n.type_ann {
        fill_ts_type_ann(deps, type_ann);
      }
    }
    Pat::Assign(n) => {
      fill_pat(deps, &n.left);
    }
    Pat::Invalid(_) => {
      // ignore
    }
    Pat::Expr(expr) => {
      fill_expr(deps, expr);
    }
  }
}

fn fill_ts_type_ann(deps: &mut Vec<SymbolNodeDep>, type_ann: &TsTypeAnn) {
  fill_ts_type(deps, &type_ann.type_ann)
}

fn fill_ts_type(deps: &mut Vec<SymbolNodeDep>, n: &TsType) {
  let mut visitor = SymbolDepFillVisitor { deps };
  n.visit_with(&mut visitor);
}

fn fill_expr(deps: &mut Vec<SymbolNodeDep>, n: &Expr) {
  fn member_prop_to_str(member_prop: &MemberProp) -> Option<String> {
    match member_prop {
      MemberProp::Ident(ident) => Some(ident.sym.to_string()),
      MemberProp::PrivateName(n) => Some(format!("#{}", n.id.sym)),
      MemberProp::Computed(n) => match &*n.expr {
        Expr::Lit(Lit::Str(str)) => Some(str.value.to_string()),
        _ => None,
      },
    }
  }

  fn expr_into_id_and_parts(expr: &Expr) -> Option<(Id, Vec<String>)> {
    match expr {
      Expr::Member(member) => {
        let (id, mut parts) = expr_into_id_and_parts(&member.obj)?;
        parts.push(member_prop_to_str(&member.prop)?);
        Some((id, parts))
      }
      Expr::Ident(ident) => Some((ident.to_id(), vec![])),
      _ => None,
    }
  }

  if let Some((id, parts)) = expr_into_id_and_parts(n) {
    if parts.is_empty() {
      deps.push(SymbolNodeDep::Id(id))
    } else {
      deps.push(SymbolNodeDep::QualifiedId(id, parts))
    }
  } else {
    let mut visitor = SymbolDepFillVisitor { deps };
    n.visit_with(&mut visitor);
  }
}

struct SymbolDepFillVisitor<'a> {
  deps: &'a mut Vec<SymbolNodeDep>,
}

impl<'a> Visit for SymbolDepFillVisitor<'a> {
  fn visit_ident(&mut self, n: &Ident) {
    let id = n.to_id();
    self.deps.push(id.into());
  }

  fn visit_ts_import_type(&mut self, n: &TsImportType) {
    let parts = match &n.qualifier {
      Some(qualifier) => {
        let (leftmost_id, mut parts) = ts_entity_name_to_parts(qualifier);
        parts.insert(0, leftmost_id.0.to_string());
        parts
      }
      None => Vec::new(),
    };
    self
      .deps
      .push(SymbolNodeDep::ImportType(n.arg.value.to_string(), parts));
    n.type_args.visit_with(self);
  }

  fn visit_ts_qualified_name(&mut self, n: &TsQualifiedName) {
    let (id, parts) = ts_qualified_name_parts(n);
    self.deps.push(SymbolNodeDep::QualifiedId(id, parts));
  }
}
