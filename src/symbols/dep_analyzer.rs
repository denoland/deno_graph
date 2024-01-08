// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::ast::BindingIdent;
use deno_ast::swc::ast::Class;
use deno_ast::swc::ast::DefaultDecl;
use deno_ast::swc::ast::Expr;
use deno_ast::swc::ast::Function;
use deno_ast::swc::ast::Id;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::Lit;
use deno_ast::swc::ast::MemberExpr;
use deno_ast::swc::ast::MemberProp;
use deno_ast::swc::ast::Param;
use deno_ast::swc::ast::ParamOrTsParamProp;
use deno_ast::swc::ast::Pat;
use deno_ast::swc::ast::PropName;
use deno_ast::swc::ast::TsCallSignatureDecl;
use deno_ast::swc::ast::TsConstructSignatureDecl;
use deno_ast::swc::ast::TsEnumDecl;
use deno_ast::swc::ast::TsExprWithTypeArgs;
use deno_ast::swc::ast::TsGetterSignature;
use deno_ast::swc::ast::TsImportType;
use deno_ast::swc::ast::TsIndexSignature;
use deno_ast::swc::ast::TsInterfaceDecl;
use deno_ast::swc::ast::TsMethodSignature;
use deno_ast::swc::ast::TsParamProp;
use deno_ast::swc::ast::TsParamPropParam;
use deno_ast::swc::ast::TsPropertySignature;
use deno_ast::swc::ast::TsQualifiedName;
use deno_ast::swc::ast::TsSetterSignature;
use deno_ast::swc::ast::TsTupleElement;
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

#[derive(Debug, Copy, Clone)]
pub enum ResolveDepsMode {
  /// Resolve dependencies of types only (used for deno doc).
  TypesOnly,
  /// Resolve dependencies of types and expressions (used for fast check).
  TypesAndExpressions,
}

impl ResolveDepsMode {
  pub fn visit_exprs(&self) -> bool {
    match self {
      ResolveDepsMode::TypesOnly => false,
      ResolveDepsMode::TypesAndExpressions => true,
    }
  }
}

pub fn resolve_deps(
  node_ref: SymbolNodeRef,
  mode: ResolveDepsMode,
) -> Vec<SymbolNodeDep> {
  let mut filler = DepsFiller {
    deps: Vec::new(),
    mode,
  };
  filler.fill(node_ref);
  filler.deps
}

struct DepsFiller {
  deps: Vec<SymbolNodeDep>,
  mode: ResolveDepsMode,
}

impl DepsFiller {
  fn fill(&mut self, node_ref: SymbolNodeRef<'_>) {
    match node_ref {
      SymbolNodeRef::Module(_) | SymbolNodeRef::TsNamespace(_) => {
        // no deps, as this has children
      }
      SymbolNodeRef::ClassDecl(n) => {
        self.visit_class(&n.class);
      }
      SymbolNodeRef::ExportDecl(_, n) => match n {
        ExportDeclRef::Class(n) => self.visit_class(&n.class),
        ExportDeclRef::Fn(n) => self.visit_function(&n.function),
        ExportDeclRef::Var(_, n, _) => {
          self.visit_var_declarator(n);
        }
        ExportDeclRef::TsEnum(n) => self.visit_ts_enum_decl(n),
        ExportDeclRef::TsInterface(n) => self.visit_ts_interface_decl(n),
        ExportDeclRef::TsModule(_) => {
          // no deps, as this has children
        }
        ExportDeclRef::TsTypeAlias(n) => self.visit_ts_type_alias_decl(n),
      },
      SymbolNodeRef::ExportDefaultDecl(n) => match &n.decl {
        DefaultDecl::Class(n) => self.visit_class(&n.class),
        DefaultDecl::Fn(n) => {
          self.visit_function(&n.function);
        }
        DefaultDecl::TsInterfaceDecl(n) => {
          self.visit_ts_interface_decl(n);
        }
      },
      SymbolNodeRef::ExportDefaultExpr(n) => {
        self.visit_expr(&n.expr);
      }
      SymbolNodeRef::FnDecl(n) => self.visit_function(&n.function),
      SymbolNodeRef::TsEnum(n) => {
        self.visit_ts_enum_decl(n);
      }
      SymbolNodeRef::TsInterface(n) => self.visit_ts_interface_decl(n),
      SymbolNodeRef::TsTypeAlias(n) => {
        self.visit_ts_type_alias_decl(n);
      }
      SymbolNodeRef::Var(_, n, _) => {
        self.visit_var_declarator(n);
      }
      SymbolNodeRef::AutoAccessor(n) => {
        if let Some(type_ann) = &n.type_ann {
          self.visit_ts_type_ann(type_ann)
        }
      }
      SymbolNodeRef::ClassMethod(n) => {
        if self.mode.visit_exprs() {
          self.visit_prop_name(&n.key);
        }

        if let Some(type_params) = &n.function.type_params {
          self.visit_ts_type_param_decl(type_params)
        }
        for param in &n.function.params {
          self.visit_param(param)
        }
        if let Some(return_type) = &n.function.return_type {
          self.visit_ts_type_ann(return_type)
        }
      }
      SymbolNodeRef::ClassProp(n) => {
        if self.mode.visit_exprs() {
          self.visit_prop_name(&n.key);
        }

        if let Some(type_ann) = &n.type_ann {
          self.visit_ts_type_ann(type_ann)
        }
      }
      SymbolNodeRef::ClassParamProp(n) => self.visit_ts_param_prop(n),
      SymbolNodeRef::Constructor(n) => {
        for param in &n.params {
          match param {
            ParamOrTsParamProp::TsParamProp(param) => {
              self.visit_ts_param_prop(param)
            }
            ParamOrTsParamProp::Param(param) => self.visit_param(param),
          }
        }
      }
      SymbolNodeRef::TsIndexSignature(n) => {
        self.visit_ts_index_signature(n);
      }
      SymbolNodeRef::TsCallSignatureDecl(n) => {
        self.visit_ts_call_signature_decl(n);
      }
      SymbolNodeRef::TsConstructSignatureDecl(n) => {
        self.visit_ts_construct_signature_decl(n);
      }
      SymbolNodeRef::TsPropertySignature(n) => {
        self.visit_ts_property_signature(n);
      }
      SymbolNodeRef::TsGetterSignature(n) => {
        self.visit_ts_getter_signature(n);
      }
      SymbolNodeRef::TsSetterSignature(n) => {
        self.visit_ts_setter_signature(n);
      }
      SymbolNodeRef::TsMethodSignature(n) => {
        self.visit_ts_method_signature(n);
      }
    }
  }
}

impl Visit for DepsFiller {
  fn visit_ts_index_signature(&mut self, n: &TsIndexSignature) {
    for param in &n.params {
      self.visit_ts_fn_param(param)
    }
    if let Some(type_ann) = &n.type_ann {
      self.visit_ts_type_ann(type_ann)
    }
  }

  fn visit_ts_call_signature_decl(&mut self, n: &TsCallSignatureDecl) {
    if let Some(type_params) = &n.type_params {
      self.visit_ts_type_param_decl(type_params);
    }
    for param in &n.params {
      self.visit_ts_fn_param(param);
    }
    if let Some(type_ann) = &n.type_ann {
      self.visit_ts_type_ann(type_ann)
    }
  }

  fn visit_ts_construct_signature_decl(
    &mut self,
    n: &TsConstructSignatureDecl,
  ) {
    if let Some(type_params) = &n.type_params {
      self.visit_ts_type_param_decl(type_params);
    }
    for param in &n.params {
      self.visit_ts_fn_param(param);
    }
    if let Some(type_ann) = &n.type_ann {
      self.visit_ts_type_ann(type_ann)
    }
  }

  fn visit_ts_property_signature(&mut self, n: &TsPropertySignature) {
    if let Some(init) = &n.init {
      self.visit_expr(init);
    }
    if let Some(type_params) = &n.type_params {
      self.visit_ts_type_param_decl(type_params);
    }
    for param in &n.params {
      self.visit_ts_fn_param(param);
    }
    if let Some(type_ann) = &n.type_ann {
      self.visit_ts_type_ann(type_ann)
    }
  }

  fn visit_ts_getter_signature(&mut self, n: &TsGetterSignature) {
    if let Some(type_ann) = &n.type_ann {
      self.visit_ts_type_ann(type_ann)
    }
  }

  fn visit_ts_setter_signature(&mut self, n: &TsSetterSignature) {
    self.visit_ts_fn_param(&n.param);
  }

  fn visit_ts_method_signature(&mut self, n: &TsMethodSignature) {
    if let Some(type_params) = &n.type_params {
      self.visit_ts_type_param_decl(type_params);
    }
    for param in &n.params {
      self.visit_ts_fn_param(param)
    }
    if let Some(type_ann) = &n.type_ann {
      self.visit_ts_type_ann(type_ann)
    }
  }

  fn visit_class(&mut self, n: &Class) {
    if let Some(type_params) = &n.type_params {
      self.visit_ts_type_param_decl(type_params);
    }
    if let Some(expr) = &n.super_class {
      self.visit_expr(expr);
    }
    if let Some(type_params) = &n.super_type_params {
      self.visit_ts_type_param_instantiation(type_params)
    }
    for expr in &n.implements {
      self.visit_ts_expr_with_type_args(expr);
    }
  }

  fn visit_ts_enum_decl(&mut self, n: &TsEnumDecl) {
    for member in &n.members {
      if let Some(init) = &member.init {
        self.visit_expr(init);
      }
    }
  }

  fn visit_function(&mut self, n: &Function) {
    if let Some(type_params) = &n.type_params {
      self.visit_ts_type_param_decl(type_params);
    }
    for param in &n.params {
      self.visit_param(param);
    }
    if let Some(return_type) = &n.return_type {
      self.visit_ts_type_ann(return_type);
    }
  }

  fn visit_ts_interface_decl(&mut self, n: &TsInterfaceDecl) {
    if let Some(type_params) = &n.type_params {
      self.visit_ts_type_param_decl(type_params);
    }
    for extends in &n.extends {
      self.visit_ts_expr_with_type_args(extends);
    }
  }

  fn visit_ts_type_alias_decl(&mut self, n: &TsTypeAliasDecl) {
    if let Some(type_params) = &n.type_params {
      self.visit_ts_type_param_decl(type_params);
    }
    self.visit_ts_type(&n.type_ann)
  }

  fn visit_var_declarator(&mut self, n: &VarDeclarator) {
    self.visit_pat(&n.name);
    if self.mode.visit_exprs() && !pat_has_type_ann(&n.name) {
      if let Some(init) = &n.init {
        self.visit_expr(init);
      }
    }
  }

  fn visit_prop_name(&mut self, key: &PropName) {
    match key {
      PropName::Computed(computed) => {
        self.visit_expr(&computed.expr);
      }
      // property name idents aren't a dep
      PropName::Ident(_)
      | PropName::Str(_)
      | PropName::Num(_)
      | PropName::BigInt(_) => {
        // ignore
      }
    }
  }

  fn visit_ts_expr_with_type_args(&mut self, n: &TsExprWithTypeArgs) {
    if let Some(type_args) = &n.type_args {
      self.visit_ts_type_param_instantiation(type_args);
    }
    // visit this expr unconditionally because it's in a TsExprWithTypeArgs
    self.visit_expr(&n.expr);
  }

  fn visit_ts_type_param_decl(&mut self, type_params: &TsTypeParamDecl) {
    for param in &type_params.params {
      self.visit_ts_type_param(param);
    }
  }

  fn visit_ts_type_param(&mut self, param: &TsTypeParam) {
    if let Some(constraint) = &param.constraint {
      self.visit_ts_type(constraint);
    }
    if let Some(default) = &param.default {
      self.visit_ts_type(default);
    }
  }

  fn visit_ts_type_param_instantiation(
    &mut self,
    type_params: &TsTypeParamInstantiation,
  ) {
    for param in &type_params.params {
      self.visit_ts_type(param);
    }
  }

  fn visit_ts_param_prop(&mut self, param: &TsParamProp) {
    match &param.param {
      TsParamPropParam::Ident(ident) => {
        if let Some(type_ann) = &ident.type_ann {
          self.visit_ts_type_ann(type_ann)
        }
      }
      TsParamPropParam::Assign(assign) => match &*assign.left {
        Pat::Ident(ident) => {
          if let Some(type_ann) = &ident.type_ann {
            self.visit_ts_type_ann(type_ann)
          }
        }
        _ => {
          unreachable!();
        }
      },
    }
  }

  fn visit_param(&mut self, param: &Param) {
    self.visit_pat(&param.pat);
  }

  fn visit_pat(&mut self, pat: &Pat) {
    match pat {
      Pat::Ident(n) => {
        if let Some(type_ann) = &n.type_ann {
          self.visit_ts_type_ann(type_ann);
        }
      }
      Pat::Array(n) => {
        if let Some(type_ann) = &n.type_ann {
          self.visit_ts_type_ann(type_ann);
        }
      }
      Pat::Rest(n) => {
        if let Some(type_ann) = &n.type_ann {
          self.visit_ts_type_ann(type_ann);
        }
      }
      Pat::Object(n) => {
        if let Some(type_ann) = &n.type_ann {
          self.visit_ts_type_ann(type_ann);
        }
      }
      Pat::Assign(n) => {
        self.visit_pat(&n.left);
        let has_type_ann = pat_has_type_ann(&n.left);
        if self.mode.visit_exprs() && !has_type_ann {
          self.visit_expr(&n.right);
        }
      }
      Pat::Invalid(_) => {
        // ignore
      }
      Pat::Expr(expr) => {
        if self.mode.visit_exprs() {
          self.visit_expr(expr);
        }
      }
    }
  }

  fn visit_expr(&mut self, n: &Expr) {
    if let Some((id, parts)) = expr_into_id_and_parts(n) {
      if parts.is_empty() {
        self.deps.push(SymbolNodeDep::Id(id))
      } else {
        self.deps.push(SymbolNodeDep::QualifiedId(id, parts))
      }
    } else {
      n.visit_children_with(self);
    }
  }

  fn visit_ident(&mut self, n: &Ident) {
    let id = n.to_id();
    self.deps.push(id.into());
  }

  fn visit_binding_ident(&mut self, n: &BindingIdent) {
    // skip over the ident because it's not a dep
    n.type_ann.visit_with(self);
  }

  fn visit_member_expr(&mut self, n: &MemberExpr) {
    if let Some((id, parts)) = member_expr_into_id_and_parts(n) {
      self.deps.push(SymbolNodeDep::QualifiedId(id, parts))
    } else {
      n.visit_children_with(self);
    }
  }

  fn visit_ts_tuple_element(&mut self, n: &TsTupleElement) {
    n.ty.visit_with(self);
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

  fn visit_ts_type_ann(&mut self, type_ann: &TsTypeAnn) {
    self.visit_ts_type(&type_ann.type_ann)
  }
}

fn pat_has_type_ann(n: &Pat) -> bool {
  match n {
    Pat::Ident(n) => n.type_ann.is_some(),
    Pat::Array(n) => n.type_ann.is_some(),
    Pat::Rest(n) => n.type_ann.is_some(),
    Pat::Object(n) => n.type_ann.is_some(),
    Pat::Assign(n) => pat_has_type_ann(&n.left),
    Pat::Invalid(_) => false,
    Pat::Expr(_) => false,
  }
}

fn expr_into_id_and_parts(expr: &Expr) -> Option<(Id, Vec<String>)> {
  match expr {
    Expr::Member(member) => member_expr_into_id_and_parts(member),
    Expr::Ident(ident) => Some((ident.to_id(), vec![])),
    _ => None,
  }
}

fn member_expr_into_id_and_parts(
  member: &MemberExpr,
) -> Option<(Id, Vec<String>)> {
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

  let (id, mut parts) = expr_into_id_and_parts(&member.obj)?;
  parts.push(member_prop_to_str(&member.prop)?);
  Some((id, parts))
}
