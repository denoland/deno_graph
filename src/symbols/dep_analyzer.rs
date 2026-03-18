// Copyright 2018-2024 the Deno authors. MIT license.

use deno_ast::oxc::ast::ast::AccessorProperty;
use deno_ast::oxc::ast::ast::ArrowFunctionExpression;
use deno_ast::oxc::ast::ast::BindingIdentifier;
use deno_ast::oxc::ast::ast::BindingPattern;
use deno_ast::oxc::ast::ast::Class;
use deno_ast::oxc::ast::ast::ExportDefaultDeclarationKind;
use deno_ast::oxc::ast::ast::Expression;
use deno_ast::oxc::ast::ast::FormalParameter;
use deno_ast::oxc::ast::ast::Function;
use deno_ast::oxc::ast::ast::IdentifierReference;
use deno_ast::oxc::ast::ast::MemberExpression;
use deno_ast::oxc::ast::ast::MethodDefinition;
use deno_ast::oxc::ast::ast::PropertyDefinition;
use deno_ast::oxc::ast::ast::PropertyKey;
use deno_ast::oxc::ast::ast::TSAsExpression;
use deno_ast::oxc::ast::ast::TSCallSignatureDeclaration;
use deno_ast::oxc::ast::ast::TSClassImplements;
use deno_ast::oxc::ast::ast::TSConstructSignatureDeclaration;
use deno_ast::oxc::ast::ast::TSEnumDeclaration;
use deno_ast::oxc::ast::ast::TSImportType;
use deno_ast::oxc::ast::ast::TSImportTypeQualifier;
use deno_ast::oxc::ast::ast::TSIndexSignature;
use deno_ast::oxc::ast::ast::TSInterfaceDeclaration;
use deno_ast::oxc::ast::ast::TSInterfaceHeritage;
use deno_ast::oxc::ast::ast::TSMethodSignature;
use deno_ast::oxc::ast::ast::TSMethodSignatureKind;
use deno_ast::oxc::ast::ast::TSPropertySignature;
use deno_ast::oxc::ast::ast::TSQualifiedName;
use deno_ast::oxc::ast::ast::TSTupleElement;
use deno_ast::oxc::ast::ast::TSTypeAliasDeclaration;
use deno_ast::oxc::ast::ast::TSTypeAnnotation;
use deno_ast::oxc::ast::ast::TSTypeAssertion;
use deno_ast::oxc::ast::ast::TSTypeParameter;
use deno_ast::oxc::ast::ast::TSTypeParameterDeclaration;
use deno_ast::oxc::ast::ast::TSTypeParameterInstantiation;
use deno_ast::oxc::ast::ast::VariableDeclarator;
use deno_ast::oxc::ast::ast::match_member_expression;
use deno_ast::oxc::ast_visit::Visit;
use deno_ast::oxc::ast_visit::walk;

use super::ExportDeclRef;
use super::SymbolNodeRef;
use super::helpers::Id;
use super::helpers::ts_qualified_name_parts;

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
        self.visit_class(n);
      }
      SymbolNodeRef::ExportDecl(_, n) => match n {
        ExportDeclRef::Class(n) => self.visit_class(n),
        ExportDeclRef::Fn(n) => self.visit_function_deps(n),
        ExportDeclRef::Var(_, n, _) => {
          self.visit_variable_declarator(n);
        }
        ExportDeclRef::TsEnum(n) => self.visit_ts_enum_declaration(n),
        ExportDeclRef::TsInterface(n) => self.visit_ts_interface_declaration(n),
        ExportDeclRef::TsModule(_) => {
          // no deps, as this has children
        }
        ExportDeclRef::TsTypeAlias(n) => {
          self.visit_ts_type_alias_declaration(n)
        }
        ExportDeclRef::TsImportEquals(_) => {
          // no deps to analyze
        }
      },
      SymbolNodeRef::ExportDefaultDecl(n) => {
        self.fill_export_default_declaration_kind(&n.declaration);
      }
      SymbolNodeRef::ExportDefaultExpr(n) => {
        self.fill_export_default_declaration_kind(n);
      }
      SymbolNodeRef::FnDecl(n) => self.visit_function_deps(n),
      SymbolNodeRef::TsEnum(n) => {
        self.visit_ts_enum_declaration(n);
      }
      SymbolNodeRef::TsInterface(n) => self.visit_ts_interface_declaration(n),
      SymbolNodeRef::TsTypeAlias(n) => {
        self.visit_ts_type_alias_declaration(n);
      }
      SymbolNodeRef::Var(_, n, _) | SymbolNodeRef::UsingVar(_, n, _) => {
        self.visit_variable_declarator(n);
      }
      SymbolNodeRef::AutoAccessor(n) => {
        self.fill_auto_accessor(n);
      }
      SymbolNodeRef::ClassMethod(n) => {
        self.fill_method_definition(n);
      }
      SymbolNodeRef::ClassProp(n) => {
        self.fill_property_definition(n);
      }
      SymbolNodeRef::ClassParamProp(n) => {
        self.fill_formal_parameter(n);
      }
      SymbolNodeRef::Constructor(n) => {
        for param in &n.value.params.items {
          self.fill_formal_parameter(param);
        }
      }
      SymbolNodeRef::ExpandoProperty(n) => {
        if self.mode.visit_exprs() {
          self.visit_expression(n.assignment());
        }
      }
      SymbolNodeRef::TsIndexSignature(n) => {
        self.visit_ts_index_signature(n);
      }
      SymbolNodeRef::TsCallSignatureDecl(n) => {
        self.visit_ts_call_signature_declaration(n);
      }
      SymbolNodeRef::TsConstructSignatureDecl(n) => {
        self.visit_ts_construct_signature_declaration(n);
      }
      SymbolNodeRef::TsPropertySignature(n) => {
        self.visit_ts_property_signature(n);
      }
      SymbolNodeRef::TsMethodSignature(n)
      | SymbolNodeRef::TsGetterSignature(n)
      | SymbolNodeRef::TsSetterSignature(n) => {
        self.visit_ts_method_signature(n);
      }
    }
  }

  fn fill_export_default_declaration_kind(
    &mut self,
    n: &ExportDefaultDeclarationKind<'_>,
  ) {
    match n {
      ExportDefaultDeclarationKind::ClassDeclaration(n) => {
        self.visit_class(n);
      }
      ExportDefaultDeclarationKind::FunctionDeclaration(n) => {
        self.visit_function_deps(n);
      }
      ExportDefaultDeclarationKind::TSInterfaceDeclaration(n) => {
        self.visit_ts_interface_declaration(n);
      }
      _ => {
        // expression - visit as expression
        self.visit_expression(n.to_expression());
      }
    }
  }

  fn fill_auto_accessor(&mut self, n: &AccessorProperty<'_>) {
    if let Some(type_ann) = &n.type_annotation {
      self.visit_ts_type_annotation(type_ann);
    }
  }

  fn fill_method_definition(&mut self, n: &MethodDefinition<'_>) {
    if self.mode.visit_exprs() {
      self.visit_property_key(&n.key);
    }

    if let Some(type_params) = &n.value.type_parameters {
      self.visit_ts_type_parameter_declaration(type_params);
    }
    for param in &n.value.params.items {
      self.fill_formal_parameter(param);
    }
    if let Some(return_type) = &n.value.return_type {
      self.visit_ts_type_annotation(return_type);
    }
  }

  fn fill_property_definition(&mut self, n: &PropertyDefinition<'_>) {
    if self.mode.visit_exprs() {
      self.visit_property_key(&n.key);
    }

    if let Some(type_ann) = &n.type_annotation {
      self.visit_ts_type_annotation(type_ann);
    } else if let Some(value) = &n.value {
      let visited_type_assertion = self.visit_type_if_type_assertion(value);
      if !visited_type_assertion && self.mode.visit_exprs() {
        self.visit_expression(value);
      }
    }
  }

  fn fill_formal_parameter(&mut self, param: &FormalParameter<'_>) {
    // In OXC, type annotations are on FormalParameter, not on the pattern
    if let Some(type_ann) = &param.type_annotation {
      self.visit_ts_type_annotation(type_ann);
    } else if param.accessibility.is_some() {
      // This is a parameter property (TsParamProp equivalent).
      // No type annotation on the param itself; check if there's an
      // initializer with a type assertion.
      if let Some(initializer) = &param.initializer {
        self.visit_type_if_type_assertion(initializer);
      }
    } else {
      // Regular parameter without type annotation - check pattern
      // (e.g., AssignmentPattern might have a type assertion on the right)
      self.visit_binding_pattern(&param.pattern);
    }
  }

  fn visit_type_if_type_assertion(&mut self, expr: &Expression<'_>) -> bool {
    if matches!(
      expr,
      Expression::TSAsExpression(_) | Expression::TSTypeAssertion(_)
    ) {
      self.visit_expression(expr);
      true
    } else {
      false
    }
  }
}

impl<'a> Visit<'a> for DepsFiller {
  fn visit_ts_index_signature(&mut self, n: &TSIndexSignature<'a>) {
    for param in &n.parameters {
      self.visit_ts_type_annotation(&param.type_annotation);
    }
    self.visit_ts_type_annotation(&n.type_annotation);
  }

  fn visit_ts_call_signature_declaration(
    &mut self,
    n: &TSCallSignatureDeclaration<'a>,
  ) {
    if let Some(type_params) = &n.type_parameters {
      self.visit_ts_type_parameter_declaration(type_params);
    }
    for param in &n.params.items {
      self.fill_formal_parameter(param);
    }
    if let Some(type_ann) = &n.return_type {
      self.visit_ts_type_annotation(type_ann);
    }
  }

  fn visit_ts_construct_signature_declaration(
    &mut self,
    n: &TSConstructSignatureDeclaration<'a>,
  ) {
    if let Some(type_params) = &n.type_parameters {
      self.visit_ts_type_parameter_declaration(type_params);
    }
    for param in &n.params.items {
      self.fill_formal_parameter(param);
    }
    if let Some(type_ann) = &n.return_type {
      self.visit_ts_type_annotation(type_ann);
    }
  }

  fn visit_ts_property_signature(&mut self, n: &TSPropertySignature<'a>) {
    if n.computed {
      self.visit_expression(n.key.to_expression());
    }
    if let Some(type_ann) = &n.type_annotation {
      self.visit_ts_type_annotation(type_ann);
    }
  }

  fn visit_ts_method_signature(&mut self, n: &TSMethodSignature<'a>) {
    if n.computed {
      self.visit_expression(n.key.to_expression());
    }
    match n.kind {
      TSMethodSignatureKind::Get => {
        // getter: just visit return type
        if let Some(type_ann) = &n.return_type {
          self.visit_ts_type_annotation(type_ann);
        }
      }
      TSMethodSignatureKind::Set => {
        // setter: visit param
        for param in &n.params.items {
          self.fill_formal_parameter(param);
        }
      }
      TSMethodSignatureKind::Method => {
        if let Some(type_params) = &n.type_parameters {
          self.visit_ts_type_parameter_declaration(type_params);
        }
        for param in &n.params.items {
          self.fill_formal_parameter(param);
        }
        if let Some(type_ann) = &n.return_type {
          self.visit_ts_type_annotation(type_ann);
        }
      }
    }
  }

  fn visit_class(&mut self, n: &Class<'a>) {
    if let Some(type_params) = &n.type_parameters {
      self.visit_ts_type_parameter_declaration(type_params);
    }
    if let Some(expr) = &n.super_class {
      self.visit_expression(expr);
    }
    if let Some(type_params) = &n.super_type_arguments {
      self.visit_ts_type_parameter_instantiation(type_params);
    }
    for expr in &n.implements {
      self.visit_ts_class_implements(expr);
    }
  }

  fn visit_ts_enum_declaration(&mut self, n: &TSEnumDeclaration<'a>) {
    for member in &n.body.members {
      if let Some(init) = &member.initializer {
        self.visit_expression(init);
      }
    }
  }

  fn visit_function(
    &mut self,
    n: &Function<'a>,
    _flags: deno_ast::oxc::syntax::scope::ScopeFlags,
  ) {
    self.visit_function_deps(n);
  }

  fn visit_arrow_function_expression(
    &mut self,
    n: &ArrowFunctionExpression<'a>,
  ) {
    if let Some(type_params) = &n.type_parameters {
      self.visit_ts_type_parameter_declaration(type_params);
    }
    for param in &n.params.items {
      self.fill_formal_parameter(param);
    }
    if let Some(return_type) = &n.return_type {
      self.visit_ts_type_annotation(return_type);
    } else if n.expression {
      // Arrow with expression body and no return type annotation -
      // the body has a single expression statement
      if let Some(stmt) = n.body.statements.first()
        && let deno_ast::oxc::ast::ast::Statement::ExpressionStatement(
          expr_stmt,
        ) = stmt
      {
        self.visit_expression(&expr_stmt.expression);
      }
    }
  }

  fn visit_ts_interface_declaration(&mut self, n: &TSInterfaceDeclaration<'a>) {
    if let Some(type_params) = &n.type_parameters {
      self.visit_ts_type_parameter_declaration(type_params);
    }
    for extends in &n.extends {
      self.visit_ts_interface_heritage(extends);
    }
  }

  fn visit_ts_type_alias_declaration(
    &mut self,
    n: &TSTypeAliasDeclaration<'a>,
  ) {
    if let Some(type_params) = &n.type_parameters {
      self.visit_ts_type_parameter_declaration(type_params);
    }
    self.visit_ts_type(&n.type_annotation);
  }

  fn visit_variable_declarator(&mut self, n: &VariableDeclarator<'a>) {
    self.visit_binding_pattern(&n.id);
    if n.type_annotation.is_none()
      && !binding_pattern_has_type_ann(&n.id)
      && let Some(init) = &n.init
    {
      let visited_type_assertion = self.visit_type_if_type_assertion(init);
      if !visited_type_assertion && self.mode.visit_exprs() {
        self.visit_expression(init);
      }
    }
  }

  fn visit_property_key(&mut self, key: &PropertyKey<'a>) {
    match key {
      PropertyKey::StaticIdentifier(_) | PropertyKey::PrivateIdentifier(_) => {
        // property name idents aren't a dep
      }
      _ => {
        // Computed / expression keys
        self.visit_expression(key.to_expression());
      }
    }
  }

  fn visit_ts_class_implements(&mut self, n: &TSClassImplements<'a>) {
    if let Some(type_args) = &n.type_arguments {
      self.visit_ts_type_parameter_instantiation(type_args);
    }
    // TSClassImplements.expression is TSTypeName, extract deps from it
    let (id, parts) = super::helpers::ts_entity_name_to_parts(&n.expression);
    if parts.is_empty() {
      self.deps.push(SymbolNodeDep::Id(id));
    } else {
      self.deps.push(SymbolNodeDep::QualifiedId(id, parts));
    }
  }

  fn visit_ts_interface_heritage(&mut self, n: &TSInterfaceHeritage<'a>) {
    if let Some(type_args) = &n.type_arguments {
      self.visit_ts_type_parameter_instantiation(type_args);
    }
    // visit this expr unconditionally because it's in a heritage clause
    self.visit_expression(&n.expression);
  }

  fn visit_ts_type_parameter_declaration(
    &mut self,
    type_params: &TSTypeParameterDeclaration<'a>,
  ) {
    for param in &type_params.params {
      self.visit_ts_type_parameter(param);
    }
  }

  fn visit_ts_type_parameter(&mut self, param: &TSTypeParameter<'a>) {
    if let Some(constraint) = &param.constraint {
      self.visit_ts_type(constraint);
    }
    if let Some(default) = &param.default {
      self.visit_ts_type(default);
    }
  }

  fn visit_ts_type_parameter_instantiation(
    &mut self,
    type_params: &TSTypeParameterInstantiation<'a>,
  ) {
    for param in &type_params.params {
      self.visit_ts_type(param);
    }
  }

  fn visit_binding_pattern(&mut self, pat: &BindingPattern<'a>) {
    match pat {
      BindingPattern::BindingIdentifier(_)
      | BindingPattern::ArrayPattern(_)
      | BindingPattern::ObjectPattern(_) => {
        // In OXC, type annotations are on the parent (FormalParameter/VariableDeclarator),
        // not on the pattern itself. Nothing to visit here for deps.
      }
      BindingPattern::AssignmentPattern(n) => {
        self.visit_binding_pattern(&n.left);
        if !binding_pattern_has_type_ann(&n.left) {
          let visited_type_assertion =
            self.visit_type_if_type_assertion(&n.right);
          if !visited_type_assertion && self.mode.visit_exprs() {
            self.visit_expression(&n.right);
          }
        }
      }
    }
  }

  fn visit_expression(&mut self, n: &Expression<'a>) {
    match expr_into_id_and_parts(n) {
      Some((id, parts)) => {
        if parts.is_empty() {
          self.deps.push(SymbolNodeDep::Id(id))
        } else {
          self.deps.push(SymbolNodeDep::QualifiedId(id, parts))
        }
      }
      _ => {
        walk::walk_expression(self, n);
      }
    }
  }

  fn visit_identifier_reference(&mut self, n: &IdentifierReference<'a>) {
    let id = (n.name.to_string(), 0);
    self.deps.push(id.into());
  }

  fn visit_binding_identifier(&mut self, _n: &BindingIdentifier<'a>) {
    // skip over the ident because it's not a dep
    // In OXC, type annotations are on the parent, not on BindingIdentifier
  }

  fn visit_member_expression(&mut self, n: &MemberExpression<'a>) {
    match member_expr_into_id_and_parts(n) {
      Some((id, parts)) => {
        self.deps.push(SymbolNodeDep::QualifiedId(id, parts))
      }
      _ => {
        walk::walk_member_expression(self, n);
      }
    }
  }

  fn visit_ts_tuple_element(&mut self, n: &TSTupleElement<'a>) {
    walk::walk_ts_tuple_element(self, n);
  }

  fn visit_ts_import_type(&mut self, n: &TSImportType<'a>) {
    let parts = match &n.qualifier {
      Some(qualifier) => ts_import_type_qualifier_to_parts(qualifier),
      None => Vec::new(),
    };
    self
      .deps
      .push(SymbolNodeDep::ImportType(n.source.value.to_string(), parts));
    if let Some(type_args) = &n.type_arguments {
      self.visit_ts_type_parameter_instantiation(type_args);
    }
  }

  fn visit_ts_qualified_name(&mut self, n: &TSQualifiedName<'a>) {
    let (id, parts) = ts_qualified_name_parts(n);
    self.deps.push(SymbolNodeDep::QualifiedId(id, parts));
  }

  fn visit_ts_type_annotation(&mut self, type_ann: &TSTypeAnnotation<'a>) {
    self.visit_ts_type(&type_ann.type_annotation);
  }

  fn visit_ts_type_assertion(&mut self, n: &TSTypeAssertion<'a>) {
    self.visit_ts_type(&n.type_annotation);
  }

  fn visit_ts_as_expression(&mut self, n: &TSAsExpression<'a>) {
    self.visit_ts_type(&n.type_annotation);
  }
}

impl DepsFiller {
  fn visit_function_deps(&mut self, n: &Function<'_>) {
    if let Some(type_params) = &n.type_parameters {
      self.visit_ts_type_parameter_declaration(type_params);
    }
    for param in &n.params.items {
      self.fill_formal_parameter(param);
    }
    if let Some(return_type) = &n.return_type {
      self.visit_ts_type_annotation(return_type);
    }
  }
}

fn binding_pattern_has_type_ann(n: &BindingPattern) -> bool {
  // In OXC, BindingPattern variants don't carry type annotations.
  // Type annotations are on the parent FormalParameter or VariableDeclarator.
  // For AssignmentPattern, recurse into left.
  match n {
    BindingPattern::AssignmentPattern(n) => {
      binding_pattern_has_type_ann(&n.left)
    }
    _ => false,
  }
}

fn expr_into_id_and_parts(expr: &Expression) -> Option<(Id, Vec<String>)> {
  match expr {
    match_member_expression!(Expression) => {
      member_expr_into_id_and_parts(expr.to_member_expression())
    }
    Expression::Identifier(ident) => {
      Some(((ident.name.to_string(), 0), vec![]))
    }
    _ => None,
  }
}

fn member_expr_into_id_and_parts(
  member: &MemberExpression,
) -> Option<(Id, Vec<String>)> {
  fn member_prop_to_str(member: &MemberExpression) -> Option<String> {
    match member {
      MemberExpression::StaticMemberExpression(m) => {
        Some(m.property.name.to_string())
      }
      MemberExpression::PrivateFieldExpression(m) => {
        Some(format!("#{}", m.field.name))
      }
      MemberExpression::ComputedMemberExpression(m) => match &m.expression {
        Expression::StringLiteral(s) => Some(s.value.to_string()),
        _ => None,
      },
    }
  }

  fn member_object<'a>(member: &'a MemberExpression<'a>) -> &'a Expression<'a> {
    match member {
      MemberExpression::StaticMemberExpression(m) => &m.object,
      MemberExpression::ComputedMemberExpression(m) => &m.object,
      MemberExpression::PrivateFieldExpression(m) => &m.object,
    }
  }

  let (id, mut parts) = expr_into_id_and_parts(member_object(member))?;
  parts.push(member_prop_to_str(member)?);
  Some((id, parts))
}

fn ts_import_type_qualifier_to_parts(
  qualifier: &TSImportTypeQualifier,
) -> Vec<String> {
  fn collect_parts(qualifier: &TSImportTypeQualifier, parts: &mut Vec<String>) {
    match qualifier {
      TSImportTypeQualifier::Identifier(ident) => {
        parts.push(ident.name.to_string());
      }
      TSImportTypeQualifier::QualifiedName(qn) => {
        collect_parts(&qn.left, parts);
        parts.push(qn.right.name.to_string());
      }
    }
  }
  let mut parts = Vec::new();
  collect_parts(qualifier, &mut parts);
  parts
}
