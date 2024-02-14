use deno_ast::{
  swc::{
    ast::{
      BindingIdent, ClassMember, Decl, DefaultDecl, ExportDecl,
      ExportDefaultDecl, ExportDefaultExpr, Expr, Ident, Lit, MethodKind,
      Module, ModuleDecl, ModuleItem, Pat, Prop, PropName, PropOrSpread, Stmt,
      TsFnOrConstructorType, TsFnParam, TsFnType, TsKeywordType,
      TsKeywordTypeKind, TsPropertySignature, TsTupleElement, TsTupleType,
      TsType, TsTypeElement, TsTypeLit, VarDecl, VarDeclKind, VarDeclarator,
    },
    common::DUMMY_SP,
  },
  ModuleSpecifier, ParsedSource, SourceRange, SourceRangedForSpanned,
};

use crate::{FastCheckDiagnostic, FastCheckDiagnosticRange};

use super::swc_helpers::{
  any_type_ann, maybe_lit_to_ts_type, maybe_lit_to_ts_type_const, ts_readonly,
  ts_tuple_element, type_ann,
};

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

    let mut new_items: Vec<ModuleItem> = vec![];

    for item in body {
      match item {
        ModuleItem::ModuleDecl(module_decl) => match module_decl {
          ModuleDecl::Import(_) => {
            new_items.push(ModuleItem::ModuleDecl(module_decl));
          }
          ModuleDecl::ExportDecl(export_decl) => {
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
            let name_ident = Ident::new(name.into(), DUMMY_SP);
            let type_ann = self
              .expr_to_ts_type(*export_default_expr.expr, false)
              .map(type_ann)
              .or_else(|| Some(any_type_ann()));

            new_items.push(ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(
              VarDecl {
                span: DUMMY_SP,
                kind: VarDeclKind::Const,
                declare: true,
                decls: vec![VarDeclarator {
                  span: DUMMY_SP,
                  name: Pat::Ident(BindingIdent {
                    id: name_ident.clone(),
                    type_ann,
                  }),
                  init: None,
                  definite: false,
                }],
              },
            )))));

            new_items.push(ModuleItem::ModuleDecl(
              ModuleDecl::ExportDefaultExpr(ExportDefaultExpr {
                span: export_default_expr.span,
                expr: Box::new(Expr::Ident(name_ident)),
              }),
            ))
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
          if let Stmt::Decl(decl) = stmt {
            match decl {
              Decl::TsInterface(_)
              | Decl::TsTypeAlias(_)
              | Decl::TsEnum(_)
              | Decl::TsModule(_) => {
                new_items.push(ModuleItem::Stmt(Stmt::Decl(decl)));
              }
              // Since fast check requires explicit type annotations we
              // can drop other statements not part of an export statement.
              Decl::Class(_) | Decl::Fn(_) | Decl::Var(_) | Decl::Using(_) => {}
            }
          }
        }
      }
    }

    module.body = new_items;
    Ok(module)
  }

  fn expr_to_ts_type(&mut self, expr: Expr, as_const: bool) -> Option<TsType> {
    match expr {
      Expr::Array(arr) => {
        let mut elem_types: Vec<TsTupleElement> = vec![];

        for elems in arr.elems {
          if let Some(expr_or_spread) = elems {
            if let Some(ts_expr) =
              self.expr_to_ts_type(*expr_or_spread.expr.clone(), as_const)
            {
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

        Some(ts_readonly(TsType::TsTupleType(TsTupleType {
          span: arr.span,
          elem_types,
        })))
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
                    .expr_to_ts_type(*key_value.value, as_const)
                    .map(type_ann);

                  members.push(TsTypeElement::TsPropertySignature(
                    TsPropertySignature {
                      span: DUMMY_SP,
                      readonly: true,
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
        self.expr_to_ts_type(*ts_const.expr, true)
      }
      Expr::TsSatisfies(satisifies) => {
        self.expr_to_ts_type(*satisifies.expr, as_const)
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
          .filter_map(|param| match param.pat {
            Pat::Ident(binding_id) => Some(TsFnParam::Ident(binding_id)),
            Pat::Array(arr_pat) => Some(TsFnParam::Array(arr_pat)),
            Pat::Rest(rest_pat) => Some(TsFnParam::Rest(rest_pat)),
            Pat::Object(obj) => Some(TsFnParam::Object(obj)),
            Pat::Assign(assign_pat) => {
              self.expr_to_ts_type(*assign_pat.right, false).map(|param| {
                let name = if let Pat::Ident(ident) = *assign_pat.left {
                  ident.id.sym.as_str().to_string()
                } else {
                  self.gen_unique_name()
                };

                TsFnParam::Ident(BindingIdent {
                  id: Ident::new(name.into(), assign_pat.span),
                  type_ann: Some(type_ann(param)),
                })
              })
            }
            Pat::Expr(expr) => {
              self.mark_diagnostic_unable_to_infer(expr.range());
              None
            }
            // Invalid code is invalid, not sure why SWC doesn't throw
            // a parse error here.
            Pat::Invalid(_) => None,
          })
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
      | Expr::Arrow(_)
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
                self.expr_to_ts_type(init, false)
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
      Decl::TsInterface(_)
      | Decl::TsTypeAlias(_)
      | Decl::TsEnum(_)
      | Decl::TsModule(_) => Some(decl),
      Decl::Using(_) => {
        self.mark_diagnostic(FastCheckDtsDiagnostic::UnsupportedUsing {
          range: self.source_range_to_range(decl.range()),
        });
        None
      }
    }
  }

  fn class_body_to_type(&mut self, body: Vec<ClassMember>) -> Vec<ClassMember> {
    body
      .into_iter()
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
                .expr_to_ts_type(*value, false)
                .map(type_ann)
                .or_else(|| Some(any_type_ann()));
            }
          }
          prop.value = None;

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
}

#[cfg(test)]
mod tests {
  use crate::{
    fast_check::{transform::emit, transform_dts::FastCheckDtsTransformer},
    source::{MemoryLoader, Source},
    symbols::RootSymbol,
    BuildOptions, DefaultModuleParser, GraphKind, ModuleGraph,
  };
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
export function foo(a: string | number): number {
  return {};
}"#,
      r#"export function foo(a: string): number;
export function foo(a: string | number): number;
"#,
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
    .await
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
      "export enum Foo {\n  A,\n  B\n}",
    )
    .await;
    transform_dts_test(
      r#"export const enum Foo { A, B }"#,
      "export const enum Foo {\n  A,\n  B\n}",
    )
    .await;
  }

  #[tokio::test]
  async fn dts_default_export() {
    transform_dts_test(
      r#"export default function(a: number, b: number): number {};"#,
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

  #[tokio::test]
  async fn dts_mod_stmt_test() {
    transform_dts_test(r#"const foo: number = 42;"#, "").await;
    transform_dts_test(r#"let foo: number = 42;"#, "").await;
    transform_dts_test(r#"var foo: number = 42;"#, "").await;

    transform_dts_test(r#"type Foo = number;"#, "type Foo = number;").await;
    transform_dts_test(r#"interface Foo {}"#, "interface Foo {\n}").await;
  }
}
