use deno_ast::swc::{
  ast::{
    BindingIdent, ClassMember, Decl, DefaultDecl, ExportDecl,
    ExportDefaultExpr, Expr, Ident, Lit, MethodKind, Module, ModuleDecl,
    ModuleItem, Pat, Prop, PropName, PropOrSpread, Stmt, TsFnOrConstructorType,
    TsFnParam, TsFnType, TsPropertySignature, TsTupleElement, TsTupleType,
    TsType, TsTypeElement, TsTypeLit, VarDecl, VarDeclKind, VarDeclarator,
  },
  common::DUMMY_SP,
};

use crate::FastCheckDiagnostic;

use super::swc_helpers::{
  any_type_ann, maybe_lit_to_ts_type, maybe_lit_to_ts_type_const, ts_readonly,
  ts_tuple_element, type_ann,
};

pub struct FastCheckDtsTransformer {
  id_counter: usize,
}

impl FastCheckDtsTransformer {
  pub fn new() -> Self {
    Self { id_counter: 0 }
  }

  fn gen_unique_name(&mut self) -> String {
    self.id_counter += 1;
    format!("_dts_{}", self.id_counter)
  }

  pub fn transform(
    &mut self,
    mut module: Module,
  ) -> Result<Module, Vec<FastCheckDiagnostic>> {
    let mut body = module.body;

    let mut new_items: Vec<ModuleItem> = Vec::with_capacity(body.len());

    for item in &mut body {
      match item {
        ModuleItem::ModuleDecl(module_decl) => match module_decl {
          ModuleDecl::Import(_) => {
            new_items.push(item.clone());
          }
          ModuleDecl::ExportDecl(export_decl) => {
            if let Some(decl) = self.decl_to_type_decl(&export_decl.decl) {
              new_items.push(ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(
                ExportDecl {
                  decl,
                  span: DUMMY_SP,
                },
              )));
            }
          }
          ModuleDecl::ExportDefaultDecl(export_decl) => {
            match &mut export_decl.decl {
              DefaultDecl::Class(class_expr) => {
                class_expr.class.body =
                  self.class_body_to_type(&class_expr.class.body);
                new_items.push(item.clone());
              }
              DefaultDecl::Fn(fn_expr) => {
                fn_expr.function.body = None;
                new_items.push(item.clone());
              }
              DefaultDecl::TsInterfaceDecl(_) => new_items.push(item.clone()),
            }
          }
          ModuleDecl::ExportDefaultExpr(export_default_expr) => {
            let name = self.gen_unique_name();
            let name_ident = Ident::new(name.into(), DUMMY_SP);
            let type_ann = self
              .expr_to_ts_type(*export_default_expr.expr.clone(), false)
              .map(type_ann)
              .or(Some(any_type_ann()));

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
                span: DUMMY_SP,
                expr: Box::new(Expr::Ident(name_ident)),
              }),
            ))
          }
          // Keep all these
          ModuleDecl::TsImportEquals(_)
          | ModuleDecl::TsNamespaceExport(_)
          | ModuleDecl::TsExportAssignment(_) => {
            new_items.push(ModuleItem::ModuleDecl(module_decl.clone()));
          }
          _ => {}
        },
        ModuleItem::Stmt(stmt) => {
          if let Stmt::Decl(decl) = stmt {
            match decl {
              Decl::TsInterface(_)
              | Decl::TsTypeAlias(_)
              | Decl::TsEnum(_)
              | Decl::TsModule(_) => {
                new_items.push(ModuleItem::Stmt(stmt.clone()));
              }
              _ => {}
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
          if let Some(ts_expr) =
            elems.and_then(|item| self.expr_to_ts_type(*item.expr, as_const))
          {
            elem_types.push(ts_tuple_element(ts_expr));
          }
        }

        Some(ts_readonly(TsType::TsTupleType(TsTupleType {
          span: DUMMY_SP,
          elem_types,
        })))
      }
      Expr::Object(obj) => {
        let mut members: Vec<TsTypeElement> = Vec::with_capacity(obj.len());

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
                // TODO: Requires type resolving, skip for now
                Prop::Shorthand(_) => {}
                _ => {}
              }
            }
            PropOrSpread::Spread(_) => {}
          }
        }

        Some(TsType::TsTypeLit(TsTypeLit {
          span: DUMMY_SP,
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
      Expr::TsAs(ts_as) => Some(*ts_as.type_ann.clone()),
      Expr::Fn(fn_expr) => {
        let return_type = fn_expr
          .function
          .return_type
          .map_or(any_type_ann(), |val| val);

        let params: Vec<TsFnParam> = fn_expr
          .function
          .params
          .iter()
          .filter_map(|param| match &param.pat {
            Pat::Ident(binding_id) => {
              Some(TsFnParam::Ident(binding_id.clone()))
            }
            Pat::Array(arr_pat) => Some(TsFnParam::Array(arr_pat.clone())),
            Pat::Rest(rest_pat) => Some(TsFnParam::Rest(rest_pat.clone())),
            Pat::Object(obj) => Some(TsFnParam::Object(obj.clone())),
            Pat::Assign(assign_pat) => self
              .expr_to_ts_type(*assign_pat.right.clone(), false)
              .map(|param| {
                let name = if let Pat::Ident(ident) = *assign_pat.left.clone() {
                  ident.id.sym.as_str().to_string()
                } else {
                  self.gen_unique_name()
                };

                TsFnParam::Ident(BindingIdent {
                  id: Ident::new(name.into(), DUMMY_SP),
                  type_ann: Some(type_ann(param)),
                })
              }),
            _ => None,
          })
          .collect();

        Some(TsType::TsFnOrConstructorType(
          TsFnOrConstructorType::TsFnType(TsFnType {
            span: DUMMY_SP,
            params,
            type_ann: return_type,
            type_params: fn_expr.function.type_params,
          }),
        ))
      }
      _ => None,
    }
  }

  fn decl_to_type_decl(&mut self, decl: &Decl) -> Option<Decl> {
    match decl {
      Decl::Class(class_decl) => {
        let mut decl = class_decl.clone();
        decl.class.body = self.class_body_to_type(&decl.class.body);
        decl.declare = true;
        Some(Decl::Class(decl))
      }
      Decl::Fn(fn_decl) => {
        let mut decl = fn_decl.clone();
        decl.function.body = None;
        Some(Decl::Fn(decl))
      }
      Decl::Var(var_decl) => {
        let mut decl = *var_decl.clone();
        decl.declare = true;

        for decl in &mut decl.decls {
          if let Pat::Ident(ident) = &mut decl.name {
            if ident.type_ann.is_some() {
              decl.init = None;
              continue;
            }

            if let Some(init_box) = &decl.init {
              let init = *init_box.clone();
              ident.type_ann = self
                .expr_to_ts_type(init, false)
                .map(type_ann)
                .or_else(|| Some(any_type_ann()));
            } else {
              ident.type_ann = Some(any_type_ann());
            }
          }

          decl.init = None;
        }

        Some(Decl::Var(Box::new(decl)))
      }
      Decl::TsInterface(_)
      | Decl::TsTypeAlias(_)
      | Decl::TsEnum(_)
      | Decl::TsModule(_) => Some(decl.clone()),
      _ => None,
    }
  }

  fn class_body_to_type(&mut self, body: &[ClassMember]) -> Vec<ClassMember> {
    body
      .iter()
      .filter_map(|member| match member {
        ClassMember::Constructor(class_constructor) => {
          let mut new_constructor = class_constructor.clone();
          new_constructor.body = None;
          Some(ClassMember::Constructor(new_constructor))
        }
        ClassMember::Method(method) => {
          let mut new_method = method.clone();
          new_method.function.body = None;
          if new_method.kind == MethodKind::Setter {
            new_method.function.return_type = None;
          }
          Some(ClassMember::Method(new_method))
        }
        ClassMember::ClassProp(prop) => {
          let mut new_prop = prop.clone();
          if new_prop.type_ann.is_none() {
            if let Some(value) = new_prop.value {
              new_prop.type_ann = self
                .expr_to_ts_type(*value, false)
                .map(type_ann)
                .or(Some(any_type_ann()));
            }
          }
          new_prop.value = None;

          Some(ClassMember::ClassProp(new_prop))
        }
        ClassMember::TsIndexSignature(index_sig) => {
          Some(ClassMember::TsIndexSignature(index_sig.clone()))
        }

        _ => None,
      })
      .collect()
  }
}

#[cfg(test)]
mod tests {
  use crate::{
    fast_check::{
      transform::{emit, CommentsMut},
      transform_dts::FastCheckDtsTransformer,
    },
    source::{MemoryLoader, Source},
    symbols::RootSymbol,
    BuildOptions, DefaultModuleParser, GraphKind, ModuleGraph,
  };
  use url::Url;

  async fn transform_dts_test(source: &str, expected: &str) {
    let specifier = Url::parse("file:///mod.ts").unwrap();

    let spec_str = specifier.to_string();
    let mut loader = MemoryLoader::new(
      vec![(
        spec_str.clone(),
        Source::Module {
          specifier: spec_str,
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

    let module = module_info.source().module().clone();

    let mut transformer = FastCheckDtsTransformer::new();
    let module = transformer.transform(module).unwrap();

    let parsed_source = module_info.source();
    let comments =
      CommentsMut::new(parsed_source.comments().as_single_threaded());
    let comments2 = comments.into_multi_threaded().into_single_threaded();

    let (actual, _) =
      emit(&specifier, &comments2, parsed_source.text_info(), &module).unwrap();

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
