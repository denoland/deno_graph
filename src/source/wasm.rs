// Copyright 2018-2024 the Deno authors. MIT license.

use capacity_builder::StringBuilder;
use indexmap::IndexSet;
use wasm_dep_analyzer::ValueType;

pub fn wasm_module_to_dts(
  bytes: &[u8],
) -> Result<String, wasm_dep_analyzer::ParseError> {
  let wasm_deps = wasm_dep_analyzer::WasmDeps::parse(
    bytes,
    wasm_dep_analyzer::ParseOptions { skip_types: false },
  )?;
  Ok(wasm_module_deps_to_dts(&wasm_deps))
}

fn wasm_module_deps_to_dts(wasm_deps: &wasm_dep_analyzer::WasmDeps) -> String {
  #[derive(PartialEq, Eq)]
  enum TypePosition {
    Input,
    Output,
  }

  fn value_type_to_ts_type(
    value_type: ValueType,
    position: TypePosition,
  ) -> &'static str {
    match value_type {
      ValueType::I32 => "number",
      ValueType::I64 if position == TypePosition::Input => "bigint | number",
      ValueType::I64 => "bigint",
      ValueType::F32 => "number",
      ValueType::F64 => "number",
      ValueType::Unknown => "unknown",
    }
  }

  fn is_valid_ident(export_name: &str) -> bool {
    !export_name.is_empty()
      && deno_ast::swc::ast::Ident::verify_symbol(export_name).is_ok()
  }

  let is_valid_export_ident_per_export = wasm_deps
    .exports
    .iter()
    .map(|export| is_valid_ident(export.name))
    .collect::<Vec<_>>();
  let unique_import_modules = wasm_deps
    .imports
    .iter()
    .map(|import| import.module)
    .collect::<IndexSet<_>>();

  StringBuilder::build(|builder| {
    for import_module in &unique_import_modules {
      builder.append("import \"");
      builder.append(import_module);
      builder.append("\";\n");
    }

    for (i, export) in wasm_deps.exports.iter().enumerate() {
      let has_valid_export_ident = is_valid_export_ident_per_export[i];
      if has_valid_export_ident {
        builder.append("export ");
      }
      fn write_export_name<'a>(
        builder: &mut StringBuilder<'a>,
        export: &'a wasm_dep_analyzer::Export<'a>,
        has_valid_export_ident: bool,
        index: usize,
      ) {
        if has_valid_export_ident {
          builder.append(export.name);
        } else {
          builder.append("__deno_wasm_export_");
          builder.append(index);
          builder.append("__");
        }
      }
      let mut add_var = |type_text: &'static str| {
        builder.append("declare const ");
        write_export_name(builder, export, has_valid_export_ident, i);
        builder.append(": ");
        builder.append(type_text);
        builder.append(";\n");
      };

      match &export.export_type {
        wasm_dep_analyzer::ExportType::Function(function_signature) => {
          match function_signature {
            Ok(signature) => {
              builder.append("declare function ");
              write_export_name(builder, export, has_valid_export_ident, i);
              builder.append('(');
              for (i, param) in signature.params.iter().enumerate() {
                if i > 0 {
                  builder.append(", ");
                }
                builder.append("arg");
                builder.append(i);
                builder.append(": ");
                builder
                  .append(value_type_to_ts_type(*param, TypePosition::Input));
              }
              builder.append("): ");
              builder.append(
                signature
                  .returns
                  .first()
                  .map(|t| value_type_to_ts_type(*t, TypePosition::Output))
                  .unwrap_or("void"),
              );
              builder.append(";\n");
            }
            Err(_) => add_var("unknown"),
          }
        }
        wasm_dep_analyzer::ExportType::Table => add_var("WebAssembly.Table"),
        wasm_dep_analyzer::ExportType::Memory => add_var("WebAssembly.Memory"),
        wasm_dep_analyzer::ExportType::Global(global_type) => match global_type
        {
          Ok(global_type) => add_var(value_type_to_ts_type(
            global_type.value_type,
            TypePosition::Output,
          )),
          Err(_) => add_var("unknown"),
        },
        wasm_dep_analyzer::ExportType::Tag
        | wasm_dep_analyzer::ExportType::Unknown => add_var("unknown"),
      }

      if !has_valid_export_ident {
        builder.append("export { ");
        write_export_name(builder, export, has_valid_export_ident, i);
        builder.append(" as \"");
        builder.append(export.name);
        builder.append("\" };\n");
      }
    }
  })
  .unwrap()
}

#[cfg(test)]
mod test {
  use pretty_assertions::assert_eq;
  use wasm_dep_analyzer::Export;
  use wasm_dep_analyzer::FunctionSignature;
  use wasm_dep_analyzer::WasmDeps;

  use super::*;

  #[test]
  fn test_output() {
    let text = wasm_module_deps_to_dts(&WasmDeps {
      imports: vec![],
      exports: vec![
        Export {
          name: "name--1",
          index: 0,
          export_type: wasm_dep_analyzer::ExportType::Function(Ok(
            FunctionSignature {
              params: vec![],
              returns: vec![],
            },
          )),
        },
        Export {
          name: "name2",
          index: 1,
          export_type: wasm_dep_analyzer::ExportType::Function(Ok(
            FunctionSignature {
              params: vec![ValueType::I32, ValueType::I64],
              returns: vec![ValueType::I64],
            },
          )),
        },
        Export {
          name: "name3",
          index: 2,
          export_type: wasm_dep_analyzer::ExportType::Function(Err(
            wasm_dep_analyzer::ParseError::IntegerOverflow,
          )),
        },
        Export {
          name: "name4",
          index: 3,
          export_type: wasm_dep_analyzer::ExportType::Table,
        },
        Export {
          name: "name5",
          index: 4,
          export_type: wasm_dep_analyzer::ExportType::Memory,
        },
        Export {
          name: "name6",
          index: 5,
          export_type: wasm_dep_analyzer::ExportType::Global(Ok(
            wasm_dep_analyzer::GlobalType {
              value_type: ValueType::I32,
              mutability: false,
            },
          )),
        },
        Export {
          name: "name7",
          index: 6,
          export_type: wasm_dep_analyzer::ExportType::Global(Err(
            wasm_dep_analyzer::ParseError::NotWasm,
          )),
        },
        Export {
          name: "name8",
          index: 7,
          export_type: wasm_dep_analyzer::ExportType::Unknown,
        },
        Export {
          name: "name9--",
          index: 8,
          export_type: wasm_dep_analyzer::ExportType::Unknown,
        },
        Export {
          name: "default",
          index: 9,
          export_type: wasm_dep_analyzer::ExportType::Unknown,
        },
      ],
    });
    assert_eq!(
      text,
      "declare function __deno_wasm_export_0__(): void;
export { __deno_wasm_export_0__ as \"name--1\" };
export declare function name2(arg0: number, arg1: bigint | number): bigint;
export declare const name3: unknown;
export declare const name4: WebAssembly.Table;
export declare const name5: WebAssembly.Memory;
export declare const name6: number;
export declare const name7: unknown;
export declare const name8: unknown;
declare const __deno_wasm_export_8__: unknown;
export { __deno_wasm_export_8__ as \"name9--\" };
declare const __deno_wasm_export_9__: unknown;
export { __deno_wasm_export_9__ as \"default\" };
"
    );
  }
}
