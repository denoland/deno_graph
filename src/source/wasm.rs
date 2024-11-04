// Copyright 2018-2024 the Deno authors. MIT license.

use std::borrow::Cow;

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

  let mut text = String::new();
  let mut internal_names_count = 0;

  for import in &wasm_deps.imports {
    text.push_str(&format!("import \"{}\";\n", import.module));
  }

  text.push_str("declare namespace __DenoWasmInstance__ {\n");

  for export in &wasm_deps.exports {
    let has_valid_export_ident = is_valid_ident(export.name);
    text.push_str("  ");
    let export_name = if has_valid_export_ident {
      Cow::Borrowed(export.name)
    } else {
      let export_name =
        format!("__deno_wasm_export_{}__", internal_names_count);
      internal_names_count += 1;
      Cow::Owned(export_name)
    };
    if has_valid_export_ident {
      text.push_str("export ");
    }
    let mut add_var = |type_text: &str| {
      text.push_str("const ");
      text.push_str(&export_name);
      text.push_str(": ");
      text.push_str(type_text);
      text.push_str(";\n");
    };

    match &export.export_type {
      wasm_dep_analyzer::ExportType::Function(function_signature) => {
        match function_signature {
          Ok(signature) => {
            text.push_str("function ");
            text.push_str(&export_name);
            text.push('(');
            for (i, param) in signature.params.iter().enumerate() {
              if i > 0 {
                text.push_str(", ");
              }
              text.push_str("arg");
              text.push_str(i.to_string().as_str());
              text.push_str(": ");
              text.push_str(value_type_to_ts_type(*param, TypePosition::Input));
            }
            text.push_str("): ");
            text.push_str(
              signature
                .returns
                .first()
                .map(|t| value_type_to_ts_type(*t, TypePosition::Output))
                .unwrap_or("void"),
            );
            text.push_str(";\n");
          }
          Err(_) => add_var("unknown"),
        }
      }
      wasm_dep_analyzer::ExportType::Table => add_var("WebAssembly.Table"),
      wasm_dep_analyzer::ExportType::Memory => add_var("WebAssembly.Memory"),
      wasm_dep_analyzer::ExportType::Global(global_type) => match global_type {
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
      text.push_str(&format!(
        "  export {{ {} as \"{}\" }};\n",
        export_name, export.name
      ));
    }
  }

  text.push_str("}\n\n");
  text.push_str("export default __DenoWasmInstance__;\n");
  text
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
      ],
    });
    assert_eq!(
      text,
      "declare namespace __DenoWasmInstance__ {
  function __deno_wasm_export_0__(): void;
  export { __deno_wasm_export_0__ as \"name--1\" };
  export function name2(arg0: number, arg1: bigint | number): bigint;
  export const name3: unknown;
  export const name4: WebAssembly.Table;
  export const name5: WebAssembly.Memory;
  export const name6: number;
  export const name7: unknown;
  export const name8: unknown;
  const __deno_wasm_export_1__: unknown;
  export { __deno_wasm_export_1__ as \"name9--\" };
}

export default __DenoWasmInstance__;
"
    );
  }
}
