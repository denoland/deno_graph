// Copyright 2018-2024 the Deno authors. MIT license.

use std::borrow::Cow;

use wasm_dep_analyzer::ValueType;

pub fn wasm_module_to_dts(wasm_deps: &wasm_dep_analyzer::WasmDeps) -> String {
  fn value_type_to_ts_type(value_type: ValueType) -> &'static str {
    match value_type {
      ValueType::I32 => "number",
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

  for export in &wasm_deps.exports {
    let is_exported = is_valid_ident(export.name);
    let export_name = if is_exported {
      Cow::Borrowed(export.name)
    } else {
      let export_name =
        format!("\"__deno_wasm_export_{}__\"", internal_names_count);
      internal_names_count += 1;
      Cow::Owned(export_name)
    };
    if is_exported {
      text.push_str("export ");
    }
    let mut add_var = |type_text: &str| {
      text.push_str("declare const ");
      text.push_str(&export_name);
      text.push_str(": ");
      text.push_str(&type_text);
      text.push_str(";\n");
    };

    match &export.export_type {
      wasm_dep_analyzer::ExportType::Function(function_signature) => {
        match function_signature {
          Ok(signature) => {
            text.push_str("declare function ");
            text.push_str(&export_name);
            text.push_str("(\n");
            for (i, param) in signature.params.iter().enumerate() {
              text.push_str("  arg");
              text.push_str(i.to_string().as_str());
              text.push_str(": ");
              text.push_str(value_type_to_ts_type(*param));
              text.push_str(",\n");
            }
            text.push_str("): ");
            text.push_str(
              signature
                .returns
                .first()
                .map(|t| value_type_to_ts_type(*t))
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
        Ok(global_type) => {
          add_var(value_type_to_ts_type(global_type.value_type))
        }
        Err(_) => add_var("unknown"),
      },
      wasm_dep_analyzer::ExportType::Tag
      | wasm_dep_analyzer::ExportType::Unknown => add_var("unknown"),
    }

    if !is_exported {
      text.push_str(&format!(
        "export {{ {} as \"{}\" }};",
        export_name, export.name
      ));
    }
  }
  text
}

#[cfg(test)]
mod test {
  use wasm_dep_analyzer::Export;
  use wasm_dep_analyzer::FunctionSignature;
  use wasm_dep_analyzer::WasmDeps;

  use super::*;

  #[test]
  fn test_output() {
    let text = wasm_module_to_dts(&WasmDeps {
      imports: vec![],
      exports: vec![Export {
        name: "name--1",
        index: 0,
        export_type: wasm_dep_analyzer::ExportType::Function(Ok(
          FunctionSignature {
            params: vec![],
            returns: vec![],
          },
        )),
      }],
    });
    assert_eq!(text, "export declare function name1(): void;\n");
  }
}
