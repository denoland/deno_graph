// Copyright 2018-2024 the Deno authors. MIT license.

use capacity_builder::StringBuilder;
use deno_error::JsError;
use indexmap::IndexMap;
use thiserror::Error;
use wasm_dep_analyzer::ValueType;
use wit_component::DecodedWasm;
use wit_parser::Function;
use wit_parser::FunctionKind;
use wit_parser::InterfaceId;
use wit_parser::Resolve;
use wit_parser::Type;
use wit_parser::TypeDefKind;
use wit_parser::TypeId;
use wit_parser::WorldId;
use wit_parser::WorldItem;
use wit_parser::WorldKey;

#[derive(Debug, Clone, Error, JsError)]
#[class(type)]
pub enum WasmDtsError {
  #[error(transparent)]
  Core(wasm_dep_analyzer::ParseError),
  #[error("{0}")]
  Component(String),
}

pub fn wasm_module_to_dts(bytes: &[u8]) -> Result<String, WasmDtsError> {
  if is_core_wasm(bytes) {
    let wasm_deps = wasm_dep_analyzer::WasmDeps::parse(
      bytes,
      wasm_dep_analyzer::ParseOptions { skip_types: false },
    )
    .map_err(WasmDtsError::Core)?;
    return Ok(wasm_module_deps_to_dts(&wasm_deps));
  }

  match wit_component::decode(bytes) {
    Ok(DecodedWasm::Component(resolve, world)) => {
      return Ok(wasm_component_to_dts(&resolve, world));
    }
    Err(_) => {}
    Ok(DecodedWasm::WitPackage(..)) => {}
  }

  match wasm_dep_analyzer::WasmDeps::parse(
    bytes,
    wasm_dep_analyzer::ParseOptions { skip_types: false },
  ) {
    Ok(wasm_deps)
      if !wasm_deps.imports.is_empty() || !wasm_deps.exports.is_empty() =>
    {
      Ok(wasm_module_deps_to_dts(&wasm_deps))
    }
    Ok(wasm_deps) => match wit_component::decode(bytes) {
      Ok(DecodedWasm::WitPackage(resolve, package)) => Ok(
        wasm_wit_package_to_dts(&resolve, package)
          .unwrap_or_else(|| wasm_module_deps_to_dts(&wasm_deps)),
      ),
      _ => Ok(wasm_module_deps_to_dts(&wasm_deps)),
    },
    Err(err) => match wit_component::decode(bytes) {
      Ok(DecodedWasm::WitPackage(resolve, package)) => {
        wasm_wit_package_to_dts(&resolve, package)
          .ok_or(WasmDtsError::Core(err))
      }
      _ => Err(WasmDtsError::Core(err)),
    },
  }
}

fn is_core_wasm(bytes: &[u8]) -> bool {
  bytes.starts_with(b"\0asm\x01\0\0\0")
}

fn wasm_wit_package_to_dts(
  resolve: &Resolve,
  package: wit_parser::PackageId,
) -> Option<String> {
  let worlds = resolve.packages[package]
    .worlds
    .values()
    .copied()
    .collect::<Vec<_>>();
  if worlds.len() == 1 {
    Some(wasm_component_to_dts(resolve, worlds[0]))
  } else {
    None
  }
}

fn wasm_component_to_dts(resolve: &Resolve, world_id: WorldId) -> String {
  let world = &resolve.worlds[world_id];
  StringBuilder::build(|builder| {
    let mut type_names = TypeNamer::default();
    for item in world.exports.values() {
      collect_exported_item_types(resolve, item, &mut type_names);
    }

    let names = type_names
      .names
      .iter()
      .map(|(type_id, name)| (*type_id, name.clone()))
      .collect::<Vec<_>>();
    for (type_id, name) in names {
      write_type_decl(builder, resolve, type_id, &name);
    }

    for (key, item) in &world.exports {
      match item {
        WorldItem::Function(func) => {
          write_exported_function(builder, resolve, &key_to_name(key), func);
        }
        WorldItem::Interface { id, .. } => {
          write_exported_interface(builder, resolve, &key_to_name(key), *id);
        }
        WorldItem::Type(type_id) => {
          write_exported_type_alias(
            builder,
            resolve,
            &key_to_name(key),
            *type_id,
          );
        }
      }
    }
  })
  .unwrap()
}

fn collect_exported_item_types(
  resolve: &Resolve,
  item: &WorldItem,
  type_names: &mut TypeNamer,
) {
  match item {
    WorldItem::Function(func) => {
      collect_function_types(resolve, func, type_names)
    }
    WorldItem::Interface { id, .. } => {
      let interface = &resolve.interfaces[*id];
      for type_id in interface.types.values() {
        type_names.name_type(resolve, *type_id);
      }
      for func in interface.functions.values() {
        collect_function_types(resolve, func, type_names);
      }
    }
    WorldItem::Type(type_id) => {
      type_names.name_type(resolve, *type_id);
    }
  }
}

fn collect_function_types(
  resolve: &Resolve,
  func: &Function,
  type_names: &mut TypeNamer,
) {
  for (_, ty) in &func.params {
    type_names.name_type_ref(resolve, *ty);
  }
  if let Some(ty) = func.result {
    type_names.name_type_ref(resolve, ty);
  }
}

#[derive(Default)]
struct TypeNamer {
  names: IndexMap<TypeId, String>,
}

impl TypeNamer {
  fn name_type_ref(&mut self, resolve: &Resolve, ty: Type) {
    if let Type::Id(type_id) = ty {
      if resolve.types[type_id].name.is_some() {
        self.name_type(resolve, type_id);
      }
      self.name_nested_types(resolve, type_id);
    }
  }

  fn name_type(&mut self, resolve: &Resolve, type_id: TypeId) {
    let name = resolve.types[type_id]
      .name
      .as_deref()
      .map(ts_type_name)
      .unwrap_or_else(|| format!("__DenoWasmType{}", self.names.len()));
    self.names.entry(type_id).or_insert(name);
  }

  fn name_nested_types(&mut self, resolve: &Resolve, type_id: TypeId) {
    match &resolve.types[type_id].kind {
      TypeDefKind::Record(record) => {
        for field in &record.fields {
          self.name_type_ref(resolve, field.ty);
        }
      }
      TypeDefKind::Tuple(tuple) => {
        for ty in &tuple.types {
          self.name_type_ref(resolve, *ty);
        }
      }
      TypeDefKind::Variant(variant) => {
        for case in &variant.cases {
          if let Some(ty) = case.ty {
            self.name_type_ref(resolve, ty);
          }
        }
      }
      TypeDefKind::Option(ty)
      | TypeDefKind::List(ty)
      | TypeDefKind::FixedSizeList(ty, _)
      | TypeDefKind::Type(ty) => {
        self.name_type_ref(resolve, *ty);
      }
      TypeDefKind::Result(result) => {
        if let Some(ty) = result.ok {
          self.name_type_ref(resolve, ty);
        }
        if let Some(ty) = result.err {
          self.name_type_ref(resolve, ty);
        }
      }
      TypeDefKind::Map(key, value) => {
        self.name_type_ref(resolve, *key);
        self.name_type_ref(resolve, *value);
      }
      TypeDefKind::Future(ty) | TypeDefKind::Stream(ty) => {
        if let Some(ty) = ty {
          self.name_type_ref(resolve, *ty);
        }
      }
      TypeDefKind::Resource
      | TypeDefKind::Handle(_)
      | TypeDefKind::Flags(_)
      | TypeDefKind::Enum(_)
      | TypeDefKind::Unknown => {}
    }
  }
}

fn append_owned<'a>(builder: &mut StringBuilder<'a>, text: String) {
  builder.append_owned_unsafe(text.len(), || text);
}

fn append_str<'a>(builder: &mut StringBuilder<'a>, text: &str) {
  builder.append_owned_unsafe(text.len(), || text.to_string());
}

fn write_type_decl<'a>(
  builder: &mut StringBuilder<'a>,
  resolve: &Resolve,
  type_id: TypeId,
  name: &str,
) {
  let ty = &resolve.types[type_id];
  match &ty.kind {
    TypeDefKind::Record(record) => {
      builder.append("export interface ");
      append_str(builder, name);
      builder.append(" {\n");
      for field in &record.fields {
        builder.append("  ");
        write_property_name(builder, &field.name);
        builder.append(": ");
        write_ts_type(builder, resolve, field.ty);
        builder.append(";\n");
      }
      builder.append("}\n");
    }
    TypeDefKind::Resource => {
      builder.append("export class ");
      append_str(builder, name);
      builder.append(" {\n  private constructor();\n}\n");
    }
    _ => {
      builder.append("export type ");
      append_str(builder, name);
      builder.append(" = ");
      write_type_def_kind(builder, resolve, &ty.kind);
      builder.append(";\n");
    }
  }
}

fn write_exported_type_alias<'a>(
  builder: &mut StringBuilder<'a>,
  resolve: &Resolve,
  export_name: &str,
  type_id: TypeId,
) {
  let local_name = resolve.types[type_id]
    .name
    .as_deref()
    .map(ts_type_name)
    .unwrap_or_else(|| format!("__DenoWasmType{}", type_id.index()));
  let export_type_name = ts_type_name(export_name);
  if local_name == export_type_name {
    return;
  }
  builder.append("export type ");
  if is_valid_ident(&export_type_name) {
    append_owned(builder, export_type_name);
    builder.append(" = ");
    append_owned(builder, local_name);
    builder.append(";\n");
  } else {
    builder.append("__DenoWasmExportType");
    builder.append(type_id.index());
    builder.append("__ = ");
    append_owned(builder, local_name);
    builder.append(";\nexport { __DenoWasmExportType");
    builder.append(type_id.index());
    builder.append("__ as \"");
    append_str(builder, export_name);
    builder.append("\" };\n");
  }
}

fn write_exported_function<'a>(
  builder: &mut StringBuilder<'a>,
  resolve: &Resolve,
  name: &str,
  func: &Function,
) {
  let export_name = ts_value_name(name);
  let has_valid_name = is_valid_ident(&export_name);
  if has_valid_name {
    builder.append("export ");
  }
  builder.append("declare function ");
  if has_valid_name {
    append_str(builder, &export_name);
  } else {
    builder.append("__DenoWasmExport");
    append_owned(builder, ts_type_name(name));
    builder.append("__");
  }
  write_function_type(builder, resolve, func, true);
  builder.append(";\n");
  if !has_valid_name {
    builder.append("export { __DenoWasmExport");
    append_owned(builder, ts_type_name(name));
    builder.append("__ as \"");
    append_str(builder, name);
    builder.append("\" };\n");
  }
}

fn write_exported_interface<'a>(
  builder: &mut StringBuilder<'a>,
  resolve: &Resolve,
  name: &str,
  interface_id: InterfaceId,
) {
  let local_name = format!("__DenoWasmInterface{}", interface_id.index());
  let export_name = ts_value_name(name);
  builder.append("declare const ");
  append_str(builder, &local_name);
  builder.append(": {\n");
  let interface = &resolve.interfaces[interface_id];
  for func in interface.functions.values() {
    if !matches!(
      func.kind,
      FunctionKind::Freestanding | FunctionKind::AsyncFreestanding
    ) {
      continue;
    }
    builder.append("  ");
    write_property_name(builder, func.item_name());
    write_function_type(builder, resolve, func, false);
    builder.append(";\n");
  }
  builder.append("};\n");
  if is_valid_ident(&export_name) {
    builder.append("export { ");
    append_str(builder, &local_name);
    builder.append(" as ");
    append_str(builder, &export_name);
    builder.append(" };\n");
  } else {
    builder.append("export { ");
    append_str(builder, &local_name);
    builder.append(" as \"");
    append_str(builder, name);
    builder.append("\" };\n");
  }
}

fn write_function_type<'a>(
  builder: &mut StringBuilder<'a>,
  resolve: &Resolve,
  func: &Function,
  include_name_parens: bool,
) {
  if include_name_parens {
    builder.append('(');
  }
  for (i, (name, ty)) in func.params.iter().enumerate() {
    if i > 0 {
      builder.append(", ");
    }
    let param_name = ts_value_name(name);
    let name = if is_valid_ident(&param_name) {
      param_name.as_str()
    } else {
      "arg"
    };
    append_str(builder, name);
    if !is_valid_ident(&param_name) {
      builder.append(i);
    }
    builder.append(": ");
    write_ts_type(builder, resolve, *ty);
  }
  builder.append("): ");
  if func.kind.is_async() {
    builder.append("Promise<");
  }
  match func.result {
    Some(ty) => write_ts_type(builder, resolve, ty),
    None => builder.append("void"),
  }
  if func.kind.is_async() {
    builder.append('>');
  }
}

fn write_type_def_kind<'a>(
  builder: &mut StringBuilder<'a>,
  resolve: &Resolve,
  kind: &TypeDefKind,
) {
  match kind {
    TypeDefKind::Record(record) => {
      builder.append("{ ");
      for (i, field) in record.fields.iter().enumerate() {
        if i > 0 {
          builder.append("; ");
        }
        write_property_name(builder, &field.name);
        builder.append(": ");
        write_ts_type(builder, resolve, field.ty);
      }
      builder.append(" }");
    }
    TypeDefKind::Resource => builder.append("unknown"),
    TypeDefKind::Handle(_) => builder.append("unknown"),
    TypeDefKind::Flags(flags) => {
      builder.append("{ ");
      for (i, flag) in flags.flags.iter().enumerate() {
        if i > 0 {
          builder.append("; ");
        }
        write_property_name(builder, &flag.name);
        builder.append(": boolean");
      }
      builder.append(" }");
    }
    TypeDefKind::Tuple(tuple) => {
      builder.append('[');
      for (i, ty) in tuple.types.iter().enumerate() {
        if i > 0 {
          builder.append(", ");
        }
        write_ts_type(builder, resolve, *ty);
      }
      builder.append(']');
    }
    TypeDefKind::Variant(variant) => {
      for (i, case) in variant.cases.iter().enumerate() {
        if i > 0 {
          builder.append(" | ");
        }
        builder.append("{ tag: ");
        write_string_literal(builder, &case.name);
        if let Some(ty) = case.ty {
          builder.append("; val: ");
          write_ts_type(builder, resolve, ty);
        }
        builder.append(" }");
      }
      if variant.cases.is_empty() {
        builder.append("never");
      }
    }
    TypeDefKind::Enum(enum_) => {
      for (i, case) in enum_.cases.iter().enumerate() {
        if i > 0 {
          builder.append(" | ");
        }
        write_string_literal(builder, &case.name);
      }
      if enum_.cases.is_empty() {
        builder.append("never");
      }
    }
    TypeDefKind::Option(ty) => {
      write_ts_type(builder, resolve, *ty);
      builder.append(" | undefined");
    }
    TypeDefKind::Result(result) => {
      builder.append("{ tag: \"ok\"");
      if let Some(ok) = result.ok {
        builder.append("; val: ");
        write_ts_type(builder, resolve, ok);
      }
      builder.append(" } | { tag: \"err\"");
      if let Some(err) = result.err {
        builder.append("; val: ");
        write_ts_type(builder, resolve, err);
      }
      builder.append(" }");
    }
    TypeDefKind::List(ty) => write_array_type(builder, resolve, *ty),
    TypeDefKind::Map(key, value) => {
      builder.append("Map<");
      write_ts_type(builder, resolve, *key);
      builder.append(", ");
      write_ts_type(builder, resolve, *value);
      builder.append(">");
    }
    TypeDefKind::FixedSizeList(ty, size) => {
      builder.append("[");
      for i in 0..*size {
        if i > 0 {
          builder.append(", ");
        }
        write_ts_type(builder, resolve, *ty);
      }
      builder.append("]");
    }
    TypeDefKind::Future(ty) => {
      builder.append("Promise<");
      match ty {
        Some(ty) => write_ts_type(builder, resolve, *ty),
        None => builder.append("void"),
      }
      builder.append(">");
    }
    TypeDefKind::Stream(ty) => {
      builder.append("AsyncIterable<");
      match ty {
        Some(ty) => write_ts_type(builder, resolve, *ty),
        None => builder.append("unknown"),
      }
      builder.append(">");
    }
    TypeDefKind::Type(ty) => write_ts_type(builder, resolve, *ty),
    TypeDefKind::Unknown => builder.append("unknown"),
  }
}

fn write_ts_type<'a>(
  builder: &mut StringBuilder<'a>,
  resolve: &Resolve,
  ty: Type,
) {
  match ty {
    Type::Bool => builder.append("boolean"),
    Type::U8
    | Type::U16
    | Type::U32
    | Type::S8
    | Type::S16
    | Type::S32
    | Type::F32
    | Type::F64 => builder.append("number"),
    Type::U64 | Type::S64 => builder.append("bigint"),
    Type::Char | Type::String => builder.append("string"),
    Type::ErrorContext => builder.append("unknown"),
    Type::Id(type_id) => {
      let ty = &resolve.types[type_id];
      if let Some(name) = &ty.name {
        append_owned(builder, ts_type_name(name));
      } else {
        write_type_def_kind(builder, resolve, &ty.kind);
      }
    }
  }
}

fn write_array_type<'a>(
  builder: &mut StringBuilder<'a>,
  resolve: &Resolve,
  ty: Type,
) {
  match ty {
    Type::U8 => builder.append("Uint8Array"),
    Type::U16 => builder.append("Uint16Array"),
    Type::U32 => builder.append("Uint32Array"),
    Type::S8 => builder.append("Int8Array"),
    Type::S16 => builder.append("Int16Array"),
    Type::S32 => builder.append("Int32Array"),
    Type::U64 => builder.append("BigUint64Array"),
    Type::S64 => builder.append("BigInt64Array"),
    Type::F32 => builder.append("Float32Array"),
    Type::F64 => builder.append("Float64Array"),
    _ => {
      builder.append("Array<");
      write_ts_type(builder, resolve, ty);
      builder.append(">");
    }
  }
}

fn write_property_name<'a>(builder: &mut StringBuilder<'a>, name: &str) {
  let name = ts_value_name(name);
  if is_valid_ident(&name) {
    append_str(builder, &name);
  } else {
    write_string_literal(builder, &name);
  }
}

fn write_string_literal<'a>(builder: &mut StringBuilder<'a>, value: &str) {
  builder.append('"');
  for c in value.chars() {
    match c {
      '\\' => builder.append("\\\\"),
      '"' => builder.append("\\\""),
      '\n' => builder.append("\\n"),
      '\r' => builder.append("\\r"),
      '\t' => builder.append("\\t"),
      c => builder.append(c),
    }
  }
  builder.append('"');
}

fn key_to_name(key: &WorldKey) -> String {
  match key {
    WorldKey::Name(name) => name.clone(),
    WorldKey::Interface(id) => format!("interface-{}", id.index()),
  }
}

fn ts_type_name(name: &str) -> String {
  ts_ident_name(name, true)
}

fn ts_value_name(name: &str) -> String {
  ts_ident_name(name, false)
}

fn ts_ident_name(name: &str, pascal: bool) -> String {
  let mut result = String::with_capacity(name.len());
  let mut uppercase_next = pascal;
  for c in name.chars() {
    if c == '-' || c == '_' || c == ':' || c == '/' || c == '@' || c == '.' {
      uppercase_next = !result.is_empty();
      continue;
    }
    if result.is_empty() && c.is_ascii_digit() {
      result.push('_');
    }
    if uppercase_next {
      result.extend(c.to_uppercase());
      uppercase_next = false;
    } else {
      result.push(c);
    }
  }
  if result.is_empty() {
    "_".to_string()
  } else {
    result
  }
}

#[cfg(feature = "swc")]
fn is_valid_ident(export_name: &str) -> bool {
  !export_name.is_empty()
    && deno_ast::swc::ast::Ident::verify_symbol(export_name).is_ok()
}

#[cfg(not(feature = "swc"))]
fn is_valid_ident(_export_name: &str) -> bool {
  // Just assume everything is not valid if not using deno_ast.
  // This should not be a big deal because it just means that
  // this code will quote all the properties.
  false
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

  let is_valid_export_ident_per_export = wasm_deps
    .exports
    .iter()
    .map(|export| is_valid_ident(export.name))
    .collect::<Vec<_>>();
  let mut unique_import_modules: IndexMap<&str, Vec<&str>> =
    IndexMap::with_capacity(wasm_deps.imports.len());
  for import in &wasm_deps.imports {
    let entry = unique_import_modules.entry(import.module).or_default();
    entry.push(import.name);
  }
  StringBuilder::build(|builder| {
    let mut count = 0;
    for (import_module, named_imports) in &unique_import_modules {
      builder.append("import { ");
      // we add the named imports in order to cause a type checking error if
      // the importing module does not have it as an export
      for (i, named_import) in named_imports.iter().enumerate() {
        if i > 0 {
          builder.append(", ");
        }
        builder.append('"');
        builder.append(*named_import);
        builder.append("\" as __deno_wasm_import_");
        builder.append(count);
        builder.append("__");
        count += 1;
      }
      builder.append(" } from \"");
      builder.append(*import_module);
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
              match signature.returns.as_slice() {
                [] => builder.append("void"),
                [single] => builder
                  .append(value_type_to_ts_type(*single, TypePosition::Output)),
                multiple => {
                  builder.append('[');
                  for (i, ret) in multiple.iter().enumerate() {
                    if i > 0 {
                      builder.append(", ");
                    }
                    builder.append(value_type_to_ts_type(
                      *ret,
                      TypePosition::Output,
                    ));
                  }
                  builder.append(']');
                }
              }
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
  use wasm_dep_analyzer::Import;
  use wasm_dep_analyzer::WasmDeps;
  use wit_parser::Resolve;

  use super::*;

  #[test]
  fn test_output() {
    let text = wasm_module_deps_to_dts(&WasmDeps {
      imports: vec![
        Import {
          name: "name1",
          module: "./mod.ts",
          import_type: wasm_dep_analyzer::ImportType::Function(0),
        },
        Import {
          name: "name1",
          module: "./other.ts",
          import_type: wasm_dep_analyzer::ImportType::Function(0),
        },
        Import {
          name: "name2",
          module: "./mod.ts",
          import_type: wasm_dep_analyzer::ImportType::Function(0),
        },
      ],
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
        Export {
          name: "add_and_sub",
          index: 10,
          export_type: wasm_dep_analyzer::ExportType::Function(Ok(
            FunctionSignature {
              params: vec![ValueType::I32, ValueType::I32],
              returns: vec![ValueType::I32, ValueType::I32],
            },
          )),
        },
      ],
    });
    assert_eq!(
      text,
      "import { \"name1\" as __deno_wasm_import_0__, \"name2\" as __deno_wasm_import_1__ } from \"./mod.ts\";
import { \"name1\" as __deno_wasm_import_2__ } from \"./other.ts\";
declare function __deno_wasm_export_0__(): void;
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
export declare function add_and_sub(arg0: number, arg1: number): [number, number];
"
    );
  }

  #[test]
  fn test_component_output() {
    let mut resolve = Resolve::default();
    let package = resolve
      .push_str(
        "component.wit",
        r#"
          package deno:test;

          world component {
            record user {
              id: string,
              display-name: string,
            }

            variant lookup-result {
              found(user),
              missing,
            }

            export get-user: func(id: string) -> lookup-result;
            export encode: func(payload: list<u8>) -> list<u16>;
          }
        "#,
      )
      .unwrap();
    let world = resolve.select_world(&[package], Some("component")).unwrap();

    let text = wasm_component_to_dts(&resolve, world);
    assert_eq!(
      text,
      "export type LookupResult = { tag: \"found\"; val: User } | { tag: \"missing\" };
export interface User {
  id: string;
  displayName: string;
}
export declare function getUser(id: string): LookupResult;
export declare function encode(payload: Uint8Array): Uint16Array;
"
    );
  }
}
