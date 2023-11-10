// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

pub use self::analyzer::EsmModuleInfo;
pub use self::analyzer::ExportDeclRef;
pub use self::analyzer::FileDep;
pub use self::analyzer::FileDepName;
pub use self::analyzer::JsonModuleInfo;
pub use self::analyzer::ModuleId;
pub use self::analyzer::ModuleInfo;
pub use self::analyzer::ModuleInfoRef;
pub use self::analyzer::RootSymbol;
pub use self::analyzer::Symbol;
pub use self::analyzer::SymbolDecl;
pub use self::analyzer::SymbolDeclKind;
pub use self::analyzer::SymbolFillDiagnostic;
pub use self::analyzer::SymbolFillDiagnosticKind;
pub use self::analyzer::SymbolId;
pub use self::analyzer::SymbolNodeRef;
pub use self::analyzer::UniqueSymbolId;
pub use self::cross_module::Definition;
pub use self::cross_module::DefinitionKind;
pub use self::cross_module::DefinitionOrUnresolved;
pub use self::cross_module::DefinitionPath;
pub use self::cross_module::ExportsAndReExports;
pub use self::cross_module::ResolvedSymbolDepEntry;
pub use self::dep_analyzer::SymbolNodeDep;

mod analyzer;
mod collections;
mod cross_module;
mod dep_analyzer;
mod swc_helpers;
