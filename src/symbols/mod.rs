// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

pub use self::analyzer::EsmModuleSymbol;
pub use self::analyzer::ExportDeclRef;
pub use self::analyzer::FileDep;
pub use self::analyzer::FileDepName;
pub use self::analyzer::JsonModuleSymbol;
pub use self::analyzer::ModuleId;
pub use self::analyzer::ModuleSymbol;
pub use self::analyzer::ModuleSymbolRef;
pub use self::analyzer::RootSymbol;
pub use self::analyzer::Symbol;
pub use self::analyzer::SymbolDecl;
pub use self::analyzer::SymbolFillDiagnostic;
pub use self::analyzer::SymbolFillDiagnosticKind;
pub use self::analyzer::SymbolId;
pub use self::analyzer::SymbolNodeRef;
pub use self::analyzer::UniqueSymbolId;
pub use self::cross_module::Definition;
pub use self::cross_module::DefinitionKind;
pub use self::cross_module::DefinitionPath;
pub use self::cross_module::ResolvedSymbolDepEntry;

mod analyzer;
mod collections;
mod cross_module;
