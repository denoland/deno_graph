// Copyright 2018-2024 the Deno authors. MIT license.

use crate::analysis::find_deno_types;
use crate::analysis::find_jsx_import_source;
use crate::analysis::find_jsx_import_source_types;
use crate::analysis::find_path_reference;
use crate::analysis::find_resolution_mode;
use crate::analysis::find_ts_self_types;
use crate::analysis::find_ts_types;
use crate::analysis::find_types_reference;
use crate::analysis::is_comment_triple_slash_reference;
use crate::analysis::DependencyDescriptor;
use crate::analysis::DynamicArgument;
use crate::analysis::DynamicDependencyDescriptor;
use crate::analysis::DynamicTemplatePart;
use crate::analysis::JsDocImportInfo;
use crate::analysis::ModuleAnalyzer;
use crate::analysis::ModuleInfo;
use crate::analysis::SpecifierWithRange;
use crate::analysis::StaticDependencyDescriptor;
use crate::analysis::TypeScriptReference;
use crate::analysis::TypeScriptTypesResolutionMode;
use crate::graph::Position;
use crate::graph::PositionRange;
use crate::module_specifier::ModuleSpecifier;

use deno_ast::MultiThreadedComments;
use deno_ast::ProgramRef;
use deno_ast::SourcePos;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;

use deno_ast::swc::common::comments::CommentKind;
use deno_ast::MediaType;
use deno_ast::ParseDiagnostic;
use deno_ast::ParsedSource;
use deno_ast::SourceTextInfo;
use deno_error::JsErrorBox;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

use self::dep::analyze_program_dependencies;
use self::dep::DependencyComment;

mod dep;

pub struct ParseOptions<'a> {
  pub specifier: &'a ModuleSpecifier,
  pub source: Arc<str>,
  pub media_type: MediaType,
  pub scope_analysis: bool,
}

/// Parses programs to a ParsedSource.
pub trait EsParser {
  fn parse_program(
    &self,
    options: ParseOptions,
  ) -> Result<ParsedSource, ParseDiagnostic>;
}

#[derive(Default, Clone)]
pub struct DefaultEsParser;

impl EsParser for DefaultEsParser {
  fn parse_program(
    &self,
    options: ParseOptions,
  ) -> Result<ParsedSource, ParseDiagnostic> {
    deno_ast::parse_program(deno_ast::ParseParams {
      specifier: options.specifier.clone(),
      text: options.source,
      media_type: options.media_type,
      capture_tokens: options.scope_analysis,
      scope_analysis: options.scope_analysis,
      maybe_syntax: None,
    })
  }
}

/// Stores parsed sources.
///
/// Note: This interface is racy and not thread safe, as it's assumed
/// it will only store the latest changes or that the source text
/// will never change.
pub trait ParsedSourceStore {
  /// Sets the parsed source, potentially returning the previous value.
  fn set_parsed_source(
    &self,
    specifier: ModuleSpecifier,
    parsed_source: ParsedSource,
  ) -> Option<ParsedSource>;
  fn get_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource>;
  fn remove_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    // todo(dsherret): remove this default implementation once a breaking change is done to deno_graph
    self.get_parsed_source(specifier)
  }
  /// Gets a `deno_ast::ParsedSource` from the store, upgrading it
  /// to have scope analysis if it doesn't already.
  fn get_scope_analysis_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource>;
}

/// Default store that works on a single thread.
#[derive(Default)]
pub struct DefaultParsedSourceStore {
  store: RefCell<HashMap<ModuleSpecifier, ParsedSource>>,
}

impl ParsedSourceStore for DefaultParsedSourceStore {
  fn set_parsed_source(
    &self,
    specifier: ModuleSpecifier,
    parsed_source: ParsedSource,
  ) -> Option<ParsedSource> {
    self.store.borrow_mut().insert(specifier, parsed_source)
  }

  fn get_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.borrow().get(specifier).cloned()
  }

  fn remove_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.borrow_mut().remove(specifier)
  }

  fn get_scope_analysis_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    let mut store = self.store.borrow_mut();
    let parsed_source = store.get_mut(specifier)?;
    if parsed_source.has_scope_analysis() {
      Some(parsed_source.clone())
    } else {
      let parsed_source = store.remove(specifier).unwrap();
      let parsed_source = parsed_source.into_with_scope_analysis();
      store.insert(specifier.clone(), parsed_source.clone());
      Some(parsed_source.clone())
    }
  }
}

/// Stores parsed files in the provided store after parsing.
/// in a provided store. Parses that match the previous one
/// will be cached.
///
/// Note that this will insert into the store whatever was
/// last parsed, so if two threads race to parse, when they're
/// both done it will have whatever was last stored.
#[derive(Clone, Copy)]
pub struct CapturingEsParser<'a> {
  parser: Option<&'a dyn EsParser>,
  store: &'a dyn ParsedSourceStore,
}

impl<'a> CapturingEsParser<'a> {
  pub fn new(
    parser: Option<&'a dyn EsParser>,
    store: &'a dyn ParsedSourceStore,
  ) -> Self {
    Self { parser, store }
  }

  fn get_from_store_if_matches(
    &self,
    options: &ParseOptions,
  ) -> Option<ParsedSource> {
    let parsed_source = if options.scope_analysis {
      self
        .store
        .get_scope_analysis_parsed_source(options.specifier)?
    } else {
      self.store.get_parsed_source(options.specifier)?
    };
    if parsed_source.media_type() == options.media_type
      && parsed_source.text().as_ref() == options.source.as_ref()
    {
      Some(parsed_source)
    } else {
      None
    }
  }
}

impl EsParser for CapturingEsParser<'_> {
  fn parse_program(
    &self,
    options: ParseOptions,
  ) -> Result<ParsedSource, ParseDiagnostic> {
    if let Some(parsed_source) = self.get_from_store_if_matches(&options) {
      Ok(parsed_source)
    } else {
      let default_parser = DefaultEsParser;
      let parser = self.parser.unwrap_or(&default_parser);
      let specifier = options.specifier.clone();
      let parsed_source = parser.parse_program(options)?;
      self
        .store
        .set_parsed_source(specifier, parsed_source.clone());
      Ok(parsed_source)
    }
  }
}

#[derive(Default)]
pub struct DefaultModuleAnalyzer;

#[async_trait::async_trait(?Send)]
impl ModuleAnalyzer for DefaultModuleAnalyzer {
  async fn analyze(
    &self,
    specifier: &deno_ast::ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, JsErrorBox> {
    ParserModuleAnalyzer::default()
      .analyze(specifier, source, media_type)
      .await
  }
}

/// Default module analyzer that analyzes based on a deno_ast::ParsedSource.
pub struct ParserModuleAnalyzer<'a> {
  parser: &'a dyn EsParser,
}

impl<'a> ParserModuleAnalyzer<'a> {
  /// Creates a new module analyzer.
  pub fn new(parser: &'a dyn EsParser) -> Self {
    Self { parser }
  }

  /// Gets the module info from a parsed source.
  pub fn module_info(parsed_source: &ParsedSource) -> ModuleInfo {
    let program = parsed_source.program_ref();
    Self::module_info_from_swc(
      parsed_source.media_type(),
      program,
      parsed_source.text_info_lazy(),
      parsed_source.comments(),
    )
  }

  pub fn module_info_from_swc(
    media_type: MediaType,
    program: ProgramRef,
    text_info: &SourceTextInfo,
    comments: &MultiThreadedComments,
  ) -> ModuleInfo {
    let leading_comments = match program.body().next() {
      Some(item) => comments.get_leading(item.start()),
      None => match program.shebang() {
        Some(_) => comments.get_trailing(program.end()),
        None => comments.get_leading(program.start()),
      },
    };
    ModuleInfo {
      is_script: program.compute_is_script(),
      dependencies: analyze_dependencies(program, text_info, comments),
      ts_references: analyze_ts_references(text_info, leading_comments),
      self_types_specifier: analyze_ts_self_types(
        media_type,
        text_info,
        leading_comments,
      ),
      jsx_import_source: analyze_jsx_import_source(
        media_type,
        text_info,
        leading_comments,
      ),
      jsx_import_source_types: analyze_jsx_import_source_types(
        media_type,
        text_info,
        leading_comments,
      ),
      jsdoc_imports: analyze_jsdoc_imports(media_type, text_info, comments),
    }
  }

  pub fn analyze_sync(
    &self,
    specifier: &deno_ast::ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, ParseDiagnostic> {
    let parsed_source = self.parser.parse_program(ParseOptions {
      specifier,
      source,
      media_type,
      // scope analysis is not necessary for module parsing
      scope_analysis: false,
    })?;
    Ok(ParserModuleAnalyzer::module_info(&parsed_source))
  }
}

impl Default for ParserModuleAnalyzer<'_> {
  fn default() -> Self {
    Self {
      parser: &DefaultEsParser,
    }
  }
}

#[async_trait::async_trait(?Send)]
impl ModuleAnalyzer for ParserModuleAnalyzer<'_> {
  async fn analyze(
    &self,
    specifier: &deno_ast::ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, JsErrorBox> {
    self
      .analyze_sync(specifier, source, media_type)
      .map_err(JsErrorBox::from_err)
  }
}

/// Helper struct for creating a single object that implements
/// `deno_graph::ModuleAnalyzer`, `deno_graph::EsParser`,
/// and `deno_graph::ParsedSourceStore`. All parses will be captured
/// to prevent them from occuring more than one time.
pub struct CapturingModuleAnalyzer {
  parser: Box<dyn EsParser>,
  store: Box<dyn ParsedSourceStore>,
}

impl Default for CapturingModuleAnalyzer {
  fn default() -> Self {
    Self::new(None, None)
  }
}

impl CapturingModuleAnalyzer {
  pub fn new(
    parser: Option<Box<dyn EsParser>>,
    store: Option<Box<dyn ParsedSourceStore>>,
  ) -> Self {
    Self {
      parser: parser.unwrap_or_else(|| Box::<DefaultEsParser>::default()),
      store: store
        .unwrap_or_else(|| Box::<DefaultParsedSourceStore>::default()),
    }
  }

  pub fn as_capturing_parser(&self) -> CapturingEsParser {
    CapturingEsParser::new(Some(&*self.parser), &*self.store)
  }
}

#[async_trait::async_trait(?Send)]
impl ModuleAnalyzer for CapturingModuleAnalyzer {
  async fn analyze(
    &self,
    specifier: &deno_ast::ModuleSpecifier,
    source: Arc<str>,
    media_type: MediaType,
  ) -> Result<ModuleInfo, JsErrorBox> {
    let capturing_parser = self.as_capturing_parser();
    let module_analyzer = ParserModuleAnalyzer::new(&capturing_parser);
    module_analyzer.analyze(specifier, source, media_type).await
  }
}

impl EsParser for CapturingModuleAnalyzer {
  fn parse_program(
    &self,
    options: ParseOptions,
  ) -> Result<ParsedSource, ParseDiagnostic> {
    let capturing_parser = self.as_capturing_parser();
    capturing_parser.parse_program(options)
  }
}

impl ParsedSourceStore for CapturingModuleAnalyzer {
  fn set_parsed_source(
    &self,
    specifier: ModuleSpecifier,
    parsed_source: ParsedSource,
  ) -> Option<ParsedSource> {
    self.store.set_parsed_source(specifier, parsed_source)
  }

  fn get_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.get_parsed_source(specifier)
  }

  fn remove_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.remove_parsed_source(specifier)
  }

  fn get_scope_analysis_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<ParsedSource> {
    self.store.get_scope_analysis_parsed_source(specifier)
  }
}

fn analyze_dependencies(
  program: deno_ast::ProgramRef,
  text_info: &SourceTextInfo,
  comments: &MultiThreadedComments,
) -> Vec<DependencyDescriptor> {
  let deps = analyze_program_dependencies(program, comments);

  deps
    .into_iter()
    .map(|d| match d {
      self::dep::DependencyDescriptor::Static(d) => {
        DependencyDescriptor::Static(StaticDependencyDescriptor {
          kind: d.kind,
          types_specifier: analyze_ts_or_deno_types(
            text_info,
            &d.leading_comments,
          ),
          specifier: d.specifier.to_string(),
          specifier_range: PositionRange::from_source_range(
            d.specifier_range,
            text_info,
          ),
          import_attributes: d.import_attributes,
        })
      }
      self::dep::DependencyDescriptor::Dynamic(d) => {
        DependencyDescriptor::Dynamic(DynamicDependencyDescriptor {
          kind: d.kind,
          types_specifier: analyze_ts_or_deno_types(
            text_info,
            &d.leading_comments,
          ),
          argument: match d.argument {
            self::dep::DynamicArgument::String(text) => {
              DynamicArgument::String(text.to_string())
            }
            self::dep::DynamicArgument::Template(parts) => {
              DynamicArgument::Template(
                parts
                  .into_iter()
                  .map(|part| match part {
                    self::dep::DynamicTemplatePart::String(text) => {
                      DynamicTemplatePart::String {
                        value: text.to_string(),
                      }
                    }
                    self::dep::DynamicTemplatePart::Expr => {
                      DynamicTemplatePart::Expr
                    }
                  })
                  .collect(),
              )
            }
            self::dep::DynamicArgument::Expr => DynamicArgument::Expr,
          },
          argument_range: PositionRange::from_source_range(
            d.argument_range,
            text_info,
          ),
          import_attributes: d.import_attributes,
        })
      }
    })
    .collect()
}

fn analyze_ts_references(
  text_info: &SourceTextInfo,
  leading_comments: Option<&Vec<deno_ast::swc::common::comments::Comment>>,
) -> Vec<TypeScriptReference> {
  let mut references = Vec::new();
  if let Some(c) = leading_comments {
    for comment in c {
      if comment.kind == CommentKind::Line
        && is_comment_triple_slash_reference(&comment.text)
      {
        let comment_start = comment.start();
        if let Some(m) = find_path_reference(&comment.text) {
          references.push(TypeScriptReference::Path(SpecifierWithRange {
            text: m.as_str().to_string(),
            range: comment_source_to_position_range(
              comment_start,
              m.range(),
              text_info,
              false,
            ),
          }));
        } else if let Some(m) = find_types_reference(&comment.text) {
          let resolution_mode = find_resolution_mode(&comment.text)
            .and_then(|m| TypeScriptTypesResolutionMode::from_str(m.as_str()));
          references.push(TypeScriptReference::Types {
            specifier: SpecifierWithRange {
              text: m.as_str().to_string(),
              range: comment_source_to_position_range(
                comment_start,
                m.range(),
                text_info,
                false,
              ),
            },
            resolution_mode,
          });
        }
      }
    }
  }
  references
}

fn analyze_jsx_import_source(
  media_type: MediaType,
  text_info: &SourceTextInfo,
  leading_comments: Option<&Vec<deno_ast::swc::common::comments::Comment>>,
) -> Option<SpecifierWithRange> {
  if !matches!(media_type, MediaType::Jsx | MediaType::Tsx) {
    return None;
  }

  leading_comments.and_then(|c| {
    c.iter().find_map(|c| {
      if c.kind != CommentKind::Block {
        return None; // invalid
      }
      let m = find_jsx_import_source(&c.text)?;
      Some(SpecifierWithRange {
        text: m.as_str().to_string(),
        range: comment_source_to_position_range(
          c.start(),
          m.range(),
          text_info,
          true,
        ),
      })
    })
  })
}

fn analyze_jsx_import_source_types(
  media_type: MediaType,
  text_info: &SourceTextInfo,
  leading_comments: Option<&Vec<deno_ast::swc::common::comments::Comment>>,
) -> Option<SpecifierWithRange> {
  if !matches!(media_type, MediaType::Jsx | MediaType::Tsx) {
    return None;
  }

  leading_comments.and_then(|c| {
    c.iter().find_map(|c| {
      if c.kind != CommentKind::Block {
        return None; // invalid
      }
      let m = find_jsx_import_source_types(&c.text)?;
      Some(SpecifierWithRange {
        text: m.as_str().to_string(),
        range: comment_source_to_position_range(
          c.start(),
          m.range(),
          text_info,
          true,
        ),
      })
    })
  })
}

fn analyze_ts_self_types(
  media_type: MediaType,
  text_info: &SourceTextInfo,
  leading_comments: Option<&Vec<deno_ast::swc::common::comments::Comment>>,
) -> Option<SpecifierWithRange> {
  if media_type.is_typed() {
    return None;
  }

  leading_comments.and_then(|c| {
    c.iter().find_map(|c| {
      let m = find_ts_self_types(&c.text)?;
      Some(SpecifierWithRange {
        text: m.as_str().to_string(),
        range: comment_source_to_position_range(
          c.start(),
          m.range(),
          text_info,
          false,
        ),
      })
    })
  })
}

/// Searches comments for any `@ts-types` or `@deno-types` compiler hints.
pub fn analyze_ts_or_deno_types(
  text_info: &SourceTextInfo,
  leading_comments: &[DependencyComment],
) -> Option<SpecifierWithRange> {
  let comment = leading_comments.last()?;

  if let Some(m) = find_ts_types(&comment.text) {
    return Some(SpecifierWithRange {
      text: m.as_str().to_string(),
      range: comment_source_to_position_range(
        comment.range.start(),
        m.range(),
        text_info,
        false,
      ),
    });
  }
  let deno_types = find_deno_types(&comment.text)?;
  Some(SpecifierWithRange {
    text: deno_types.text.to_string(),
    range: comment_source_to_position_range(
      comment.range.start(),
      deno_types.range,
      text_info,
      deno_types.is_quoteless,
    ),
  })
}

fn analyze_jsdoc_imports(
  media_type: MediaType,
  text_info: &SourceTextInfo,
  comments: &MultiThreadedComments,
) -> Vec<JsDocImportInfo> {
  // Analyze any JSDoc type imports
  // We only analyze these on JavaScript types of modules, since they are
  // ignored by TypeScript when type checking anyway and really shouldn't be
  // there, but some people do strange things.
  if !matches!(
    media_type,
    MediaType::JavaScript | MediaType::Jsx | MediaType::Mjs | MediaType::Cjs
  ) {
    return Vec::new();
  }

  let mut deps = Vec::new();
  for comment in comments.iter_unstable() {
    if comment.kind != CommentKind::Block || !comment.text.starts_with('*') {
      continue;
    }

    let js_docs = comment
      .text
      .match_indices("{")
      .filter_map(|(i, _)| {
        parse_jsdoc_dynamic_import(&comment.text[i..])
          .ok()
          .map(|(_input, jsdoc)| (i, jsdoc))
      })
      .chain(comment.text.match_indices("@import").filter_map(|(i, _)| {
        parse_jsdoc_import_decl(&comment.text[i..])
          .ok()
          .map(|(_input, jsdoc)| (i, jsdoc))
      }));
    for (byte_index, js_doc) in js_docs {
      deps.push(JsDocImportInfo {
        specifier: SpecifierWithRange {
          text: js_doc.specifier,
          range: comment_source_to_position_range(
            comment.range().start,
            byte_index + js_doc.specifier_range.start
              ..byte_index + js_doc.specifier_range.end,
            text_info,
            false,
          ),
        },
        resolution_mode: js_doc.resolution_mode,
      });
    }
  }
  deps.sort_by(|a, b| a.specifier.range.start.cmp(&b.specifier.range.start));
  deps
}

#[derive(Debug, Clone)]
struct JsDocImport {
  specifier: String,
  specifier_range: std::ops::Range<usize>,
  resolution_mode: Option<TypeScriptTypesResolutionMode>,
}

fn parse_jsdoc_import_decl(input: &str) -> monch::ParseResult<JsDocImport> {
  use monch::*;

  fn skip_named_imports(input: &str) -> monch::ParseResult<()> {
    // { ... }
    let (input, _) = ch('{')(input)?;
    let (input, _) = monch::take_while(|c| c != '}')(input)?;
    let (input, _) = ch('}')(input)?;
    Ok((input, ()))
  }

  fn skip_namespace_import(input: &str) -> monch::ParseResult<()> {
    // * as ns
    let (input, _) = ch('*')(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = tag("as")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = parse_ident(input)?;
    Ok((input, ()))
  }

  fn parse_attributes(
    input: &str,
  ) -> ParseResult<Option<TypeScriptTypesResolutionMode>> {
    let (input, _) = tag("with")(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, maybe_resolution_mode) =
      parse_import_attribute_block_for_resolution_mode(input)?;
    Ok((input, maybe_resolution_mode))
  }

  let initial_input = input;
  let (input, _) = tag("@import")(input)?;
  let (input, _) = whitespace(input)?;
  let (input, _) = or3(
    skip_named_imports,
    terminated(skip_namespace_import, whitespace),
    terminated(map(parse_ident, |_| ()), whitespace),
  )(input)?;
  let (input, _) = skip_whitespace(input)?;
  let (input, _) = tag("from")(input)?;
  let (input, _) = skip_whitespace(input)?;
  let start_specifier_input = input;
  let (input, specifier) = parse_quote(input)?;
  let end_specifier_input = input;
  let (input, _) = skip_whitespace(input)?;
  let (input, maybe_resolution_mode) = maybe(parse_attributes)(input)?;

  Ok((
    input,
    JsDocImport {
      specifier: specifier.to_string(),
      specifier_range: initial_input.len() - start_specifier_input.len() + 1
        ..initial_input.len() - end_specifier_input.len() - 1,
      resolution_mode: maybe_resolution_mode.flatten(),
    },
  ))
}

/// Matches a JSDoc import type reference (`{import("./example.js")}`
fn parse_jsdoc_dynamic_import(input: &str) -> monch::ParseResult<JsDocImport> {
  fn parse_second_param_obj_with_leading_comma(
    input: &str,
  ) -> monch::ParseResult<Option<TypeScriptTypesResolutionMode>> {
    let (input, _) = ch(',')(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = ch('{')(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = tag("with")(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = ch(':')(input)?;
    let (input, _) = skip_whitespace(input)?;

    let (input, maybe_resolution_mode) =
      parse_import_attribute_block_for_resolution_mode(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = ch('}')(input)?;

    Ok((input, maybe_resolution_mode))
  }

  // \{[^}]*import\(['"]([^'"]+)['"]\)[^}]*}"
  use monch::*;
  let original_input = input;
  let (mut input, _) = ch('{')(input)?;
  {
    let original_input = input;
    for (index, c) in input.char_indices() {
      if c == '}' {
        return ParseError::backtrace();
      }
      input = &original_input[index..];
      if input.starts_with("import") {
        break;
      }
    }
  }
  let (input, _) = tag("import")(input)?;
  let (input, _) = skip_whitespace(input)?;
  let (input, _) = ch('(')(input)?;
  let (input, _) = skip_whitespace(input)?;
  let start_specifier_input = input;
  let (input, specifier) = parse_quote(input)?;
  let end_specifier_input = input;
  let (input, _) = skip_whitespace(input)?;
  let (input, maybe_resolution_mode) =
    maybe(parse_second_param_obj_with_leading_comma)(input)?;
  let (input, _) = skip_whitespace(input)?;
  let (input, _) = ch(')')(input)?;
  let (input, _) = take_while(|c| c != '}')(input)?;
  let (input, _) = ch('}')(input)?;

  Ok((
    input,
    JsDocImport {
      specifier: specifier.to_string(),
      specifier_range: original_input.len() - start_specifier_input.len() + 1
        ..original_input.len() - end_specifier_input.len() - 1,
      resolution_mode: maybe_resolution_mode.flatten(),
    },
  ))
}

fn parse_import_attribute_block_for_resolution_mode(
  input: &str,
) -> monch::ParseResult<Option<TypeScriptTypesResolutionMode>> {
  use monch::*;
  map(parse_import_attribute_block, |attributes| {
    attributes
      .iter()
      .find(|(key, _)| *key == "resolution-mode")
      .and_then(|(_, value)| TypeScriptTypesResolutionMode::from_str(value))
  })(input)
}

fn parse_import_attribute_block(
  input: &str,
) -> monch::ParseResult<Vec<(&str, &str)>> {
  use monch::*;
  fn parse_attribute(input: &str) -> ParseResult<(&str, &str)> {
    let (input, key) = or(parse_quote, parse_ident)(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = ch(':')(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, value) = parse_quote(input)?;
    Ok((input, (key, value)))
  }

  let (input, _) = ch('{')(input)?;
  let (input, _) = skip_whitespace(input)?;

  let (input, attributes) = separated_list(
    parse_attribute,
    delimited(skip_whitespace, ch(','), skip_whitespace),
  )(input)?;
  let (input, _) = skip_whitespace(input)?;
  let (input, _) = ch('}')(input)?;
  Ok((input, attributes))
}

fn parse_ident(input: &str) -> monch::ParseResult<&str> {
  use monch::*;
  let start_input = input;
  let (input, c) = next_char(input)?;
  if !c.is_alphabetic() {
    return Err(monch::ParseError::Backtrace);
  }
  // good enough for now
  let (input, _) =
    take_while(|c| !c.is_whitespace() && c != ':' && c != '-')(input)?;
  Ok((input, &start_input[..start_input.len() - input.len()]))
}

fn parse_quote(input: &str) -> monch::ParseResult<&str> {
  use monch::*;
  let (input, open_char) = or(ch('"'), ch('\''))(input)?;
  let (input, text) = take_while(|c| c != open_char)(input)?;
  let (input, _) = ch(open_char)(input)?;
  Ok((input, text))
}

fn comment_source_to_position_range(
  comment_start: SourcePos,
  inner_range: std::ops::Range<usize>,
  text_info: &SourceTextInfo,
  is_specifier_quoteless: bool,
) -> PositionRange {
  // the comment text starts after the double slash or slash star, so add 2
  let comment_start = comment_start + 2;
  // -1 and +1 to include the quotes, but not for pragmas that don't have quotes
  let padding = if is_specifier_quoteless { 0 } else { 1 };
  PositionRange {
    start: Position::from_source_pos(
      comment_start + inner_range.start - padding,
      text_info,
    ),
    end: Position::from_source_pos(
      comment_start + inner_range.end + padding,
      text_info,
    ),
  }
}

#[cfg(test)]
mod tests {
  use crate::analysis::JsDocImportInfo;

  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn test_parse() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.tsx").expect("bad specifier");
    let source = r#"
    /// <reference path="./ref.d.ts" />
    /// <reference types="./types.d.ts" />
    // @jsxImportSource http://example.com/invalid
    /* @jsxImportSource http://example.com/preact */
    // @jsxImportSourceTypes http://example.com/invalidTypes
    /* @jsxImportSourceTypes http://example.com/preactTypes */
    import {
      A,
      B,
      C,
      D,
    } from "https://deno.land/x/example@v1.0.0/mod.ts";

    export * from "./mod.ts";

    import type { Component } from "https://esm.sh/preact";
    import { h, Fragment } from "https://esm.sh/preact";

    // other
    // @deno-types="https://deno.land/x/types/react/index.d.ts"
    import React from "https://cdn.skypack.dev/react";

    // @deno-types=https://deno.land/x/types/react/index.d.ts
    import React2 from "https://cdn.skypack.dev/react";

    // @deno-types="https://deno.land/x/types/react/index.d.ts"
    // other comment first
    import React3 from "https://cdn.skypack.dev/react";

    const a = await import("./a.ts");

    const React4 = await /* @deno-types="https://deno.land/x/types/react/index.d.ts" */ import("https://cdn.skypack.dev/react");
    "#;
    let parsed_source = DefaultEsParser
      .parse_program(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::Tsx,
        scope_analysis: false,
      })
      .unwrap();
    let text_info = parsed_source.text_info_lazy();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    let dependencies = module_info.dependencies;
    assert_eq!(dependencies.len(), 9);

    let ts_references = module_info.ts_references;
    assert_eq!(ts_references.len(), 2);
    match &ts_references[0] {
      TypeScriptReference::Path(specifier) => {
        assert_eq!(specifier.text, "./ref.d.ts");
        assert_eq!(
          text_info.range_text(&specifier.range.as_source_range(text_info)),
          r#""./ref.d.ts""#
        );
      }
      TypeScriptReference::Types { .. } => panic!("expected path"),
    }
    match &ts_references[1] {
      TypeScriptReference::Path(_) => panic!("expected types"),
      TypeScriptReference::Types {
        specifier,
        resolution_mode: mode,
      } => {
        assert_eq!(*mode, None);
        assert_eq!(specifier.text, "./types.d.ts");
        assert_eq!(
          text_info.range_text(&specifier.range.as_source_range(text_info)),
          r#""./types.d.ts""#
        );
      }
    }

    let dep_deno_types = &dependencies[4]
      .as_static()
      .unwrap()
      .types_specifier
      .as_ref()
      .unwrap();
    assert_eq!(
      dep_deno_types.text,
      "https://deno.land/x/types/react/index.d.ts"
    );
    assert_eq!(
      text_info.range_text(&dep_deno_types.range.as_source_range(text_info)),
      r#""https://deno.land/x/types/react/index.d.ts""#
    );

    let dep_deno_types = &dependencies[5]
      .as_static()
      .unwrap()
      .types_specifier
      .as_ref()
      .unwrap();
    assert_eq!(
      dep_deno_types.text,
      "https://deno.land/x/types/react/index.d.ts"
    );
    assert_eq!(
      text_info.range_text(&dep_deno_types.range.as_source_range(text_info)),
      r#"https://deno.land/x/types/react/index.d.ts"#
    );

    assert!(dependencies[6]
      .as_static()
      .unwrap()
      .types_specifier
      .is_none());

    let dep_deno_types = &dependencies[8]
      .as_dynamic()
      .unwrap()
      .types_specifier
      .as_ref()
      .unwrap();
    assert_eq!(
      dep_deno_types.text,
      "https://deno.land/x/types/react/index.d.ts"
    );
    assert_eq!(
      text_info.range_text(&dep_deno_types.range.as_source_range(text_info)),
      r#""https://deno.land/x/types/react/index.d.ts""#
    );

    let jsx_import_source = module_info.jsx_import_source.unwrap();
    assert_eq!(jsx_import_source.text, "http://example.com/preact");
    assert_eq!(
      text_info.range_text(&jsx_import_source.range.as_source_range(text_info)),
      "http://example.com/preact"
    );

    let jsx_import_source_types = module_info.jsx_import_source_types.unwrap();
    assert_eq!(
      jsx_import_source_types.text,
      "http://example.com/preactTypes"
    );
    assert_eq!(
      text_info
        .range_text(&jsx_import_source_types.range.as_source_range(text_info)),
      "http://example.com/preactTypes"
    );

    assert!(module_info.self_types_specifier.is_none());
  }

  #[test]
  fn test_parse_resolution_mode() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.mts").expect("bad specifier");
    let source = r#"
    /// <reference types="./types.d.ts" resolution-mode="require" />
    /// <reference types="node" resolution-mode="import" />
    /// <reference types="other" resolution-mode="asdf" />
    "#;
    let parsed_source = DefaultEsParser
      .parse_program(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::Mts,
        scope_analysis: false,
      })
      .unwrap();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    let ts_references = module_info.ts_references;
    assert_eq!(ts_references.len(), 3);
    match &ts_references[0] {
      TypeScriptReference::Path(_) => unreachable!(),
      TypeScriptReference::Types {
        specifier,
        resolution_mode: mode,
      } => {
        assert_eq!(*mode, Some(TypeScriptTypesResolutionMode::Require));
        assert_eq!(specifier.text, "./types.d.ts");
      }
    }
    match &ts_references[1] {
      TypeScriptReference::Path(_) => unreachable!(),
      TypeScriptReference::Types {
        specifier,
        resolution_mode: mode,
      } => {
        assert_eq!(*mode, Some(TypeScriptTypesResolutionMode::Import));
        assert_eq!(specifier.text, "node");
      }
    }
    match &ts_references[2] {
      TypeScriptReference::Path(_) => unreachable!(),
      TypeScriptReference::Types {
        specifier,
        resolution_mode: mode,
      } => {
        assert_eq!(*mode, None);
        assert_eq!(specifier.text, "other");
      }
    }
  }

  #[test]
  fn test_analyze_dependencies() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad specifier");
    let source = r#"
    import * as a from "./a.ts";
    import "./b.ts";
    import { c } from "./c.ts";
    import d from "./d.ts";
    import e, { ee } from "./e.ts";
    const f = await import("./f.ts");
    export * from "./g.ts";
    export { h } from "./h.ts";

    import type { i } from "./i.d.ts";
    export type { j } from "./j.d.ts";
    "#;
    let parsed_source = DefaultEsParser
      .parse_program(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::TypeScript,
        scope_analysis: false,
      })
      .unwrap();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    let text_info = parsed_source.text_info_lazy();
    let dependencies = module_info.dependencies;
    assert_eq!(dependencies.len(), 10);
    let dep = dependencies[0].as_static().unwrap();
    assert_eq!(dep.specifier.to_string(), "./a.ts");
    assert_eq!(
      text_info.range_text(&dep.specifier_range.as_source_range(text_info)),
      "\"./a.ts\""
    );
    let dep = dependencies[1].as_static().unwrap();
    assert_eq!(dep.specifier.to_string(), "./b.ts");
    assert_eq!(
      text_info.range_text(&dep.specifier_range.as_source_range(text_info)),
      "\"./b.ts\""
    );
  }

  #[test]
  fn test_analyze_self_types() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.js").expect("bad specifier");
    let source = r#"
      // @ts-self-types="./self.d.ts"

      import * as a from "./a.ts";
    "#;
    let parsed_source = DefaultEsParser
      .parse_program(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::JavaScript,
        scope_analysis: false,
      })
      .unwrap();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    let text_info = parsed_source.text_info_lazy();
    let dependencies = module_info.dependencies;
    assert_eq!(dependencies.len(), 1);
    let dep = dependencies[0].as_static().unwrap();
    assert_eq!(dep.specifier.to_string(), "./a.ts");
    assert_eq!(
      text_info.range_text(&dep.specifier_range.as_source_range(text_info)),
      "\"./a.ts\""
    );

    let self_types_specifier = module_info.self_types_specifier.unwrap();
    assert_eq!(self_types_specifier.text, "./self.d.ts");
    assert_eq!(
      text_info
        .range_text(&self_types_specifier.range.as_source_range(text_info)),
      "\"./self.d.ts\""
    );
  }

  #[test]
  fn test_analyze_dependencies_import_attributes() {
    let specifier =
      ModuleSpecifier::parse("file:///a/test.ts").expect("bad specifier");
    for keyword in ["assert", "with"] {
      let source = format!(
        "
      import a from \"./a.json\" {keyword} {{ type: \"json\" }};
      await import(\"./b.json\", {{ {keyword}: {{ type: \"json\" }} }});
      "
      );
      let parsed_source = DefaultEsParser
        .parse_program(ParseOptions {
          specifier: &specifier,
          source: source.into(),
          media_type: MediaType::TypeScript,
          scope_analysis: false,
        })
        .unwrap();
      let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
      let dependencies = module_info.dependencies;
      assert_eq!(dependencies.len(), 2);
      let dep = dependencies[0].as_static().unwrap();
      assert_eq!(dep.specifier.to_string(), "./a.json");
      assert_eq!(dep.import_attributes.get("type"), Some("json"));
      let dep = dependencies[1].as_dynamic().unwrap();
      assert_eq!(
        dep.argument,
        DynamicArgument::String("./b.json".to_string())
      );
      assert_eq!(dep.import_attributes.get("type"), Some("json"));
    }
  }

  #[test]
  fn test_analyze_jsdoc_imports() {
    let specifier = ModuleSpecifier::parse("file:///a/test.js").unwrap();
    let source = r#"
/** @module */

/**
 * Some stuff here
 *
 * @type {import("./a.js").A}
 */
const a = "a";

/**
 * Some other stuff here
 *
 * @param {import('./b.js').C}
 * @returns {import("./d.js")}
 */
function b(c) {
  return;
}

/**
 * @type {Set<import("./e.js", { with: { "resolution-mode": "require" } }).F>}
 */
const f = new Set();

/** @import { SomeType } from "./a.ts" */
/** @import * as namespace from "./b.ts" */
/** @import defaultImport from './c.ts' with { "resolution-mode": "require" } */
"#;
    let parsed_source = DefaultEsParser
      .parse_program(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::JavaScript,
        scope_analysis: false,
      })
      .unwrap();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    assert!(module_info.is_script);
    let dependencies = module_info.jsdoc_imports;
    assert_eq!(
      dependencies,
      [
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "./a.js".to_string(),
            range: PositionRange {
              start: Position {
                line: 6,
                character: 17
              },
              end: Position {
                line: 6,
                character: 25
              }
            }
          },
          resolution_mode: None,
        },
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "./b.js".to_string(),
            range: PositionRange {
              start: Position {
                line: 13,
                character: 18
              },
              end: Position {
                line: 13,
                character: 26
              }
            }
          },
          resolution_mode: None,
        },
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "./d.js".to_string(),
            range: PositionRange {
              start: Position {
                line: 14,
                character: 20
              },
              end: Position {
                line: 14,
                character: 28
              }
            }
          },
          resolution_mode: None,
        },
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "./e.js".to_string(),
            range: PositionRange {
              start: Position {
                line: 21,
                character: 21
              },
              end: Position {
                line: 21,
                character: 29
              }
            }
          },
          resolution_mode: Some(TypeScriptTypesResolutionMode::Require),
        },
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "./a.ts".to_string(),
            range: PositionRange {
              start: Position {
                line: 25,
                character: 30,
              },
              end: Position {
                line: 25,
                character: 38,
              },
            },
          },
          resolution_mode: None,
        },
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "./b.ts".to_string(),
            range: PositionRange {
              start: Position {
                line: 26,
                character: 32,
              },
              end: Position {
                line: 26,
                character: 40,
              },
            },
          },
          resolution_mode: None,
        },
        JsDocImportInfo {
          specifier: SpecifierWithRange {
            text: "./c.ts".to_string(),
            range: PositionRange {
              start: Position {
                line: 27,
                character: 31,
              },
              end: Position {
                line: 27,
                character: 39,
              },
            },
          },
          resolution_mode: Some(TypeScriptTypesResolutionMode::Require),
        }
      ]
    );
  }

  #[test]
  fn test_import_equals() {
    let specifier = ModuleSpecifier::parse("file:///a/test.ts").unwrap();
    let source = r#"
export import value = require("./a.js");
import value2 = require("./b.js");
"#;
    let parsed_source = DefaultEsParser
      .parse_program(ParseOptions {
        specifier: &specifier,
        source: source.into(),
        media_type: MediaType::TypeScript,
        scope_analysis: false,
      })
      .unwrap();
    let module_info = ParserModuleAnalyzer::module_info(&parsed_source);
    assert!(module_info.is_script);
    let dependencies = module_info.dependencies;
    assert_eq!(dependencies.len(), 2);
  }

  #[tokio::test]
  async fn test_analyze_ts_references_and_jsx_import_source_with_shebang() {
    let specifier = ModuleSpecifier::parse("file:///a/test.tsx").unwrap();
    let source = r#"#!/usr/bin/env -S deno run
/// <reference path="./ref.d.ts" />
/* @jsxImportSource preact */
export {};
"#;
    let module_info = DefaultModuleAnalyzer
      .analyze(&specifier, source.into(), MediaType::Tsx)
      .await
      .unwrap();
    assert_eq!(
      module_info,
      ModuleInfo {
        is_script: false,
        dependencies: vec![],
        ts_references: vec![TypeScriptReference::Path(SpecifierWithRange {
          text: "./ref.d.ts".to_owned(),
          range: PositionRange {
            start: Position {
              line: 1,
              character: 20,
            },
            end: Position {
              line: 1,
              character: 32,
            },
          },
        })],
        self_types_specifier: None,
        jsx_import_source: Some(SpecifierWithRange {
          text: "preact".to_owned(),
          range: PositionRange {
            start: Position {
              line: 2,
              character: 20,
            },
            end: Position {
              line: 2,
              character: 26,
            },
          },
        }),
        jsx_import_source_types: None,
        jsdoc_imports: vec![],
      },
    );
  }

  #[test]
  fn test_parse_jsdoc_import_decl() {
    fn parse_resolution_mode(
      text: &str,
    ) -> Option<TypeScriptTypesResolutionMode> {
      parse_jsdoc_import_decl(text)
        .ok()
        .and_then(|v| v.1.resolution_mode)
    }

    // named imports
    assert!(
      parse_jsdoc_import_decl("@import { SomeType } from \"./a.ts\"").is_ok()
    );
    // quotes in named imports
    assert!(parse_jsdoc_import_decl(
      "@import { SomeType, \"test\" as test, 'b' as test2 } from \"./a.ts\""
    )
    .is_ok());
    // single quotes and namespace import
    assert!(parse_jsdoc_import_decl("@import * as test from './a.ts'").is_ok());
    // missing space certain tokens
    assert!(parse_jsdoc_import_decl("@import *as test from'./a.ts'").is_ok());
    // default import
    assert!(parse_jsdoc_import_decl("@import test from './a.ts'").is_ok());
    // mixing quotes (invalid)
    assert!(parse_jsdoc_import_decl("@import test from \"./a.ts'").is_err());
    assert!(parse_jsdoc_import_decl("@import test from './a.ts\"").is_err());
    assert_eq!(
      parse_resolution_mode("@import { SomeType } from \"./a.ts\" with { 'resolution-mode': 'import' }"),
      Some(TypeScriptTypesResolutionMode::Import)
    );
    assert_eq!(
      parse_resolution_mode(
        "@import v from 'test' with { 'resolution-mode': \"require\" }"
      ),
      Some(TypeScriptTypesResolutionMode::Require)
    );
    assert_eq!(
      parse_resolution_mode("@import v from 'test' with { type: 'other', 'resolution-mode': \"require\" }"),
      Some(TypeScriptTypesResolutionMode::Require)
    );
  }

  #[test]
  fn test_parse_jsdoc_dynamic_import() {
    fn parse_resolution_mode(
      text: &str,
    ) -> Option<TypeScriptTypesResolutionMode> {
      parse_jsdoc_dynamic_import(text)
        .ok()
        .and_then(|v| v.1.resolution_mode)
    }

    assert!(parse_jsdoc_dynamic_import("{ import('testing') }").is_ok());
    assert!(parse_jsdoc_dynamic_import("{ Test<import('testing')> }").is_ok());
    assert!(parse_jsdoc_dynamic_import("{ * // … test }").is_err());
    assert_eq!(
      parse_resolution_mode(
        r#"{Set<import("./e.js", { with: { "resolution-mode": "require" } }).F>}"#
      ),
      Some(TypeScriptTypesResolutionMode::Require)
    );
    assert_eq!(
      parse_resolution_mode(
        r#"{import("a", { with: { type: "test", "resolution-mode": "import" } })}"#
      ),
      Some(TypeScriptTypesResolutionMode::Import)
    );
  }
}
