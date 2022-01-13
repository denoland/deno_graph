// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::colors;
use crate::graph::Dependency;
use crate::graph::EsModule;
use crate::graph::Module;
use crate::graph::ModuleGraph;
use crate::graph::ModuleGraphError;
use crate::graph::ModuleSlot;
use crate::graph::Resolved;
use crate::graph::SyntheticModule;
use crate::module_specifier::ModuleSpecifier;

use std::collections::HashSet;
use std::fmt;

const SIBLING_CONNECTOR: char = '├';
const LAST_SIBLING_CONNECTOR: char = '└';
const CHILD_DEPS_CONNECTOR: char = '┬';
const CHILD_NO_DEPS_CONNECTOR: char = '─';
const VERTICAL_CONNECTOR: char = '│';
const EMPTY_CONNECTOR: char = ' ';

/// A function that converts a float to a string the represents a human
/// readable version of that number.
fn human_size(size: f64) -> String {
  let negative = if size.is_sign_positive() { "" } else { "-" };
  let size = size.abs();
  let units = ["B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"];
  if size < 1_f64 {
    return format!("{}{}{}", negative, size, "B");
  }
  let delimiter = 1024_f64;
  let exponent = std::cmp::min(
    (size.ln() / delimiter.ln()).floor() as i32,
    (units.len() - 1) as i32,
  );
  let pretty_bytes = format!("{:.2}", size / delimiter.powi(exponent))
    .parse::<f64>()
    .unwrap()
    * 1_f64;
  let unit = units[exponent as usize];
  format!("{}{}{}", negative, pretty_bytes, unit)
}

impl fmt::Display for ModuleGraph {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.roots.is_empty() || self.roots.len() > 1 {
      return writeln!(
        f,
        "{} displaying graphs that have multiple roots is not supported.",
        colors::red("error:")
      );
    }
    let root_specifier = self.resolve(&self.roots[0]);
    match self.module_slots.get(&root_specifier) {
      Some(ModuleSlot::Module(root)) => {
        if let Some(cache_info) = root.maybe_cache_info() {
          if let Some(local) = &cache_info.local {
            writeln!(
              f,
              "{} {}",
              colors::bold("local:"),
              local.to_string_lossy()
            )?;
          }
          if let Some(emit) = &cache_info.emit {
            writeln!(
              f,
              "{} {}",
              colors::bold("emit:"),
              emit.to_string_lossy()
            )?;
          }
          if let Some(map) = &cache_info.map {
            writeln!(f, "{} {}", colors::bold("map:"), map.to_string_lossy())?;
          }
        }
        writeln!(f, "{} {}", colors::bold("type:"), root.media_type())?;
        let total_size: f64 = self
          .module_slots
          .iter()
          .filter_map(|(_, m)| match m {
            ModuleSlot::Module(module) => Some(module.size() as f64),
            _ => None,
          })
          .sum();
        let dep_count = self
          .module_slots
          .iter()
          .filter(|(_, m)| m.is_module())
          .count()
          - 1;
        writeln!(
          f,
          "{} {} unique {}",
          colors::bold("dependencies:"),
          dep_count,
          colors::gray(format!("(total {})", human_size(total_size)))
        )?;
        writeln!(
          f,
          "\n{} {}",
          root_specifier,
          colors::gray(format!("({})", human_size(root.size() as f64)))
        )?;
        let mut seen = HashSet::new();
        if let Some(dependencies) = root.maybe_dependencies() {
          let dep_len = dependencies.len();
          for (idx, (_, dep)) in dependencies.iter().enumerate() {
            dep.fmt_info(
              f,
              "",
              idx == dep_len - 1 && root.maybe_types_dependency().is_none(),
              self,
              &mut seen,
            )?;
          }
        }
        Ok(())
      }
      Some(ModuleSlot::Err(err)) => {
        writeln!(f, "{} {}", colors::red("error:"), err)
      }
      Some(ModuleSlot::Missing) => {
        writeln!(f, "{} module could not be found", colors::red("error:"))
      }
      _ => {
        writeln!(f, "{} an internal error occurred", colors::red("error:"))
      }
    }
  }
}

impl Dependency {
  fn fmt_info<S: AsRef<str> + fmt::Display + Clone>(
    &self,
    f: &mut fmt::Formatter,
    prefix: S,
    last: bool,
    graph: &ModuleGraph,
    seen: &mut HashSet<ModuleSpecifier>,
  ) -> fmt::Result {
    if self.maybe_code.is_some() {
      fmt_resolved_info(
        &self.maybe_code,
        f,
        prefix.clone(),
        self.maybe_type.is_none() && last,
        graph,
        false,
        seen,
      )?;
    }
    if self.maybe_type.is_some() {
      fmt_resolved_info(&self.maybe_type, f, prefix, last, graph, true, seen)?;
    }
    Ok(())
  }
}

impl Module {
  fn fmt_info<S: AsRef<str> + fmt::Display + Clone>(
    &self,
    f: &mut fmt::Formatter,
    prefix: S,
    last: bool,
    graph: &ModuleGraph,
    type_dep: bool,
    seen: &mut HashSet<ModuleSpecifier>,
  ) -> fmt::Result {
    match self {
      Self::Es(m) => m.fmt_info(f, prefix, last, graph, type_dep, seen),
      Self::Synthetic(m) => m.fmt_info(f, prefix, last, seen),
    }
  }
}

impl SyntheticModule {
  fn fmt_info<S: AsRef<str> + fmt::Display + Clone>(
    &self,
    f: &mut fmt::Formatter,
    prefix: S,
    last: bool,
    seen: &mut HashSet<ModuleSpecifier>,
  ) -> fmt::Result {
    let was_seen = seen.contains(&self.specifier);
    let (specifier_str, size_str) = if was_seen {
      let specifier_str = colors::gray(&self.specifier).to_string();
      (specifier_str, colors::gray(" *").to_string())
    } else {
      let specifier_str = self.specifier.to_string();
      let size_str =
        colors::gray(format!(" ({})", human_size(self.size() as f64)))
          .to_string();
      (specifier_str, size_str)
    };

    seen.insert(self.specifier.clone());

    fmt_info_msg(
      f,
      prefix,
      last,
      false,
      format!("{}{}", specifier_str, size_str),
    )
  }
}

impl EsModule {
  fn fmt_info<S: AsRef<str> + fmt::Display + Clone>(
    &self,
    f: &mut fmt::Formatter,
    prefix: S,
    last: bool,
    graph: &ModuleGraph,
    type_dep: bool,
    seen: &mut HashSet<ModuleSpecifier>,
  ) -> fmt::Result {
    let was_seen = seen.contains(&self.specifier);
    let children = !((self.dependencies.is_empty()
      && self.maybe_types_dependency.is_none())
      || was_seen);
    let (specifier_str, size_str) = if was_seen {
      let specifier_str = if type_dep {
        colors::italic_gray(&self.specifier).to_string()
      } else {
        colors::gray(&self.specifier).to_string()
      };
      (specifier_str, colors::gray(" *").to_string())
    } else {
      let specifier_str = if type_dep {
        colors::italic(&self.specifier).to_string()
      } else {
        self.specifier.to_string()
      };
      let size_str =
        colors::gray(format!(" ({})", human_size(self.size() as f64)))
          .to_string();
      (specifier_str, size_str)
    };

    seen.insert(self.specifier.clone());

    fmt_info_msg(
      f,
      prefix.clone(),
      last,
      children,
      format!("{}{}", specifier_str, size_str),
    )?;

    if !was_seen {
      let mut prefix = prefix.to_string();
      if last {
        prefix.push(EMPTY_CONNECTOR);
      } else {
        prefix.push(VERTICAL_CONNECTOR);
      }
      prefix.push(EMPTY_CONNECTOR);
      let dep_len = self.dependencies.len();
      if let Some((_, type_dep)) = &self.maybe_types_dependency {
        fmt_resolved_info(
          type_dep,
          f,
          &prefix,
          dep_len == 0,
          graph,
          true,
          seen,
        )?;
      }
      for (idx, (_, dep)) in self.dependencies.iter().enumerate() {
        dep.fmt_info(
          f,
          &prefix,
          idx == dep_len - 1 && self.maybe_types_dependency.is_none(),
          graph,
          seen,
        )?;
      }
    }
    Ok(())
  }
}

impl ModuleGraphError {
  fn fmt_info<S: AsRef<str> + fmt::Display + Clone>(
    &self,
    f: &mut fmt::Formatter,
    prefix: S,
    last: bool,
    specifier: &ModuleSpecifier,
    seen: &mut HashSet<ModuleSpecifier>,
  ) -> fmt::Result {
    seen.insert(specifier.clone());
    match self {
      Self::InvalidSource(_, _) => {
        fmt_error_msg(f, prefix, last, specifier, "(invalid source)")
      }
      Self::InvalidTypeAssertion { .. } => {
        fmt_error_msg(f, prefix, last, specifier, "(invalid import assertion)")
      }
      Self::LoadingErr(_, _) => {
        fmt_error_msg(f, prefix, last, specifier, "(loading error)")
      }
      Self::ParseErr(_, _) => {
        fmt_error_msg(f, prefix, last, specifier, "(parsing error)")
      }
      Self::ResolutionError(_) => {
        fmt_error_msg(f, prefix, last, specifier, "(resolution error)")
      }
      Self::UnsupportedImportAssertionType(_, _) => fmt_error_msg(
        f,
        prefix,
        last,
        specifier,
        "(unsupported import assertion)",
      ),
      Self::UnsupportedMediaType(_, _) => {
        fmt_error_msg(f, prefix, last, specifier, "(unsupported)")
      }
      Self::Missing(_) => {
        fmt_error_msg(f, prefix, last, specifier, "(missing)")
      }
    }
  }
}

fn fmt_info_msg<S, M>(
  f: &mut fmt::Formatter,
  prefix: S,
  last: bool,
  children: bool,
  msg: M,
) -> fmt::Result
where
  S: AsRef<str> + fmt::Display + Clone,
  M: AsRef<str> + fmt::Display,
{
  let sibling_connector = if last {
    LAST_SIBLING_CONNECTOR
  } else {
    SIBLING_CONNECTOR
  };
  let child_connector = if children {
    CHILD_DEPS_CONNECTOR
  } else {
    CHILD_NO_DEPS_CONNECTOR
  };
  writeln!(
    f,
    "{} {}",
    colors::gray(format!(
      "{}{}─{}",
      prefix, sibling_connector, child_connector
    )),
    msg
  )
}

fn fmt_error_msg<S, M>(
  f: &mut fmt::Formatter,
  prefix: S,
  last: bool,
  specifier: &ModuleSpecifier,
  error_msg: M,
) -> fmt::Result
where
  S: AsRef<str> + fmt::Display + Clone,
  M: AsRef<str> + fmt::Display,
{
  fmt_info_msg(
    f,
    prefix,
    last,
    false,
    format!("{} {}", colors::red(specifier), colors::red_bold(error_msg)),
  )
}

fn fmt_resolved_info<S: AsRef<str> + fmt::Display + Clone>(
  resolved: &Resolved,
  f: &mut fmt::Formatter,
  prefix: S,
  last: bool,
  graph: &ModuleGraph,
  type_dep: bool,
  seen: &mut HashSet<ModuleSpecifier>,
) -> fmt::Result {
  match resolved {
    Some(Ok((specifier, _))) => {
      let resolved_specifier = graph.resolve(specifier);
      match graph.try_get(&resolved_specifier) {
        Ok(Some(module)) => {
          module.fmt_info(f, prefix, last, graph, type_dep, seen)
        }
        Err(err) => err.fmt_info(f, prefix, last, &resolved_specifier, seen),
        Ok(None) => fmt_info_msg(
          f,
          prefix,
          last,
          false,
          format!(
            "{} {}",
            colors::red(specifier),
            colors::red_bold("(missing)")
          ),
        ),
      }
    }
    Some(Err(err)) => fmt_info_msg(
      f,
      prefix,
      last,
      false,
      format!(
        "{} {}",
        colors::italic(err.to_string()),
        colors::red_bold("(resolve error)")
      ),
    ),
    _ => Ok(()),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ast::DefaultSourceParser;
  use crate::colors::strip_ansi_codes;
  use crate::graph::BuildKind;
  use crate::graph::Builder;
  use crate::source::CacheInfo;
  use crate::source::MemoryLoader;
  use std::path::PathBuf;

  #[tokio::test]
  async fn test_info_graph() {
    let mut loader = MemoryLoader::new(
      vec![
        (
          "https://deno.land/x/example/a.ts",
          Ok((
            "https://deno.land/x/example/a.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"import * as b from "./b.ts";
            import type { F } from "./f.d.ts";
            import * as g from "./g.js";
            "#,
          )),
        ),
        (
          "https://deno.land/x/example/b.ts",
          Ok((
            "https://deno.land/x/example/b.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"
            // @deno-types="./c.d.ts"
            import * as c from "./c.js";
            import * as d from "./d.ts";"#,
          )),
        ),
        (
          "https://deno.land/x/example/c.js",
          Ok((
            "https://deno.land/x/example/c.js",
            Some(vec![("content-type", "application/javascript")]),
            r#"export const c = "c";"#,
          )),
        ),
        (
          "https://deno.land/x/example/c.d.ts",
          Ok((
            "https://deno.land/x/example/c.d.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"export const c: "c";"#,
          )),
        ),
        (
          "https://deno.land/x/example/d.ts",
          Ok((
            "https://deno.land/x/example/d.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"import * as e from "./e.ts";
            export const d = "d";"#,
          )),
        ),
        (
          "https://deno.land/x/example/e.ts",
          Ok((
            "https://deno.land/x/example/e.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"import * as b from "./b.ts";
            export const e = "e";"#,
          )),
        ),
        (
          "https://deno.land/x/example/f.d.ts",
          Ok((
            "https://deno.land/x/example/f.d.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"export interface F { }"#,
          )),
        ),
        (
          "https://deno.land/x/example/g.js",
          Ok((
            "https://deno.land/x/example/g.js",
            Some(vec![
              ("content-type", "application/javascript"),
              ("x-typescript-types", "./g.d.ts"),
            ]),
            r#"export const g = "g";"#,
          )),
        ),
        (
          "https://deno.land/x/example/g.d.ts",
          Ok((
            "https://deno.land/x/example/g.d.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"export const g: "g";"#,
          )),
        ),
      ],
      vec![(
        "https://deno.land/x/example/a.ts",
        CacheInfo {
          local: Some(PathBuf::from(
            "/cache/deps/https/deno.land/x/example/a.ts",
          )),
          emit: Some(PathBuf::from(
            "/cache/deps/https/deno.land/x/example/a.js",
          )),
          ..Default::default()
        },
      )],
    );
    let root_specifier =
      ModuleSpecifier::parse("https://deno.land/x/example/a.ts").unwrap();
    let source_parser = DefaultSourceParser::new();
    let builder = Builder::new(
      vec![root_specifier],
      false,
      &mut loader,
      None,
      None,
      &source_parser,
      None,
    );
    let graph = builder.build(BuildKind::All, None).await;
    assert_eq!(
      strip_ansi_codes(format!("{}", graph)),
      r#"local: /cache/deps/https/deno.land/x/example/a.ts
emit: /cache/deps/https/deno.land/x/example/a.js
type: TypeScript
dependencies: 8 unique (total 477B)

https://deno.land/x/example/a.ts (129B)
├─┬ https://deno.land/x/example/b.ts (120B)
│ ├── https://deno.land/x/example/c.js (21B)
│ ├── https://deno.land/x/example/c.d.ts (20B)
│ └─┬ https://deno.land/x/example/d.ts (62B)
│   └─┬ https://deno.land/x/example/e.ts (62B)
│     └── https://deno.land/x/example/b.ts *
├── https://deno.land/x/example/f.d.ts (22B)
└─┬ https://deno.land/x/example/g.js (21B)
  └── https://deno.land/x/example/g.d.ts (20B)
"#
    );
  }

  #[tokio::test]
  async fn test_info_graph_import_assertion() {
    let mut loader = MemoryLoader::new(
      vec![
        (
          "https://deno.land/x/example/a.ts",
          Ok((
            "https://deno.land/x/example/a.ts",
            Some(vec![("content-type", "application/typescript")]),
            r#"import b from "./b.json" assert { type: "json" };
            const c = await import("./c.json", { assert: { type: "json" } });
            "#,
          )),
        ),
        (
          "https://deno.land/x/example/b.json",
          Ok((
            "https://deno.land/x/example/b.json",
            Some(vec![("content-type", "application/json")]),
            r#"{"b":"c"}"#,
          )),
        ),
        (
          "https://deno.land/x/example/c.json",
          Ok((
            "https://deno.land/x/example/c.json",
            Some(vec![("content-type", "application/json")]),
            r#"{"c":1}"#,
          )),
        ),
      ],
      vec![(
        "https://deno.land/x/example/a.ts",
        CacheInfo {
          local: Some(PathBuf::from(
            "/cache/deps/https/deno.land/x/example/a.ts",
          )),
          emit: Some(PathBuf::from(
            "/cache/deps/https/deno.land/x/example/a.js",
          )),
          ..Default::default()
        },
      )],
    );
    let root_specifier =
      ModuleSpecifier::parse("https://deno.land/x/example/a.ts").unwrap();
    let source_parser = DefaultSourceParser::new();
    let builder = Builder::new(
      vec![root_specifier],
      false,
      &mut loader,
      None,
      None,
      &source_parser,
      None,
    );
    let graph = builder.build(BuildKind::All, None).await;
    println!("{}", graph);
    assert_eq!(
      strip_ansi_codes(format!("{}", graph)),
      r#"local: /cache/deps/https/deno.land/x/example/a.ts
emit: /cache/deps/https/deno.land/x/example/a.js
type: TypeScript
dependencies: 2 unique (total 156B)

https://deno.land/x/example/a.ts (140B)
├── https://deno.land/x/example/b.json (9B)
└── https://deno.land/x/example/c.json (7B)
"#
    );
  }
}
