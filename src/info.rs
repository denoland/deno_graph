// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::colors;
use crate::graph::Dependency;
use crate::graph::Module;
use crate::graph::ModuleGraph;
use crate::graph::ModuleGraphError;
use crate::graph::ModuleSlot;
use crate::graph::Resolved;
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
    match self.module_slots.get(&self.root) {
      Some(ModuleSlot::Module(root)) => {
        writeln!(f, "{} {}", colors::bold("type:"), root.media_type)?;
        if let Some(cache_info) = &root.maybe_cache_info {
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
        let total_size: f64 = self
          .module_slots
          .iter()
          .filter_map(|(_, m)| {
            if let ModuleSlot::Module(module) = m {
              Some(module.size() as f64)
            } else {
              None
            }
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
          self.root,
          colors::gray(format!("({})", human_size(root.size() as f64)))
        )?;
        let mut seen = HashSet::new();
        let dep_len = root.dependencies.len();
        for (idx, (_, dep)) in root.dependencies.iter().enumerate() {
          dep.fmt_info(
            f,
            "",
            idx == dep_len - 1 && root.maybe_types_dependency.is_none(),
            self,
            &mut seen,
          )?;
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
        writeln!(f, "{} an internal error occured", colors::red("error:"))
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
    if !self.maybe_code.is_none() {
      self.maybe_code.fmt_info(
        f,
        prefix.clone(),
        self.maybe_type.is_none() && last,
        graph,
        false,
        seen,
      )?;
    }
    if !self.maybe_type.is_none() {
      self
        .maybe_type
        .fmt_info(f, prefix, last, graph, true, seen)?;
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
      Self::InvalidSource(_) => fmt_info_msg(
        f,
        prefix,
        last,
        false,
        format!(
          "{} {}",
          colors::red(specifier),
          colors::red_bold("(invalid source)")
        ),
      ),
      Self::LoadingErr(_) => fmt_info_msg(
        f,
        prefix,
        last,
        false,
        format!(
          "{} {}",
          colors::red(specifier),
          colors::red_bold("(loading error)")
        ),
      ),
      Self::ParseErr(_) => fmt_info_msg(
        f,
        prefix,
        last,
        false,
        format!(
          "{} {}",
          colors::red(specifier),
          colors::red_bold("(parsing error)")
        ),
      ),
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

impl Resolved {
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
      Self::Specifier(specifier, _) => {
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
              "{} missing: {}",
              colors::red_bold("[internal error]"),
              specifier
            ),
          ),
        }
      }
      Self::Err(err, _) => fmt_info_msg(
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
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ast::DefaultSourceParser;
  use crate::colors::strip_ansi_codes;
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
      root_specifier,
      false,
      &mut loader,
      None,
      None,
      &source_parser,
    );
    let graph = builder.build().await;
    assert_eq!(
      strip_ansi_codes(format!("{}", graph)),
      r#"type: TypeScript
local: /cache/deps/https/deno.land/x/example/a.ts
emit: /cache/deps/https/deno.land/x/example/a.js
dependencies: 6 unique (total 395B)

https://deno.land/x/example/a.ts (88B)
├─┬ https://deno.land/x/example/b.ts (120B)
│ ├── https://deno.land/x/example/c.js (21B)
│ ├── https://deno.land/x/example/c.d.ts (20B)
│ └─┬ https://deno.land/x/example/d.ts (62B)
│   └─┬ https://deno.land/x/example/e.ts (62B)
│     └── https://deno.land/x/example/b.ts *
└── https://deno.land/x/example/f.d.ts (22B)
"#
    );
  }
}
