// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use regex::Regex;
use std::fmt;
use std::io::Write;
// use termcolor::Color::{Ansi256, Black, Blue, Cyan, Green, Red, White, Yellow};
use termcolor::Color::Ansi256;
use termcolor::Color::Red;
use termcolor::{Ansi, ColorSpec, WriteColor};

lazy_static::lazy_static! {
  // STRIP_ANSI_RE and strip_ansi_codes are lifted from the "console" crate.
  // Copyright 2017 Armin Ronacher <armin.ronacher@active-4.com>. MIT License.
  static ref STRIP_ANSI_RE: Regex = Regex::new(r"[\x1b\x9b][\[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-PRZcf-nqry=><]").unwrap();
  static ref NO_COLOR: bool = {
    std::env::var_os("NO_COLOR").is_some()
  };
}

/// Helper function to strip ansi codes.
#[cfg(any(test, feature = "wasm"))]
pub fn strip_ansi_codes<S: AsRef<str>>(s: S) -> String {
  STRIP_ANSI_RE.replace_all(s.as_ref(), "").to_string()
}

pub fn use_color() -> bool {
  !(*NO_COLOR)
}

fn style<S: AsRef<str>>(s: S, colorspec: ColorSpec) -> impl fmt::Display {
  if !use_color() {
    return String::from(s.as_ref());
  }
  let mut v = Vec::new();
  let mut ansi_writer = Ansi::new(&mut v);
  ansi_writer.set_color(&colorspec).unwrap();
  ansi_writer.write_all(s.as_ref().as_bytes()).unwrap();
  ansi_writer.reset().unwrap();
  String::from_utf8_lossy(&v).into_owned()
}

pub fn red_bold<S: AsRef<str>>(s: S) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Red)).set_bold(true);
  style(s, style_spec)
}

// pub fn green_bold<S: AsRef<str>>(s: S) -> impl fmt::Display {
//   let mut style_spec = ColorSpec::new();
//   style_spec.set_fg(Some(Green)).set_bold(true);
//   style(s, style_spec)
// }

pub fn italic<S: AsRef<str>>(s: S) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_italic(true);
  style(s, style_spec)
}

pub fn italic_gray<S: AsRef<str>>(s: S) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Ansi256(8))).set_italic(true);
  style(s, style_spec)
}

// pub fn italic_bold<S: AsRef<str>>(s: S) -> impl fmt::Display {
//   let mut style_spec = ColorSpec::new();
//   style_spec.set_bold(true).set_italic(true);
//   style(s, style_spec)
// }

// pub fn white_on_red<S: AsRef<str>>(s: S) -> impl fmt::Display {
//   let mut style_spec = ColorSpec::new();
//   style_spec.set_bg(Some(Red)).set_fg(Some(White));
//   style(s, style_spec)
// }

// pub fn black_on_green<S: AsRef<str>>(s: S) -> impl fmt::Display {
//   let mut style_spec = ColorSpec::new();
//   style_spec.set_bg(Some(Green)).set_fg(Some(Black));
//   style(s, style_spec)
// }

// pub fn yellow<S: AsRef<str>>(s: S) -> impl fmt::Display {
//   let mut style_spec = ColorSpec::new();
//   style_spec.set_fg(Some(Yellow));
//   style(s, style_spec)
// }

// pub fn cyan<S: AsRef<str>>(s: S) -> impl fmt::Display {
//   let mut style_spec = ColorSpec::new();
//   style_spec.set_fg(Some(Cyan));
//   style(s, style_spec)
// }

pub fn red<S: AsRef<str>>(s: S) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Red));
  style(s, style_spec)
}

// pub fn green<S: AsRef<str>>(s: S) -> impl fmt::Display {
//   let mut style_spec = ColorSpec::new();
//   style_spec.set_fg(Some(Green));
//   style(s, style_spec)
// }

pub fn bold<S: AsRef<str>>(s: S) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_bold(true);
  style(s, style_spec)
}

pub fn gray<S: AsRef<str>>(s: S) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Ansi256(8)));
  style(s, style_spec)
}

// pub fn intense_blue<S: AsRef<str>>(s: S) -> impl fmt::Display {
//   let mut style_spec = ColorSpec::new();
//   style_spec.set_fg(Some(Blue)).set_intense(true);
//   style(s, style_spec)
// }
