pub const BOM_CHAR: char = '\u{FEFF}';

/// Strips the byte order mark if it exists from the provided text.
pub fn strip_bom(text: &str) -> &str {
  if text.starts_with(BOM_CHAR) {
    &text[BOM_CHAR.len_utf8()..]
  } else {
    text
  }
}
