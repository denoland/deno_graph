use std::borrow::Cow;
use std::sync::Arc;

pub const BOM_CHAR: char = '\u{FEFF}';

/// Attempts to detect the character encoding of the provided bytes.
///
/// Supports UTF-8, UTF-16 Little Endian and UTF-16 Big Endian.
pub fn detect_charset(bytes: &'_ [u8]) -> &'static str {
  const UTF16_LE_BOM: &[u8] = b"\xFF\xFE";
  const UTF16_BE_BOM: &[u8] = b"\xFE\xFF";

  if bytes.starts_with(UTF16_LE_BOM) {
    "utf-16le"
  } else if bytes.starts_with(UTF16_BE_BOM) {
    "utf-16be"
  } else {
    // Assume everything else is utf-8
    "utf-8"
  }
}

/// Attempts to convert the provided bytes to a UTF-8 string.
///
/// Supports all encodings supported by the encoding_rs crate, which includes
/// all encodings specified in the WHATWG Encoding Standard, and only those
/// encodings (see: <https://encoding.spec.whatwg.org/>).
pub fn convert_to_utf8<'a>(
  bytes: &'a [u8],
  charset: &'_ str,
) -> Result<Cow<'a, str>, std::io::Error> {
  match encoding_rs::Encoding::for_label(charset.as_bytes()) {
    Some(encoding) => encoding
      .decode_without_bom_handling_and_without_replacement(bytes)
      .ok_or_else(|| std::io::ErrorKind::InvalidData.into()),
    None => Err(std::io::Error::new(
      std::io::ErrorKind::InvalidInput,
      format!("Unsupported charset: {charset}"),
    )),
  }
}

/// Strips the byte order mark if it exists from the provided text.
pub fn strip_bom_mut(text: &mut String) {
  if text.starts_with(BOM_CHAR) {
    text.drain(..BOM_CHAR.len_utf8());
  }
}

/// Converts an `Arc<[u8]>` to an `Arc<str>`.
pub fn arc_bytes_to_text(bytes: Arc<[u8]>) -> Result<Arc<str>, std::io::Error> {
  let charset = detect_charset(bytes.as_ref());
  let text = match convert_to_utf8(bytes.as_ref(), charset)? {
    Cow::Borrowed(text) => {
      if text.starts_with(BOM_CHAR) {
        text[..BOM_CHAR.len_utf8()].to_string()
      } else {
        return Ok(
          // SAFETY: we know it's avalid utf-8 string at this point
          unsafe {
            let raw_ptr = Arc::into_raw(bytes);
            Arc::from_raw(std::mem::transmute::<*const [u8], *const str>(
              raw_ptr,
            ))
          },
        );
      }
    }
    Cow::Owned(mut text) => {
      strip_bom_mut(&mut text);
      text
    }
  };
  let text: Arc<str> = Arc::from(text);
  Ok(text)
}

#[cfg(test)]
mod test {
  use super::*;

  fn test_detection(test_data: &[u8], expected_charset: &str) {
    let detected_charset = detect_charset(test_data);
    assert_eq!(
      expected_charset.to_lowercase(),
      detected_charset.to_lowercase()
    );
  }

  #[test]
  fn test_detection_utf8_no_bom() {
    let test_data = "Hello UTF-8 it is \u{23F0} for Deno!"
      .to_owned()
      .into_bytes();
    test_detection(&test_data, "utf-8");
  }

  #[test]
  fn test_detection_utf16_little_endian() {
    let test_data = b"\xFF\xFEHello UTF-16LE".to_owned().to_vec();
    test_detection(&test_data, "utf-16le");
  }

  #[test]
  fn test_detection_utf16_big_endian() {
    let test_data = b"\xFE\xFFHello UTF-16BE".to_owned().to_vec();
    test_detection(&test_data, "utf-16be");
  }

  #[test]
  fn strip_bom_mut_with_bom() {
    let mut text = format!("{BOM_CHAR}text");
    strip_bom_mut(&mut text);
    assert_eq!(text, "text");
  }

  #[test]
  fn strip_bom_mut_without_bom() {
    let mut text = "text".to_string();
    strip_bom_mut(&mut text);
    assert_eq!(text, "text");
  }
}
