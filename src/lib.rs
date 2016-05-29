use std::fmt;
use std::error;

/// A flag that allows control over the decoding strictness.
#[derive(Debug)]
#[derive(PartialEq)]
pub enum ParseMode {
    /// Perform strict checking over the input, and return an error if any
    /// input appears malformed.
    Strict,
    /// Perform robust parsing, and gracefully handle any malformed input. This
    /// can result in the decoded output being different than what was intended.
    Robust,
}

/// An error type that represents different kinds of decoding errors.
#[derive(Debug)]
pub enum QuotedPrintableError {
    /// A byte was found in the input that was outside of the allowed range. The
    /// allowed range is the horizontal tab (ASCII 0x09), CR/LF characters (ASCII
    /// 0x0A and 0x0D), and anything in the ASCII range 0x20 to 0x7E, inclusive.
    InvalidByte,
    /// Lines where found in the input that exceeded 76 bytes in length, excluding
    /// the terminating CRLF.
    LineTooLong,
    /// An '=' character was found in the input without the proper number of
    /// hex-characters following it. This includes '=' characters followed
    /// by a single character and then the CRLF pair, for example.
    IncompleteHexOctet,
    /// An '=' character was found with two following characters, but they were
    /// not hex characters. '=Hi' for example would be an invalid encoding.
    InvalidHexOctet,
    /// An '=' character was found with two following hex characters, but the
    /// hex characters were lowercase rather than uppercase. The spec explicitly
    /// requires uppercase hex to be used, so this is considered an error.
    LowercaseHexOctet,
}

impl fmt::Display for QuotedPrintableError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            QuotedPrintableError::InvalidByte => write!(f, "A unallowed byte was found in the quoted-printable input"),
            QuotedPrintableError::LineTooLong => write!(f, "A line length in the quoted-printed input exceeded 76 bytes"),
            QuotedPrintableError::IncompleteHexOctet => write!(f, "A '=' followed by only one character was found in the input"),
            QuotedPrintableError::InvalidHexOctet => write!(f, "A '=' followed by non-hex characters was found in the input"),
            QuotedPrintableError::LowercaseHexOctet => write!(f, "A '=' was followed by lowercase hex characters"),
        }
    }
}

impl error::Error for QuotedPrintableError {
    fn description(&self) -> &str {
        "An error occurred while attempting to decode quoted-printable input"
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}

/// Decodes a piece quoted-printable data.
///
/// The quoted-printable transfer-encoding is defined in IETF RFC 2045, section
/// 6.7. This function attempts to decode input that is conformant with that
/// spec. Note that quoted-printable encoding is independent of charset, and so
/// this function returns a Vec<u8> of bytes upon success. It is up to the caller
/// to convert that to a String if desired; the charset required to do so must
/// come from somewhere else.
///
/// # Examples
///
/// ```
///     use quoted_printable::{decode, ParseMode};
///     let decoded = decode("hello=3Dworld=0D=0A", ParseMode::Robust).unwrap();
///     assert_eq!("hello=world\r\n", String::from_utf8(decoded).unwrap());
/// ```
///
/// # Errors
///
/// If this function is called with ParseMode::Strict, then it may return
/// a QuotedPrintableError if it detects that the input does not strictly conform
/// to the quoted-printable spec. If this function is called with ParseMode::Robust,
/// then it will attempt to gracefully handle any errors that arise. This might
/// result in input bytes being stripped out and ignored in some cases. Refer
/// to IETF RFC 2045, section 6.7 for details on what constitutes valid and
/// invalid input, and what a "robust" implementation would do in the face of
/// invalid input.
pub fn decode(input: &str, mode: ParseMode) -> Result<Vec<u8>, QuotedPrintableError> {
    let filtered = input.chars()
        .filter(|&c| c == '\t' || c == '\r' || c == '\n' || (c >= ' ' && c <= '~'))
        .collect::<String>();
    if mode == ParseMode::Strict && filtered.len() != input.len() {
        return Err(QuotedPrintableError::InvalidByte);
    }
    let mut decoded = Vec::new();
    let mut lines = filtered.lines();
    let mut add_line_break = None;
    loop {
        let mut bytes = match lines.next() {
            Some(v) => v.trim_right().bytes(),
            None => {
                if mode == ParseMode::Strict && add_line_break == Some(false) {
                    return Err(QuotedPrintableError::IncompleteHexOctet);
                }
                break;
            }
        };

        if mode == ParseMode::Strict && bytes.len() > 76 {
            return Err(QuotedPrintableError::LineTooLong);
        }

        if add_line_break == Some(true) {
            decoded.push(b'\r');
            decoded.push(b'\n');
            add_line_break = Some(false);
        }

        loop {
            let byte = match bytes.next() {
                Some(v) => v,
                None => {
                    add_line_break = Some(true);
                    break;
                }
            };

            if byte == b'=' {
                let upper = match bytes.next() {
                    Some(v) => v,
                    None => break,
                };
                let lower = match bytes.next() {
                    Some(v) => v,
                    None => {
                        if mode == ParseMode::Strict {
                            return Err(QuotedPrintableError::IncompleteHexOctet);
                        }
                        decoded.push(byte);
                        decoded.push(upper);
                        add_line_break = Some(true);
                        break;
                    }
                };
                let upper_char = upper as char;
                let lower_char = lower as char;
                if upper_char.is_digit(16) && lower_char.is_digit(16) {
                    if mode == ParseMode::Strict {
                        if upper_char.to_uppercase().next() != Some(upper_char) ||
                           lower_char.to_uppercase().next() != Some(lower_char) {
                            return Err(QuotedPrintableError::LowercaseHexOctet);
                        }
                    }
                    let combined = upper_char.to_digit(16).unwrap() << 4 |
                                   lower_char.to_digit(16).unwrap();
                    decoded.push(combined as u8);
                } else {
                    if mode == ParseMode::Strict {
                        return Err(QuotedPrintableError::InvalidHexOctet);
                    }
                    decoded.push(byte);
                    decoded.push(upper);
                    decoded.push(lower);
                }
            } else {
                decoded.push(byte);
            }
        }
    }
    Ok(decoded)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode() {
        assert_eq!("hello world",
                   String::from_utf8(decode("hello world", ParseMode::Strict).unwrap()).unwrap());
        assert_eq!("Now's the time for all folk to come to the aid of their country.",
                   String::from_utf8(decode("Now's the time =\r\nfor all folk to come=\r\n to \
                                             the aid of their country.",
                                            ParseMode::Strict)
                           .unwrap())
                       .unwrap());
        assert_eq!("\r\nhello=world",
                   String::from_utf8(decode("=0D=0Ahello=3Dworld", ParseMode::Strict).unwrap())
                       .unwrap());
        assert_eq!("hello world\r\ngoodbye world",
                   String::from_utf8(decode("hello world\r\ngoodbye world", ParseMode::Strict)
                           .unwrap())
                       .unwrap());
        assert_eq!("hello world\r\ngoodbye world",
                   String::from_utf8(decode("hello world   \r\ngoodbye world   ",
                                            ParseMode::Strict)
                           .unwrap())
                       .unwrap());
        assert_eq!("hello world\r\ngoodbye world x",
                   String::from_utf8(decode("hello world   \r\ngoodbye world =  \r\nx",
                                            ParseMode::Strict)
                           .unwrap())
                       .unwrap());

        assert_eq!(true, decode("hello world=x", ParseMode::Strict).is_err());
        assert_eq!("hello world=x",
                   String::from_utf8(decode("hello world=x", ParseMode::Robust).unwrap()).unwrap());

        assert_eq!(true, decode("hello =world=", ParseMode::Strict).is_err());
        assert_eq!("hello =world",
                   String::from_utf8(decode("hello =world=", ParseMode::Robust).unwrap()).unwrap());

        assert_eq!(true, decode("hello world=3d", ParseMode::Strict).is_err());
        assert_eq!("hello world=",
                   String::from_utf8(decode("hello world=3d", ParseMode::Robust).unwrap())
                       .unwrap());

        assert_eq!(true, decode("hello world=3m", ParseMode::Strict).is_err());
        assert_eq!("hello world=3m",
                   String::from_utf8(decode("hello world=3m", ParseMode::Robust).unwrap())
                       .unwrap());

        assert_eq!(true, decode("hello\u{FF}world", ParseMode::Strict).is_err());
        assert_eq!("helloworld",
                   String::from_utf8(decode("hello\u{FF}world", ParseMode::Robust).unwrap())
                       .unwrap());

        assert_eq!(true,
                   decode("12345678901234567890123456789012345678901234567890123456789012345678901234567", ParseMode::Strict).is_err());
        assert_eq!("12345678901234567890123456789012345678901234567890123456789012345678901234567",
                   String::from_utf8(decode("12345678901234567890123456789012345678901234567890123456789012345678901234567", ParseMode::Robust).unwrap()).unwrap());
        assert_eq!("1234567890123456789012345678901234567890123456789012345678901234567890123456",
                   String::from_utf8(decode("1234567890123456789012345678901234567890123456789012345678901234567890123456", ParseMode::Strict).unwrap()).unwrap());
    }
}
