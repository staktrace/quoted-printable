#![forbid(unsafe_code)]

use std::error;
use std::fmt;
use std::io;
use std::io::Write;

const LINE_LENGTH_LIMIT: usize = 76;

static HEX_CHARS: &[char] = &[
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
];

/// A flag that allows control over the decoding strictness.
#[derive(Debug, PartialEq)]
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
    /// 0x0D and 0x0A), and anything in the ASCII range 0x20 to 0x7E, inclusive.
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
            QuotedPrintableError::InvalidByte => {
                write!(
                    f,
                    "A unallowed byte was found in the quoted-printable input"
                )
            }
            QuotedPrintableError::LineTooLong => {
                write!(
                    f,
                    "A line length in the quoted-printed input exceeded 76 bytes"
                )
            }
            QuotedPrintableError::IncompleteHexOctet => {
                write!(
                    f,
                    "A '=' followed by only one character was found in the input"
                )
            }
            QuotedPrintableError::InvalidHexOctet => {
                write!(
                    f,
                    "A '=' followed by non-hex characters was found in the input"
                )
            }
            QuotedPrintableError::LowercaseHexOctet => {
                write!(f, "A '=' was followed by lowercase hex characters")
            }
        }
    }
}

impl error::Error for QuotedPrintableError {
    fn description(&self) -> &str {
        "invalid quoted-printable input"
    }

    fn cause(&self) -> Option<&dyn error::Error> {
        None
    }
}

/// Decodes a piece of quoted-printable data.
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
///     let decoded = decode("hello=3Dworld=0D=0A".as_bytes(), ParseMode::Robust).unwrap();
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
#[inline(always)]
pub fn decode<R: AsRef<[u8]>>(input: R, mode: ParseMode) -> Result<Vec<u8>, QuotedPrintableError> {
    _decode(input.as_ref(), mode)
}

fn _decode(input: &[u8], mode: ParseMode) -> Result<Vec<u8>, QuotedPrintableError> {
    let filtered = input
        .into_iter()
        .filter_map(|&c| match c {
            b'\t' | b'\r' | b'\n' | b' '..=b'~' => Some(c as char),
            _ => None,
        })
        .collect::<String>();
    if mode == ParseMode::Strict && filtered.len() != input.len() {
        return Err(QuotedPrintableError::InvalidByte);
    }
    let mut decoded = Vec::new();
    let mut lines = filtered.lines();
    let mut add_line_break = None;
    loop {
        let mut bytes = match lines.next() {
            Some(v) => v.trim_end().bytes(),
            None => {
                if mode == ParseMode::Strict && add_line_break == Some(false) {
                    return Err(QuotedPrintableError::IncompleteHexOctet);
                }
                break;
            }
        };

        if mode == ParseMode::Strict && bytes.len() > LINE_LENGTH_LIMIT {
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
                        if upper_char.to_uppercase().next() != Some(upper_char)
                            || lower_char.to_uppercase().next() != Some(lower_char)
                        {
                            return Err(QuotedPrintableError::LowercaseHexOctet);
                        }
                    }
                    let combined =
                        upper_char.to_digit(16).unwrap() << 4 | lower_char.to_digit(16).unwrap();
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

fn append<W: Write>(
    writer: &mut W,
    to_append: &[u8],
    bytes_on_line: &mut usize,
    is_last_byte: bool,
) -> io::Result<()> {
    // If the new input leads to line overflow or does not give
    // enough space for inserting the '=' for a soft line break we
    // add a soft line break
    //
    // On the last byte to write, we know that there are no other
    // characters proceeding so we allow the 76th character to be
    // something other than a soft line break.
    if (is_last_byte && *bytes_on_line + to_append.len() > LINE_LENGTH_LIMIT)
        || (!is_last_byte && *bytes_on_line + to_append.len() >= LINE_LENGTH_LIMIT)
    {
        writer.write_all(b"=\r\n")?;
        *bytes_on_line = 0;
    }

    writer.write_all(to_append)?;
    *bytes_on_line += to_append.len();
    Ok(())
}

/// Encodes some bytes into quoted-printable format.
///
/// The quoted-printable transfer-encoding is defined in IETF RFC 2045, section
/// 6.7. This function encodes a set of raw bytes into a format conformant with
/// that spec. The output contains CRLF pairs as needed so that each line is
/// wrapped to 76 characters or less (not including the CRLF).
///
/// # Examples
///
/// ```
///     use quoted_printable::encode;
///     let encoded = encode("hello, \u{20ac} zone!");
///     assert_eq!("hello, =E2=82=AC zone!", String::from_utf8(encoded).unwrap());
/// ```
#[inline(always)]
pub fn encode<R: AsRef<[u8]>>(input: R) -> Vec<u8> {
    encode_to_buffer(input.as_ref())
}

fn _encode<W: Write>(writer: &mut W, input: &[u8]) -> io::Result<()> {
    let mut it = input.iter().cloned().peekable();
    let mut bytes_on_current_line = 0;

    while let Some(byte) = it.next() {
        let peek = it.peek();

        // if the next character is empty this byte is the last byte
        let is_last_byte = peek.is_none();
        match byte {
            b'\r' => match peek {
                Some(b'\n') => {
                    bytes_on_current_line = 0;
                    writer.write_all(b"\r\n")?;
                    it.next(); // drop the next byte since we used it
                }
                _ => append(writer, b"=0D", &mut bytes_on_current_line, is_last_byte)?,
            },
            _ => encode_byte(writer, byte, &mut bytes_on_current_line, is_last_byte)?,
        }
    }

    Ok(())
}

#[inline(always)]
fn encode_to_buffer(input: &[u8]) -> Vec<u8> {
    // initialize buffer with 1.2x the size of the input
    // for most inputs, this should ensure the buffer
    // will not have to be grown to fit the entire input.
    let mut buffer: Vec<u8> = Vec::with_capacity(((input.len() as f32) * 1.2) as usize);
    _encode(&mut buffer, input).expect("writing into vecs should never cause io errors");
    buffer
}

/// Encodes some bytes into quoted-printable format.
///
/// The difference to `encode` is that this function returns a `String`.
///
/// The quoted-printable transfer-encoding is defined in IETF RFC 2045, section
/// 6.7. This function encodes a set of raw bytes into a format conformant with
/// that spec. The output contains CRLF pairs as needed so that each line is
/// wrapped to 76 characters or less (not including the CRLF).
///
/// # Examples
///
/// ```
///     use quoted_printable::encode_to_str;
///     let encoded = encode_to_str("hello, \u{20ac} zone!");
///     assert_eq!("hello, =E2=82=AC zone!", encoded);
/// ```
#[inline(always)]
pub fn encode_to_str<R: AsRef<[u8]>>(input: R) -> String {
    let buffer = encode_to_buffer(input.as_ref());
    match String::from_utf8_lossy(&buffer) {
        std::borrow::Cow::Borrowed(borrowed) => {
            drop(borrowed);
            // given that the vec could represent the string immutably
            // the vec must be utf8 encodable. this next line cannot panic.
            String::from_utf8(buffer).expect("the buffer must be utf8 encodable")
        }
        // note: all quoted-printable encoded strings should be utf8 encodable
        // this case is here for completion but it should never occur in practice
        std::borrow::Cow::Owned(owned) => owned,
    }
}

/// Encodes some bytes into quoted-printable format.
///
/// The difference to `encode` is that this function writes into a type that implements `Write`.
///
/// The quoted-printable transfer-encoding is defined in IETF RFC 2045, section
/// 6.7. This function encodes a set of raw bytes into a format conformant with
/// that spec. The output contains CRLF pairs as needed so that each line is
/// wrapped to 76 characters or less (not including the CRLF).
///
/// # Examples
///
/// ```
///     use std::io::Write;
///     use quoted_printable::encode_to_writer;
///     let mut writer = Vec::new();
///     let encoded = encode_to_writer(&mut writer, "hello, \u{20ac} zone!").unwrap();
///     assert_eq!(b"hello, =E2=82=AC zone!", writer.as_slice());
/// ```
#[inline(always)]
pub fn encode_to_writer<R: AsRef<[u8]>, W: Write>(writer: &mut W, input: R) -> io::Result<()> {
    _encode(writer, input.as_ref())
}

#[inline]
fn encode_byte<W: Write>(
    writer: &mut W,
    byte: u8,
    bytes_on_line: &mut usize,
    is_last_byte: bool,
) -> io::Result<()> {
    match byte {
        b'=' => append(writer, b"=3D", bytes_on_line, is_last_byte)?,
        b'\t' | b' '..=b'~' => append(writer, &[byte], bytes_on_line, is_last_byte)?,
        _ => append(writer, &hex_encode_byte(byte), bytes_on_line, is_last_byte)?,
    }

    Ok(())
}

#[inline(always)]
fn hex_encode_byte(byte: u8) -> [u8; 3] {
    [
        b'=',
        lower_nibble_to_hex(byte >> 4) as u8,
        lower_nibble_to_hex(byte) as u8,
    ]
}

#[inline(always)]
fn lower_nibble_to_hex(half_byte: u8) -> char {
    HEX_CHARS[(half_byte & 0x0F) as usize]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode() {
        assert_eq!(
            "hello world",
            String::from_utf8(decode("hello world", ParseMode::Strict).unwrap()).unwrap()
        );
        assert_eq!(
            "Now's the time for all folk to come to the aid of their country.",
            String::from_utf8(
                decode(
                    "Now's the time =\r\nfor all folk to come=\r\n \
                                                 to the aid of their country.",
                    ParseMode::Strict,
                )
                .unwrap(),
            )
            .unwrap()
        );
        assert_eq!(
            "\r\nhello=world",
            String::from_utf8(decode("=0D=0Ahello=3Dworld", ParseMode::Strict).unwrap()).unwrap()
        );
        assert_eq!(
            "hello world\r\ngoodbye world",
            String::from_utf8(decode("hello world\r\ngoodbye world", ParseMode::Strict).unwrap(),)
                .unwrap()
        );
        assert_eq!(
            "hello world\r\ngoodbye world",
            String::from_utf8(
                decode("hello world   \r\ngoodbye world   ", ParseMode::Strict).unwrap(),
            )
            .unwrap()
        );
        assert_eq!(
            "hello world\r\ngoodbye world x",
            String::from_utf8(
                decode(
                    "hello world   \r\ngoodbye world =  \r\nx",
                    ParseMode::Strict,
                )
                .unwrap(),
            )
            .unwrap()
        );

        assert_eq!(true, decode("hello world=x", ParseMode::Strict).is_err());
        assert_eq!(
            "hello world=x",
            String::from_utf8(decode("hello world=x", ParseMode::Robust).unwrap()).unwrap()
        );

        assert_eq!(true, decode("hello =world=", ParseMode::Strict).is_err());
        assert_eq!(
            "hello =world",
            String::from_utf8(decode("hello =world=", ParseMode::Robust).unwrap()).unwrap()
        );

        assert_eq!(true, decode("hello world=3d", ParseMode::Strict).is_err());
        assert_eq!(
            "hello world=",
            String::from_utf8(decode("hello world=3d", ParseMode::Robust).unwrap()).unwrap()
        );

        assert_eq!(true, decode("hello world=3m", ParseMode::Strict).is_err());
        assert_eq!(
            "hello world=3m",
            String::from_utf8(decode("hello world=3m", ParseMode::Robust).unwrap()).unwrap()
        );

        assert_eq!(true, decode("hello\u{FF}world", ParseMode::Strict).is_err());
        assert_eq!(
            "helloworld",
            String::from_utf8(decode("hello\u{FF}world", ParseMode::Robust).unwrap()).unwrap()
        );

        assert_eq!(
            true,
            decode(
                "12345678901234567890123456789012345678901234567890123456789012345678901234567",
                ParseMode::Strict,
            )
            .is_err()
        );
        assert_eq!(
            "12345678901234567890123456789012345678901234567890123456789012345678901234567",
            String::from_utf8(
                decode(
                    "12345678901234567890123456789012345678901234567890123456789012345678901234567",
                    ParseMode::Robust,
                )
                .unwrap(),
            )
            .unwrap()
        );
        assert_eq!(
            "1234567890123456789012345678901234567890123456789012345678901234567890123456",
            String::from_utf8(
                decode(
                    "1234567890123456789012345678901234567890123456789012345678901234567890123456",
                    ParseMode::Strict,
                )
                .unwrap(),
            )
            .unwrap()
        );
    }

    #[test]
    fn test_encode() {
        assert_eq!("hello, world!", encode_to_str("hello, world!".as_bytes()));
        assert_eq!(
            "hello,=0Cworld!",
            encode_to_str("hello,\u{c}world!".as_bytes())
        );
        assert_eq!(
            "this=00is=C3=BFa=3Dlong=0Dstring=0Athat gets wrapped and stuff, \
                    woohoo!=C3=\r\n=89",
            encode_to_str(
                "this\u{0}is\u{FF}a=long\rstring\nthat gets \
                                             wrapped and stuff, woohoo!\u{c9}",
            )
        );
        assert_eq!(
            "this=00is=C3=BFa=3Dlong=0Dstring=0Athat just fits in a line,   woohoo!=C3=89",
            encode_to_str(
                "this\u{0}is\u{FF}a=long\rstring\nthat just fits \
                                             in a line,   woohoo!\u{c9}",
            )
        );
        assert_eq!(
            "this \r\nhas linebreaks\r\n built right in.",
            encode_to_str("this \r\nhas linebreaks\r\n built right in.")
        );
        // Test that soft line breaks get inserted at the right place
        assert_eq!(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXY",
            encode_to_str(
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXY",
            )
        );
        // Two-line test: first line is all x's second line sets the 76th character as Y
        assert_eq!(
"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=\r\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXY",
            encode_to_str(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXY",
            )
        );
        // two-line test: first line is all x's second line sets the 77th character as Y
        assert_eq!(
"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=\r\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=\r\nXY",
            encode_to_str(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXY",
            )
        );
        assert_eq!(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=\r\nXY",
            encode_to_str(
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXY",
            )
        );
        assert_eq!(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=\r\nXXY",
            encode_to_str(
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXY",
            )
        );
        // Test that soft line breaks don't break up an encoded octet
        assert_eq!(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=00Y",
            encode_to_str(
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\u{0}Y",
            )
        );
        assert_eq!(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=\r\n=00Y",
            encode_to_str(
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\u{0}Y",
            )
        );
        assert_eq!(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=\r\n=00Y",
            encode_to_str(
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\u{0}Y",
            )
        );
        assert_eq!(
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=\r\n=00Y",
            encode_to_str(
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\u{0}Y",
            )
        );
        assert_eq!("=0D=3D", encode_to_str("\r="));
        assert_eq!("=0D\r\n", encode_to_str("\r\r\n"));
        assert_eq!("a=0D\r\nb", encode_to_str("a\r\r\nb"));
        assert_eq!("=0D", encode_to_str("\r"));
        assert_eq!("=0D=0D", encode_to_str("\r\r"));
    }

    #[test]
    fn test_lower_nibble_to_hex() {
        let test_data: &[(u8, char, char)] = &[
            (0, '0', '0'),
            (1, '0', '1'),
            (9, '0', '9'),
            (10, '0', 'A'),
            (15, '0', 'F'),
            (16, '1', '0'),
            (255, 'F', 'F'),
        ];

        for &(nr, high, low) in test_data.iter() {
            let got_high = lower_nibble_to_hex(nr >> 4);
            assert_eq!(high, got_high);
            let got_low = lower_nibble_to_hex(nr);
            assert_eq!(low, got_low);
        }
    }
}
