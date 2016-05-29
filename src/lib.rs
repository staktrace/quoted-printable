#[derive(Debug)]
#[derive(PartialEq)]
pub enum ParseMode {
    Strict,
    Robust,
}

#[derive(Debug)]
pub enum QuotedPrintableError {
    InvalidByte,
    LineTooLong,
    IncompleteHexOctet,
    InvalidHexOctet,
    LowercaseHexOctet,
}

pub fn decode(input: &str, mode: ParseMode) -> Result<Vec<u8>, QuotedPrintableError> {
    let filtered = input.chars().filter(|&c| c == '\t' || c == '\r' || c == '\n' || (c >= ' ' && c <= '~')).collect::<String>();
    if mode == ParseMode::Strict && filtered.len() != input.len() {
        return Err(QuotedPrintableError::InvalidByte);
    }
    let mut decoded = Vec::new();
    let mut lines = filtered.lines();
    let mut add_line_break = false;
    loop {
        let mut bytes = match lines.next() {
            Some(v) => v.trim_right().bytes(),
            None => break,
        };

        if mode == ParseMode::Strict && bytes.len() > 76 {
            return Err(QuotedPrintableError::LineTooLong);
        }

        if add_line_break {
            decoded.push(b'\r');
            decoded.push(b'\n');
            add_line_break = false;
        }

        loop {
            let byte = match bytes.next() {
                Some(v) => v,
                None => {
                    add_line_break = true;
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
                        add_line_break = true;
                        break;
                    }
                };
                let upper_char = upper as char;
                let lower_char = lower as char;
                if upper_char.is_digit(16) && lower_char.is_digit(16) {
                    if mode == ParseMode::Strict {
                        if upper_char.to_uppercase().next() != Some(upper_char) || lower_char.to_uppercase().next() != Some(lower_char) {
                            return Err(QuotedPrintableError::LowercaseHexOctet);
                        }
                    }
                    let combined = upper_char.to_digit(16).unwrap() << 4 | lower_char.to_digit(16).unwrap();
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
        assert_eq!("hello world", String::from_utf8(decode("hello world", ParseMode::Strict).unwrap()).unwrap());
        assert_eq!("Now's the time for all folk to come to the aid of their country.",
                   String::from_utf8(decode("Now's the time =\r\nfor all folk to come=\r\n to the aid of their country.", ParseMode::Strict).unwrap()).unwrap());
        assert_eq!("\r\nhello=world", String::from_utf8(decode("=0D=0Ahello=3Dworld", ParseMode::Strict).unwrap()).unwrap());
        assert_eq!("hello world\r\ngoodbye world", String::from_utf8(decode("hello world\r\ngoodbye world", ParseMode::Strict).unwrap()).unwrap());
        assert_eq!("hello world\r\ngoodbye world", String::from_utf8(decode("hello world   \r\ngoodbye world   ", ParseMode::Strict).unwrap()).unwrap());
        assert_eq!("hello world\r\ngoodbye world ", String::from_utf8(decode("hello world   \r\ngoodbye world =  ", ParseMode::Strict).unwrap()).unwrap());

        assert_eq!(true, decode("hello world=x", ParseMode::Strict).is_err());
        assert_eq!("hello world=x", String::from_utf8(decode("hello world=x", ParseMode::Robust).unwrap()).unwrap());

        assert_eq!(true, decode("hello world=3d", ParseMode::Strict).is_err());
        assert_eq!("hello world=", String::from_utf8(decode("hello world=3d", ParseMode::Robust).unwrap()).unwrap());

        assert_eq!(true, decode("hello world=3m", ParseMode::Strict).is_err());
        assert_eq!("hello world=3m", String::from_utf8(decode("hello world=3m", ParseMode::Robust).unwrap()).unwrap());

        assert_eq!(true, decode("hello\u{FF}world", ParseMode::Strict).is_err());
        assert_eq!("helloworld", String::from_utf8(decode("hello\u{FF}world", ParseMode::Robust).unwrap()).unwrap());

        assert_eq!(true, decode("12345678901234567890123456789012345678901234567890123456789012345678901234567", ParseMode::Strict).is_err());
        assert_eq!("12345678901234567890123456789012345678901234567890123456789012345678901234567", String::from_utf8(decode("12345678901234567890123456789012345678901234567890123456789012345678901234567", ParseMode::Robust).unwrap()).unwrap());
        assert_eq!("1234567890123456789012345678901234567890123456789012345678901234567890123456", String::from_utf8(decode("1234567890123456789012345678901234567890123456789012345678901234567890123456", ParseMode::Strict).unwrap()).unwrap());
    }
}
