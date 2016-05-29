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
    fn test_basic_qp() {
        assert_eq!("hello world", String::from_utf8(decode("hello world", ParseMode::Strict).unwrap()).unwrap());
        assert_eq!("Now's the time for all folk to come to the aid of their country.",
                   String::from_utf8(decode("Now's the time =\r\nfor all folk to come=\r\n to the aid of their country.", ParseMode::Strict).unwrap()).unwrap());
    }
}
