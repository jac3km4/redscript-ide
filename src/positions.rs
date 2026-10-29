use lsp_types::Position;

pub fn position(source: &str, offset: u32) -> Option<Position> {
    let prefix = source.get(..offset as usize)?;
    let line = prefix.bytes().filter(|&b| b == b'\n').count();
    let line_start = prefix.rfind('\n').map_or(0, |i| i + 1);
    Some(Position::new(
        u32::try_from(line).ok()?,
        u32::try_from(prefix[line_start..].encode_utf16().count()).ok()?,
    ))
}

pub fn byte_offset(chars: impl Iterator<Item = char>, column: u32) -> Option<usize> {
    let mut units = 0;
    let mut bytes = 0;
    for ch in chars {
        if units == column {
            return Some(bytes);
        }
        if ch == '\r' || ch == '\n' {
            return None;
        }
        units += ch.len_utf16() as u32;
        bytes += ch.len_utf8();
        if units > column {
            return None;
        }
    }
    (units == column).then_some(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn utf16_positions_round_trip() {
        let source = "first\r\né😀error";
        let offset = source.find("error").unwrap() as u32;
        assert_eq!(position(source, offset), Some(Position::new(1, 3)));
        assert_eq!(byte_offset("é😀error".chars(), 3), Some(6));
        assert_eq!(position("", 0), Some(Position::new(0, 0)));
        assert_eq!(position("😀", 4), Some(Position::new(0, 2)));
    }

    #[test]
    fn rejects_invalid_positions() {
        assert_eq!(byte_offset("😀".chars(), 1), None);
        assert_eq!(byte_offset("a\r\n".chars(), 2), None);
        assert_eq!(byte_offset("a".chars(), 2), None);
        assert_eq!(position("😀", 1), None);
    }
}
