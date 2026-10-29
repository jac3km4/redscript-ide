use hashbrown::HashMap;
use lsp_types as lsp;
use ropey::Rope;

#[derive(Debug, Default)]
pub struct Buffers {
    map: HashMap<lsp::Uri, Buffer>,
}

impl Buffers {
    pub fn add(&mut self, url: lsp::Uri, content: String) {
        self.map.insert(url, Buffer::new(content));
    }

    pub fn remove(&mut self, url: &lsp::Uri) {
        self.map.remove(url);
    }

    pub fn update_range(&mut self, url: &lsp::Uri, range: lsp::Range, text: String) {
        let buf = self.map.get_mut(url).unwrap();
        buf.update_range(range, text);
    }

    pub fn get(&self, url: &lsp::Uri) -> Option<&Buffer> {
        self.map.get(url)
    }
}

#[derive(Debug)]
pub struct Buffer {
    contents: Rope,
}

impl Buffer {
    pub fn new(contents: String) -> Self {
        Self {
            contents: Rope::from_str(&contents),
        }
    }

    pub fn update_range(&mut self, range: lsp::Range, text: String) {
        let Some(start) = self.get_pos(range.start.line, range.start.character) else {
            return;
        };
        let Some(end) = self.get_pos(range.end.line, range.end.character) else {
            return;
        };
        let start = self.contents.byte_to_char(start as usize);
        let end = self.contents.byte_to_char(end as usize);
        if start > end {
            return;
        }
        self.contents.remove(start..end);
        self.contents.insert(start, &text);
    }

    pub fn get_pos(&self, line: u32, col: u32) -> Option<u32> {
        let line = line as usize;
        let contents = self.contents.get_line(line)?;
        let line_start = self.contents.try_line_to_byte(line).ok()?;
        let column = crate::positions::byte_offset(contents.chars(), col)?;
        u32::try_from(line_start + column).ok()
    }

    pub fn contents(&self) -> &Rope {
        &self.contents
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn edits_and_queries_use_utf16_columns() {
        let mut buffer = Buffer::new("first\r\né😀error".into());
        assert_eq!(buffer.get_pos(1, 3), Some(13));
        assert_eq!(buffer.get_pos(1, 2), None);
        assert_eq!(buffer.get_pos(5, 0), None);
        buffer.update_range(
            lsp::Range::new(lsp::Position::new(1, 3), lsp::Position::new(1, 8)),
            "fixed".into(),
        );
        assert_eq!(buffer.contents().to_string(), "first\r\né😀fixed");
    }
}
