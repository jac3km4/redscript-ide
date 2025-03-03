use std::collections::HashMap;

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
        let start =
            self.contents.line_to_char(range.start.line as usize) + range.start.character as usize;
        let end =
            self.contents.line_to_char(range.end.line as usize) + range.end.character as usize;
        self.contents.remove(start..end);
        self.contents.insert(start, &text);
    }

    pub fn get_pos(&self, line: u32, col: u32) -> Option<u32> {
        let line_start = self.contents.line_to_char(line as usize);
        let byte_index = self.contents.char_to_byte(line_start + col as usize);
        Some(byte_index as u32)
    }

    pub fn contents(&self) -> &Rope {
        &self.contents
    }
}
