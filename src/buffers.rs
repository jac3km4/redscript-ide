use dashmap::DashMap;
use lspower::lsp;
use redscript::ast::Pos;
use ropey::Rope;

#[derive(Default)]
pub struct Buffers {
    map: DashMap<lsp::Url, Buffer>,
}

impl Buffers {
    pub fn add(&self, url: lsp::Url, content: String) {
        self.map.insert(url, Buffer::new(content));
    }

    pub fn update(&self, url: &lsp::Url, change: lsp::TextDocumentContentChangeEvent) {
        let mut buf = self.map.get_mut(url).unwrap();
        buf.update(change);
    }

    pub fn get(&self, url: &lsp::Url) -> Option<dashmap::mapref::one::Ref<'_, lsp::Url, Buffer>> {
        self.map.get(url)
    }
}

pub struct Buffer {
    contents: Rope,
}

impl Buffer {
    pub fn new(contents: String) -> Self {
        Self {
            contents: Rope::from_str(&contents),
        }
    }

    pub fn update(&mut self, change: lsp::TextDocumentContentChangeEvent) {
        match change.range {
            Some(range) => {
                let start = self.contents.line_to_char(range.start.line as usize) + range.start.character as usize;
                let end = self.contents.line_to_char(range.end.line as usize) + range.end.character as usize;
                self.contents.remove(start..end);
                self.contents.insert(start, &change.text);
            }
            None => {
                self.contents = Rope::from_str(&change.text);
            }
        }
    }

    pub fn get_pos(&self, line: u32, col: u32) -> Option<Pos> {
        let line_start = self.contents.line_to_byte(line as usize);
        let byte_index = self.contents.char_to_byte(line_start + col as usize);
        Some(Pos::new(byte_index))
    }

    pub fn get_loc(&self, pos: Pos) -> Option<(u32, u32)> {
        let line = self.contents.byte_to_line(pos.into());
        let col = self.contents.byte_to_char(pos.into()) - line;
        Some((line as u32, col as u32 - 1))
    }

    pub fn contents(&self) -> &Rope {
        &self.contents
    }
}
