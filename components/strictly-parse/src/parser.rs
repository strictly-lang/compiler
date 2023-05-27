use strictly_ir::location::{Location, Span};

pub struct Parser<'p> {
    pub db: &'p dyn crate::Db,
    pub source_text: &'p str,
    pub anchors: Vec<Location>,
    pub position: Location,
}

impl Parser<'_> {
    pub fn probe<T: std::fmt::Debug>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Option<T>,
    ) -> Option<T> {
        let p = self.position;
        let anchors = self.anchors.len();
        if let Some(v) = f(self) {
            Some(v)
        } else {
            self.position = p;
            self.anchors.truncate(anchors);
            None
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.source_text[self.position.as_usize()..].chars().next()
    }

    // Returns a span ranging from `start_position` until the current position (exclusive)
    pub fn span_from(&self, start_position: Location) -> Span {
        let anchor = self.get_anchor();
        let start = start_position - anchor;
        let end = self.position - anchor;
        Span::new(start, end)
    }

    pub fn consume(&mut self, ch: char) {
        debug_assert!(self.peek() == Some(ch));
        self.position += ch.len_utf8();
    }

    pub fn push_anchor(&mut self, position: Location) {
        self.anchors.push(position);
    }

    pub fn pop_anchor(&mut self) {
        self.anchors.pop();
    }

    pub fn get_anchor(&self) -> Location {
        *self.anchors.last().unwrap()
    }
}
