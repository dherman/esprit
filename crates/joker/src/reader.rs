use track::Posn;
use std::collections::VecDeque;

pub struct Reader<I> {
    chars: I,
    ahead: VecDeque<char>,
    curr_posn: Posn
}

impl<I> Reader<I> where I: Iterator<Item=char> {
    pub fn new(chars: I) -> Reader<I> {
        Reader {
            chars: chars,
            ahead: VecDeque::with_capacity(4),
            curr_posn: Posn::origin()
        }
    }

    pub fn peek(&mut self, n: usize) -> Option<char> {
        debug_assert!(n < self.ahead.capacity(), "Lookahead buffer can't hold that many items");
        for _ in self.ahead.len()..(n + 1) {
            match self.chars.next() {
                Some(ch) => {
                    self.ahead.push_back(ch)
                }
                None => {
                    return None
                }
            }
        }
        self.ahead.get(n).cloned()
    }

    pub fn curr_posn(&self) -> Posn { self.curr_posn }
}

impl<I> Iterator for Reader<I> where I: Iterator<Item=char> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        let curr_char = self.ahead.pop_front().or_else(|| {
            self.chars.next()
        });

        if (curr_char == Some('\r') && self.peek(0) != Some('\n')) ||
           curr_char == Some('\n') ||
           curr_char == Some('\u{2028}') ||
           curr_char == Some('\u{2029}') {
            self.curr_posn.line += 1;
            self.curr_posn.column = 0;
        } else {
            self.curr_posn.column += 1;
        }

        self.curr_posn.offset += 1;

        curr_char
    }
}