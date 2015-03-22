use token::Posn;

pub struct Reader<I> {
    chars: I,
    curr_char: Option<char>,
    next_char: Option<char>,
    curr_posn: Posn
}

impl<I> Reader<I> where I: Iterator<Item=char> {
    pub fn new(mut chars: I) -> Reader<I> {
        let curr_char = chars.next();
        let next_char = if curr_char.is_some() { chars.next() } else { None };
        Reader {
            chars: chars,
            curr_char: curr_char,
            next_char: next_char,
            curr_posn: Posn {
                offset: 0,
                line: 0,
                column: 0
            }
        }
    }

    pub fn curr_char(&mut self) -> Option<char> { self.curr_char }
    pub fn curr_posn(&mut self) -> Posn { self.curr_posn }
    pub fn next_char(&mut self) -> Option<char> { self.next_char }

    pub fn bump(&mut self) {
        let curr_char = self.next_char;
        let next_char = if curr_char.is_some() { self.chars.next() } else { None };

        self.curr_char = curr_char;
        self.next_char = next_char;

        if (curr_char == Some('\r') && next_char != Some('\n')) ||
           curr_char == Some('\n') ||
           curr_char == Some('\u{2028}') ||
           curr_char == Some('\u{2029}') {
            self.curr_posn.line += 1;
            self.curr_posn.column = 0;
        } else {
            self.curr_posn.column += 1;
        }

        self.curr_posn.offset += 1;
    }
}
