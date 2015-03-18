use std::collections::LinkedList;

use token::Token;

use std::cell::Cell;
use std::rc::Rc;
use regex::Regex;
use context::Context;
use token::Posn;

pub trait ESCharExt {
    fn is_es_newline(self) -> bool;
    fn is_es_whitespace(self) -> bool;
}

impl ESCharExt for char {
    fn is_es_newline(self) -> bool {
        match self {
            '\u{000a}' | '\u{000d}' | '\u{2028}' | '\u{2029}' => true,
            _ => false
        }
    }

    fn is_es_whitespace(self) -> bool {
        match self {
              '\u{0009}' | '\u{000b}' | '\u{000c}' | '\u{0020}' | '\u{00a0}'
            | '\u{1680}' | '\u{2000}' | '\u{2001}' | '\u{2002}' | '\u{2003}' | '\u{2004}'
            | '\u{2005}' | '\u{2006}' | '\u{2009}' | '\u{200a}' | '\u{202f}' | '\u{205f}'
            | '\u{3000}' | '\u{feff}' => true,
            _ => false
        }
    }
}

struct LineOrientedReader<I> {
    chars: I,
    curr_char: Option<char>,
    next_char: Option<char>,
    curr_posn: Posn
}

impl<I> LineOrientedReader<I> where I: Iterator<Item=char> {
    pub fn new(mut chars: I) -> LineOrientedReader<I> {
        let curr_char = chars.next();
        let next_char = if curr_char.is_some() { chars.next() } else { None };
        LineOrientedReader {
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

struct TokenBuffer {
    tokens: LinkedList<Token>
}

impl TokenBuffer {
    fn new() -> TokenBuffer {
        TokenBuffer {
            tokens: LinkedList::new()
        }
    }

    fn is_empty(&mut self) -> bool {
        self.tokens.len() == 0
    }

    fn push_token(&mut self, token: Token) {
        assert!(self.tokens.len() == 0);
        self.tokens.push_back(token);
    }

    fn read_token(&mut self) -> Token {
        assert!(self.tokens.len() > 0);
        self.tokens.pop_front().unwrap()
    }

    fn peek_token(&mut self) -> &Token {
        assert!(self.tokens.len() > 0);
        self.tokens.front().unwrap()
    }

    fn unread_token(&mut self, token: Token) {
        assert!(self.tokens.len() >= 0);
        assert!(self.tokens.len() < 3);
        self.tokens.push_front(token);
    }
}

pub struct Lexer<I> {
    reader: LineOrientedReader<I>,
    cx: Rc<Cell<Context>>,
    lookahead: TokenBuffer
}

impl<I> Lexer<I> where I: Iterator<Item=char> {
    // constructor

    pub fn new(chars: I, cx: Rc<Cell<Context>>) -> Lexer<I> {
        Lexer {
            reader: LineOrientedReader::new(chars),
            cx: cx,
            lookahead: TokenBuffer::new()
        }
    }

    // public methods

    pub fn is_eof(&mut self) -> bool {
        match *self.peek_token() {
            Token::EOF => true,
            _ => false
        }
    }

    pub fn peek_token(&mut self) -> &Token {
        if self.lookahead.is_empty() {
            let token = self.read_next_token();
            self.lookahead.push_token(token);
        }
        self.lookahead.peek_token()
    }

    pub fn read_token(&mut self) -> Token {
        if self.lookahead.is_empty() {
            self.read_next_token()
        } else {
            self.lookahead.read_token()
        }
    }

    pub fn unread_token(&mut self, token: Token) {
        self.lookahead.unread_token(token);
    }

    // private methods

    fn skip_line_comment(&mut self) {
    }

    fn skip_block_comment(&mut self) {
    }

    fn div_or_regexp(&mut self) -> Token {
        if self.cx.borrow().get().operator {
            unimplemented!()
        } else {
            unimplemented!()
        }
    }

    fn read_next_token(&mut self) -> Token {
        self.skip_whitespace();
        loop {
            match self.reader.curr_char() {
                None => return Token::EOF,
                Some(ch) => {
                    if ch == '/' {
                        match self.reader.next_char() {
                            Some('/') => self.skip_line_comment(),
                            Some('*') => self.skip_block_comment(),
                            _ => return self.div_or_regexp()
                        }
                    }
                    unimplemented!()
                }
            }
        }
    }

    fn is_whitespace(&mut self) -> bool {
        match self.reader.curr_char() {
            Some(ch) => ch.is_es_whitespace(),
            None => false
        }
    }

    fn eat(&mut self) -> Option<char> {
        let ch = self.reader.curr_char();
        self.reader.bump();
        ch
    }

    fn skip_whitespace(&mut self) {
        while self.is_whitespace() {
            self.reader.bump();
        }
    }
}

impl<I> Iterator for Lexer<I> where I: Iterator<Item=char> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.eat()
            .map(|ch| {
                match ch {
                    '1' => Token::DecimalInt(String::from_str("1")),
                    '+' => Token::Plus,
                    _ => Token::Error(ch)
                }
            })
    }
}
