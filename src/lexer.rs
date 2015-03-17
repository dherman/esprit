use token::Token;

use regex::Regex;
use context::ParseContext;

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

pub struct Lexer<I, C> {
    chars: I,
    cx: C
}

impl<I, C> Lexer<I, C> where I: Iterator<Item=char>, C: ParseContext {
    pub fn new(chars: I, cx: C) -> Lexer<I, C> {
        Lexer { chars: chars, cx: cx }
    }
}

impl<I, C> Iterator for Lexer<I, C> where I: Iterator<Item=char> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let char = self.chars.by_ref().find(|c| !c.is_es_whitespace() );

        char.map(|chr| {
            match chr {
                '1' => Token::DecimalInt(String::from_str("1")),
                '+' => Token::Plus,
                _ => Token::Error(chr)
            }
        })
    }
}
