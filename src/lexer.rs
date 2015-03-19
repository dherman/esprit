use std::collections::LinkedList;
use std::collections::HashMap;
use std::borrow::Borrow;

use token::Token;

use std::cell::Cell;
use std::rc::Rc;
use regex::Regex;
use context::Context;
use token::Posn;
use token::ReservedWord;

pub trait ESCharExt {
    fn is_es_newline(self) -> bool;
    fn is_es_whitespace(self) -> bool;
    fn is_es_identifier(self) -> bool;
    fn is_es_identifier_start(self) -> bool;
    fn is_es_identifier_continue(self) -> bool;
    fn is_es_single_escape_char(self) -> bool;
    fn is_es_hex_digit(self) -> bool;
    fn is_es_oct_digit(self) -> bool;
}

#[derive(Debug, PartialEq)]
pub enum LexError {
    UnexpectedEOF,
    // FIXME: split this up into specific situational errors
    UnexpectedChar(char),
    InvalidDigit(char)
}

/*
// https://github.com/ariya/esprima/blob/master/tools/generate-identifier-regex.js
// https://gist.github.com/mathiasbynens/6334847
// http://unicode.org/reports/tr31/

pub trait UnicodeCharExt {
    fn is_L(self) -> bool;
    fn is_Lu(self) -> bool;
    fn is_Ll(self) -> bool;
    fn is_Lt(self) -> bool;
    fn is_Lm(self) -> bool;
    fn is_Lo(self) -> bool;
    fn is_Nl(self) -> bool;
    fn is_OtherID_Start(self) -> bool;
}

impl UnicodeCharExt for char {
    fn is_L(self) -> bool {
        self.is_Lu() ||
        self.is_Ll() ||
        self.is_Lt() ||
        self.is_Lm() ||
        self.is_Lo()
    }

    fn is_Lu(self) -> bool {
        false
    }

    fn is_Ll(self) -> bool {
        false
    }

    fn is_Lt(self) -> bool {
        false
    }

    fn is_Lm(self) -> bool {
        false
    }

    fn is_Lo(self) -> bool {
        false
    }

    fn is_Nl(self) -> bool {
        false
    }

    fn is_OtherID_Start(self) -> bool {
        false
    }
}
*/

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

    fn is_es_identifier(self) -> bool {
        self.is_es_identifier_continue()
    }

    fn is_es_identifier_start(self) -> bool {
        self == '$' ||
        self == '_' ||
        self.is_alphabetic()
    }

    fn is_es_identifier_continue(self) -> bool {
        self.is_es_identifier_start() ||
        self.is_numeric()
    }

    fn is_es_single_escape_char(self) -> bool {
        match self {
            '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' => true,
            _ => false
        }
    }

    fn is_es_hex_digit(self) -> bool {
        match self {
              '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
            | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
            | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' => true,
            _ => false
        }
    }

    fn is_es_oct_digit(self) -> bool {
        match self {
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => true,
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

// test case: x=0;y=g=1;alert(eval("while(x)break\n/y/g.exec('y')"))
//       see: https://groups.google.com/d/msg/mozilla.dev.tech.js-engine.internals/2JLH5jRcr7E/Mxc7ZKc5r6sJ

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
        assert!(self.tokens.len() < 3);
        self.tokens.push_front(token);
    }
}

macro_rules! reserved_words {
    [ $( ( $key:expr, $val:ident ) ),* ] => {
        {
            let mut temp_map = HashMap::new();
            $(
                temp_map.insert($key, ReservedWord::$val);
            )*
            temp_map
        }
    };
}

pub struct Lexer<I> {
    reader: LineOrientedReader<I>,
    cx: Rc<Cell<Context>>,
    lookahead: TokenBuffer,
    reserved: HashMap<&'static str, ReservedWord>
}

impl<I> Lexer<I> where I: Iterator<Item=char> {
    // constructor

    pub fn new(chars: I, cx: Rc<Cell<Context>>) -> Lexer<I> {
        let mut reserved = reserved_words![
            ("null",     Null),     ("true",       True),       ("false",    False),
            ("break",    Break),    ("case",       Case),       ("catch",    Catch),
            ("class",    Class),    ("const",      Const),      ("continue", Continue),
            ("debugger", Debugger), ("default",    Default),    ("delete",   Delete),
            ("do",       Do),       ("else",       Else),       ("export",   Export),
            ("extends",  Extends),  ("finally",    Finally),    ("for",      For),
            ("function", Function), ("if",         If),         ("import",   Import),
            ("in",       In),       ("instanceof", Instanceof), ("new",      New),
            ("return",   Return),   ("super",      Super),      ("switch",   Switch),
            ("this",     This),     ("throw",      Throw),      ("try",      Try),
            ("typeof",   Typeof),   ("var",        Var),        ("void",     Void),
            ("while",    While),    ("with",       With),       ("yield",    Yield),
            ("enum",     Enum) //,  ("await",      Await)
        ];
        Lexer {
            reader: LineOrientedReader::new(chars),
            cx: cx,
            lookahead: TokenBuffer::new(),
            reserved: reserved
        }
    }

    // public methods

    pub fn peek_token(&mut self) -> Result<&Token, LexError> {
        if self.lookahead.is_empty() {
            let token = try!(self.read_next_token());
            self.lookahead.push_token(token);
        }
        Ok(self.lookahead.peek_token())
    }

    pub fn read_token(&mut self) -> Result<Token, LexError> {
        if self.lookahead.is_empty() {
            self.read_next_token()
        } else {
            Ok(self.lookahead.read_token())
        }
    }

    pub fn unread_token(&mut self, token: Token) {
        self.lookahead.unread_token(token);
    }

    // private methods

    fn bump_until<F: Fn(char) -> bool>(&mut self, pred: F) {
        loop {
            match self.reader.curr_char() {
                Some(ch) if pred(ch) => return,
                None => return,
                _ => ()
            }
            self.bump();
        }
    }

    fn take_until<F: Fn(char) -> bool>(&mut self, s: &mut String, pred: F) {
        loop {
            match self.reader.curr_char() {
                Some(ch) if pred(ch) => return,
                Some(ch) => { s.push(ch); }
                None => return,
            }
        }
    }

    fn skip_line_comment(&mut self) {
        self.bump();
        self.bump();
        self.bump_until(|ch| ch.is_es_newline());
    }

    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        self.bump();
        self.bump();
        self.bump_until(|ch| ch == '*');
        if self.reader.curr_char() == None {
            return Err(LexError::UnexpectedEOF);
        }
        self.bump();
        self.bump();
        Ok(())
    }

    fn div_or_regexp(&mut self) -> Token {
        if self.cx.get().is_operator() {
            self.reader.bump();
            Token::Slash
        } else {
            unimplemented!()
        }
    }

    fn if_assign(&mut self, cons: Token, alt: Token) -> Token {
        self.reader.bump();
        if self.reader.curr_char() == Some('=') { self.reader.bump(); cons } else { alt }
    }

    fn if_equality(&mut self, zero: Token, one: Token, two: Token) -> Token {
        self.bump();
        if self.reader.curr_char() == Some('=') {
            self.bump();
            if self.reader.curr_char() == Some('=') {
                self.bump();
                two
            } else {
                one
            }
        } else {
            zero
        }
    }

    fn lt(&mut self) -> Token {
        self.bump();
        match self.reader.curr_char() {
            Some('<') => {
                self.bump();
                if self.reader.curr_char() == Some('=') {
                    self.bump();
                    Token::LShiftAssign
                } else {
                    Token::LShift
                }
            },
            Some('=') => { self.bump(); Token::LEq }
            _ => Token::LAngle
        }
    }

    fn gt(&mut self) -> Token {
        self.bump();
        match self.reader.curr_char() {
            Some('>') => {
                self.bump();
                match self.reader.curr_char() {
                    Some('>') => {
                        self.bump();
                        if self.reader.curr_char() == Some('=') {
                            self.bump();
                            Token::URShiftAssign
                        } else {
                            Token::URShift
                        }
                    },
                    Some('=') => { self.bump(); Token::RShiftAssign },
                    _ => Token::RShift
                }
            },
            Some('=') => { self.bump(); Token::GEq },
            _ => Token::RAngle
        }
    }

    fn plus(&mut self) -> Token {
        self.bump();
        match self.reader.curr_char() {
            Some('+') => { self.bump(); Token::Inc },
            Some('=') => { self.bump(); Token::PlusAssign },
            _ => Token::Plus
        }
    }

    fn minus(&mut self) -> Token {
        self.bump();
        match self.reader.curr_char() {
            Some('-') => { self.bump(); Token::Dec },
            Some('=') => { self.bump(); Token::MinusAssign },
            _ => Token::Minus
        }
    }

    fn decimal_digits_into(&mut self, s: &mut String) -> Result<(), LexError> {
        match self.reader.curr_char() {
            Some(ch) if !ch.is_digit(10) => return Err(LexError::UnexpectedChar(ch)),
            None => return Err(LexError::UnexpectedEOF),
            _ => ()
        }
        let mut s = String::new();
        self.take_until(&mut s, |ch| !ch.is_digit(10));
        Ok(())
    }

    fn decimal_digits(&mut self) -> Result<String, LexError> {
        let mut s = String::new();
        try!(self.decimal_digits_into(&mut s));
        Ok(s)
    }

    fn exp_part(&mut self) -> Result<Option<String>, LexError> {
        match self.reader.curr_char() {
            Some(ch@'e') | Some(ch@'E') => {
                let mut s = String::new();
                s.push(ch);
                self.bump();
                match self.reader.curr_char() {
                    Some('+') | Some('-') => { s.push(self.eat().unwrap()); }
                    _ => ()
                }
                try!(self.decimal_digits_into(&mut s));
                Ok(Some(s))
            }
            _ => Ok(None)
        }
    }

    fn decimal_int(&mut self) -> Result<String, LexError> {
        let mut s = String::new();
        match self.reader.curr_char() {
            Some('0') => { s.push('0'); return Ok(s); }
            Some(ch) if ch.is_digit(10) => { self.bump(); s.push(ch); }
            Some(ch) => return Err(LexError::UnexpectedChar(ch)),
            None => return Err(LexError::UnexpectedEOF)
        }
        self.take_until(&mut s, |ch| !ch.is_digit(10));
        Ok(s)
    }

    fn int<F: Copy + Fn(char) -> bool, G: Fn(char, String) -> Token>(&mut self, pred: F, cons: G) -> Result<Token, LexError> {
        assert!(self.reader.curr_char().is_some());
        assert!(self.reader.next_char().is_some());
        let mut s = String::new();
        self.bump();
        let flag = self.eat().unwrap();
        try!(self.digit_into(&mut s, pred));
        while self.reader.curr_char().map_or(false, |ch| pred(ch)) {
            s.push(self.eat().unwrap());
        }
        Ok(cons(flag, s))
    }

    fn hex_int(&mut self) -> Result<Token, LexError> {
        self.int(|ch| ch.is_es_hex_digit(), |ch, s| Token::HexInt(ch, s))
    }

    fn oct_int(&mut self) -> Result<Token, LexError> {
        assert!(self.reader.curr_char().is_some());
        assert!(self.reader.next_char().is_some());
        let mut s = String::new();
        self.bump();
        let oO = self.eat().unwrap();
        try!(self.oct_digit_into(&mut s));
        while self.reader.curr_char().map_or(false, |ch| ch.is_es_oct_digit()) {
            s.push(self.eat().unwrap());
        }
        Ok(Token::OctalInt(Some(oO), s))
    }

    fn deprecated_oct_int(&mut self) -> Token {
        let mut s = String::new();
        while self.reader.curr_char().map_or(false, |ch| ch.is_es_oct_digit()) {
            s.push(self.eat().unwrap());
        }
        Token::OctalInt(None, s)
    }

    fn number(&mut self) -> Result<Token, LexError> {
        if self.reader.curr_char() == Some('.') {
            let frac = try!(self.decimal_digits());
            let exp = try!(self.exp_part());
            return Ok(Token::Float(None, Some(frac), exp));
        }
        if self.reader.curr_char() == Some('0') {
            match self.reader.next_char() {
                Some('x') | Some('X') => return self.hex_int(),
                Some('o') | Some('O') => return self.oct_int(),
                Some(ch) if ch.is_digit(10) => return Ok(self.deprecated_oct_int()),
                _ => {
                    self.bump();
                    return Ok(Token::DecimalInt(String::from_str("0")));
                }
            }
        }
        let pos = try!(self.decimal_int());
        let dot;
        let frac = if self.reader.curr_char() == Some('.') {
            dot = true;
            self.bump();
            match self.reader.curr_char() {
                Some(ch) if ch.is_digit(10) => Some(try!(self.decimal_digits())),
                _ => None
            }
        } else {
            dot = false;
            None
        };
        let exp = try!(self.exp_part());
        if dot { Ok(Token::Float(Some(pos), frac, exp)) } else { Ok(Token::DecimalInt(pos)) }
    }

    fn string(&mut self) -> Result<Token, LexError> {
        let mut s = String::new();
        loop {
            assert!(self.reader.curr_char().is_some());
            let quote = self.eat().unwrap();
            self.take_until(&mut s, |ch| {
                ch == quote ||
                ch == '\\' ||
                ch.is_es_newline()
            });
            match self.reader.curr_char() {
                Some('\\') => { try!(self.string_escape(&mut s)); },
                Some(ch) => {
                    if ch.is_es_newline() {
                        return Err(LexError::UnexpectedChar(ch));
                    }
                    self.bump();
                },
                None => return Err(LexError::UnexpectedEOF)
            }
        }
        Ok(Token::String(s))
    }

    fn string_escape(&mut self, s: &mut String) -> Result<(), LexError> {
        s.push(self.eat().unwrap());
        match self.reader.curr_char() {
            Some('0') => {
                self.bump();
                let mut i = 0_u32;
                while self.reader.curr_char().map_or(false, |ch| ch.is_digit(10)) && i < 3 {
                    s.push(self.eat().unwrap());
                }
            },
            Some(ch) if ch.is_es_single_escape_char() => {
                s.push(self.eat().unwrap());
            },
            Some('x') => {
                self.bump();
                s.push('x');
                try!(self.hex_digit_into(s));
                try!(self.hex_digit_into(s));
            },
            Some('u') => {
                self.bump();
                if self.reader.curr_char() == Some('{') {
                    s.push('{');
                    self.bump();
                    try!(self.hex_digit_into(s));
                    while self.reader.curr_char() != Some('}') {
                        try!(self.hex_digit_into(s));
                    }
                    s.push('}');
                    self.bump();
                } else {
                    for i in 0..4 {
                        try!(self.hex_digit_into(s));
                    }
                }
            },
            Some(ch) if ch.is_es_newline() => {
                self.newline_into(s);
            },
            Some(ch) => {
                self.bump();
                s.push(ch);
            },
            None => () // error will be reported from caller
        }
        Ok(())
    }

    fn digit_into<F: Fn(char) -> bool>(&mut self, s: &mut String, pred: F) -> Result<(), LexError> {
        match self.reader.curr_char() {
            Some(ch) if pred(ch) => {
                self.bump();
                s.push(ch);
                Ok(())
            },
            Some(ch) => Err(LexError::InvalidDigit(ch)),
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn oct_digit_into(&mut self, s: &mut String) -> Result<(), LexError> {
        self.digit_into(s, |ch| ch.is_es_oct_digit())
    }

    fn hex_digit_into(&mut self, s: &mut String) -> Result<(), LexError> {
        self.digit_into(s, |ch| ch.is_es_hex_digit())
    }

    fn word(&mut self) -> Token {
        let mut s = String::new();
        assert!(self.reader.curr_char().is_some());
        s.push(self.eat().unwrap());
        self.take_until(&mut s, |ch| !ch.is_es_identifier_continue());
        match self.reserved.get(&s[..]) {
            Some(word) => Token::Reserved(*word),
            None => Token::Identifier(s)
        }
    }

    fn read_next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();
        println!("inspecting {:?}", self.reader.curr_char());
        loop {
            match self.reader.curr_char() {
                Some('/') => {
                    match self.reader.next_char() {
                        Some('/') => self.skip_line_comment(),
                        Some('*') => { try!(self.skip_block_comment()); },
                        _ => return Ok(self.div_or_regexp())
                    }
                },
                Some('.') => {
                    match self.reader.next_char() {
                        Some(ch) if ch.is_digit(10) => { return self.number() },
                        _ => { self.reader.bump(); return Ok(Token::Dot) }
                    }
                }
                Some('{') => { self.bump(); return Ok(Token::LBrace) },
                Some('}') => { self.bump(); return Ok(Token::RBrace) },
                Some('[') => { self.bump(); return Ok(Token::LBrack) },
                Some(']') => { self.bump(); return Ok(Token::RBrack) },
                Some('(') => { self.bump(); return Ok(Token::LParen) },
                Some(')') => { self.bump(); return Ok(Token::RParen) },
                Some(';') => { self.bump(); return Ok(Token::Semi) },
                Some(':') => { self.bump(); return Ok(Token::Colon) },
                Some(',') => { self.bump(); return Ok(Token::Comma) },
                Some('<') => return Ok(self.lt()),
                Some('>') => return Ok(self.gt()),
                Some('=') => return Ok(self.if_equality(Token::Assign, Token::Eq, Token::StrictEq)),
                Some('+') => return Ok(self.plus()),
                Some('-') => return Ok(self.minus()),
                Some('*') => return Ok(self.if_assign(Token::StarAssign, Token::Star)),
                Some('%') => return Ok(self.if_assign(Token::ModAssign, Token::Mod)),
                Some('^') => return Ok(self.if_assign(Token::BitXorAssign, Token::BitXor)),
                Some('&') => {
                    self.bump();
                    match self.reader.curr_char() {
                        Some('&') => { self.bump(); return Ok(Token::LogicalAnd) },
                        _ => return Ok(Token::BitAnd)
                    }
                },
                Some('|') => {
                    self.bump();
                    match self.reader.curr_char() {
                        Some('|') => { self.bump(); return Ok(Token::LogicalOr) },
                        _ => return Ok(Token::BitOr)
                    }
                },
                Some('~') => { self.bump(); return Ok(Token::Tilde) },
                Some('!') => return Ok(self.if_equality(Token::Bang, Token::NEq, Token::StrictNEq)),
                Some('?') => { self.bump(); return Ok(Token::Question) },
                Some('"') => return self.string(),
                Some('\'') => return self.string(),
                Some(ch) if ch.is_es_newline() => {
                    self.newline();
                    if self.cx.get().is_asi_possible() {
                        return Ok(Token::Newline);
                    }
                }
                Some(ch) if ch.is_digit(10) => return self.number(),
                Some(ch) if ch.is_es_identifier_start() => return Ok(self.word()),
                Some(ch) => return Err(LexError::UnexpectedChar(ch)),
                None => return Ok(Token::EOF)
            }
        }
    }

    fn newline(&mut self) {
        assert!(self.reader.curr_char().map_or(false, |ch| ch.is_es_newline()));
        if self.reader.curr_char() == Some('\r') && self.reader.next_char() == Some('\n') {
            self.bump();
        }
        self.bump();
    }

    fn newline_into(&mut self, s: &mut String) {
        assert!(self.reader.curr_char().map_or(false, |ch| ch.is_es_newline()));
        if self.reader.curr_char() == Some('\r') && self.reader.next_char() == Some('\n') {
            s.push('\r');
            s.push('\n');
            self.bump();
            self.bump();
            return;
        }
        s.push(self.eat().unwrap());
    }

    fn is_whitespace(&mut self) -> bool {
        match self.reader.curr_char() {
            Some(ch) => ch.is_es_whitespace(),
            None => false
        }
    }

    fn bump(&mut self) {
        self.reader.bump();
    }

    fn eat(&mut self) -> Option<char> {
        let ch = self.reader.curr_char();
        self.bump();
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
        match self.read_token() {
            Ok(Token::EOF) => None,
            Ok(t) => Some(t),
            Err(_) => None
        }
    }
}
