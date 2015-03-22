use std::collections::HashMap;
use std::borrow::Borrow;
use std::char;

use token::Token;

use std::cell::Cell;
use std::rc::Rc;
use regex::Regex;
use context::Context;
use token::Posn;
use token::ReservedWord;
use eschar::ESCharExt;
use reader::Reader;
use tokbuf::TokenBuffer;

#[derive(Debug, PartialEq)]
pub enum LexError {
    UnexpectedEOF,
    // FIXME: split this up into specific situational errors
    UnexpectedChar(char),
    InvalidDigit(char),
    IllegalUnicode(u32)
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

fn add_digits(digits: Vec<u32>, radix: u32) -> u32 {
    let mut place = 1;
    let mut sum = 0;
    for digit in digits.iter().rev() {
        sum += digit * place;
        place *= radix;
    }
    sum
}

pub struct Lexer<I> {
    reader: Reader<I>,
    cx: Rc<Cell<Context>>,
    lookahead: TokenBuffer,
    reserved: HashMap<&'static str, ReservedWord>
}

impl<I> Lexer<I> where I: Iterator<Item=char> {
    // constructor

    pub fn new(chars: I, cx: Rc<Cell<Context>>) -> Lexer<I> {
        Lexer {
            reader: Reader::new(chars),
            cx: cx,
            lookahead: TokenBuffer::new(),
            reserved: reserved_words![
                ("null",       Null),       ("true",       True),       ("false",    False),
                ("break",      Break),      ("case",       Case),       ("catch",    Catch),
                ("class",      Class),      ("const",      Const),      ("continue", Continue),
                ("debugger",   Debugger),   ("default",    Default),    ("delete",   Delete),
                ("do",         Do),         ("else",       Else),       ("export",   Export),
                ("extends",    Extends),    ("finally",    Finally),    ("for",      For),
                ("function",   Function),   ("if",         If),         ("import",   Import),
                ("in",         In),         ("instanceof", Instanceof), ("new",      New),
                ("return",     Return),     ("super",      Super),      ("switch",   Switch),
                ("this",       This),       ("throw",      Throw),      ("try",      Try),
                ("typeof",     Typeof),     ("var",        Var),        ("void",     Void),
                ("while",      While),      ("with",       With),       ("yield",    Yield),
                ("enum",       Enum),    // ("await",      Await),
                ("implements", Implements), ("interface",  Interface),  ("package",  Package),
                ("private",    Private),    ("protected",  Protected),  ("public",   Public)
            ]
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

    fn skip_until<F>(&mut self, pred: &F)
      where F: Fn(char) -> bool
    {
        loop {
            match self.reader.curr_char() {
                Some(ch) if pred(ch) => return,
                None => return,
                _ => ()
            }
            self.skip();
        }
    }

    fn take_until<F>(&mut self, s: &mut String, pred: &F)
      where F: Fn(char) -> bool
    {
        loop {
            match self.reader.curr_char() {
                Some(ch) if pred(ch) => return,
                Some(ch) => { s.push(ch); }
                None => return,
            }
        }
    }

    fn skip_line_comment(&mut self) {
        self.skip();
        self.skip();
        self.skip_until(&|ch| ch.is_es_newline());
    }

    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        self.skip();
        self.skip();
        self.skip_until(&|ch| ch == '*');
        if self.reader.curr_char() == None {
            return Err(LexError::UnexpectedEOF);
        }
        self.skip();
        self.skip();
        Ok(())
    }

    fn div_or_regexp(&mut self) -> Result<Token, LexError> {
        if self.cx.get().is_operator() {
            self.skip();
            if self.reader.curr_char() == Some('=') {
                self.skip();
                Ok(Token::SlashAssign)
            } else {
                Ok(Token::Slash)
            }
        } else {
            self.regexp()
        }
    }

    fn regexp(&mut self) -> Result<Token, LexError> {
        self.skip();
        let mut s = String::new();
        while self.reader.curr_char() != Some('/') {
            try!(self.regexp_char(&mut s));
        }
        self.skip();
        Ok(Token::RegExp(s))
    }

    fn regexp_char(&mut self, s: &mut String) -> Result<(), LexError> {
        match self.reader.curr_char() {
            Some('\\') => self.regexp_backslash(s),
            Some('[') => self.regexp_class(s),
            Some(ch) if ch.is_es_newline() => Err(LexError::UnexpectedChar(ch)),
            Some(ch) => { self.skip(); s.push(ch); Ok(()) },
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn regexp_backslash(&mut self, s: &mut String) -> Result<(), LexError> {
        s.push('\\');
        self.skip();
        match self.reader.curr_char() {
            Some(ch) if ch.is_es_newline() => Err(LexError::UnexpectedChar(ch)),
            Some(ch) => { self.skip(); s.push(ch); Ok(()) },
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn regexp_class(&mut self, s: &mut String) -> Result<(), LexError> {
        self.skip();
        s.push('[');
        while self.reader.curr_char().map_or(false, |ch| ch != ']') {
            try!(self.regexp_class_char(s));
        }
        self.skip();
        s.push(']');
        Ok(())
    }

    fn regexp_class_char(&mut self, s: &mut String) -> Result<(), LexError> {
        match self.reader.curr_char() {
            Some('\\') => self.regexp_backslash(s),
            Some(ch) => { self.skip(); s.push(ch); Ok(()) },
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn if_assign(&mut self, cons: Token, alt: Token) -> Token {
        self.reader.skip();
        if self.reader.curr_char() == Some('=') { self.reader.skip(); cons } else { alt }
    }

    fn if_equality(&mut self, zero: Token, one: Token, two: Token) -> Token {
        self.skip();
        if self.reader.curr_char() == Some('=') {
            self.skip();
            if self.reader.curr_char() == Some('=') {
                self.skip();
                two
            } else {
                one
            }
        } else {
            zero
        }
    }

    fn lt(&mut self) -> Token {
        self.skip();
        match self.reader.curr_char() {
            Some('<') => {
                self.skip();
                if self.reader.curr_char() == Some('=') {
                    self.skip();
                    Token::LShiftAssign
                } else {
                    Token::LShift
                }
            },
            Some('=') => { self.skip(); Token::LEq }
            _ => Token::LAngle
        }
    }

    fn gt(&mut self) -> Token {
        self.skip();
        match self.reader.curr_char() {
            Some('>') => {
                self.skip();
                match self.reader.curr_char() {
                    Some('>') => {
                        self.skip();
                        if self.reader.curr_char() == Some('=') {
                            self.skip();
                            Token::URShiftAssign
                        } else {
                            Token::URShift
                        }
                    },
                    Some('=') => { self.skip(); Token::RShiftAssign },
                    _ => Token::RShift
                }
            },
            Some('=') => { self.skip(); Token::GEq },
            _ => Token::RAngle
        }
    }

    fn plus(&mut self) -> Token {
        self.skip();
        match self.reader.curr_char() {
            Some('+') => { self.skip(); Token::Inc },
            Some('=') => { self.skip(); Token::PlusAssign },
            _ => Token::Plus
        }
    }

    fn minus(&mut self) -> Token {
        self.skip();
        match self.reader.curr_char() {
            Some('-') => { self.skip(); Token::Dec },
            Some('=') => { self.skip(); Token::MinusAssign },
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
        self.take_until(&mut s, &|ch| !ch.is_digit(10));
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
                self.skip();
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
            Some(ch) if ch.is_digit(10) => { self.skip(); s.push(ch); }
            Some(ch) => return Err(LexError::UnexpectedChar(ch)),
            None => return Err(LexError::UnexpectedEOF)
        }
        self.take_until(&mut s, &|ch| !ch.is_digit(10));
        Ok(s)
    }

    fn int<F, G>(&mut self, radix: u32, pred: &F, cons: &G) -> Result<Token, LexError>
      where F: Fn(char) -> bool,
            G: Fn(char, String) -> Token
    {
        assert!(self.reader.curr_char().is_some());
        assert!(self.reader.next_char().is_some());
        let mut s = String::new();
        self.skip();
        let flag = self.eat().unwrap();
        try!(self.digit_into(&mut s, radix, pred));
        while self.reader.curr_char().map_or(false, |ch| pred(ch)) {
            s.push(self.eat().unwrap());
        }
        Ok(cons(flag, s))
    }

    fn hex_int(&mut self) -> Result<Token, LexError> {
        self.int(16, &|ch| ch.is_es_hex_digit(), &Token::HexInt)
    }

    fn oct_int(&mut self) -> Result<Token, LexError> {
        self.int(8, &|ch| ch.is_es_oct_digit(), &|ch, s| Token::OctalInt(Some(ch), s))
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
                    self.skip();
                    return Ok(Token::DecimalInt(String::from_str("0")));
                }
            }
        }
        let pos = try!(self.decimal_int());
        let dot;
        let frac = if self.reader.curr_char() == Some('.') {
            dot = true;
            self.skip();
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
            self.take_until(&mut s, &|ch| {
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
                    self.skip();
                },
                None => return Err(LexError::UnexpectedEOF)
            }
        }
        Ok(Token::String(s))
    }

    fn unicode_escape_seq(&mut self, s: &mut String) -> Result<u32, LexError> {
        self.skip();
        if self.reader.curr_char() == Some('{') {
            s.push('{');
            self.skip();
            let mut digits = Vec::with_capacity(8);
            digits.push(try!(self.hex_digit_into(s)));
            while self.reader.curr_char() != Some('}') {
                digits.push(try!(self.hex_digit_into(s)));
            }
            s.push('}');
            self.skip();
            Ok(add_digits(digits, 16))
        } else {
            let mut place = 0x1000;
            let mut code_point = 0;
            for i in 0..4 {
                code_point += try!(self.hex_digit_into(s)) * place;
                place >>= 4;
            }
            Ok(code_point)
        }
    }

    fn string_escape(&mut self, s: &mut String) -> Result<(), LexError> {
        s.push(self.eat().unwrap());
        match self.reader.curr_char() {
            Some('0') => {
                self.skip();
                let mut i = 0_u32;
                while self.reader.curr_char().map_or(false, |ch| ch.is_digit(10)) && i < 3 {
                    s.push(self.eat().unwrap());
                }
            },
            Some(ch) if ch.is_es_single_escape_char() => {
                s.push(self.eat().unwrap());
            },
            Some('x') => {
                self.skip();
                s.push('x');
                try!(self.hex_digit_into(s));
                try!(self.hex_digit_into(s));
            },
            Some('u') => {
                try!(self.unicode_escape_seq(s));
            },
            Some(ch) if ch.is_es_newline() => {
                self.newline_into(s);
            },
            Some(ch) => {
                self.skip();
                s.push(ch);
            },
            None => () // error will be reported from caller
        }
        Ok(())
    }

    fn digit_into<F>(&mut self, s: &mut String, radix: u32, pred: &F) -> Result<u32, LexError>
      where F: Fn(char) -> bool
    {
        match self.reader.curr_char() {
            Some(ch) if pred(ch) => {
                self.skip();
                s.push(ch);
                assert!(ch.is_digit(radix));
                Ok(ch.to_digit(radix).unwrap())
            },
            Some(ch) => Err(LexError::InvalidDigit(ch)),
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn oct_digit_into(&mut self, s: &mut String) -> Result<u32, LexError> {
        self.digit_into(s, 8, &|ch| ch.is_es_oct_digit())
    }

    fn hex_digit_into(&mut self, s: &mut String) -> Result<u32, LexError> {
        self.digit_into(s, 16, &|ch| ch.is_es_hex_digit())
    }

    fn word(&mut self) -> Result<Token, LexError> {
        let mut s = String::new();
        assert!(self.reader.curr_char().is_some());
        s.push(self.eat().unwrap());
        while self.reader.curr_char().map_or(false, |ch| ch == '\\' || ch.is_es_identifier_continue()) {
            let ch = self.reader.curr_char().unwrap();
            if (ch == '\\') {
                try!(self.word_escape(&mut s));
                continue;
            }
            self.skip();
            s.push(ch);
        }
        self.take_until(&mut s, &|ch| !ch.is_es_identifier_continue());
        match self.reserved.get(&s[..]) {
            Some(word) => Ok(Token::Reserved(*word)),
            None => Ok(Token::Identifier(s))
        }
    }

    fn word_escape(&mut self, s: &mut String) -> Result<(), LexError> {
        match self.reader.curr_char() {
            Some('u') => (),
            Some(ch) => return Err(LexError::UnexpectedChar(ch)),
            None => return Err(LexError::UnexpectedEOF)
        }
        let mut dummy = String::new();
        self.skip();
        let code_point = try!(self.unicode_escape_seq(&mut dummy));
        match char::from_u32(code_point) {
            Some(ch) => { s.push(ch); Ok(()) },
            None => Err(LexError::IllegalUnicode(code_point))
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
                        _ => return self.div_or_regexp()
                    }
                },
                Some('.') => {
                    match self.reader.next_char() {
                        Some(ch) if ch.is_digit(10) => { return self.number() },
                        _ => { self.reader.skip(); return Ok(Token::Dot) }
                    }
                }
                Some('{') => { self.skip(); return Ok(Token::LBrace) },
                Some('}') => { self.skip(); return Ok(Token::RBrace) },
                Some('[') => { self.skip(); return Ok(Token::LBrack) },
                Some(']') => { self.skip(); return Ok(Token::RBrack) },
                Some('(') => { self.skip(); return Ok(Token::LParen) },
                Some(')') => { self.skip(); return Ok(Token::RParen) },
                Some(';') => { self.skip(); return Ok(Token::Semi) },
                Some(':') => { self.skip(); return Ok(Token::Colon) },
                Some(',') => { self.skip(); return Ok(Token::Comma) },
                Some('<') => return Ok(self.lt()),
                Some('>') => return Ok(self.gt()),
                Some('=') => return Ok(self.if_equality(Token::Assign, Token::Eq, Token::StrictEq)),
                Some('+') => return Ok(self.plus()),
                Some('-') => return Ok(self.minus()),
                Some('*') => return Ok(self.if_assign(Token::StarAssign, Token::Star)),
                Some('%') => return Ok(self.if_assign(Token::ModAssign, Token::Mod)),
                Some('^') => return Ok(self.if_assign(Token::BitXorAssign, Token::BitXor)),
                Some('&') => {
                    self.skip();
                    match self.reader.curr_char() {
                        Some('&') => { self.skip(); return Ok(Token::LogicalAnd) },
                        _ => return Ok(Token::BitAnd)
                    }
                },
                Some('|') => {
                    self.skip();
                    match self.reader.curr_char() {
                        Some('|') => { self.skip(); return Ok(Token::LogicalOr) },
                        _ => return Ok(Token::BitOr)
                    }
                },
                Some('~') => { self.skip(); return Ok(Token::Tilde) },
                Some('!') => return Ok(self.if_equality(Token::Bang, Token::NEq, Token::StrictNEq)),
                Some('?') => { self.skip(); return Ok(Token::Question) },
                Some('"') => return self.string(),
                Some('\'') => return self.string(),
                Some(ch) if ch.is_es_newline() => {
                    self.newline();
                    if self.cx.get().is_asi_possible() {
                        return Ok(Token::Newline);
                    }
                }
                Some(ch) if ch.is_digit(10) => return self.number(),
                Some(ch) if ch.is_es_identifier_start() => return self.word(),
                Some(ch) => return Err(LexError::UnexpectedChar(ch)),
                None => return Ok(Token::EOF)
            }
        }
    }

    fn newline(&mut self) {
        assert!(self.reader.curr_char().map_or(false, |ch| ch.is_es_newline()));
        if self.reader.curr_char() == Some('\r') && self.reader.next_char() == Some('\n') {
            self.skip();
        }
        self.skip();
    }

    fn newline_into(&mut self, s: &mut String) {
        assert!(self.reader.curr_char().map_or(false, |ch| ch.is_es_newline()));
        if self.reader.curr_char() == Some('\r') && self.reader.next_char() == Some('\n') {
            s.push('\r');
            s.push('\n');
            self.skip();
            self.skip();
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

    fn skip(&mut self) {
        self.reader.skip();
    }

    fn eat(&mut self) -> Option<char> {
        let ch = self.reader.curr_char();
        self.skip();
        ch
    }

    fn skip_whitespace(&mut self) {
        while self.is_whitespace() {
            self.reader.skip();
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
