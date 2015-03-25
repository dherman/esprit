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

    // generic lexing utilities

    fn read(&mut self) -> char {
        let ch = self.reader.curr_char().unwrap();
        self.skip();
        ch
    }

    fn reread(&mut self, ch: char) -> char {
        assert!(self.peek() == Some(ch));
        self.skip();
        ch
    }

    fn peek(&mut self) -> Option<char> {
        self.reader.curr_char()
    }

    fn peek2(&mut self) -> (Option<char>, Option<char>) {
        (self.reader.curr_char(), self.reader.next_char())
    }

    fn skip(&mut self) {
        self.reader.skip();
    }

    fn skip2(&mut self) {
        self.skip();
        self.skip();
    }

    fn matches(&mut self, ch: char) -> bool {
        (self.peek() == Some(ch)) && { self.skip(); true }
    }

    fn skip_while<F>(&mut self, pred: &F)
      where F: Fn(char) -> bool
    {
        self.skip_until(&|ch| !pred(ch))
    }

    fn skip_until<F>(&mut self, pred: &F)
      where F: Fn(char) -> bool
    {
        loop {
            match self.peek() {
                Some(ch) if pred(ch) => return,
                None => return,
                _ => ()
            }
            self.skip();
        }
    }

    fn skip_until2<F>(&mut self, pred: &F)
      where F: Fn(char, char) -> bool
    {
        loop {
            match self.peek2() {
                (None, _) | (_, None) => return,
                (Some(curr), Some(next)) if pred(curr, next) => return,
                _ => { self.skip(); }
            }
        }
    }

    fn read_into_until<F>(&mut self, s: &mut String, pred: &F)
      where F: Fn(char) -> bool
    {
        loop {
            match self.peek() {
                Some(ch) if pred(ch) => return,
                Some(ch) => { s.push(ch); }
                None => return,
            }
        }
    }

    fn read_until_with<F, G>(&mut self, pred: &F, read: &mut G) -> Result<(), LexError>
      where F: Fn(char) -> bool,
            G: FnMut(&mut Self) -> Result<(), LexError>
    {
        loop {
            match self.peek() {
                Some(ch) if pred(ch) => return Ok(()),
                Some(ch) => { try!(read(self)); },
                None => return Ok(())
            }
        }
    }

    fn expect(&mut self, expected: char) -> Result<(), LexError> {
        match self.peek() {
            Some(ch) if ch == expected => (),
            Some(ch) => return Err(LexError::UnexpectedChar(ch)),
            None => return Err(LexError::UnexpectedEOF)
        }
        self.skip();
        Ok(())
    }

    // lexical grammar

    fn skip_newline(&mut self) {
        assert!(self.peek().map_or(false, |ch| ch.is_es_newline()));
        if self.peek2() == (Some('\r'), Some('\n')) {
            self.skip2();
            return;
        }
        self.skip();
    }

    fn read_newline_into(&mut self, s: &mut String) {
        assert!(self.peek().map_or(false, |ch| ch.is_es_newline()));
        if self.peek2() == (Some('\r'), Some('\n')) {
            s.push_str("\r\n");
            self.skip2();
            return;
        }
        s.push(self.read());
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(&|ch| ch.is_es_whitespace());
    }

    fn skip_line_comment(&mut self) {
        self.skip2();
        self.skip_until(&|ch| ch.is_es_newline());
    }

    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        self.skip_until2(&|curr, next| curr == '*' && next == '/');
        try!(self.expect('*'));
        try!(self.expect('/'));
        Ok(())
    }

    fn read_regexp(&mut self) -> Result<Token, LexError> {
        let mut s = String::new();
        self.reread('/');
        try!(self.read_until_with(&|ch| ch == '/', &mut |this| { this.read_regexp_char(&mut s) }));
        self.reread('/');
        Ok(Token::RegExp(s))
    }

    fn read_regexp_char(&mut self, s: &mut String) -> Result<(), LexError> {
        match self.peek() {
            Some('\\') => self.read_regexp_backslash(s),
            Some('[') => self.read_regexp_class(s),
            Some(ch) if ch.is_es_newline() => Err(LexError::UnexpectedChar(ch)),
            Some(ch) => { s.push(self.reread(ch)); Ok(()) },
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn read_regexp_backslash(&mut self, s: &mut String) -> Result<(), LexError> {
        s.push(self.reread('\\'));
        match self.peek() {
            Some(ch) if ch.is_es_newline() => Err(LexError::UnexpectedChar(ch)),
            Some(ch) => { s.push(self.reread(ch)); Ok(()) },
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn read_regexp_class(&mut self, s: &mut String) -> Result<(), LexError> {
        s.push(self.reread('['));
        try!(self.read_until_with(&|ch| ch == ']', &mut |this| { this.read_regexp_class_char(s) }));
        s.push(self.reread(']'));
        Ok(())
    }

    fn read_regexp_class_char(&mut self, s: &mut String) -> Result<(), LexError> {
        match self.peek() {
            Some('\\') => self.read_regexp_backslash(s),
            Some(ch) => { s.push(self.reread(ch)); Ok(()) },
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn read_decimal_digits_into(&mut self, s: &mut String) -> Result<(), LexError> {
        match self.peek() {
            Some(ch) if !ch.is_digit(10) => return Err(LexError::UnexpectedChar(ch)),
            None => return Err(LexError::UnexpectedEOF),
            _ => ()
        }
        let mut s = String::new();
        self.read_into_until(&mut s, &|ch| !ch.is_digit(10));
        Ok(())
    }

    fn read_decimal_digits(&mut self) -> Result<String, LexError> {
        let mut s = String::new();
        try!(self.read_decimal_digits_into(&mut s));
        Ok(s)
    }

    fn read_exp_part(&mut self) -> Result<Option<String>, LexError> {
        match self.peek() {
            Some('e') | Some('E') => {
                let mut s = String::new();
                s.push(self.read());
                match self.peek() {
                    Some('+') | Some('-') => { s.push(self.read()); }
                    _ => ()
                }
                try!(self.read_decimal_digits_into(&mut s));
                Ok(Some(s))
            }
            _ => Ok(None)
        }
    }

    fn read_decimal_int(&mut self) -> Result<String, LexError> {
        let mut s = String::new();
        match self.peek() {
            Some('0') => { s.push('0'); return Ok(s); }
            Some(ch) if ch.is_digit(10) => { s.push(self.reread(ch)); }
            Some(ch) => return Err(LexError::UnexpectedChar(ch)),
            None => return Err(LexError::UnexpectedEOF)
        }
        self.read_into_until(&mut s, &|ch| !ch.is_digit(10));
        Ok(s)
    }

    fn read_radix_int<F, G>(&mut self, radix: u32, pred: &F, cons: &G) -> Result<Token, LexError>
      where F: Fn(char) -> bool,
            G: Fn(char, String) -> Token
    {
        assert!(self.reader.curr_char().is_some());
        assert!(self.reader.next_char().is_some());
        let mut s = String::new();
        self.skip();
        let flag = self.read();
        try!(self.read_digit_into(&mut s, radix, pred));
        self.read_into_until(&mut s, pred);
        Ok(cons(flag, s))
    }

    fn read_hex_int(&mut self) -> Result<Token, LexError> {
        self.read_radix_int(16, &|ch| ch.is_es_hex_digit(), &Token::HexInt)
    }

    fn read_oct_int(&mut self) -> Result<Token, LexError> {
        self.read_radix_int(8, &|ch| ch.is_es_oct_digit(), &|ch, s| Token::OctalInt(Some(ch), s))
    }

    fn read_bin_int(&mut self) -> Result<Token, LexError> {
        self.read_radix_int(2, &|ch| ch.is_es_bin_digit(), &Token::BinaryInt)
    }

    fn read_deprecated_oct_int(&mut self) -> Token {
        let mut s = String::new();
        self.read_into_until(&mut s, &|ch| ch.is_digit(10));
        if s.chars().all(|ch| ch.is_es_oct_digit()) {
            Token::OctalInt(None, s)
        } else {
            Token::DecimalInt(s)
        }
    }

    fn read_number(&mut self) -> Result<Token, LexError> {
        match self.peek2() {
            (Some('0'), Some('x')) | (Some('0'), Some('X')) => self.read_hex_int(),
            (Some('0'), Some('o')) | (Some('0'), Some('O')) => self.read_oct_int(),
            (Some('0'), Some('b')) | (Some('0'), Some('B')) => self.read_bin_int(),
            (Some('0'), Some(ch)) if ch.is_digit(10) => return Ok({
                self.skip();
                self.read_deprecated_oct_int()
            }),
            (Some('.'), _) => {
                let frac = try!(self.read_decimal_digits());
                let exp = try!(self.read_exp_part());
                Ok(Token::Float(None, Some(frac), exp))
            }
            (Some(ch), _) if ch.is_digit(10) => {
                let pos = try!(self.read_decimal_int());
                let (dot, frac) = if self.matches('.') {
                    (true, match self.peek() {
                        Some(ch) if ch.is_digit(10) => Some(try!(self.read_decimal_digits())),
                        _ => None
                    })
                } else {
                    (false, None)
                };
                let exp = try!(self.read_exp_part());
                Ok(if dot {
                    Token::Float(Some(pos), frac, exp)
                } else {
                    Token::DecimalInt(pos)
                })
            }
            (Some(ch), _) => Err(LexError::UnexpectedChar(ch)),
            (None, _) => Err(LexError::UnexpectedEOF)
        }
    }

    fn read_string(&mut self) -> Result<Token, LexError> {
        let mut s = String::new();
        loop {
            assert!(self.peek().is_some());
            let quote = self.read();
            self.read_into_until(&mut s, &|ch| {
                ch == quote ||
                ch == '\\' ||
                ch.is_es_newline()
            });
            match self.peek() {
                Some('\\') => { try!(self.read_string_escape(&mut s)); },
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

    fn read_unicode_escape_seq(&mut self, s: &mut String) -> Result<u32, LexError> {
        self.skip();
        if self.matches('{') {
            s.push('{');
            let mut digits = Vec::with_capacity(8);
            digits.push(try!(self.read_hex_digit_into(s)));
            try!(self.read_until_with(&|ch| ch == '}', &mut |this| {
                digits.push(try!(this.read_hex_digit_into(s)));
                Ok(())
            }));
            s.push(self.reread('}'));
            Ok(add_digits(digits, 16))
        } else {
            let mut place = 0x1000;
            let mut code_point = 0;
            for i in 0..4 {
                code_point += try!(self.read_hex_digit_into(s)) * place;
                place >>= 4;
            }
            Ok(code_point)
        }
    }

    fn read_string_escape(&mut self, s: &mut String) -> Result<(), LexError> {
        s.push(self.read());
        match self.peek() {
            Some('0') => {
                self.skip();
                for i in 0..3 {
                    match self.peek() {
                        Some(ch) if ch.is_digit(8) => { s.push(ch); },
                        _ => { break; }
                    }
                }
            },
            Some(ch) if ch.is_es_single_escape_char() => {
                s.push(self.read());
            },
            Some('x') => {
                s.push(self.reread('x'));
                try!(self.read_hex_digit_into(s));
                try!(self.read_hex_digit_into(s));
            },
            Some('u') => {
                try!(self.read_unicode_escape_seq(s));
            },
            Some(ch) if ch.is_es_newline() => {
                self.read_newline_into(s);
            },
            Some(ch) => {
                s.push(self.reread(ch));
            },
            None => () // error will be reported from caller
        }
        Ok(())
    }

    fn read_digit_into<F>(&mut self, s: &mut String, radix: u32, pred: &F) -> Result<u32, LexError>
      where F: Fn(char) -> bool
    {
        match self.peek() {
            Some(ch) if pred(ch) => {
                s.push(self.reread(ch));
                assert!(ch.is_digit(radix));
                Ok(ch.to_digit(radix).unwrap())
            },
            Some(ch) => Err(LexError::InvalidDigit(ch)),
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn read_hex_digit_into(&mut self, s: &mut String) -> Result<u32, LexError> {
        self.read_digit_into(s, 16, &|ch| ch.is_es_hex_digit())
    }

    fn read_word(&mut self) -> Result<Token, LexError> {
        assert!(self.peek().map_or(false, |ch| ch.is_es_identifier_start()));
        let mut s = String::new();
        s.push(self.read());
        try!(self.read_until_with(&|ch| ch == '\\' || ch.is_es_identifier_continue(), &mut |this| {
            match this.read() {
                '\\' => this.read_word_escape(&mut s),
                ch => { s.push(this.reread(ch)); Ok(()) }
            }
        }));
        match self.reserved.get(&s[..]) {
            Some(word) => Ok(Token::Reserved(*word)),
            None => Ok(Token::Identifier(s))
        }
    }

    fn read_word_escape(&mut self, s: &mut String) -> Result<(), LexError> {
        try!(self.expect('u'));
        let mut dummy = String::new();
        self.skip();
        let code_point = try!(self.read_unicode_escape_seq(&mut dummy));
        match char::from_u32(code_point) {
            Some(ch) => { s.push(ch); Ok(()) },
            None => Err(LexError::IllegalUnicode(code_point))
        }
    }

    fn read_next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();
        println!("inspecting {:?}", self.peek());
        loop {
            match self.peek2() {
                (Some('/'), Some('/')) => { self.skip_line_comment(); },
                (Some('/'), Some('*')) => { try!(self.skip_block_comment()); },
                (Some('/'), _) if !self.cx.get().is_operator() => return self.read_regexp(),
                (Some('/'), Some('=')) => return Ok({ self.skip2(); Token::SlashAssign }),
                (Some('/'), _) => return Ok({ self.skip(); Token::Slash }),
                (Some('.'), Some(ch)) if ch.is_digit(10) => return self.read_number(),
                (Some('.'), _) => return Ok({ self.skip(); Token::Dot }),
                (Some('{'), _) => return Ok({ self.skip(); Token::LBrace }),
                (Some('}'), _) => return Ok({ self.skip(); Token::RBrace }),
                (Some('['), _) => return Ok({ self.skip(); Token::LBrack }),
                (Some(']'), _) => return Ok({ self.skip(); Token::RBrack }),
                (Some('('), _) => return Ok({ self.skip(); Token::LParen }),
                (Some(')'), _) => return Ok({ self.skip(); Token::RParen }),
                (Some(';'), _) => return Ok({ self.skip(); Token::Semi }),
                (Some(':'), _) => return Ok({ self.skip(); Token::Colon }),
                (Some(','), _) => return Ok({ self.skip(); Token::Comma }),
                (Some('<'), Some('<')) => return Ok({
                    self.skip2();
                    if self.matches('=') { Token::LShiftAssign } else { Token::LShift }
                }),
                (Some('<'), Some('=')) => return Ok({ self.skip2(); Token::LEq }),
                (Some('<'), _) => return Ok({ self.skip(); Token::LAngle }),
                (Some('>'), Some('>')) => return Ok({
                    self.skip2();
                    match self.peek2() {
                        (Some('>'), Some('=')) => return Ok({ self.skip2(); Token::URShiftAssign }),
                        (Some('>'), _) => return Ok({ self.skip(); Token::URShift }),
                        (Some('='), _) => return Ok({ self.skip(); Token::RShiftAssign }),
                        _ => return Ok(Token::RShift)
                    }
                }),
                (Some('>'), Some('=')) => return Ok({ self.skip2(); Token::GEq }),
                (Some('>'), _) => return Ok({ self.skip(); Token::RAngle }),
                (Some('='), Some('=')) => return Ok({
                    self.skip2();
                    if self.matches('=') { Token::StrictEq } else { Token::Eq }
                }),
                (Some('='), _) => return Ok({ self.skip(); Token::Assign }),
                (Some('+'), Some('+')) => return Ok({ self.skip2(); Token::Inc }),
                (Some('+'), Some('=')) => return Ok({ self.skip2(); Token::PlusAssign }),
                (Some('+'), _) => return Ok({ self.skip(); Token::Plus }),
                (Some('-'), Some('-')) => return Ok({ self.skip2(); Token::Dec }),
                (Some('-'), Some('=')) => return Ok({ self.skip2(); Token::MinusAssign }),
                (Some('-'), _) => return Ok({ self.skip(); Token::Minus }),
                (Some('*'), Some('=')) => return Ok({ self.skip2(); Token::StarAssign }),
                (Some('*'), _) => return Ok({ self.skip(); Token::Star }),
                (Some('%'), Some('=')) => return Ok({ self.skip2(); Token::ModAssign }),
                (Some('%'), _) => return Ok({ self.skip(); Token::Mod }),
                (Some('^'), Some('=')) => return Ok({ self.skip2(); Token::BitXorAssign }),
                (Some('^'), _) => return Ok({ self.skip(); Token::BitXor }),
                (Some('&'), Some('&')) => return Ok({ self.skip2(); Token::LogicalAnd }),
                (Some('&'), Some('=')) => return Ok({ self.skip2(); Token::BitAndAssign }),
                (Some('&'), _) => return Ok({ self.skip(); Token::BitAnd }),
                (Some('|'), Some('|')) => return Ok({ self.skip2(); Token::LogicalOr }),
                (Some('|'), Some('=')) => return Ok({ self.skip2(); Token::BitOrAssign }),
                (Some('|'), _) => return Ok({ self.skip(); Token::BitOr }),
                (Some('~'), _) => { self.skip(); return Ok(Token::Tilde) },
                (Some('!'), Some('=')) => return Ok({
                    self.skip2();
                    if self.matches('=') { Token::StrictNEq } else { Token::NEq }
                }),
                (Some('!'), _) => return Ok({ self.skip(); Token::Bang }),
                (Some('?'), _) => { self.skip(); return Ok(Token::Question) },
                (Some('"'), _) | (Some('\''), _) => return self.read_string(),
                (Some(ch), _) if ch.is_es_newline() => {
                    self.skip_newline();
                    if self.cx.get().is_asi_possible() {
                        return Ok(Token::Newline);
                    }
                }
                (Some(ch), _) if ch.is_digit(10) => return self.read_number(),
                (Some(ch), _) if ch.is_es_identifier_start() => return self.read_word(),
                (Some(ch), _) => return Err(LexError::UnexpectedChar(ch)),
                (None, _) => return Ok(Token::EOF)
            }
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
