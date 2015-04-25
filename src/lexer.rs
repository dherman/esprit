use std::collections::HashMap;
use std::char;

use track::*;
use token::{Token, TokenData, Exp, CharCase, Sign, NumberLiteral, Radix};

use std::cell::Cell;
use std::rc::Rc;
use context::Context;
use context::Mode::*;
use token::Word;
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
                temp_map.insert($key, Word::$val);
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

struct SpanTracker {
    start: Posn
}

impl SpanTracker {
    fn end<I>(&self, lexer: &Lexer<I>, value: TokenData) -> Token
      where I: Iterator<Item=char>
    {
        let end = lexer.posn();
        Token::new(&self.start, &end, value)
    }
}

pub struct Lexer<I> {
    reader: Reader<I>,
    cx: Rc<Cell<Context>>,
    lookahead: TokenBuffer,
    reserved: HashMap<&'static str, Word>,
    strict_reserved: HashMap<&'static str, Word>
}

impl<I> Lexer<I> where I: Iterator<Item=char> {
    // constructor

    pub fn new(chars: I, cx: Rc<Cell<Context>>) -> Lexer<I> {
        Lexer {
            reader: Reader::new(chars),
            cx: cx,
            lookahead: TokenBuffer::new(),
            reserved: reserved_words![
                // Pseudo-reserved words
                ("arguments",  Arguments),  ("eval",       Eval),

                // Keyword
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
                ("while",      While),      ("with",       With),

                // FutureReservedWord
                ("enum",       Enum)
            ],
            strict_reserved: reserved_words![
                // FutureReservedWord (strict mode)
                ("implements", Implements), ("interface",  Interface),  ("package",  Package),
                ("private",    Private),    ("protected",  Protected),  ("public",   Public),

                // Pseudo-reserved words (strict mode)
                ("let",        Let),        ("static",     Static),     ("yield",    Yield)
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

    pub fn skip_token(&mut self) -> Result<(), LexError> {
        try!(self.read_token());
        Ok(())
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

    // source location

    pub fn posn(&self) -> Posn {
        self.reader.curr_posn()
    }

    fn start(&self) -> SpanTracker {
        SpanTracker { start: self.posn() }
    }

    // generic lexing utilities

    fn read(&mut self) -> char {
        let ch = self.reader.curr_char().unwrap();
        self.skip();
        ch
    }

    fn reread(&mut self, ch: char) -> char {
        debug_assert!(self.peek() == Some(ch));
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
                Some(ch) => { s.push(self.reread(ch)); }
                None => return,
            }
        }
    }

    fn read_into_until2<F>(&mut self, s: &mut String, pred: &F)
      where F: Fn(char, char) -> bool
    {
        loop {
            match self.peek2() {
                (None, _) | (_, None) => return,
                (Some(curr), Some(next)) if pred(curr, next) => return,
                _ => { s.push(self.read()); }
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
                Some(_) => { try!(read(self)); }
                None => return Ok(())
            }
        }
    }

    fn fail_if<F>(&mut self, pred: &F) -> Result<(), LexError>
      where F: Fn(char) -> bool
    {
        match self.peek() {
            Some(ch) if pred(ch) => Err(LexError::UnexpectedChar(ch)),
            _ => Ok(())
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

    fn skip_newlines(&mut self) {
        debug_assert!(self.peek().map_or(false, |ch| ch.is_es_newline()));
        loop {
            match self.peek2() {
                (Some('\r'), Some('\n'))            => { self.skip2(); }
                (Some(ch), _) if ch.is_es_newline() => { self.skip(); }
                _                                   => { break; }
            }
        }
    }

    fn read_newline_into(&mut self, s: &mut String) {
        debug_assert!(self.peek().map_or(false, |ch| ch.is_es_newline()));
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

    fn read_line_comment(&mut self) -> Token {
        let span = self.start();
        self.skip2();
        let mut s = String::new();
        self.read_into_until(&mut s, &|ch| ch.is_es_newline());
        span.end(self, TokenData::LineComment(s))
    }

    fn skip_line_comment(&mut self) {
        self.skip2();
        self.skip_until(&|ch| ch.is_es_newline());
    }

    fn read_block_comment(&mut self) -> Result<Token, LexError> {
        let span = self.start();
        let mut s = String::new();
        self.reread('/');
        self.reread('*');
        self.read_into_until2(&mut s, &|curr, next| curr == '*' && next == '/');
        try!(self.expect('*'));
        try!(self.expect('/'));
        Ok(span.end(self, TokenData::BlockComment(s)))
    }

    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        self.skip_until2(&|curr, next| curr == '*' && next == '/');
        try!(self.expect('*'));
        try!(self.expect('/'));
        Ok(())
    }

    fn read_regexp(&mut self) -> Result<Token, LexError> {
        let span = self.start();
        let mut s = String::new();
        self.reread('/');
        try!(self.read_until_with(&|ch| ch == '/', &mut |this| { this.read_regexp_char(&mut s) }));
        self.reread('/');
        let flags = try!(self.read_word_parts());
        Ok(span.end(self, TokenData::RegExp(s, flags.chars().collect())))
    }

    fn read_regexp_char(&mut self, s: &mut String) -> Result<(), LexError> {
        match self.peek() {
            Some('\\') => self.read_regexp_backslash(s),
            Some('[') => self.read_regexp_class(s),
            Some(ch) if ch.is_es_newline() => Err(LexError::UnexpectedChar(ch)),
            Some(ch) => { s.push(self.reread(ch)); Ok(()) }
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn read_regexp_backslash(&mut self, s: &mut String) -> Result<(), LexError> {
        s.push(self.reread('\\'));
        match self.peek() {
            Some(ch) if ch.is_es_newline() => Err(LexError::UnexpectedChar(ch)),
            Some(ch) => { s.push(self.reread(ch)); Ok(()) }
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
            Some(ch) => { s.push(self.reread(ch)); Ok(()) }
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn read_decimal_digits_into(&mut self, s: &mut String) -> Result<(), LexError> {
        match self.peek() {
            Some(ch) if !ch.is_digit(10) => return Err(LexError::UnexpectedChar(ch)),
            None => return Err(LexError::UnexpectedEOF),
            _ => ()
        }
        self.read_into_until(s, &|ch| !ch.is_digit(10));
        Ok(())
    }

    fn read_decimal_digits(&mut self) -> Result<String, LexError> {
        let mut s = String::new();
        try!(self.read_decimal_digits_into(&mut s));
        Ok(s)
    }

    fn read_exp_part(&mut self) -> Result<Option<Exp>, LexError> {
        let e = match self.peek() {
            Some('e') => CharCase::LowerCase,
            Some('E') => CharCase::UpperCase,
            _ => { return Ok(None); }
        };
        self.skip();
        let sign = match self.peek() {
            Some('+') => { self.skip(); Some(Sign::Plus) }
            Some('-') => { self.skip(); Some(Sign::Minus) }
            _ => None
        };
        let mut value = String::new();
        try!(self.read_decimal_digits_into(&mut value));
        Ok(Some(Exp { e: e, sign: sign, value: value }))
    }

    fn read_decimal_int(&mut self) -> Result<String, LexError> {
        let mut s = String::new();
        match self.peek() {
            Some('0') => { s.push(self.reread('0')); return Ok(s); }
            Some(ch) if ch.is_digit(10) => { s.push(self.reread(ch)); }
            Some(ch) => return Err(LexError::UnexpectedChar(ch)),
            None => return Err(LexError::UnexpectedEOF)
        }
        self.read_into_until(&mut s, &|ch| !ch.is_digit(10));
        Ok(s)
    }

    fn read_radix_int<F, G>(&mut self, radix: u32, pred: &F, cons: &G) -> Result<Token, LexError>
      where F: Fn(char) -> bool,
            G: Fn(CharCase, String) -> TokenData
    {
        debug_assert!(self.reader.curr_char() == Some('0'));
        debug_assert!(self.reader.next_char().map_or(false, |ch| ch.is_alphabetic()));
        let span = self.start();
        let mut s = String::new();
        self.skip();
        let flag = if self.read().is_lowercase() {
            CharCase::LowerCase
        } else {
            CharCase::UpperCase
        };
        try!(self.read_digit_into(&mut s, radix, pred));
        self.read_into_until(&mut s, &|ch| !pred(ch));
        Ok(span.end(self, cons(flag, s)))
    }

    fn read_hex_int(&mut self) -> Result<Token, LexError> {
        self.read_radix_int(16, &|ch| ch.is_es_hex_digit(), &|cc, s| {
            TokenData::Number(NumberLiteral::RadixInt(Radix::Hex(cc), s))
        })
    }

    fn read_oct_int(&mut self) -> Result<Token, LexError> {
        self.read_radix_int(8, &|ch| ch.is_es_oct_digit(), &|cc, s| {
            TokenData::Number(NumberLiteral::RadixInt(Radix::Oct(Some(cc)), s))
        })
    }

    fn read_bin_int(&mut self) -> Result<Token, LexError> {
        self.read_radix_int(2, &|ch| ch.is_es_bin_digit(), &|cc, s| {
            TokenData::Number(NumberLiteral::RadixInt(Radix::Bin(cc), s))
        })
    }

    fn read_deprecated_oct_int(&mut self) -> Token {
        let span = self.start();
        self.skip();
        let mut s = String::new();
        self.read_into_until(&mut s, &|ch| !ch.is_digit(10));
        span.end(self, TokenData::Number(if s.chars().all(|ch| ch.is_es_oct_digit()) {
            NumberLiteral::RadixInt(Radix::Oct(None), s)
        } else {
            NumberLiteral::DecimalInt(format!("0{}", s), None)
        }))
    }

    fn read_number(&mut self) -> Result<Token, LexError> {
        let result = try!(match self.peek2() {
            (Some('0'), Some('x')) | (Some('0'), Some('X')) => self.read_hex_int(),
            (Some('0'), Some('o')) | (Some('0'), Some('O')) => self.read_oct_int(),
            (Some('0'), Some('b')) | (Some('0'), Some('B')) => self.read_bin_int(),
            (Some('0'), Some(ch)) if ch.is_digit(10) => Ok(self.read_deprecated_oct_int()),
            (Some('.'), _) => {
                let span = self.start();
                self.skip();
                let frac = try!(self.read_decimal_digits());
                let exp = try!(self.read_exp_part());
                Ok(span.end(self, TokenData::Number(NumberLiteral::Float(None, Some(frac), exp))))
            }
            (Some(ch), _) if ch.is_digit(10) => {
                let span = self.start();
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
                Ok(span.end(self, TokenData::Number(if dot {
                    NumberLiteral::Float(Some(pos), frac, exp)
                } else {
                    NumberLiteral::DecimalInt(pos, exp)
                })))
            }
            (Some(ch), _) => Err(LexError::UnexpectedChar(ch)),
            (None, _) => Err(LexError::UnexpectedEOF)
        });
        try!(self.fail_if(&|ch| ch.is_es_identifier_start() || ch.is_digit(10)));
        Ok(result)
    }

    fn read_string(&mut self) -> Result<Token, LexError> {
        debug_assert!(self.peek().is_some());
        let span = self.start();
        let mut s = String::new();
        let quote = self.read();
        loop {
            self.read_into_until(&mut s, &|ch| {
                ch == quote ||
                ch == '\\' ||
                ch.is_es_newline()
            });
            match self.peek() {
                Some('\\') => { try!(self.read_string_escape(&mut s)); }
                Some(ch) if ch.is_es_newline() => {
                    return Err(LexError::UnexpectedChar(ch));
                }
                Some(_) => { self.skip(); break; }
                None => return Err(LexError::UnexpectedEOF)
            }
        }
        Ok(span.end(self, TokenData::String(s)))
    }

    fn read_unicode_escape_seq(&mut self, s: &mut String) -> Result<u32, LexError> {
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
            for _ in 0..4 {
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
                for _ in 0..3 {
                    match self.peek() {
                        Some(ch) if ch.is_digit(8) => { s.push(ch); },
                        _ => { break; }
                    }
                }
            }
            Some(ch) if ch.is_es_single_escape_char() => {
                s.push(self.read());
            }
            Some('x') => {
                s.push(self.reread('x'));
                try!(self.read_hex_digit_into(s));
                try!(self.read_hex_digit_into(s));
            }
            Some('u') => {
                s.push(self.reread('u'));
                try!(self.read_unicode_escape_seq(s));
            }
            Some(ch) if ch.is_es_newline() => {
                self.read_newline_into(s);
            }
            Some(ch) => {
                s.push(self.reread(ch));
            }
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
                debug_assert!(ch.is_digit(radix));
                Ok(ch.to_digit(radix).unwrap())
            },
            Some(ch) => Err(LexError::InvalidDigit(ch)),
            None => Err(LexError::UnexpectedEOF)
        }
    }

    fn read_hex_digit_into(&mut self, s: &mut String) -> Result<u32, LexError> {
        self.read_digit_into(s, 16, &|ch| ch.is_es_hex_digit())
    }

    fn read_word_parts(&mut self) -> Result<String, LexError> {
        let mut s = String::new();
        try!(self.read_until_with(&|ch| ch != '\\' && !ch.is_es_identifier_continue(), &mut |this| {
            match this.read() {
                '\\' => this.read_word_escape(&mut s),
                ch => { s.push(ch); Ok(()) }
            }
        }));
        Ok(s)
    }

    fn read_word(&mut self) -> Result<Token, LexError> {
        debug_assert!(self.peek().map_or(false, |ch| ch == '\\' || ch.is_es_identifier_start()));
        let span = self.start();
        let s = try!(self.read_word_parts());
        if s.len() == 0 {
            match self.peek() {
                Some(ch) => { return Err(LexError::UnexpectedChar(ch)); }
                None => { return Err(LexError::UnexpectedEOF); }
            }
        }
        Ok(span.end(self, match (self.reserved.get(&s[..]), self.cx.get()) {
            (Some(word), _) => TokenData::Reserved(*word),
            (None, Context { mode: Sloppy, generator: true, .. }) if s == "yield" => {
                TokenData::Reserved(Word::Yield)
            }
            (None, Context { mode: Sloppy, .. }) => TokenData::Identifier(s),
            (None, Context { mode: Module, .. }) if s == "await" => {
                TokenData::Reserved(Word::Await)
            }
            (None, Context { mode: Strict, .. }) | (None, Context { mode: Module, .. }) => {
                match self.strict_reserved.get(&s[..]) {
                    Some(word) => TokenData::Reserved(*word),
                    None => TokenData::Identifier(s)
                }
            }
        }))
    }

    fn read_word_escape(&mut self, s: &mut String) -> Result<(), LexError> {
        try!(self.expect('u'));
        let mut dummy = String::new();
        let code_point = try!(self.read_unicode_escape_seq(&mut dummy));
        match char::from_u32(code_point) {
            Some(ch) => { s.push(ch); Ok(()) }
            None => Err(LexError::IllegalUnicode(code_point))
        }
    }

    fn read_punc(&mut self, value: TokenData) -> Token {
        let span = self.start();
        self.skip();
        span.end(self, value)
    }

    fn read_punc2(&mut self, value: TokenData) -> Token {
        let span = self.start();
        self.skip2();
        span.end(self, value)
    }

    fn read_punc2_3(&mut self, ch: char, value2: TokenData, value3: TokenData) -> Token {
        let span = self.start();
        self.skip2();
        let value = if self.matches(ch) { value3 } else { value2 };
        span.end(self, value)
    }

    fn read_next_token(&mut self) -> Result<Token, LexError> {
        loop {
            match self.peek2() {
                (Some(ch), _) if ch.is_es_whitespace() => { self.skip_whitespace() }
                (Some('/'), Some('/')) => {
                    if self.cx.get().comments {
                        return Ok(self.read_line_comment());
                    } else {
                        self.skip_line_comment();
                    }
                }
                (Some('/'), Some('*')) => {
                    if self.cx.get().comments {
                        return self.read_block_comment();
                    } else {
                        try!(self.skip_block_comment());
                    }
                }
                (Some('/'), _) if !self.cx.get().operator => return self.read_regexp(),
                (Some('/'), Some('=')) => return Ok(self.read_punc2(TokenData::SlashAssign)),
                (Some('/'), _) => return Ok(self.read_punc(TokenData::Slash)),
                (Some('.'), Some(ch)) if ch.is_digit(10) => return self.read_number(),
                (Some('.'), _) => return Ok(self.read_punc(TokenData::Dot)),
                (Some('{'), _) => return Ok(self.read_punc(TokenData::LBrace)),
                (Some('}'), _) => return Ok(self.read_punc(TokenData::RBrace)),
                (Some('['), _) => return Ok(self.read_punc(TokenData::LBrack)),
                (Some(']'), _) => return Ok(self.read_punc(TokenData::RBrack)),
                (Some('('), _) => return Ok(self.read_punc(TokenData::LParen)),
                (Some(')'), _) => return Ok(self.read_punc(TokenData::RParen)),
                (Some(';'), _) => return Ok(self.read_punc(TokenData::Semi)),
                (Some(':'), _) => return Ok(self.read_punc(TokenData::Colon)),
                (Some(','), _) => return Ok(self.read_punc(TokenData::Comma)),
                (Some('<'), Some('<')) => return Ok(self.read_punc2_3('=', TokenData::LShift, TokenData::LShiftAssign)),
                (Some('<'), Some('=')) => return Ok(self.read_punc2(TokenData::LEq)),
                (Some('<'), _) => return Ok(self.read_punc(TokenData::LAngle)),
                (Some('>'), Some('>')) => return Ok({
                    let span = self.start();
                    self.skip2();
                    let value = match self.peek2() {
                        (Some('>'), Some('=')) => { self.skip2(); TokenData::URShiftAssign }
                        (Some('>'), _) => { self.skip(); TokenData::URShift }
                        (Some('='), _) => { self.skip(); TokenData::RShiftAssign }
                        _ => TokenData::RShift
                    };
                    span.end(self, value)
                }),
                (Some('>'), Some('=')) => return Ok(self.read_punc2(TokenData::GEq)),
                (Some('>'), _) => return Ok(self.read_punc(TokenData::RAngle)),
                (Some('='), Some('=')) => return Ok(self.read_punc2_3('=', TokenData::Eq, TokenData::StrictEq)),
                (Some('='), _) => return Ok(self.read_punc(TokenData::Assign)),
                (Some('+'), Some('+')) => return Ok(self.read_punc2(TokenData::Inc)),
                (Some('+'), Some('=')) => return Ok(self.read_punc2(TokenData::PlusAssign)),
                (Some('+'), _) => return Ok(self.read_punc(TokenData::Plus)),
                (Some('-'), Some('-')) => return Ok(self.read_punc2(TokenData::Dec)),
                (Some('-'), Some('=')) => return Ok(self.read_punc2(TokenData::MinusAssign)),
                (Some('-'), _) => return Ok(self.read_punc(TokenData::Minus)),
                (Some('*'), Some('=')) => return Ok(self.read_punc2(TokenData::StarAssign)),
                (Some('*'), _) => return Ok(self.read_punc(TokenData::Star)),
                (Some('%'), Some('=')) => return Ok(self.read_punc2(TokenData::ModAssign)),
                (Some('%'), _) => return Ok(self.read_punc(TokenData::Mod)),
                (Some('^'), Some('=')) => return Ok(self.read_punc2(TokenData::BitXorAssign)),
                (Some('^'), _) => return Ok(self.read_punc(TokenData::BitXor)),
                (Some('&'), Some('&')) => return Ok(self.read_punc2(TokenData::LogicalAnd)),
                (Some('&'), Some('=')) => return Ok(self.read_punc2(TokenData::BitAndAssign)),
                (Some('&'), _) => return Ok(self.read_punc(TokenData::BitAnd)),
                (Some('|'), Some('|')) => return Ok(self.read_punc2(TokenData::LogicalOr)),
                (Some('|'), Some('=')) => return Ok(self.read_punc2(TokenData::BitOrAssign)),
                (Some('|'), _) => return Ok(self.read_punc(TokenData::BitOr)),
                (Some('~'), _) => return Ok(self.read_punc(TokenData::Tilde)),
                (Some('!'), Some('=')) => return Ok(self.read_punc2_3('=', TokenData::NEq, TokenData::StrictNEq)),
                (Some('!'), _) => return Ok(self.read_punc(TokenData::Bang)),
                (Some('?'), _) => return Ok(self.read_punc(TokenData::Question)),
                (Some('"'), _) | (Some('\''), _) => return self.read_string(),
                (Some(ch), _) if ch.is_es_newline() => {
                    let span = self.start();
                    self.skip_newlines();
                    if self.cx.get().newlines {
                        return Ok(span.end(self, TokenData::Newline));
                    }
                }
                (Some(ch), _) if ch.is_digit(10) => return self.read_number(),
                (Some(ch), _) if ch.is_es_identifier_start() => return self.read_word(),
                (Some('\\'), _) => return self.read_word(),
                (Some(ch), _) => return Err(LexError::UnexpectedChar(ch)),
                (None, _) => {
                    let here = self.posn();
                    return Ok(Token::new(&here, &here, TokenData::EOF))
                }
            }
        }
    }
}

impl<I> Iterator for Lexer<I> where I: Iterator<Item=char> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.read_token() {
            Ok(Token { value: TokenData::EOF, .. }) => None,
            Ok(t) => Some(t),
            Err(_) => None
        }
    }
}

#[cfg(test)]
mod tests {

    use test::{deserialize_lexer_tests, LexerTest};
    use lexer::{Lexer, LexError};
    use context::Context;
    use token::{Token, TokenData};
    use std::cell::Cell;
    use std::rc::Rc;
    use std::str::Chars;

    fn lex2(source: &String, context: Context) -> Result<(Token, Token), LexError> {
        let chars = source.chars();
        let cx = Rc::new(Cell::new(context));
        let mut lexer = Lexer::new(chars, cx.clone());
        Ok((try!(lexer.read_token()), try!(lexer.read_token())))
    }

    fn assert_test2(expected: &Result<TokenData, String>, expected_next: TokenData, actual: Result<(Token, Token), LexError>) {
        match (expected, &actual) {
            (&Ok(ref expected), &Ok((Token { value: ref actual, .. }, Token { value: ref actual_next, .. }))) => {
                assert_eq!(expected, actual);
                assert_eq!(&expected_next, actual_next);
            }
            (&Ok(_), &Err(ref err)) => {
                panic!("unexpected lexer error: {:?}", err);
            }
            (&Err(_), &Ok(_)) => {
                panic!("unexpected token, expected error");
            }
            (&Err(_), &Err(_)) => { }
        }
    }

    #[test]
    pub fn go() {
        let tests = deserialize_lexer_tests(include_str!("../tests/lexer/tests.json"));
        for LexerTest { source, context, expected } in tests {
            assert_test2(&expected, TokenData::EOF, lex2(&source, context));
            assert_test2(&expected, TokenData::EOF, lex2(&format!("{} ", source), context));
            assert_test2(&expected, TokenData::EOF, lex2(&format!(" {}", source), context));
            assert_test2(&expected, TokenData::EOF, lex2(&format!(" {} ", source), context));
            assert_test2(&expected, TokenData::Semi, lex2(&format!("{};", source), context));
        }
    }

}
