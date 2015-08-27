use std::fmt;
use std::fmt::{Display, Formatter};
use std::str::FromStr;
use joker::track::*;
use joker::token::{TokenData, Token};
use joker::word::Reserved;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Semi {
    Inserted,
    Explicit(Option<Posn>)
}

impl Untrack for Semi {
    fn untrack(&mut self) {
        *self = Semi::Explicit(None);
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnopTag {
    Minus,
    Plus,
    Not,
    BitNot,
    Typeof,
    Void,
    Delete
}

impl FromStr for UnopTag {
    type Err = ();

    fn from_str(s: &str) -> Result<UnopTag, ()> {
        Ok(match s {
            "-"      => UnopTag::Minus,
            "+"      => UnopTag::Plus,
            "!"      => UnopTag::Not,
            "~"      => UnopTag::BitNot,
            "typeof" => UnopTag::Typeof,
            "void"   => UnopTag::Void,
            "delete" => UnopTag::Delete,
            _        => { return Err(()); }
        })
    }
}

pub type Unop = Tracked<UnopTag>;

impl Untrack for UnopTag {
    fn untrack(&mut self) { }
}

pub trait Precedence {
    fn precedence(&self) -> u32;
}

impl<T: Precedence> Precedence for Tracked<T> {
    fn precedence(&self) -> u32 {
        self.value.precedence()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinopTag {
    Eq,
    NEq,
    StrictEq,
    StrictNEq,
    Lt,
    LEq,
    Gt,
    GEq,
    LShift,
    RShift,
    URShift,
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    BitOr,
    BitXor,
    BitAnd,
    In,
    Instanceof,
}

impl FromStr for BinopTag {
    type Err = ();

    fn from_str(s: &str) -> Result<BinopTag, ()> {
        Ok(match s {
            "=="         => BinopTag::Eq,
            "!="         => BinopTag::NEq,
            "==="        => BinopTag::StrictEq,
            "!=="        => BinopTag::StrictNEq,
            "<"          => BinopTag::Lt,
            "<="         => BinopTag::LEq,
            ">"          => BinopTag::Gt,
            ">="         => BinopTag::GEq,
            "<<"         => BinopTag::LShift,
            ">>"         => BinopTag::RShift,
            ">>>"        => BinopTag::URShift,
            "+"          => BinopTag::Plus,
            "-"          => BinopTag::Minus,
            "*"          => BinopTag::Times,
            "/"          => BinopTag::Div,
            "%"          => BinopTag::Mod,
            "|"          => BinopTag::BitOr,
            "^"          => BinopTag::BitXor,
            "&"          => BinopTag::BitAnd,
            "in"         => BinopTag::In,
            "instanceof" => BinopTag::Instanceof,
            _            => { return Err(()); }
        })
    }
}

impl Precedence for BinopTag {
    fn precedence(&self) -> u32 {
        match *self {
            BinopTag::Eq         => 7,
            BinopTag::NEq        => 7,
            BinopTag::StrictEq   => 7,
            BinopTag::StrictNEq  => 7,
            BinopTag::Lt         => 8,
            BinopTag::LEq        => 8,
            BinopTag::Gt         => 8,
            BinopTag::GEq        => 8,
            BinopTag::LShift     => 9,
            BinopTag::RShift     => 9,
            BinopTag::URShift    => 9,
            BinopTag::Plus       => 10,
            BinopTag::Minus      => 10,
            BinopTag::Times      => 11,
            BinopTag::Div        => 11,
            BinopTag::Mod        => 11,
            BinopTag::BitOr      => 4,
            BinopTag::BitXor     => 5,
            BinopTag::BitAnd     => 6,
            BinopTag::In         => 8,
            BinopTag::Instanceof => 8,
        }
    }
}

pub type Binop = Tracked<BinopTag>;

impl Display for BinopTag {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.write_str(match *self {
            BinopTag::Eq         => "==",
            BinopTag::NEq        => "!=",
            BinopTag::StrictEq   => "===",
            BinopTag::StrictNEq  => "!==",
            BinopTag::Lt         => "<",
            BinopTag::LEq        => "<=",
            BinopTag::Gt         => ">",
            BinopTag::GEq        => ">=",
            BinopTag::LShift     => "<<",
            BinopTag::RShift     => ">>",
            BinopTag::URShift    => ">>>",
            BinopTag::Plus       => "+",
            BinopTag::Minus      => "-",
            BinopTag::Times      => "*",
            BinopTag::Div        => "/",
            BinopTag::Mod        => "%",
            BinopTag::BitOr      => "|",
            BinopTag::BitXor     => "^",
            BinopTag::BitAnd     => "&",
            BinopTag::In         => "in",
            BinopTag::Instanceof => "instanceof"
        })
    }
}

impl Untrack for BinopTag {
    fn untrack(&mut self) { }
}

#[derive(Debug, Eq, PartialEq)]
pub enum LogopTag {
    Or,
    And
}

impl FromStr for LogopTag {
    type Err = ();

    fn from_str(s: &str) -> Result<LogopTag, ()> {
        Ok(match s {
            "||" => LogopTag::Or,
            "&&" => LogopTag::And,
            _    => { return Err(()); }
        })
    }
}

impl Precedence for LogopTag {
    fn precedence(&self) -> u32 {
        match *self {
            LogopTag::Or  => 2,
            LogopTag::And => 3
        }
    }
}

pub type Logop = Tracked<LogopTag>;

impl Display for LogopTag {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.write_str(match *self {
            LogopTag::Or  => "||",
            LogopTag::And => "&&"
        })
    }
}

impl Untrack for LogopTag {
    fn untrack(&mut self) { }
}

#[derive(Debug, Eq, PartialEq)]
pub enum AssopTag {
    Eq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    ModEq,
    LShiftEq,
    RShiftEq,
    URShiftEq,
    BitOrEq,
    BitXorEq,
    BitAndEq
}

impl FromStr for AssopTag {
    type Err = ();

    fn from_str(s: &str) -> Result<AssopTag, ()> {
        Ok(match s {
            "="    => AssopTag::Eq,
            "+="   => AssopTag::PlusEq,
            "-="   => AssopTag::MinusEq,
            "*="   => AssopTag::TimesEq,
            "/="   => AssopTag::DivEq,
            "%="   => AssopTag::ModEq,
            "<<="  => AssopTag::LShiftEq,
            ">>="  => AssopTag::RShiftEq,
            ">>>=" => AssopTag::URShiftEq,
            "|="   => AssopTag::BitOrEq,
            "^="   => AssopTag::BitXorEq,
            "&="   => AssopTag::BitAndEq,
            _      => { return Err(()); }
        })
    }
}

impl Precedence for AssopTag {
    fn precedence(&self) -> u32 { 0 }
}

pub type Assop = Tracked<AssopTag>;

impl Display for AssopTag {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.write_str(match *self {
            AssopTag::Eq        => "=",
            AssopTag::PlusEq    => "+=",
            AssopTag::MinusEq   => "-=",
            AssopTag::TimesEq   => "*=",
            AssopTag::DivEq     => "/=",
            AssopTag::ModEq     => "%=",
            AssopTag::LShiftEq  => "<<=",
            AssopTag::RShiftEq  => ">>=",
            AssopTag::URShiftEq => ">>>=",
            AssopTag::BitOrEq   => "|=",
            AssopTag::BitXorEq  => "^=",
            AssopTag::BitAndEq  => "&="
        })
    }
}

impl Untrack for AssopTag {
    fn untrack(&mut self) { }
}

pub trait ToOp {
    fn to_binop(&self, bool) -> Option<Binop>;
    fn to_logop(&self) -> Option<Logop>;
    fn to_assop(&self) -> Option<Assop>;
}

impl ToOp for Token {
    fn to_binop(&self, allow_in: bool) -> Option<Binop> {
        Some(match self.value {
            TokenData::Star                               => BinopTag::Times,
            TokenData::Slash                              => BinopTag::Div,
            TokenData::Mod                                => BinopTag::Mod,
            TokenData::Plus                               => BinopTag::Plus,
            TokenData::Minus                              => BinopTag::Minus,
            TokenData::LShift                             => BinopTag::LShift,
            TokenData::RShift                             => BinopTag::RShift,
            TokenData::URShift                            => BinopTag::URShift,
            TokenData::LAngle                             => BinopTag::Lt,
            TokenData::RAngle                             => BinopTag::Gt,
            TokenData::LEq                                => BinopTag::LEq,
            TokenData::GEq                                => BinopTag::GEq,
            TokenData::Reserved(Reserved::Instanceof)     => BinopTag::Instanceof,
            TokenData::Reserved(Reserved::In) if allow_in => BinopTag::In,
            TokenData::Eq                                 => BinopTag::Eq,
            TokenData::NEq                                => BinopTag::NEq,
            TokenData::StrictEq                           => BinopTag::StrictEq,
            TokenData::StrictNEq                          => BinopTag::StrictNEq,
            TokenData::BitAnd                             => BinopTag::BitAnd,
            TokenData::BitXor                             => BinopTag::BitXor,
            TokenData::BitOr                              => BinopTag::BitOr,
            _ => { return None; }
        }.tracked(self.location()))
    }

    fn to_logop(&self) -> Option<Logop> {
        Some(match self.value {
            TokenData::LogicalAnd => LogopTag::And,
            TokenData::LogicalOr  => LogopTag::Or,
            _ => { return None; }
        }.tracked(self.location()))
    }

    fn to_assop(&self) -> Option<Assop> {
        Some(match self.value {
            TokenData::Assign        => AssopTag::Eq,
            TokenData::PlusAssign    => AssopTag::PlusEq,
            TokenData::MinusAssign   => AssopTag::MinusEq,
            TokenData::StarAssign    => AssopTag::TimesEq,
            TokenData::SlashAssign   => AssopTag::DivEq,
            TokenData::ModAssign     => AssopTag::ModEq,
            TokenData::LShiftAssign  => AssopTag::LShiftEq,
            TokenData::RShiftAssign  => AssopTag::RShiftEq,
            TokenData::URShiftAssign => AssopTag::URShiftEq,
            TokenData::BitAndAssign  => AssopTag::BitAndEq,
            TokenData::BitOrAssign   => AssopTag::BitOrEq,
            TokenData::BitXorAssign  => AssopTag::BitXorEq,
            _ => { return None; }
        }.tracked(self.location()))
    }
}
