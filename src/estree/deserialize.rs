use rustc_serialize::json;
use rustc_serialize::json::{Json, Object, Array};
use token::{TokenData, Reserved, Exp, CharCase, Sign, NumberLiteral, Radix, Name};
use ast::*;
use track::*;

macro_rules! right {
    ( $l:expr, $r:expr ) => { $r }
}

macro_rules! tuplify {
    ( $v:expr, ( $($dummy:tt),* ) ) => {
        {
            let mut t = $v.into_iter();
            ($(
                right!($dummy, t.next().unwrap())
            ),*)
        }
    };
}

#[derive(Debug)]
pub enum DeserializeError {
    WrongJsonType(&'static str, JsonType),
    WrongNodeType(&'static str, String),
    MissingField(&'static str),
    InvalidString(&'static str, String),
    InvalidArray(&'static str, json::Array),
    InvalidObject(&'static str, json::Object),
    InvalidLHS(&'static str),
    UninitializedPattern(CompoundPatt),
    UnexpectedInitializer(Expr)
}

fn type_error<T>(expected: &'static str, actual: JsonType) -> Deserialize<T> {
    Err(DeserializeError::WrongJsonType(expected, actual))
}

fn node_error<T>(expected: &'static str, actual: String) -> Deserialize<T> {
    Err(DeserializeError::WrongNodeType(expected, actual))
}

fn string_error<T>(expected: &'static str, actual: String) -> Deserialize<T> {
    Err(DeserializeError::InvalidString(expected, actual))
}

fn array_error<T>(expected: &'static str, actual: json::Array) -> Deserialize<T> {
    Err(DeserializeError::InvalidArray(expected, actual))
}

fn object_error<T>(expected: &'static str, actual: json::Object) -> Deserialize<T> {
    Err(DeserializeError::InvalidObject(expected, actual))
}

pub type Deserialize<T> = Result<T, DeserializeError>;

pub trait MatchJson {
    fn into_array(self) -> Deserialize<json::Array>;
    fn into_string(self) -> Deserialize<String>;
    fn into_string_opt(self) -> Deserialize<Option<String>>;
    fn into_name(self) -> Deserialize<Name>;
    fn into_object(self) -> Deserialize<Object>;
    fn into_object_opt(self) -> Deserialize<Option<Object>>;
    fn into_bool(self) -> Deserialize<bool>;
}

pub trait IntoToken {
    fn into_char_case(self) -> Deserialize<CharCase>;
    fn into_char_case_opt(self) -> Deserialize<Option<CharCase>>;
    fn into_exp_opt(self) -> Deserialize<Option<Exp>>;
    fn into_token(self) -> Deserialize<TokenData>;
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum JsonType {
    Null,
    Boolean,
    String,
    Number,
    Object,
    Array
}

fn json_typeof(data: &Json) -> JsonType {
    match *data {
        Json::Null       => JsonType::Null,
        Json::String(_)  => JsonType::String,
        Json::Object(_)  => JsonType::Object,
        Json::I64(_)     => JsonType::Number,
        Json::U64(_)     => JsonType::Number,
        Json::F64(_)     => JsonType::Number,
        Json::Array(_)   => JsonType::Array,
        Json::Boolean(_) => JsonType::Boolean
    }
}

impl MatchJson for Json {
    fn into_array(self) -> Deserialize<json::Array> {
        match self {
            Json::Array(array) => Ok(array),
            _                  => type_error("array", json_typeof(&self))
        }
    }

    fn into_string(self) -> Deserialize<String> {
        match self {
            Json::String(string) => Ok(string),
            _                    => type_error("string", json_typeof(&self))
        }
    }

    fn into_string_opt(self) -> Deserialize<Option<String>> {
        match self {
            Json::Null           => Ok(None),
            Json::String(string) => Ok(Some(string)),
            _                    => type_error("string or null", json_typeof(&self))
        }
    }

    fn into_name(self) -> Deserialize<Name> {
        Ok(Name::new(try!(self.into_string())))
    }

    fn into_object(self) -> Deserialize<json::Object> {
        match self {
            Json::Object(object) => Ok(object),
            _                    => type_error("object", json_typeof(&self))
        }
    }

    fn into_object_opt(self) -> Deserialize<Option<Object>> {
        match self {
            Json::Object(object) => Ok(Some(object)),
            Json::Null           => Ok(None),
            _                    => type_error("object or null", json_typeof(&self))
        }
    }

    fn into_bool(self) -> Deserialize<bool> {
        match self {
            Json::Boolean(val) => Ok(val),
            _                  => type_error("boolean", json_typeof(&self))
        }
    }
}

fn validate_token(arr: json::Array) -> Deserialize<json::Array> {
    if arr.len() == 0 {
        return array_error("array of length > 0", arr);
    }

    let one = (1, "array of length 1");
    let two = (2, "array of length 2");
    let three = (3, "array of length 3");
    let four = (4, "array of length 4");

    let (expected_len, expected_msg) = {
        let elt = arr.iter().next().unwrap();
        let ty = match elt.as_string() {
            None      => { return type_error("string", json_typeof(&elt)); },
            Some(str) => str
        };
        match ty {
            "Reserved"   => two,
            "DecimalInt" => three,
            "BinaryInt"  => three,
            "OctalInt"   => three,
            "HexInt"     => three,
            "Float"      => four,
            "String"     => two,
            "RegExp"     => three,
            "Identifier" => two,
            _            => one
        }
    };

    if arr.len() != expected_len {
        return array_error(expected_msg, arr);
    }

    Ok(arr)
}

impl IntoToken for Json {
    fn into_char_case(self) -> Deserialize<CharCase> {
        let s = try!(self.into_string());
        if s.len() == 0 {
            return string_error("lowercase or uppercase letter", s);
        }
        let ch = s.chars().next().unwrap();
        if ch.is_lowercase() {
            Ok(CharCase::LowerCase)
        } else if ch.is_uppercase() {
            Ok(CharCase::UpperCase)
        } else {
            string_error("lowercase or uppercase letter", s)
        }
    }

    fn into_char_case_opt(self) -> Deserialize<Option<CharCase>> {
        match self {
            Json::Null => Ok(None),
            _          => self.into_char_case().map(Some)
        }
    }

    fn into_exp_opt(self) -> Deserialize<Option<Exp>> {
        match self {
            Json::Null => Ok(None),
            _          => {
                let arr = try!(self.into_array());
                if arr.len() != 3 {
                    return array_error("array of length 3", arr);
                }
                let (e, sign, value) = tuplify!(arr, ((), (), ()));
                Ok(Some(Exp {
                    e: try!(e.into_char_case()),
                    sign: match try!(sign.into_string_opt()) {
                        None    => None,
                        Some(s) => {
                            if s.len() != 1 {
                                return string_error("'+' or '-'", s);
                            }
                            match s.chars().next().unwrap() {
                                '+' => Some(Sign::Plus),
                                '-' => Some(Sign::Minus),
                                _   => { return string_error("'+' or '-'", s); }
                            }
                        }
                    },
                    value: try!(value.into_string())
                }))
            }
        }
    }

    fn into_token(self) -> Deserialize<TokenData> {
        let mut arr = try!(self.into_array());

        // Check the array lengths in an external validation helper.
        // This lets us modularize the validation and avoids having to patch
        // the array back up to return in the error struct.
        arr = try!(validate_token(arr));

        let ty = try!(arr.remove(0).into_string());
        Ok(match &ty[..] {
            "Reserved"      => {
                let word = try!(arr.remove(0).into_string().and_then(|str| str.into_reserved()));
                TokenData::Reserved(word)
            }
            "LBrace"        => TokenData::LBrace,
            "RBrace"        => TokenData::RBrace,
            "LParen"        => TokenData::LParen,
            "RParen"        => TokenData::RParen,
            "LBrack"        => TokenData::LBrack,
            "RBrack"        => TokenData::RBrack,
            "Dot"           => TokenData::Dot,
            //"Ellipsis"    => TokenData::Ellipsis,
            "Semi"          => TokenData::Semi,
            "Comma"         => TokenData::Comma,
            "LAngle"        => TokenData::LAngle,
            "RAngle"        => TokenData::RAngle,
            "LEq"           => TokenData::LEq,
            "GEq"           => TokenData::GEq,
            "Eq"            => TokenData::Eq,
            "NEq"           => TokenData::NEq,
            "StrictEq"      => TokenData::StrictEq,
            "StrictNEq"     => TokenData::StrictNEq,
            "Plus"          => TokenData::Plus,
            "Minus"         => TokenData::Minus,
            "Star"          => TokenData::Star,
            "Mod"           => TokenData::Mod,
            "Slash"         => TokenData::Slash,
            "Inc"           => TokenData::Inc,
            "Dec"           => TokenData::Dec,
            "LShift"        => TokenData::LShift,
            "RShift"        => TokenData::RShift,
            "URShift"       => TokenData::URShift,
            "BitAnd"        => TokenData::BitAnd,
            "BitOr"         => TokenData::BitOr,
            "BitXor"        => TokenData::BitXor,
            "Bang"          => TokenData::Bang,
            "Tilde"         => TokenData::Tilde,
            "LogicalAnd"    => TokenData::LogicalAnd,
            "LogicalOr"     => TokenData::LogicalOr,
            "Question"      => TokenData::Question,
            "Colon"         => TokenData::Colon,
            "Assign"        => TokenData::Assign,
            "PlusAssign"    => TokenData::PlusAssign,
            "MinusAssign"   => TokenData::MinusAssign,
            "StarAssign"    => TokenData::StarAssign,
            "SlashAssign"   => TokenData::SlashAssign,
            "ModAssign"     => TokenData::ModAssign,
            "LShiftAssign"  => TokenData::LShiftAssign,
            "RShiftAssign"  => TokenData::RShiftAssign,
            "URShiftAssign" => TokenData::URShiftAssign,
            "BitAndAssign"  => TokenData::BitAndAssign,
            "BitOrAssign"   => TokenData::BitOrAssign,
            "BitXorAssign"  => TokenData::BitXorAssign,
            "Arrow"         => TokenData::Arrow,
            "EOF"           => TokenData::EOF,
            "DecimalInt"    => {
                let (value, exp) = tuplify!(arr, ((), ()));
                let value = try!(value.into_string());
                let exp = try!(exp.into_exp_opt());
                TokenData::Number(NumberLiteral::DecimalInt(value, exp))
            }
            "BinaryInt"     => {
                let (flag, value) = tuplify!(arr, ((), ()));
                let flag = try!(flag.into_char_case());
                let value = try!(value.into_string());
                TokenData::Number(NumberLiteral::RadixInt(Radix::Bin(flag), value))
            }
            "OctalInt"      => {
                let (flag, value) = tuplify!(arr, ((), ()));
                let flag = try!(flag.into_char_case_opt());
                let value = try!(value.into_string());
                TokenData::Number(NumberLiteral::RadixInt(Radix::Oct(flag), value))
            }
            "HexInt"        => {
                let (flag, value) = tuplify!(arr, ((), ()));
                let flag = try!(flag.into_char_case());
                let value = try!(value.into_string());
                TokenData::Number(NumberLiteral::RadixInt(Radix::Hex(flag), value))
            }
            "Float"         => {
                let (int, frac, exp) = tuplify!(arr, ((), (), ()));
                let int = try!(int.into_string_opt());
                let frac = try!(frac.into_string_opt());
                let exp = try!(exp.into_exp_opt());
                TokenData::Number(NumberLiteral::Float(int, frac, exp))
            }
            "String"        => TokenData::String(try!(arr.remove(0).into_string())),
            "RegExp"        => {
                let (pattern, flags) = tuplify!(arr, ((), ()));
                let pattern = try!(pattern.into_string());
                let flags = try!(flags.into_string()).chars().collect();
                TokenData::RegExp(pattern, flags)
            }
            "Identifier"    => TokenData::Identifier(try!(arr.remove(0).into_name())),
            _               => { return array_error("token", arr); }
        })
    }
}

pub trait IntoOperator {
    fn into_unop(self) -> Deserialize<Unop>;
    fn into_binop(self) -> Deserialize<Binop>;
    fn into_assop(self) -> Deserialize<Assop>;
    fn into_logop(self) -> Deserialize<Logop>;
}

impl IntoOperator for String {
    fn into_unop(self) -> Deserialize<Unop> {
        Ok((match &self[..] {
            "-"       => UnopTag::Minus,
            "+"       => UnopTag::Plus,
            "!"       => UnopTag::Not,
            "~"       => UnopTag::BitNot,
            "typeof"  => UnopTag::Typeof,
            "void"    => UnopTag::Void,
            "delete"  => UnopTag::Delete,
            _         => { return string_error("unop", self); }
        }).tracked(None))
    }

    fn into_binop(self) -> Deserialize<Binop> {
        Ok((match &self[..] {
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
            _            => { return string_error("binop", self); }
        }).tracked(None))
    }

    fn into_assop(self) -> Deserialize<Assop> {
        Ok((match &self[..] {
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
            _      => { return string_error("assop", self); }
        }).tracked(None))
    }

    fn into_logop(self) -> Deserialize<Logop> {
        Ok((match &self[..] {
            "||" => LogopTag::Or,
            "&&" => LogopTag::And,
            _    => { return string_error("logop", self); }
        }).tracked(None))
    }
}

pub trait IntoReserved {
    fn into_reserved(self) -> Deserialize<Reserved>;
}

impl IntoReserved for String {
    fn into_reserved(self) -> Deserialize<Reserved> {
        Ok(match &self[..] {
            "Null"       => Reserved::Null,
            "True"       => Reserved::True,
            "False"      => Reserved::False,
            "Break"      => Reserved::Break,
            "Case"       => Reserved::Case,
            "Catch"      => Reserved::Catch,
            "Class"      => Reserved::Class,
            "Const"      => Reserved::Const,
            "Continue"   => Reserved::Continue,
            "Debugger"   => Reserved::Debugger,
            "Default"    => Reserved::Default,
            "Delete"     => Reserved::Delete,
            "Do"         => Reserved::Do,
            "Else"       => Reserved::Else,
            "Export"     => Reserved::Export,
            "Extends"    => Reserved::Extends,
            "Finally"    => Reserved::Finally,
            "For"        => Reserved::For,
            "Function"   => Reserved::Function,
            "If"         => Reserved::If,
            "Import"     => Reserved::Import,
            "In"         => Reserved::In,
            "Instanceof" => Reserved::Instanceof,
            "New"        => Reserved::New,
            "Return"     => Reserved::Return,
            "Super"      => Reserved::Super,
            "Switch"     => Reserved::Switch,
            "This"       => Reserved::This,
            "Throw"      => Reserved::Throw,
            "Try"        => Reserved::Try,
            "Typeof"     => Reserved::Typeof,
            "Var"        => Reserved::Var,
            "Void"       => Reserved::Void,
            "While"      => Reserved::While,
            "With"       => Reserved::With,
            "Enum"       => Reserved::Enum,
            _            => { return string_error("keyword", self); }
        })
    }
}

pub trait IntoNode {
    fn into_program(self) -> Deserialize<Script>;
    fn into_statement_list_item(self) -> Deserialize<StmtListItem>;
    fn into_declarator(self) -> Deserialize<Dtor>;
    fn into_statement(self) -> Deserialize<Stmt>;
    fn into_expression(self) -> Deserialize<Expr>;
    fn into_binary_expression(self) -> Deserialize<Expr>;
    fn into_assignment_expression(self) -> Deserialize<Expr>;
    fn into_logical_expression(self) -> Deserialize<Expr>;
    fn into_unary_expression(self) -> Deserialize<Expr>;
    fn into_update_expression(self) -> Deserialize<Expr>;
    fn into_member_expression(self) -> Deserialize<Expr>;
    fn into_call_expression(self) -> Deserialize<Expr>;
    fn into_new_expression(self) -> Deserialize<Expr>;
    fn into_array_expression(self) -> Deserialize<Expr>;
    fn into_function_expression(self) -> Deserialize<Expr>;
    fn into_sequence_expression(self) -> Deserialize<Expr>;
    fn into_object_expression(self) -> Deserialize<Expr>;
    fn into_conditional_expression(self) -> Deserialize<Expr>;
    fn into_identifier(self) -> Deserialize<Id>;
    fn into_property(self) -> Deserialize<Prop>;
    fn into_property_key(self) -> Deserialize<PropKey>;
    fn into_literal(self) -> Deserialize<Expr>;
    fn into_function(self) -> Deserialize<Fun>;
    fn into_pattern(self) -> Deserialize<Patt>;
    fn into_case(self) -> Deserialize<Case>;
    fn into_catch(self) -> Deserialize<Catch>;
}

impl IntoNode for Object {
    fn into_program(mut self) -> Deserialize<Script> {
        Ok((ScriptData { body: try!(self.extract_statement_list("body")) }).tracked(None))
    }

    fn into_statement_list_item(self) -> Deserialize<StmtListItem> {
        let is_fun = match &(try!(self.peek_type()))[..] {
            "FunctionDeclaration" => true,
            _                     => false
        };
        if is_fun {
            Ok(StmtListItem::Decl(DeclData::Fun(try!(self.into_function())).tracked(None)))
        } else {
            Ok(StmtListItem::Stmt(try!(self.into_statement())))
        }
    }

    fn into_declarator(mut self) -> Deserialize<Dtor> {
        let lhs = try!(self.extract_pattern("id"));
        let init = try!(self.extract_expression_opt("init"));
        Dtor::from_init_opt(lhs, init).map_err(DeserializeError::UninitializedPattern)
    }

    fn into_statement(mut self) -> Deserialize<Stmt> {
        let ty = try!(self.extract_string("type"));
        match &ty[..] {
            "VariableDeclaration" => {
                let dtors = try!(self.extract_declarator_list("declarations"));
                Ok(StmtData::Var(dtors, Semi::Explicit(None)).tracked(None))
            }
            "EmptyStatement" => {
                Ok(StmtData::Empty.tracked(None))
            }
            "ExpressionStatement" => {
                let expr = try!(self.extract_expression("expression"));
                Ok(StmtData::Expr(expr, Semi::Explicit(None)).tracked(None))
            }
            "IfStatement" => {
                let test = try!(self.extract_expression("test"));
                let cons = Box::new(try!(self.extract_statement("consequent")));
                let alt = try!(self.extract_statement_opt("alternate")).map(Box::new);
                Ok(StmtData::If(test, cons, alt).tracked(None))
            }
            "DoWhileStatement" => {
                let body = Box::new(try!(self.extract_statement("body")));
                let test = try!(self.extract_expression("test"));
                Ok(StmtData::DoWhile(body, test, Semi::Explicit(None)).tracked(None))
            }
            "WhileStatement" => {
                let test = try!(self.extract_expression("test"));
                let body = Box::new(try!(self.extract_statement("body")));
                Ok(StmtData::While(test, body).tracked(None))
            }
            "ForStatement" => {
                let init0 = try!(self.extract_object_opt("init"));
                let init = match init0 {
                    None => None,
                    Some(mut obj) => {
                        match &(try!(obj.peek_type()))[..] {
                            "VariableDeclaration" => {
                                let dtors = try!(obj.extract_declarator_list("declarations"));
                                let kind = try!(obj.extract_string("kind"));
                                let head = (match &kind[..] {
                                    "var" => ForHeadData::Var(dtors),
                                    "let" => ForHeadData::Let(dtors),
                                    _ => { return string_error("var or let", kind); }
                                }).tracked(None);
                                Some(Box::new(head))
                            }
                            _ => Some(Box::new(ForHeadData::Expr(try!(obj.into_expression())).tracked(None)))
                        }
                    }
                };
                let test = try!(self.extract_expression_opt("test"));
                let update = try!(self.extract_expression_opt("update"));
                let body = Box::new(try!(self.extract_statement("body")));
                Ok(StmtData::For(init, test, update, body).tracked(None))
            }
            "ForInStatement" => {
                let mut left0 = try!(self.extract_object("left"));
                let left = Box::new((match &(try!(left0.peek_type()))[..] {
                    "VariableDeclaration" => {
                        let mut dtors = try!(left0.extract_array("declarations"));
                        let kind = try!(left0.extract_string("kind"));
                        if dtors.len() != 1 {
                            return array_error("array of length 1", dtors);
                        }
                        let mut obj = try!(dtors.remove(0).into_object());
                        let lhs = try!(obj.extract_pattern("id"));
                        let init = try!(obj.extract_expression_opt("init"));
                        match &kind[..] {
                            "var" => match lhs {
                                Patt::Simple(id) => {
                                    match init {
                                        None       => ForInHeadData::Var(Patt::Simple(id)),
                                        Some(expr) => ForInHeadData::VarInit(id, expr)
                                    }
                                }
                                Patt::Compound(patt) => {
                                    match init {
                                        None       => ForInHeadData::Var(Patt::Compound(patt)),
                                        Some(expr) => { return Err(DeserializeError::UnexpectedInitializer(expr)); }
                                    }
                                }
                            },
                            "let" => {
                                match init {
                                    None       => ForInHeadData::Let(lhs),
                                    Some(expr) => { return Err(DeserializeError::UnexpectedInitializer(expr)); }
                                }
                            }
                            _ => { return string_error("var or let", kind); }
                        }
                    }
                    _ => ForInHeadData::Expr(try!(left0.into_expression()))
                }).tracked(None));
                let right = try!(self.extract_expression("right"));
                let body = try!(self.extract_statement("body"));
                Ok(StmtData::ForIn(left, right, Box::new(body)).tracked(None))
            }
            "ForOfStatement" => {
                let left0 = try!(self.extract_object("left"));
                let left = Box::new((match &(try!(left0.peek_type()))[..] {
                    "VariableDeclaration" => {
                        let mut dtors = try!(self.extract_array("declarations"));
                        let kind = try!(self.extract_string("kind"));
                        if dtors.len() != 1 {
                            return array_error("array of length 1", dtors);
                        }
                        let mut obj = try!(dtors.remove(0).into_object());
                        let lhs = try!(obj.extract_pattern("id"));
                        match &kind[..] {
                            "var" => ForOfHeadData::Var(lhs),
                            "let" => ForOfHeadData::Let(lhs),
                            _ => { return string_error("var or let", kind); }
                        }
                    },
                    _ => ForOfHeadData::Expr(try!(left0.into_expression()))
                }).tracked(None));
                let right = try!(self.extract_expression("right"));
                let body = try!(self.extract_statement("body"));
                Ok(StmtData::ForOf(left, right, Box::new(body)).tracked(None))
            }
            "BlockStatement" => {
                let body = try!(self.extract_statement_list("body"));
                Ok(StmtData::Block(body).tracked(None))
            }
            "ReturnStatement" => {
                let arg = try!(self.extract_expression_opt("argument"));
                Ok(StmtData::Return(arg, Semi::Explicit(None)).tracked(None))
            }
            "LabeledStatement" => {
                let label = try!(self.extract_id("label"));
                let body = Box::new(try!(self.extract_statement("body")));
                Ok(StmtData::Label(label, body).tracked(None))
            }
            "BreakStatement" => {
                let label = try!(self.extract_id_opt("label"));
                Ok(StmtData::Break(label, Semi::Explicit(None)).tracked(None))
            }
            "ContinueStatement" => {
                let label = try!(self.extract_id_opt("label"));
                Ok(StmtData::Cont(label, Semi::Explicit(None)).tracked(None))
            }
            "SwitchStatement" => {
                let disc = try!(self.extract_expression("discriminant"));
                let cases = try!(self.extract_case_list("cases"));
                Ok(StmtData::Switch(disc, cases).tracked(None))
            }
            "WithStatement" => {
                let obj = try!(self.extract_expression("object"));
                let body = Box::new(try!(self.extract_statement("body")));
                Ok(StmtData::With(obj, body).tracked(None))
            }
            "ThrowStatement" => {
                let arg = try!(self.extract_expression("argument"));
                Ok(StmtData::Throw(arg, Semi::Explicit(None)).tracked(None))
            }
            "DebuggerStatement" => {
                Ok(StmtData::Debugger(Semi::Explicit(None)).tracked(None))
            }
            "TryStatement" => {
                let mut block = try!(self.extract_object("block"));
                let body = try!(block.extract_statement_list("body"));
                let catch = try!(self.extract_catch_opt("handler")).map(Box::new);
                let mut finally = match try!(self.extract_object_opt("finalizer")) {
                    Some(mut finalizer) => Some(try!(finalizer.extract_statement_list("body"))),
                    None => None
                };
                Ok(StmtData::Try(body, catch, finally).tracked(None))
            }
            // FIXME: remaining statement cases
            _ => string_error("statement type", ty)
        }
    }

    fn into_expression(self) -> Deserialize<Expr> {
        match &(try!(self.peek_type()))[..] {
            "Identifier"            => self.into_identifier().map(|id| id.into_expr()),
            "Literal"               => self.into_literal(),
            "BinaryExpression"      => self.into_binary_expression(),
            "AssignmentExpression"  => self.into_assignment_expression(),
            "LogicalExpression"     => self.into_logical_expression(),
            "UnaryExpression"       => self.into_unary_expression(),
            "UpdateExpression"      => self.into_update_expression(),
            "MemberExpression"      => self.into_member_expression(),
            "CallExpression"        => self.into_call_expression(),
            "NewExpression"         => self.into_new_expression(),
            "ArrayExpression"       => self.into_array_expression(),
            "FunctionExpression"    => self.into_function_expression(),
            "SequenceExpression"    => self.into_sequence_expression(),
            "ObjectExpression"      => self.into_object_expression(),
            "ConditionalExpression" => self.into_conditional_expression(),
            "ThisExpression"        => Ok(ExprData::This.tracked(None)),
            _                  => { return object_error("expression", self); }
        }
    }

    fn into_unary_expression(mut self) -> Deserialize<Expr> {
        let op = try!(try!(self.extract_string("operator")).into_unop());
        let arg = try!(self.extract_expression("argument"));
        Ok(ExprData::Unop(op, Box::new(arg)).tracked(None))
    }

    fn into_update_expression(mut self) -> Deserialize<Expr> {
        let op = try!(self.extract_string("operator"));
        let arg = Box::new(try!(self.extract_expression("argument")));
        let prefix = try!(self.extract_bool("prefix"));
        Ok((match (&op[..], prefix) {
            ("++", true)  => ExprData::PreInc(arg),
            ("++", false) => ExprData::PostInc(arg),
            ("--", true)  => ExprData::PreDec(arg),
            ("--", false) => ExprData::PostDec(arg),
            _ => { return string_error("'++' or '--'", op); }
        }).tracked(None))
    }

    fn into_member_expression(mut self) -> Deserialize<Expr> {
        let obj = Box::new(try!(self.extract_expression("object")));
        if try!(self.extract_bool("computed")) {
            let prop = Box::new(try!(self.extract_expression("property")));
            Ok(ExprData::Brack(obj, prop).tracked(None))
        } else {
            let prop = try!(self.extract_id("property"));
            Ok(ExprData::Dot(obj, prop).tracked(None))
        }
    }

    fn into_call_expression(mut self) -> Deserialize<Expr> {
        let callee = Box::new(try!(self.extract_expression("callee")));
        let args = try!(try!(self.extract_object_array("arguments")).map(|obj| obj.into_expression()));
        Ok(ExprData::Call(callee, args).tracked(None))
    }

    fn into_new_expression(mut self) -> Deserialize<Expr> {
        let callee = Box::new(try!(self.extract_expression("callee")));
        let args = try!(try!(self.extract_object_array("arguments")).map(|obj| obj.into_expression()));
        Ok(ExprData::New(callee, Some(args)).tracked(None))
    }

    fn into_array_expression(mut self) -> Deserialize<Expr> {
        let elts = try!(try!(self.extract_object_opt_array("elements")).map(|opt| match opt {
            None => Ok(None),
            Some(obj) => obj.into_expression().map(Some)
        }));
        Ok(ExprData::Arr(elts).tracked(None))
    }

    fn into_function_expression(mut self) -> Deserialize<Expr> {
        let fun = try!(self.into_function());
        Ok(ExprData::Fun(fun).tracked(None))
    }

    fn into_sequence_expression(mut self) -> Deserialize<Expr> {
        let elts = try!(try!(self.extract_object_array("expressions")).map(|obj| obj.into_expression()));
        Ok(ExprData::Seq(elts).tracked(None))
    }

    fn into_object_expression(mut self) -> Deserialize<Expr> {
        let props = try!(try!(self.extract_object_array("properties")).map(|obj| obj.into_property()));
        Ok(ExprData::Obj(props).tracked(None))
    }

    fn into_conditional_expression(mut self) -> Deserialize<Expr> {
        let test = Box::new(try!(self.extract_expression("test")));
        let cons = Box::new(try!(self.extract_expression("consequent")));
        let alt = Box::new(try!(self.extract_expression("alternate")));
        Ok(ExprData::Cond(test, cons, alt).tracked(None))
    }

    fn into_binary_expression(mut self) -> Deserialize<Expr> {
        let op = try!(try!(self.extract_string("operator")).into_binop());
        let left = try!(self.extract_expression("left"));
        let right = try!(self.extract_expression("right"));
        Ok(ExprData::Binop(op, Box::new(left), Box::new(right)).tracked(None))
    }

    fn into_assignment_expression(mut self) -> Deserialize<Expr> {
        let op = try!(try!(self.extract_string("operator")).into_assop());
        let left = try!(self.extract_assignment_pattern("left"));
        let right = try!(self.extract_expression("right"));
        Ok(ExprData::Assign(op, left, Box::new(right)).tracked(None))
    }

    fn into_logical_expression(mut self) -> Deserialize<Expr> {
        let op = try!(try!(self.extract_string("operator")).into_logop());
        let left = try!(self.extract_expression("left"));
        let right = try!(self.extract_expression("right"));
        Ok(ExprData::Logop(op, Box::new(left), Box::new(right)).tracked(None))
    }

    fn into_identifier(mut self) -> Deserialize<Id> {
        Ok((IdData { name: Name::new(try!(self.extract_string("name"))) }).tracked(None))
    }

    fn into_property(mut self) -> Deserialize<Prop> {
        let key = try!(try!(self.extract_object("key")).into_property_key());
        let mut val = try!(self.extract_object("value"));
        let kind = try!(self.extract_string("kind"));
        let val = (match &kind[..] {
            "init" => PropValData::Init(try!(val.into_expression())),
            "get" => PropValData::Get(try!(try!(val.extract_object("body")).extract_statement_list("body"))),
            "set" => {
                unimplemented!()
            }
            _ => { return type_error("'init', 'get', or 'set'", JsonType::String); }
        }).tracked(None);
        Ok((PropData { key: key, val: val }).tracked(None))
    }

    fn into_property_key(mut self) -> Deserialize<PropKey> {
        if &(try!(self.peek_type()))[..] == "Identifier" {
            let id = try!(self.into_identifier());
            return Ok(PropKeyData::Id(id.value.name.into_string()).tracked(None));
        }
        match try!(self.into_literal()).value {
            ExprData::Number(lit) => Ok(PropKeyData::Number(lit).tracked(None)),
            ExprData::String(val) => Ok(PropKeyData::String(val).tracked(None)),
            _ => { return type_error("identifier, number literal, or string literal", JsonType::Object); }
        }
    }

    fn into_literal(mut self) -> Deserialize<Expr> {
        let json = try!(self.extract("value"));
        Ok((match json {
            Json::Null => ExprData::Null,
            Json::Boolean(val) => if val { ExprData::True } else { ExprData::False },
            Json::String(val) => ExprData::String(val),
            Json::I64(val) => ExprData::Number(NumberLiteral::DecimalInt(format!("{}", val), None)),
            Json::U64(val) => ExprData::Number(NumberLiteral::DecimalInt(format!("{}", val), None)),
            Json::F64(val) => {
                let s = format!("{}", val);
                let v: Vec<&str> = s.split('.').collect();
                let int_part = Some(v[0].to_owned());
                let fract_part = if v.len() > 1 { Some(v[1].to_owned()) } else { None };
                ExprData::Number(NumberLiteral::Float(int_part, fract_part, None))
            }
            Json::Object(_) => {
                let mut regex = try!(self.extract_object("regex"));
                let pattern = try!(regex.extract_string("pattern"));
                let flags = try!(regex.extract_string("flags"));
                ExprData::RegExp(pattern, flags.chars().collect())
            }
            _ => { return type_error("null, number, boolean, string, or object", json_typeof(&json)); }
        }).tracked(None))
    }

    fn into_function(mut self) -> Deserialize<Fun> {
        let id = try!(self.extract_id_opt("id"));
        let params = (ParamsData {
            list: try!(try!(self.extract_object_array("params")).map(|obj| obj.into_pattern()))
        }).tracked(None);
        let body = match try!(self.extract_statement("body")).value {
            StmtData::Block(items) => items,
            node                   => { return node_error("BlockStatement", format!("{:?}", node)); }
        };
        Ok((FunData { id: id, params: params, body: body }).tracked(None))
    }

    fn into_pattern(self) -> Deserialize<Patt> {
        self.into_identifier().map(|id| id.into_patt())
    }

    fn into_case(mut self) -> Deserialize<Case> {
        let test = try!(self.extract_expression_opt("test"));
        let body = try!(self.extract_statement_list("consequent"));
        Ok((CaseData { test: test, body: body }).tracked(None))
    }

    fn into_catch(mut self) -> Deserialize<Catch> {
        let param = try!(self.extract_pattern("param"));
        let mut body = try!(self.extract_object("body"));
        let body = try!(body.extract_statement_list("body"));
        Ok((CatchData { param: param, body: body }).tracked(None))
    }
}

trait DeserializeMap<T, U> {
    fn map<F: Fn(T) -> Deserialize<U>>(self, F) -> Deserialize<Vec<U>>;
}

impl<T, U> DeserializeMap<T, U> for Vec<T> {
    fn map<F: Fn(T) -> Deserialize<U>>(self, f: F) -> Deserialize<Vec<U>> {
        let mut list = Vec::with_capacity(self.len());
        for data in self {
            list.push(try!(f(data)));
        }
        Ok(list)
    }
}

pub trait ExtractField {
    fn extract(&mut self, &'static str) -> Deserialize<Json>;
    fn extract_string(&mut self, &'static str) -> Deserialize<String>;
    fn extract_array(&mut self, &'static str) -> Deserialize<json::Array>;
    fn extract_object_array(&mut self, &'static str) -> Deserialize<Vec<Object>>;
    fn extract_object_opt_array(&mut self, &'static str) -> Deserialize<Vec<Option<Object>>>;
    fn extract_object(&mut self, &'static str) -> Deserialize<Object>;
    fn extract_object_opt(&mut self, &'static str) -> Deserialize<Option<Object>>;
    fn extract_bool(&mut self, &'static str) -> Deserialize<bool>;
    fn extract_id(&mut self, &'static str) -> Deserialize<Id>;
    fn extract_id_opt(&mut self, &'static str) -> Deserialize<Option<Id>>;
    fn extract_assignment_pattern(&mut self, &'static str) -> Deserialize<APatt>;
    fn peek_type(&self) -> Deserialize<&String>;
}

impl ExtractField for json::Object {
    fn extract(&mut self, name: &'static str) -> Deserialize<Json> {
        match self.remove(name) {
            Some(json) => Ok(json),
            None       => Err(DeserializeError::MissingField(name))
        }
    }

    fn extract_string(&mut self, name: &'static str) -> Deserialize<String> {
        self.extract(name).and_then(|data| data.into_string())
    }

    fn extract_array(&mut self, name: &'static str) -> Deserialize<json::Array> {
        self.extract(name).and_then(|data| data.into_array())
    }

    fn extract_object_array(&mut self, name: &'static str) -> Deserialize<Vec<Object>> {
        self.extract(name).and_then(|data| data.into_array())
                          .and_then(|arr| arr.map(|elt| elt.into_object()))
    }

    fn extract_object_opt_array(&mut self, name: &'static str) -> Deserialize<Vec<Option<Object>>> {
        self.extract(name).and_then(|data| data.into_array())
                          .and_then(|arr| arr.map(|elt| elt.into_object_opt()))
    }

    fn extract_object(&mut self, name: &'static str) -> Deserialize<Object> {
        self.extract(name).and_then(|data| data.into_object())
    }

    fn extract_object_opt(&mut self, name: &'static str) -> Deserialize<Option<Object>> {
        self.extract(name).and_then(|data| data.into_object_opt())
    }

    fn extract_bool(&mut self, name: &'static str) -> Deserialize<bool> {
        self.extract(name).and_then(|data| data.into_bool())
    }

    fn extract_id(&mut self, name: &'static str) -> Deserialize<Id> {
        self.extract_object(name).and_then(|data| data.into_identifier())
    }

    fn extract_id_opt(&mut self, name: &'static str) -> Deserialize<Option<Id>> {
        Ok(match try!(self.extract_object_opt(name)) {
            None      => None,
            Some(obj) => Some(try!(obj.into_identifier()))
        })
    }

    fn extract_assignment_pattern(&mut self, name: &'static str) -> Deserialize<APatt> {
        let expr = try!(self.extract_expression(name));
        match expr.into_assignment_pattern() {
            Ok(patt) => Ok(patt),
            _ => Err(DeserializeError::InvalidLHS(name))
        }
    }

    fn peek_type(&self) -> Deserialize<&String> {
        match self.get("type") {
            None                       => Err(DeserializeError::MissingField("type")),
            Some(&Json::String(ref s)) => { Ok(s) },
            Some(data)                 => type_error("string", json_typeof(&data))
        }
    }
}

pub trait ExtractNode {
    fn extract_statement(&mut self, &'static str) -> Deserialize<Stmt>;
    fn extract_expression(&mut self, &'static str) -> Deserialize<Expr>;
    fn extract_expression_opt(&mut self, &'static str) -> Deserialize<Option<Expr>>;
    fn extract_statement_opt(&mut self, &'static str) -> Deserialize<Option<Stmt>>;
    fn extract_statement_list(&mut self, &'static str) -> Deserialize<Vec<StmtListItem>>;
    fn extract_pattern(&mut self, &'static str) -> Deserialize<Patt>;
    fn extract_declarator_list(&mut self, &'static str) -> Deserialize<Vec<Dtor>>;
    fn extract_case_list(&mut self, &'static str) -> Deserialize<Vec<Case>>;
    fn extract_catch_opt(&mut self, &'static str) -> Deserialize<Option<Catch>>;
}

impl ExtractNode for Object {
    fn extract_statement(&mut self, name: &'static str) -> Deserialize<Stmt> {
        try!(self.extract_object(name)).into_statement()
    }

    fn extract_expression(&mut self, name: &'static str) -> Deserialize<Expr> {
        try!(self.extract_object(name)).into_expression()
    }

    fn extract_expression_opt(&mut self, name: &'static str) -> Deserialize<Option<Expr>> {
        match try!(self.extract_object_opt(name)) {
            None => Ok(None),
            Some(obj) => Ok(Some(try!(obj.into_expression())))
        }
    }

    fn extract_statement_opt(&mut self, name: &'static str) -> Deserialize<Option<Stmt>> {
        match try!(self.extract_object_opt(name)) {
            None => Ok(None),
            Some(obj) => Ok(Some(try!(obj.into_statement())))
        }
    }

    fn extract_statement_list(&mut self, name: &'static str) -> Deserialize<Vec<StmtListItem>> {
        try!(self.extract_object_array(name)).map(|obj| obj.into_statement_list_item())
    }

    fn extract_pattern(&mut self, name: &'static str) -> Deserialize<Patt> {
        try!(self.extract_object(name)).into_pattern()
    }

    fn extract_declarator_list(&mut self, name: &'static str) -> Deserialize<Vec<Dtor>> {
        try!(self.extract_object_array(name)).map(|obj| obj.into_declarator())
    }

    fn extract_case_list(&mut self, name: &'static str) -> Deserialize<Vec<Case>> {
        try!(self.extract_object_array(name)).map(|obj| obj.into_case())
    }

    fn extract_catch_opt(&mut self, name: &'static str) -> Deserialize<Option<Catch>> {
        Ok(match try!(self.extract_object_opt(name)) {
            Some(obj) => Some(try!(obj.into_catch())),
            None => None
        })
    }
}
