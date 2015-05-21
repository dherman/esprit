use rustc_serialize::json;
use rustc_serialize::json::{Json, Object, Array};
use token::{TokenData, Word, Exp, CharCase, Sign, NumberLiteral, Radix};
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
    fn into_object(self) -> Deserialize<Object>;
    fn into_object_opt(self) -> Deserialize<Option<Object>>;
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
                let word = try!(arr.remove(0).into_string().and_then(|str| str.into_word()));
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
            "Identifier"    => TokenData::Identifier(try!(arr.remove(0).into_string())),
            _               => { return array_error("token", arr); }
        })
    }
}

pub trait IntoWord {
    fn into_word(self) -> Deserialize<Word>;
}

impl IntoWord for String {
    fn into_word(self) -> Deserialize<Word> {
        Ok(match &self[..] {
            "Null"       => Word::Null,
            "True"       => Word::True,
            "False"      => Word::False,
            "Arguments"  => Word::Arguments,
            "Eval"       => Word::Eval,
            "Break"      => Word::Break,
            "Case"       => Word::Case,
            "Catch"      => Word::Catch,
            "Class"      => Word::Class,
            "Const"      => Word::Const,
            "Continue"   => Word::Continue,
            "Debugger"   => Word::Debugger,
            "Default"    => Word::Default,
            "Delete"     => Word::Delete,
            "Do"         => Word::Do,
            "Else"       => Word::Else,
            "Export"     => Word::Export,
            "Extends"    => Word::Extends,
            "Finally"    => Word::Finally,
            "For"        => Word::For,
            "Function"   => Word::Function,
            "If"         => Word::If,
            "Import"     => Word::Import,
            "In"         => Word::In,
            "Instanceof" => Word::Instanceof,
            "Let"        => Word::Let,
            "New"        => Word::New,
            "Return"     => Word::Return,
            "Static"     => Word::Static,
            "Super"      => Word::Super,
            "Switch"     => Word::Switch,
            "This"       => Word::This,
            "Throw"      => Word::Throw,
            "Try"        => Word::Try,
            "Typeof"     => Word::Typeof,
            "Var"        => Word::Var,
            "Void"       => Word::Void,
            "While"      => Word::While,
            "With"       => Word::With,
            "Yield"      => Word::Yield,
            "Enum"       => Word::Enum,
            "Await"      => Word::Await,
            "Implements" => Word::Implements,
            "Interface"  => Word::Interface,
            "Package"    => Word::Package,
            "Private"    => Word::Private,
            "Protected"  => Word::Protected,
            "Public"     => Word::Public,
            _            => { return string_error("keyword", self); }
        })
    }
}

pub trait IntoNode {
    fn into_program(self) -> Deserialize<Script>;
    fn into_statement_list_item(self) -> Deserialize<StmtListItem>;
    fn into_var_declarator(self) -> Deserialize<VarDtor>;
    fn into_statement(self) -> Deserialize<Stmt>;
    fn into_expression(self) -> Deserialize<Expr>;
    fn into_identifier(self) -> Deserialize<Id>;
    fn into_literal(self) -> Deserialize<Expr>;
    fn into_function(self) -> Deserialize<Fun>;
    fn into_pattern(self) -> Deserialize<Patt>;
}

impl IntoNode for Json {
    fn into_program(self) -> Deserialize<Script> {
        self.into_object().and_then(|obj| obj.into_program())
    }

    fn into_statement_list_item(self) -> Deserialize<StmtListItem> {
        self.into_object().and_then(|obj| obj.into_statement_list_item())
    }

    fn into_var_declarator(self) -> Deserialize<VarDtor> {
        self.into_object().and_then(|obj| obj.into_var_declarator())
    }

    fn into_statement(self) -> Deserialize<Stmt> {
        self.into_object().and_then(|obj| obj.into_statement())
    }

    fn into_expression(self) -> Deserialize<Expr> {
        self.into_object().and_then(|obj| obj.into_expression())
    }

    fn into_identifier(self) -> Deserialize<Id> {
        self.into_object().and_then(|obj| obj.into_identifier())
    }

    fn into_literal(self) -> Deserialize<Expr> {
        self.into_object().and_then(|obj| obj.into_literal())
    }

    fn into_function(self) -> Deserialize<Fun> {
        self.into_object().and_then(|obj| obj.into_function())
    }

    fn into_pattern(self) -> Deserialize<Patt> {
        self.into_object().and_then(|obj| obj.into_pattern())
    }
}

impl IntoNode for Object {
    fn into_program(mut self) -> Deserialize<Script> {
        Ok((ScriptData {
            body: try!(self.extract("body").and_then(|data| data.into_statement_list()))
        }).tracked(None))
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

    fn into_var_declarator(mut self) -> Deserialize<VarDtor> {
        let id = try!(self.extract_id("id"));
        let init = match try!(self.extract_object_opt("init")) {
            None      => None,
            Some(obj) => Some(try!(obj.into_expression()))
        };
        Ok((VarDtorData {
            id: (PattData::Id(id)).tracked(None),
            init: init
        }).tracked(None))
    }

    fn into_expression(self) -> Deserialize<Expr> {
        match &(try!(self.peek_type()))[..] {
            "Identifier" => self.into_identifier().map(|id| id.into_expr()),
            "Literal"    => self.into_literal(),
            // FIXME: implement remaining cases
            _            => { return object_error("expression", self); }
        }
    }

    fn into_identifier(mut self) -> Deserialize<Id> {
        Ok((IdData { name: try!(self.extract_string("name")) }).tracked(None))
    }

    fn into_literal(mut self) -> Deserialize<Expr> {
        let val = try!(self.extract("value"));
        match val {
            Json::Null => Ok(ExprData::Null.tracked(None)),
            // FIXME: implement remaining cases
            _          => type_error("null, number, boolean, or string", json_typeof(&val))
        }
    }

    fn into_function(mut self) -> Deserialize<Fun> {
        let id = try!(self.extract_id_opt("id"));
        let params = (ParamsData {
            list: try!(self.extract("params").and_then(|data| data.into_pattern_list()))
        }).tracked(None);
        let body = match try!(self.extract_object("body").and_then(|obj| obj.into_statement())).value {
            StmtData::Block(items) => items,
            node                   => { return node_error("BlockStatement", format!("{:?}", node)); }
        };
        Ok((FunData { id: id, params: params, body: body }).tracked(None))
    }

    fn into_statement(mut self) -> Deserialize<Stmt> {
        let ty = try!(self.extract("type").and_then(|data| data.into_string()));
        match &ty[..] {
            "VariableDeclaration" => {
                let dtors = try!(self.extract("declarations").and_then(|data| data.into_var_declarator_list()));
                Ok(StmtData::Var(dtors, Semi::Explicit(None)).tracked(None))
            }
            "EmptyStatement" => {
                Ok(StmtData::Empty.tracked(None))
            }
            "ExpressionStatement" => {
                let expr = try!(self.extract_object("expression").and_then(|obj| obj.into_expression()));
                Ok(StmtData::Expr(expr, Semi::Explicit(None)).tracked(None))
            }
            "IfStatement" => {
                let test = try!(self.extract_object("test").and_then(|obj| obj.into_expression()));
                let cons = Box::new(try!(self.extract_object("consequent").and_then(|obj| obj.into_statement())));
                let alt = match try!(self.extract_object_opt("alternate")) {
                    None => None,
                    Some(data) => Some(Box::new(try!(data.into_statement())))
                };
                Ok(StmtData::If(test, cons, alt).tracked(None))
            }
            "DoWhileStatement" => {
                let body = Box::new(try!(self.extract_object("body").and_then(|obj| obj.into_statement())));
                let test = try!(self.extract_object("test").and_then(|obj| obj.into_expression()));
                Ok(StmtData::DoWhile(body, test, Semi::Explicit(None)).tracked(None))
            }
            "WhileStatement" => {
                let test = try!(self.extract_object("test").and_then(|obj| obj.into_expression()));
                let body = Box::new(try!(self.extract_object("body").and_then(|obj| obj.into_statement())));
                Ok(StmtData::While(test, body).tracked(None))
            }
            "BlockStatement" => {
                let body = try!(self.extract("body").and_then(|data| data.into_statement_list()));
                Ok(StmtData::Block(body).tracked(None))
            }
            "ReturnStatement" => {
                let arg = match try!(self.extract_object_opt("argument")) {
                    None      => None,
                    Some(obj) => Some(try!(obj.into_expression()))
                };
                Ok(StmtData::Return(arg, Semi::Explicit(None)).tracked(None))
            }
            "LabeledStatement" => {
                let label = try!(self.extract_id("label"));
                let body = Box::new(try!(self.extract_object("body").and_then(|obj| obj.into_statement())));
                Ok(StmtData::Label(label, body).tracked(None))
            }
            "BreakStatement" => {
                let label = try!(self.extract_id_opt("label"));
                Ok(StmtData::Break(label, Semi::Explicit(None)).tracked(None))
            }
            // FIXME: remaining statement cases
            _ => string_error("statement type", ty)
        }
    }

    fn into_pattern(self) -> Deserialize<Patt> {
        self.into_identifier().map(|id| PattData::Id(id).tracked(None))
    }
}

trait IntoNodeList {
    fn into_statement_list(self) -> Deserialize<Vec<StmtListItem>>;
    fn into_var_declarator_list(self) -> Deserialize<Vec<VarDtor>>;
    fn into_pattern_list(self) -> Deserialize<Vec<Patt>>;
}

impl IntoNodeList for json::Array {
    fn into_statement_list(self) -> Deserialize<Vec<StmtListItem>> {
        let mut list = Vec::with_capacity(self.len());
        for data in self {
            list.push(try!(data.into_statement_list_item()));
        }
        Ok(list)
    }

    fn into_var_declarator_list(self) -> Deserialize<Vec<VarDtor>> {
        let mut list = Vec::with_capacity(self.len());
        for data in self {
            list.push(try!(data.into_var_declarator()));
        }
        Ok(list)
    }

    fn into_pattern_list(self) -> Deserialize<Vec<Patt>> {
        let mut list = Vec::with_capacity(self.len());
        for data in self {
            list.push(try!(data.into_pattern()));
        }
        Ok(list)
    }
}

impl IntoNodeList for Json {
    fn into_statement_list(self) -> Deserialize<Vec<StmtListItem>> {
        self.into_array().and_then(|arr| arr.into_statement_list())
    }

    fn into_var_declarator_list(self) -> Deserialize<Vec<VarDtor>> {
        self.into_array().and_then(|arr| arr.into_var_declarator_list())
    }

    fn into_pattern_list(self) -> Deserialize<Vec<Patt>> {
        self.into_array().and_then(|arr| arr.into_pattern_list())
    }
}

pub trait ExtractField {
    fn extract(&mut self, &'static str) -> Deserialize<Json>;
    fn extract_string(&mut self, &'static str) -> Deserialize<String>;
    fn extract_array(&mut self, &'static str) -> Deserialize<json::Array>;
    fn extract_object(&mut self, &'static str) -> Deserialize<Object>;
    fn extract_object_opt(&mut self, &'static str) -> Deserialize<Option<Object>>;
    fn extract_id(&mut self, &'static str) -> Deserialize<Id>;
    fn extract_id_opt(&mut self, &'static str) -> Deserialize<Option<Id>>;
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

    fn extract_object(&mut self, name: &'static str) -> Deserialize<Object> {
        self.extract(name).and_then(|data| data.into_object())
    }

    fn extract_object_opt(&mut self, name: &'static str) -> Deserialize<Option<Object>> {
        self.extract(name).and_then(|data| data.into_object_opt())
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

    fn peek_type(&self) -> Deserialize<&String> {
        match self.get("type") {
            None                       => Err(DeserializeError::MissingField("type")),
            Some(&Json::String(ref s)) => { Ok(s) },
            Some(data)                 => type_error("string", json_typeof(&data))
        }
    }
}
