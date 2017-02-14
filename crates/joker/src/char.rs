use ucd::Codepoint;

pub trait ESCharExt {
    fn is_es_newline(self) -> bool;
    fn is_es_whitespace(self) -> bool;
    fn is_es_identifier_start(self) -> bool;
    fn is_es_identifier_continue(self) -> bool;
    fn unescape(self) -> char;
    fn is_es_hex_digit(self) -> bool;
    fn is_es_dec_digit(self) -> bool;
    fn is_es_oct_digit(self) -> bool;
    fn is_es_bin_digit(self) -> bool;
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

    fn is_es_identifier_start(self) -> bool {
        match self {
            '$' | '_' => true,
            _ => self.is_id_start()
        }
    }

    fn is_es_identifier_continue(self) -> bool {
        match self {
            '$' | '_' | '\u{200C}' | '\u{200D}' => true,
            _ => self.is_id_continue()
        }
    }

    fn unescape(self) -> char {
        match self {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'b' => '\x08',
            'v' => '\x0B',
            'f' => '\x0C',
            _ => self
        }
    }

    fn is_es_hex_digit(self) -> bool {
        match self {
              '0'...'9'
            | 'a'...'f'
            | 'A'...'F' => true,
            _ => false
        }
    }

    fn is_es_dec_digit(self) -> bool {
        match self {
            '0'...'9' => true,
            _ => false
        }
    }

    fn is_es_oct_digit(self) -> bool {
        match self {
            '0'...'7' => true,
            _ => false
        }
    }

    fn is_es_bin_digit(self) -> bool {
        self == '0' || self == '1'
    }
}
