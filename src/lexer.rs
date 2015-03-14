use token::Token;

pub struct Lexer<I> {
    chars: I
}

impl<I> Lexer<I> where I: Iterator<Item=char> {
    pub fn new(chars: I) -> Lexer<I> {
        Lexer { chars: chars }
    }
}

impl<I> Iterator for Lexer<I> where I: Iterator<Item=char> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let char = self.chars.by_ref().find(|c| !c.is_whitespace() );

        char.map(|chr| {
            match chr {
                '1' => Token::DecimalInt(String::from_str("1")),
                '+' => Token::Plus,
                _ => Token::Error(chr)
            }
        })
    }
}
