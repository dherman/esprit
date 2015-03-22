use std::collections::LinkedList;
use token::Token;

// test case: x=0;y=g=1;alert(eval("while(x)break\n/y/g.exec('y')"))
//       see: https://groups.google.com/d/msg/mozilla.dev.tech.js-engine.internals/2JLH5jRcr7E/Mxc7ZKc5r6sJ

pub struct TokenBuffer {
    tokens: LinkedList<Token>
}

impl TokenBuffer {
    pub fn new() -> TokenBuffer {
        TokenBuffer {
            tokens: LinkedList::new()
        }
    }

    pub fn is_empty(&mut self) -> bool {
        self.tokens.len() == 0
    }

    pub fn push_token(&mut self, token: Token) {
        assert!(self.tokens.len() == 0);
        self.tokens.push_back(token);
    }

    pub fn read_token(&mut self) -> Token {
        assert!(self.tokens.len() > 0);
        self.tokens.pop_front().unwrap()
    }

    pub fn peek_token(&mut self) -> &Token {
        assert!(self.tokens.len() > 0);
        self.tokens.front().unwrap()
    }

    pub fn unread_token(&mut self, token: Token) {
        assert!(self.tokens.len() < 3);
        self.tokens.push_front(token);
    }
}
