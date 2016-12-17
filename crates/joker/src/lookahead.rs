use token::Token;
use std::collections::VecDeque;

// test case: x=0;y=g=1;alert(eval("while(x)break\n/y/g.exec('y')"))
//       see: https://groups.google.com/d/msg/mozilla.dev.tech.js-engine.internals/2JLH5jRcr7E/Mxc7ZKc5r6sJ

pub struct Buffer {
    tokens: VecDeque<Token>
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            tokens: VecDeque::with_capacity(4)
        }
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    pub fn read_token(&mut self) -> Token {
        self.tokens.pop_front().unwrap()
    }

    pub fn peek_token(&self) -> &Token {
        self.tokens.front().unwrap()
    }

    pub fn unread_token(&mut self, token: Token) {
        self.tokens.push_front(token)
    }
}
