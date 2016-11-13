//use std::collections::LinkedList;
use token::Token;

// test case: x=0;y=g=1;alert(eval("while(x)break\n/y/g.exec('y')"))
//       see: https://groups.google.com/d/msg/mozilla.dev.tech.js-engine.internals/2JLH5jRcr7E/Mxc7ZKc5r6sJ

pub struct Buffer {
    //tokens: LinkedList<Token>
    token: Option<Token>
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            token: None
            //tokens: LinkedList::new()
        }
    }

    pub fn is_empty(&self) -> bool {
        //self.tokens.len() == 0
        self.token.is_none()
    }

    pub fn push_token(&mut self, token: Token) {
        //assert!(self.tokens.len() == 0);
        //self.tokens.push_back(token);
        debug_assert!(self.token.is_none());
        self.token = Some(token)
    }

    pub fn read_token(&mut self) -> Token {
        //assert!(self.tokens.len() > 0);
        //self.tokens.pop_front().unwrap()
        debug_assert!(self.token.is_some());
        self.token.take().unwrap()
    }

    pub fn peek_token(&self) -> &Token {
        //assert!(self.tokens.len() > 0);
        //self.tokens.front().unwrap()
        debug_assert!(self.token.is_some());
        self.token.as_ref().unwrap()
    }

    pub fn unread_token(&mut self, token: Token) {
        //assert!(self.tokens.len() < 3);
        //self.tokens.push_front(token);
        debug_assert!(self.token.is_none());
        self.token = Some(token);
    }
}
