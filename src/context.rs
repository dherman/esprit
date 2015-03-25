#[derive(Debug, Copy, Eq, PartialEq)]
pub struct Context {
    pub asi: bool,
    pub operator: bool,
    pub comment_tokens: bool
}
