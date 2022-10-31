#[derive(Clone, Copy, Debug)]
pub enum PrimitiveType<'a> {
    String(&'a str),
    Pattern(&'a str),
    Integer(i64),
    Float(f64),
}

impl<'a> PartialEq for PrimitiveType<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Pattern(a), Self::Pattern(b)) => a == b,
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a.to_string() == b.to_string(),
            _ => false,
        }
    }
}

impl<'a> Eq for PrimitiveType<'a> {}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenType<'a> {
    Identifier(&'a str),
    Literal(PrimitiveType<'a>),

    LeftCurly,
    RightCurly,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    Semicolon,
    Colon,
    Dot,
    Comma,

    Equal,
    EqualEqual,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Tilde,
    NotTilde,

    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    Slash,
    Star,
    And,
    Or,
    Not,

    Begin,
    End,
    If,
    Else,
    While,
    For,
    In,
    Break,
    Continue,
    Return,
    Function,
    Eof,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Token<'a> {
    value: TokenType<'a>,
    row: usize,
    col: usize,
}

impl<'a> Token<'a> {
    pub fn new(value: TokenType<'a>, row: usize, col: usize) -> Self {
        Self { value, row, col }
    }
}
