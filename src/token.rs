use regex::Regex;

#[derive(Clone, Debug)]
pub enum PrimitiveType<'a> {
    String(&'a str),
    Pattern(Regex),
    Integer(i64),
    Float(f64),
}

#[derive(Clone, Debug)]
pub struct RePattern {
    pub value: Regex,
}

impl<'a> PartialEq for PrimitiveType<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Pattern(a), Self::Pattern(b)) => a.to_string() == b.to_string(),
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a.to_string() == b.to_string(),
            _ => false,
        }
    }
}

impl<'a> Eq for PrimitiveType<'a> {}

#[derive(Clone, PartialEq, Eq, Debug)]
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
    Percent,
    PercentEqual,
    Carrot,
    CarrotEqual,
    Question,
    Dollar,
    Newline,

    Plus,
    PlusPlus,
    PlusEqual,
    Minus,
    MinusMinus,
    MinusEqual,
    Slash,
    SlashEqual,
    Star,
    StarEqual,
    And,
    Or,
    Not,

    Begin,
    End,
    If,
    Else,
    Do,
    While,
    For,
    In,
    Break,
    Continue,
    Next,
    Return,
    Exit,
    Function,
    Print,
    Delete,
    Eof,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Token<'a> {
    pub value: TokenType<'a>,
    pub row: usize,
    pub col: usize,
}

impl<'a> Token<'a> {
    pub fn new(value: TokenType<'a>, row: usize, col: usize) -> Self {
        Self { value, row, col }
    }
}
