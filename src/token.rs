use regex::Regex;

#[derive(Clone, Debug)]
pub enum PrimitiveType<'a> {
    String(&'a str),
    Pattern(Regex),
    Integer(i64),
    Float(f64),
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

#[derive(Clone, Debug)]
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
    Comma,

    Equal,
    EqualEqual,
    NotEqual,
    LessThan,
    RightRight,
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
    Pipe,

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
    Printf,
    Getline,
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

impl<'a> Eq for TokenType<'a> {}
impl<'a> PartialEq for TokenType<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier(_), Self::Identifier(_)) => true,
            (Self::Literal(_), Self::Literal(_)) => true,
            (Self::LeftCurly, Self::LeftCurly) => true,
            (Self::RightCurly, Self::RightCurly) => true,
            (Self::LeftBracket, Self::LeftBracket) => true,
            (Self::RightBracket, Self::RightBracket) => true,
            (Self::LeftParen, Self::LeftParen) => true,
            (Self::RightParen, Self::RightParen) => true,
            (Self::Semicolon, Self::Semicolon) => true,
            (Self::Colon, Self::Colon) => true,
            (Self::Comma, Self::Comma) => true,
            (Self::Equal, Self::Equal) => true,
            (Self::EqualEqual, Self::EqualEqual) => true,
            (Self::NotEqual, Self::NotEqual) => true,
            (Self::LessThan, Self::LessThan) => true,
            (Self::GreaterThan, Self::GreaterThan) => true,
            (Self::LessEqual, Self::LessEqual) => true,
            (Self::GreaterEqual, Self::GreaterEqual) => true,
            (Self::Tilde, Self::Tilde) => true,
            (Self::NotTilde, Self::NotTilde) => true,
            (Self::Percent, Self::Percent) => true,
            (Self::PercentEqual, Self::PercentEqual) => true,
            (Self::Carrot, Self::Carrot) => true,
            (Self::CarrotEqual, Self::CarrotEqual) => true,
            (Self::Question, Self::Question) => true,
            (Self::Dollar, Self::Dollar) => true,
            (Self::Newline, Self::Newline) => true,
            (Self::Plus, Self::Plus) => true,
            (Self::PlusPlus, Self::PlusPlus) => true,
            (Self::PlusEqual, Self::PlusEqual) => true,
            (Self::Minus, Self::Minus) => true,
            (Self::MinusMinus, Self::MinusMinus) => true,
            (Self::MinusEqual, Self::MinusEqual) => true,
            (Self::Slash, Self::Slash) => true,
            (Self::SlashEqual, Self::SlashEqual) => true,
            (Self::Star, Self::Star) => true,
            (Self::StarEqual, Self::StarEqual) => true,
            (Self::And, Self::And) => true,
            (Self::Or, Self::Or) => true,
            (Self::Not, Self::Not) => true,
            (Self::Begin, Self::Begin) => true,
            (Self::End, Self::End) => true,
            (Self::If, Self::If) => true,
            (Self::Else, Self::Else) => true,
            (Self::Do, Self::Do) => true,
            (Self::While, Self::While) => true,
            (Self::For, Self::For) => true,
            (Self::In, Self::In) => true,
            (Self::Break, Self::Break) => true,
            (Self::Continue, Self::Continue) => true,
            (Self::Next, Self::Next) => true,
            (Self::Return, Self::Return) => true,
            (Self::Exit, Self::Exit) => true,
            (Self::Function, Self::Function) => true,
            (Self::Print, Self::Print) => true,
            (Self::Printf, Self::Printf) => true,
            (Self::Delete, Self::Delete) => true,
            (Self::Pipe, Self::Pipe) => true,
            (Self::RightRight, Self::RightRight) => true,
            (Self::Eof, Self::Eof) => true,
            (_, _) => false,
        }
    }
}
