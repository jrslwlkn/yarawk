use std::cmp::Ordering;

use regex::Regex;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Token<'a> {
    pub value: TokenType<'a>,
    pub row: usize,
    pub col: usize,
}

#[derive(Clone, Debug)]
pub enum TokenType<'a> {
    Identifier(&'a str),
    Literal(PrimitiveType),

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
    Getline,
    Delete,
    Eof,
}

#[derive(Clone, Debug)]
pub enum PrimitiveType {
    String(String),
    Pattern(Regex),
    Integer(i64),
    Float(f64),
}

impl PartialOrd for PrimitiveType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Integer(lhs), Self::Integer(rhs)) => Some(lhs.cmp(rhs)),
            (Self::Float(lhs), Self::Float(rhs)) => Some(if lhs > rhs {
                Ordering::Greater
            } else if lhs < rhs {
                Ordering::Less
            } else {
                Ordering::Equal
            }),
            (Self::Integer(lhs), Self::Float(rhs)) => Some(if *lhs as f64 > *rhs {
                Ordering::Greater
            } else if (*lhs as f64) < *rhs {
                Ordering::Less
            } else {
                Ordering::Equal
            }),
            (Self::Float(lhs), Self::Integer(rhs)) => Some(if *lhs > *rhs as f64 {
                Ordering::Greater
            } else if *lhs < *rhs as f64 {
                Ordering::Less
            } else {
                Ordering::Equal
            }),
            (Self::String(lhs), Self::String(rhs)) => Some(lhs.cmp(rhs)),
            (Self::Pattern(lhs), Self::Pattern(rhs)) => Some(lhs.to_string().cmp(&rhs.to_string())),
            (Self::String(lhs), Self::Pattern(rhs)) => Some(lhs.cmp(&rhs.to_string())),
            (Self::Pattern(lhs), Self::String(rhs)) => Some(lhs.to_string().cmp(rhs)),
            (Self::String(lhs), Self::Integer(rhs)) => {
                Some(lhs.parse::<i64>().unwrap_or(0).cmp(rhs))
            }
            (Self::Integer(lhs), Self::String(rhs)) => {
                Some(lhs.cmp(&rhs.parse::<i64>().unwrap_or(0)))
            }
            (Self::String(lhs), Self::Float(rhs)) => {
                lhs.parse::<f64>().unwrap_or(0.0).partial_cmp(rhs)
            }
            (Self::Float(lhs), Self::String(rhs)) => {
                lhs.partial_cmp(&rhs.parse::<f64>().unwrap_or(0.0))
            }
            _ => None,
        }
    }
}

impl Ord for PrimitiveType {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(other) {
            None => Ordering::Equal,
            Some(ret) => ret,
        }
    }
}

impl PartialEq for PrimitiveType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Pattern(a), Self::Pattern(b)) => a.to_string() == b.to_string(),
            (Self::String(a), Self::Pattern(b)) => *a == b.to_string(),
            (Self::Pattern(a), Self::String(b)) => a.to_string() == *b,
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Integer(a), Self::Float(b)) => *a as f64 == *b,
            (Self::Float(a), Self::Integer(b)) => *a == *b as f64,
            _ => false,
        }
    }
}

impl PrimitiveType {
    pub fn to_string(&self) -> String {
        match self {
            Self::String(val) => val.to_string(),
            Self::Pattern(val) => val.to_string(),
            Self::Integer(val) => val.to_string(),
            Self::Float(val) => val.to_string(),
        }
    }

    pub fn to_regex(&self) -> Regex {
        match self {
            Self::String(val) => Regex::new(val.to_string().as_str()).unwrap(),
            Self::Pattern(val) => val.clone(),
            Self::Integer(val) => Regex::new(val.to_string().as_str()).unwrap(),
            Self::Float(val) => Regex::new(val.to_string().as_str()).unwrap(),
        }
    }
}

impl Eq for PrimitiveType {}
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
            (Self::Delete, Self::Delete) => true,
            (Self::Pipe, Self::Pipe) => true,
            (Self::RightRight, Self::RightRight) => true,
            (Self::Eof, Self::Eof) => true,
            (_, _) => false,
        }
    }
}
