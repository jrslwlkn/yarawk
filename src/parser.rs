use crate::token::{PrimitiveType, Token, TokenType};

pub enum StatementType<'a> {
    Break,
    Continue,
    Next,
    Expression(ExpressionType<'a>),
    IoStatement, // TODO: figure out
    Print(ExpressionType<'a>),
    Exit(ExpressionType<'a>),
    Return(ExpressionType<'a>),
    Delete(ExpressionType<'a>), // TODO: this should eval to Integer
    If(ExpressionType<'a>, Vec<Box<StatementType<'a>>>),
    IfElse(
        ExpressionType<'a>,
        Vec<Box<StatementType<'a>>>, // if statement
        Vec<Box<StatementType<'a>>>, // else statements
    ),
    DoWhile(Vec<Box<StatementType<'a>>>, ExpressionType<'a>),
    While(ExpressionType<'a>, Vec<Box<StatementType<'a>>>),
    For(
        ExpressionType<'a>,
        ExpressionType<'a>,
        ExpressionType<'a>,
        Vec<Box<StatementType<'a>>>,
    ),
    ForIn(ExpressionType<'a>, Vec<Box<StatementType<'a>>>),
}

pub enum ExpressionType<'a> {
    Empty,
    Literal(PrimitiveType<'a>),
    Grouping(Box<ExpressionType<'a>>),
    Function(&'a str, Box<ExpressionType<'a>>),
    Unary(TokenType<'a>, Box<ExpressionType<'a>>),
    Binary(
        Box<ExpressionType<'a>>,
        TokenType<'a>,
        Box<ExpressionType<'a>>,
    ),
}

pub struct Program<'a> {
    functions: Vec<StatementType<'a>>,
    begin: Vec<StatementType<'a>>,
    end: Vec<StatementType<'a>>,
    actions: Vec<(ExpressionType<'a>, Vec<StatementType<'a>>)>,
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens }
    }

    fn peek(&self, start: usize, tokens: &Vec<Token>) -> bool {
        todo!()
    }

    pub fn parse(&self) -> Program {
        let mut functions = Vec::<StatementType<'a>>::new();
        let mut begin = Vec::<StatementType<'a>>::new();
        let mut end = Vec::<StatementType<'a>>::new();
        let mut actions = Vec::<(ExpressionType<'a>, Vec<StatementType<'a>>)>::new();
        for (idx, cur) in self.tokens.iter().enumerate() {}

        return Program {
            functions,
            begin,
            end,
            actions,
        };
    }
}
