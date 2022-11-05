use std::{mem::discriminant, slice::Iter};

use crate::token::{PrimitiveType, Token, TokenType};

#[derive(PartialEq, Eq)]
pub enum Statement<'a> {
    Break,
    Continue,
    Next,
    Expression(Expression<'a>),
    IoStatement, // TODO: figure out
    Print(Expression<'a>),
    Exit(Expression<'a>),
    Return(Expression<'a>),
    Delete(Expression<'a>), // TODO: this should eval to Integer
    If(Expression<'a>, Vec<Box<Statement<'a>>>),
    IfElse(
        Expression<'a>,
        Vec<Box<Statement<'a>>>, // if statement
        Vec<Box<Statement<'a>>>, // else statements
    ),
    DoWhile(Vec<Box<Statement<'a>>>, Expression<'a>),
    While(Expression<'a>, Vec<Box<Statement<'a>>>),
    For(
        Expression<'a>,
        Expression<'a>,
        Expression<'a>,
        Vec<Box<Statement<'a>>>,
    ),
    ForIn(Expression<'a>, Vec<Box<Statement<'a>>>),
}

#[derive(PartialEq, Eq)]
pub enum Expression<'a> {
    Empty,
    Literal(PrimitiveType<'a>),
    Grouping(Box<Expression<'a>>),
    Function(&'a str, Box<Expression<'a>>),
    Unary(TokenType<'a>, Box<Expression<'a>>),
    Binary(Box<Expression<'a>>, TokenType<'a>, Box<Expression<'a>>),
}

pub struct Program<'a> {
    functions: Vec<(&'a str, Vec<Statement<'a>>)>,
    begin: Vec<Statement<'a>>,
    end: Vec<Statement<'a>>,
    actions: Vec<(Expression<'a>, Vec<Statement<'a>>)>,
}

pub struct Parser<'a> {
    cur: Iter<'a, Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self { cur: tokens.iter() }
    }

    fn peek(&self, tokens: Vec<TokenType<'a>>) -> bool {
        let mut _cur = self.cur.clone();
        for token in tokens {
            match (_cur.next(), token) {
                (None, _) => return false,
                (Some(lhs), rhs) if discriminant(&lhs.value) != discriminant(&rhs) => return false,
                _ => {}
            }
        }
        true
    }

    fn advance(&mut self, tokens: Vec<TokenType<'a>>) {
        let mut prev: Option<&Token> = None;
        for token in tokens {
            let val = self.cur.next();
            match (val, token) {
                (None, _) => panic!(
                    "expected: `{{`, @ {}:{}",
                    prev.unwrap().row,
                    prev.unwrap().col,
                ),
                (Some(lhs), rhs) if lhs.value != rhs => panic!(
                    "expected: `{{`, @ {}:{}",
                    prev.unwrap().row,
                    prev.unwrap().col,
                ),
                _ => {}
            }
            prev = val;
        }
    }

    fn begin(&mut self, dest: &mut Vec<Statement<'a>>) {
        self.advance(vec![TokenType::Begin, TokenType::LeftCurly]);
        while self.statement(dest) {
            todo!()
        }
        self.advance(vec![TokenType::RightCurly]);
    }

    fn end(&self, dest: &mut Vec<Statement<'a>>) {
        todo!()
    }

    fn function(&self, dest: &mut Vec<(&'a str, Vec<Statement<'a>>)>) {
        todo!()
    }

    fn action(&self, dest: &mut Vec<(Expression<'a>, Vec<Statement<'a>>)>) {
        todo!()
    }

    fn statement(&self, dest: &mut Vec<Statement<'a>>) -> bool {
        todo!()
    }

    fn expression(&self) -> Expression {
        todo!()
    }

    pub fn parse(&mut self) -> Program {
        let mut functions = Vec::<(&'a str, Vec<Statement<'a>>)>::new();
        let mut begin = Vec::<Statement<'a>>::new();
        let mut end = Vec::<Statement<'a>>::new();
        let mut actions = Vec::<(Expression<'a>, Vec<Statement<'a>>)>::new();

        loop {
            let mut cur = self.cur.clone().peekable();
            match cur.peek() {
                None => break,
                Some(token) => match token.value {
                    TokenType::Begin => self.begin(&mut begin),
                    TokenType::End => self.end(&mut end),
                    TokenType::Function => self.function(&mut functions),
                    _ => self.action(&mut actions),
                },
            }
        }

        return Program {
            functions,
            begin,
            end,
            actions,
        };
    }
}
