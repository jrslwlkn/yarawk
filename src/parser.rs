use std::{mem::discriminant, slice::Iter};

use crate::token::{PrimitiveType, Token, TokenType};

#[derive(PartialEq, Eq, Clone)]
pub enum Statement<'a> {
    Empty,
    Block(Vec<Box<Statement<'a>>>),
    Break,
    Continue,
    Next,
    Expression(Expression<'a>),
    Assignment,
    IoStatement, // TODO: figure out
    Print(Expression<'a>),
    Exit(Expression<'a>),
    Return(Expression<'a>),
    Delete(Expression<'a>),
    If(
        Expression<'a>,
        Vec<Box<Statement<'a>>>,
        Vec<Box<Statement<'a>>>,
    ),
    DoWhile(Vec<Box<Statement<'a>>>, Expression<'a>),
    While(Expression<'a>, Vec<Box<Statement<'a>>>),
    For(
        Expression<'a>,
        Expression<'a>,
        Option<Expression<'a>>, // if None - for-in, otherwise normal for-loop
        Vec<Box<Statement<'a>>>,
    ),
}

#[derive(PartialEq, Eq, Clone)]
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

    fn peek(&self) -> Option<&Token<'a>> {
        let mut cur = self.cur.clone().peekable();
        let peeked = cur.peek();
        match peeked {
            None => None,
            Some(t) => Some(*t),
        }
    }

    fn check(&self, types: Vec<TokenType<'a>>) -> bool {
        let mut _cur = self.cur.clone();
        for token in types {
            match (_cur.next(), token) {
                (None, _) => return false,
                (Some(lhs), rhs) if discriminant(&lhs.value) != discriminant(&rhs) => return false,
                _ => {}
            }
        }
        true
    }

    fn check_one(&self, token_type: TokenType<'a>) -> bool {
        self.check(vec![token_type])
    }

    fn advance(&mut self, types: Vec<TokenType<'a>>) {
        let mut prev: Option<&Token> = None;
        for token in types {
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

    fn advance_one(&mut self, token_type: TokenType<'a>) {
        self.advance(vec![token_type])
    }

    fn begin(&mut self, dest: &mut Vec<Statement<'a>>) {
        self.advance(vec![TokenType::Begin, TokenType::LeftCurly]);
        dest.extend(self.statements());
        self.advance_one(TokenType::RightCurly);
    }

    fn end(&mut self, dest: &mut Vec<Statement<'a>>) {
        self.advance(vec![TokenType::End, TokenType::LeftCurly]);
        dest.extend(self.statements());
        self.advance_one(TokenType::RightCurly);
    }

    fn function(&mut self, dest: &mut Vec<(&'a str, Vec<Statement<'a>>)>) {
        self.advance_one(TokenType::Function);
        let name_token = self.peek();
        let mut name: &'a str = "";
        match name_token {
            Some(t) => match t.value {
                TokenType::Identifier(n) => name = n,
                _ => {}
            },
            None => {}
        }
        self.advance(vec![TokenType::Identifier(""), TokenType::LeftParen]);
        let mut statements = Vec::<Statement<'a>>::new();
        while !self.peek().is_none() && !self.check_one(TokenType::RightParen) {
            // parse parameters as variable declarations in function scope
            todo!();
        }
        self.advance(vec![TokenType::RightParen, TokenType::LeftCurly]);
        statements.extend(self.statements());
        self.advance_one(TokenType::RightCurly);
        dest.push((name, statements));
    }

    fn action(&mut self, dest: &mut Vec<(Expression<'a>, Vec<Statement<'a>>)>) {
        let expression = self.expression();
        self.advance_one(TokenType::LeftCurly);
        let mut statements = self.statements();
        self.advance_one(TokenType::RightCurly);
        dest.push((expression, statements));
    }

    fn skip_semicolons(&mut self) {
        // handle trailing semicolons
        while self.check_one(TokenType::Semicolon) {
            self.cur.next();
        }
    }

    fn skip_by(&mut self, num: usize) {
        for _ in 0..num {
            self.cur.next();
        }
    }

    // TODO: statements span until a semicolon or newline is encountered
    //       expressions span until a semicolon is encountered
    //       so within expressions, newlines may be fine
    fn statement(&mut self) -> Statement<'a> {
        self.skip_semicolons();
        let ret = match self.peek() {
            None => Statement::Empty,
            Some(token) => match token.value {
                TokenType::Break => Statement::Break,
                TokenType::Continue => Statement::Continue,
                TokenType::Next => Statement::Next,
                TokenType::Return => Statement::Return(self.expression()),
                TokenType::Exit => Statement::Exit(self.expression()),
                TokenType::Print => Statement::Print(self.expression()),
                TokenType::Delete => Statement::Delete(self.expression()),
                TokenType::If => {
                    self.advance(vec![TokenType::If, TokenType::LeftParen]);
                    let cond = self.expression();
                    self.advance_one(TokenType::RightParen);
                    let mut ifs = Vec::<Box<Statement<'a>>>::new();
                    let mut elses = Vec::<Box<Statement<'a>>>::new();
                    if self.check_one(TokenType::LeftCurly) {
                        self.advance_one(TokenType::LeftCurly);
                        ifs.extend(self.boxed_statements());
                        self.advance_one(TokenType::RightCurly);
                    } else {
                        ifs.extend(self.boxed_statements());
                    }
                    if self.check_one(TokenType::Else) {
                        if self.check_one(TokenType::LeftCurly) {
                            self.advance_one(TokenType::LeftCurly);
                            elses.extend(self.boxed_statements());
                            self.advance_one(TokenType::RightCurly);
                        } else {
                            elses.extend(self.boxed_statements());
                        }
                    }
                    Statement::If(cond, ifs, elses)
                }
                TokenType::Do => {
                    self.advance_one(TokenType::Do);
                    let mut statements = Vec::<Box<Statement<'a>>>::new();
                    if self.check_one(TokenType::LeftCurly) {
                        self.advance_one(TokenType::LeftCurly);
                        statements.extend(self.boxed_statements());
                        self.advance_one(TokenType::RightCurly);
                    } else {
                        statements.extend(self.boxed_statements());
                    }
                    self.advance(vec![TokenType::While, TokenType::LeftParen]);
                    let cond = self.expression();
                    self.advance_one(TokenType::RightParen);
                    Statement::DoWhile(statements, cond)
                }
                TokenType::While => {
                    self.advance(vec![TokenType::While, TokenType::LeftParen]);
                    let cond = self.expression();
                    self.advance_one(TokenType::RightParen);
                    let mut statements = Vec::<Box<Statement<'a>>>::new();
                    if self.check_one(TokenType::LeftCurly) {
                        self.advance_one(TokenType::LeftCurly);
                        statements.extend(self.boxed_statements());
                        self.advance_one(TokenType::RightCurly);
                    } else {
                        statements.extend(self.boxed_statements());
                    }
                    Statement::While(cond, statements)
                }
                TokenType::For => {
                    self.advance(vec![TokenType::For, TokenType::LeftParen]);
                    let expression1 = self.expression();
                    let expression2: Expression;
                    let mut expression3: Option<Expression> = None;
                    if self.check_one(TokenType::Semicolon) {
                        self.advance_one(TokenType::Semicolon);
                        expression2 = self.expression();
                        self.advance_one(TokenType::Semicolon);
                        expression3 = Some(self.expression());
                    } else if self.check_one(TokenType::In) {
                        self.advance_one(TokenType::In);
                        expression2 = self.expression();
                    } else {
                        panic!(
                            "expected: `;` or `in` @ {}:{}",
                            self.cur.next().unwrap().row,
                            self.cur.next().unwrap().col
                        );
                    }
                    self.advance_one(TokenType::RightParen);
                    let mut statements = Vec::<Box<Statement<'a>>>::new();
                    if self.check_one(TokenType::LeftCurly) {
                        self.advance_one(TokenType::LeftCurly);
                        statements.extend(self.boxed_statements());
                        self.advance_one(TokenType::RightCurly);
                    } else {
                        statements.extend(self.boxed_statements());
                    }
                    Statement::For(expression1, expression2, expression3, statements)
                }
                TokenType::LeftCurly => {
                    self.advance_one(TokenType::LeftCurly);
                    let block = Statement::Block(self.boxed_statements());
                    self.advance_one(TokenType::RightCurly);
                    block
                }
                _ => Statement::Expression(self.expression()), // FIXME: io expressions ?
            },
        };
        self.skip_by(1); // advance by matched token (could be a semicolon too)
        self.skip_semicolons();
        ret
    }

    fn statements(&mut self) -> Vec<Statement<'a>> {
        // parse statements either until the end of the tokens or until we encounter a right curly
        self.skip_semicolons();
        let mut ret = Vec::<Statement<'a>>::new();
        while !self.peek().is_none() && !self.check_one(TokenType::RightCurly) {
            ret.push(self.statement());
        }
        ret
    }

    fn boxed_statements(&mut self) -> Vec<Box<Statement<'a>>> {
        // parse statements either until the end of the tokens or until we encounter a right curly
        self.skip_semicolons();
        let mut ret = Vec::<Box<Statement<'a>>>::new();
        while !self.peek().is_none() && !self.check_one(TokenType::RightCurly) {
            ret.push(Box::new(self.statement()));
        }
        ret
    }

    fn expression(&mut self) -> Expression<'a> {
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
