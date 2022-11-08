use std::slice::Iter;

use crate::token::{PrimitiveType, Token, TokenType};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Statement<'a> {
    Empty,
    Block(Vec<Box<Statement<'a>>>),
    Break,
    Continue,
    Next,
    Expression(Expression<'a>),
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expression<'a> {
    Empty,
    Literal(PrimitiveType<'a>),
    Variable(&'a str),
    FieldVariable(Box<Expression<'a>>),
    ArrayVariable(Box<Expression<'a>>),
    Grouping(Box<Expression<'a>>),
    Function(&'a str, Box<Vec<Expression<'a>>>),
    Unary(UnaryOperator, Box<Expression<'a>>),
    Binary(BinaryOperator, Box<Expression<'a>>, Box<Expression<'a>>),
}

impl<'a> Expression<'a> {
    fn precedence(&self) -> u8 {
        match self {
            Expression::Empty => 255,
            Expression::Literal(_) => 255,
            Expression::Variable(_) => 0,
            Expression::Function(_, _) => 0,
            Expression::Grouping(_) => 0,
            Expression::ArrayVariable(_) => 1,
            Expression::FieldVariable(_) => 2,
            Expression::Unary(op, _) => match op {
                UnaryOperator::PostPlusPlus
                | UnaryOperator::PostMinusMinus
                | UnaryOperator::PrePlusPlus
                | UnaryOperator::PreMinusMinus => 3,
                UnaryOperator::PrePlus | UnaryOperator::PreMinus | UnaryOperator::Not => 5,
            },
            Expression::Binary(op, _, _) => match op {
                BinaryOperator::Power => 4,
                BinaryOperator::Multiply | &BinaryOperator::Divide | BinaryOperator::Modulo => 6,
                BinaryOperator::Plus | BinaryOperator::Minus => 7,
                BinaryOperator::Concat => 8,
                BinaryOperator::EqualEqual
                | BinaryOperator::NotEqual
                | BinaryOperator::LessThan
                | BinaryOperator::LessEqual
                | BinaryOperator::GreaterThan
                | BinaryOperator::GreaterEqual => 9,
                BinaryOperator::Tilde | BinaryOperator::NotTilde => 10,
                BinaryOperator::In => 11,
                BinaryOperator::And => 12,
                BinaryOperator::Or => 13,
                BinaryOperator::Ternary => 14,
                BinaryOperator::Equal => 15,
            },
        }
    }

    fn is_precedent_to(&self, t: &TokenType) -> Option<bool> {
        Some(match t {
            TokenType::PlusPlus | TokenType::MinusMinus => self.precedence() <= 3,
            TokenType::Carrot => self.precedence() <= 4,
            TokenType::Star | TokenType::Slash | TokenType::Percent => self.precedence() <= 5,
            TokenType::Plus | TokenType::Minus => self.precedence() <= 7, // treating these only as binary here
            TokenType::Identifier(v) if v == &"in" => self.precedence() <= 11,
            TokenType::LeftParen | TokenType::Literal(_) | TokenType::Identifier(_) => {
                self.precedence() <= 8
            } // treating these as concat
            TokenType::EqualEqual
            | TokenType::NotEqual
            | TokenType::LessThan
            | TokenType::LessEqual
            | TokenType::GreaterThan
            | TokenType::GreaterEqual => self.precedence() <= 9,
            TokenType::Tilde | TokenType::NotTilde => self.precedence() <= 10,
            TokenType::And => self.precedence() <= 12,
            TokenType::Or => self.precedence() <= 13,
            TokenType::Question => todo!(), // FIXME: need to check in fn and keep parsing ternary
            TokenType::Equal
            | TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::StarEqual
            | TokenType::SlashEqual
            | TokenType::PercentEqual
            | TokenType::CarrotEqual => self.precedence() <= 14,
            _ => return None,
        })
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum UnaryOperator {
    PrePlus,
    PrePlusPlus,
    PreMinus,
    PreMinusMinus,
    PostPlusPlus,
    PostMinusMinus,
    Not,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BinaryOperator {
    Concat,
    EqualEqual,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Tilde,
    NotTilde,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Power,
    Equal,
    Or,
    And,
    Ternary,
    In,
}

impl BinaryOperator {
    fn convert(t: &TokenType) -> Self {
        match t {
            TokenType::Carrot => Self::Power,
            TokenType::Star => Self::Multiply,
            TokenType::Slash => Self::Divide,
            TokenType::Percent => Self::Modulo,
            TokenType::Plus => Self::Plus,
            TokenType::Minus => Self::Minus,
            TokenType::Identifier(v) if v == &"in" => Self::In,
            TokenType::LeftParen | TokenType::Literal(_) | TokenType::Identifier(_) => Self::Concat,
            TokenType::EqualEqual => Self::EqualEqual,
            TokenType::NotEqual => Self::NotEqual,
            TokenType::LessThan => Self::LessThan,
            TokenType::LessEqual => Self::LessEqual,
            TokenType::GreaterThan => Self::GreaterThan,
            TokenType::GreaterEqual => Self::GreaterEqual,
            TokenType::Tilde => Self::Tilde,
            TokenType::NotTilde => Self::NotTilde,
            TokenType::And => Self::And,
            TokenType::Or => Self::Or,
            TokenType::Question => todo!(), // FIXME: need to check in fn and keep parsing ternary
            TokenType::Equal => Self::Equal,
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::StarEqual
            | TokenType::SlashEqual
            | TokenType::PercentEqual
            | TokenType::CarrotEqual => Self::Equal,
            _ => unreachable!(),
        }
    }
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
        for t in types {
            match (_cur.next(), t) {
                (None, _) => return false,
                (Some(lhs), rhs) if lhs.value != rhs => return false,
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
            match (val, &token) {
                (None, _) => panic!(
                    "expected: `{:?}`, @ {}:{}",
                    &token,
                    prev.unwrap_or(&Token::new(TokenType::Comma, 0, 0)).row,
                    prev.unwrap_or(&Token::new(TokenType::Comma, 0, 0)).col,
                ),
                (Some(lhs), rhs) if lhs.value != *rhs => panic!(
                    "expected: `{:?}`, @ {}:{}",
                    &token,
                    prev.unwrap_or(&Token::new(TokenType::Comma, 0, 0)).row,
                    prev.unwrap_or(&Token::new(TokenType::Comma, 0, 0)).col,
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
        let mut is_first = true;
        while !self.peek().is_none() && !self.check_one(TokenType::RightParen) {
            if !is_first {
                self.advance_one(TokenType::Comma);
            }
            // parse parameters as variable declarations in function scope
            statements.push(Statement::Expression(Expression::Binary(
                BinaryOperator::Equal,
                Box::new(self.expression()),
                Box::new(Expression::Literal(PrimitiveType::Integer(0))),
            )));
            is_first = false;
        }
        self.advance(vec![TokenType::RightParen, TokenType::LeftCurly]);
        statements.extend(self.statements());
        self.advance_one(TokenType::RightCurly);
        dest.push((name, statements));
    }

    fn action(&mut self, dest: &mut Vec<(Expression<'a>, Vec<Statement<'a>>)>) {
        let expression = self.expression();
        self.advance_one(TokenType::LeftCurly);
        let statements = self.statements();
        self.advance_one(TokenType::RightCurly);
        dest.push((expression, statements));
    }

    fn skip_newlines_and_semicolons(&mut self) {
        // handle trailing semicolons
        while self.check_one(TokenType::Semicolon) | self.check_one(TokenType::Newline) {
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
        self.skip_newlines_and_semicolons();
        let ret = match self.peek() {
            None => Statement::Empty,
            Some(token) => match token.value {
                TokenType::Break => {
                    self.skip_by(1);
                    Statement::Break
                }
                TokenType::Continue => {
                    self.skip_by(1);
                    Statement::Continue
                }
                TokenType::Next => {
                    self.skip_by(1);
                    Statement::Next
                }
                TokenType::Return => {
                    self.skip_by(1);
                    Statement::Return(self.expression())
                }
                TokenType::Exit => {
                    self.skip_by(1);
                    Statement::Exit(self.expression())
                }
                TokenType::Print => {
                    self.skip_by(1);
                    Statement::Print(self.expression())
                }
                TokenType::Delete => {
                    self.skip_by(1);
                    Statement::Delete(self.expression())
                }
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
        self.skip_newlines_and_semicolons();
        ret
    }

    fn statements(&mut self) -> Vec<Statement<'a>> {
        // parse statements either until the end of the tokens or until we encounter a right curly
        self.skip_newlines_and_semicolons();
        let mut ret = Vec::<Statement<'a>>::new();
        while !self.peek().is_none() && !self.check_one(TokenType::RightCurly) {
            let s = self.statement();
            if ret.is_empty() || s != Statement::Expression(Expression::Empty) {
                // no need to keep trailing empty expressions
                ret.push(s);
            }
        }
        ret
    }

    fn boxed_statements(&mut self) -> Vec<Box<Statement<'a>>> {
        // parse statements either until the end of the tokens or until we encounter a right curly
        self.skip_newlines_and_semicolons();
        let mut ret = Vec::<Box<Statement<'a>>>::new();
        while !self.peek().is_none() && !self.check_one(TokenType::RightCurly) {
            ret.push(Box::new(self.statement()));
        }
        ret
    }

    fn wrap_expression(&mut self, expression: Expression<'a>) -> Expression<'a> {
        match self.peek() {
            None => expression,
            Some(t) => match expression.is_precedent_to(&t.value) {
                None => expression,
                _ if t.value == TokenType::Equal => {
                    self.skip_by(1);
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(expression.clone()),
                        Box::new(self.expression()),
                    )
                }
                _ if t.value == TokenType::MinusEqual => {
                    self.skip_by(1);
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(expression.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Minus,
                            Box::new(expression.clone()),
                            Box::new(self.expression()),
                        )),
                    )
                }
                _ if t.value == TokenType::PlusEqual => {
                    self.skip_by(1);
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(expression.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Plus,
                            Box::new(expression.clone()),
                            Box::new(self.expression()),
                        )),
                    )
                }
                _ if t.value == TokenType::StarEqual => {
                    self.skip_by(1);
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(expression.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Multiply,
                            Box::new(expression.clone()),
                            Box::new(self.expression()),
                        )),
                    )
                }
                _ if t.value == TokenType::SlashEqual => {
                    self.skip_by(1);
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(expression.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Divide,
                            Box::new(expression.clone()),
                            Box::new(self.expression()),
                        )),
                    )
                }
                _ if t.value == TokenType::PercentEqual => {
                    self.skip_by(1);
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(expression.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Divide,
                            Box::new(expression.clone()),
                            Box::new(self.expression()),
                        )),
                    )
                }
                Some(true)
                    if t.value == TokenType::PlusPlus || t.value == TokenType::MinusMinus =>
                {
                    let ret = Expression::Unary(
                        match t.value {
                            TokenType::PlusPlus => UnaryOperator::PostPlusPlus,
                            TokenType::MinusMinus => UnaryOperator::PostMinusMinus,
                            _ => unreachable!(),
                        },
                        Box::new(expression.clone()),
                    );
                    self.skip_by(1);
                    ret
                }
                Some(false)
                    if t.value == TokenType::PlusPlus || t.value == TokenType::MinusMinus =>
                {
                    let ret = Expression::Binary(
                        match t.value {
                            TokenType::PlusPlus => BinaryOperator::Plus,
                            TokenType::MinusMinus => BinaryOperator::Minus,
                            _ => unreachable!(),
                        },
                        Box::new(expression.clone()),
                        Box::new(Expression::Literal(PrimitiveType::Integer(1))),
                    );
                    self.skip_by(1);
                    ret
                }
                Some(true) => {
                    let ret = Expression::Binary(
                        BinaryOperator::convert(&t.value),
                        Box::new(expression),
                        Box::new(self.expression()),
                    );
                    self.skip_by(1);
                    ret
                }
                Some(false) => {
                    let ret = Expression::Binary(
                        BinaryOperator::convert(&t.value),
                        Box::new(expression),
                        Box::new(self.expression()),
                    );
                    self.skip_by(1);
                    ret
                }
            },
        }
    }

    fn expression(&mut self) -> Expression<'a> {
        let expression: Expression<'a>;
        let ret = match self.peek() {
            None => Expression::Empty,
            Some(e) => match &e.value {
                TokenType::Literal(value) => {
                    let v = value.clone();
                    self.skip_by(1);
                    self.wrap_expression(Expression::Literal(v))
                }
                TokenType::LeftParen => {
                    self.skip_by(1);
                    expression = Expression::Grouping(Box::new(self.expression()));
                    self.wrap_expression(expression)
                }
                TokenType::Dollar => {
                    self.skip_by(1);
                    expression = Expression::FieldVariable(Box::new(self.expression()));
                    self.wrap_expression(expression)
                }
                TokenType::Identifier(name)
                    if self.check(vec![TokenType::Identifier(""), TokenType::LeftParen]) =>
                {
                    let n = *name;
                    self.skip_by(2);
                    let mut args = Vec::<Expression>::new();
                    let mut is_first = true;
                    while !self.peek().is_none() && !self.check_one(TokenType::RightParen) {
                        if !is_first {
                            self.advance_one(TokenType::Comma);
                        }
                        args.push(self.expression());
                        is_first = false;
                    }
                    let ret = Expression::Function(n, Box::new(args));
                    self.advance_one(TokenType::RightParen);
                    self.wrap_expression(ret)
                }
                TokenType::Identifier(_)
                    if self.check(vec![TokenType::Identifier(""), TokenType::LeftBracket]) =>
                {
                    self.skip_by(2);
                    let ret = Expression::ArrayVariable(Box::new(self.expression()));
                    self.advance_one(TokenType::RightBracket);
                    self.wrap_expression(ret)
                }
                TokenType::Identifier(name) => {
                    let n = *name;
                    self.skip_by(1);
                    self.wrap_expression(Expression::Variable(n))
                }
                TokenType::Plus
                | &TokenType::Minus
                | &TokenType::Not
                | &TokenType::PlusPlus
                | &TokenType::MinusMinus => {
                    let val = &e.value.clone();
                    self.skip_by(1);
                    expression = Expression::Unary(
                        match val {
                            &TokenType::Plus => UnaryOperator::PrePlus,
                            &TokenType::Minus => UnaryOperator::PreMinus,
                            &TokenType::Not => UnaryOperator::Not,
                            &TokenType::PlusPlus => UnaryOperator::PrePlusPlus,
                            &TokenType::MinusMinus => UnaryOperator::PreMinusMinus,
                            _ => unreachable!(),
                        },
                        Box::new(self.expression()),
                    );
                    self.wrap_expression(expression)
                }
                _ => Expression::Empty,
            },
        };
        ret
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

#[cfg(test)]

mod tests {
    use super::*;

    #[test]
    fn sample() {
        let tokens = vec![
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::Equal, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.actions,
            vec![(
                Expression::Binary(
                    BinaryOperator::Equal,
                    Box::new(Expression::Variable("a")),
                    Box::new(Expression::Literal(PrimitiveType::Integer(1)))
                ),
                Vec::<Statement>::new()
            )]
        )
    }

    #[test]
    fn function() {
        let tokens = vec![
            // function hello(a, b) { hello(1,2) }
            Token::new(TokenType::Function, 0, 0),
            Token::new(TokenType::Identifier("hello"), 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::Comma, 0, 0),
            Token::new(TokenType::Identifier("b"), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Identifier("hello"), 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::Comma, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(2)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.functions,
            vec![(
                "hello",
                vec![
                    Statement::Expression(Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(Expression::Variable("a")),
                        Box::new(Expression::Literal(PrimitiveType::Integer(0)))
                    )),
                    Statement::Expression(Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(Expression::Variable("b")),
                        Box::new(Expression::Literal(PrimitiveType::Integer(0)))
                    )),
                    Statement::Expression(Expression::Function(
                        "hello",
                        Box::new(vec![
                            Expression::Literal(PrimitiveType::Integer(1)),
                            Expression::Literal(PrimitiveType::Integer(2))
                        ])
                    ))
                ]
            )]
        )
    }
}
