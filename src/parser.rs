use crate::{
    parser_expression::{
        BinaryOperator, Expression, ExpressionItem, ExpressionTrace, UnaryOperator,
    },
    token::{PrimitiveType, Token, TokenType},
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Statement<'a> {
    Empty,
    Block(Vec<Box<Statement<'a>>>),
    Break,
    Continue,
    Next,
    Expression(Expression<'a>),
    IoStatement, // TODO: figure out
    Print(Vec<Expression<'a>>),
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

#[derive(Debug)]
pub struct Program<'a> {
    functions: Vec<(&'a str, Vec<Statement<'a>>)>,
    begin: Vec<Statement<'a>>,
    end: Vec<Statement<'a>>,
    actions: Vec<(Expression<'a>, Vec<Statement<'a>>)>,
}

#[derive(Clone)]
pub struct Iterator<'a, T> {
    original: &'a Vec<T>,
    index: isize,
}

impl<'a, T> Iterator<'a, T> {
    pub fn new(original: &'a Vec<T>) -> Self {
        Self {
            original,
            index: -1,
        }
    }

    pub fn current(&self) -> Option<&T> {
        if self.index < 0 || self.index as usize >= self.original.len() {
            return None;
        }
        self.original.get(self.index as usize)
    }

    pub fn prev(&mut self) -> Option<&T> {
        self.index -= 1;
        self.current()
    }

    pub fn next(&mut self) -> Option<&T> {
        self.index += 1;
        self.current()
    }

    pub fn peek_nth(&self, num: isize) -> Option<&T> {
        if self.index + num < 0 || (self.index + num) as usize >= self.original.len() {
            return None;
        }
        self.original.get((self.index + num) as usize)
    }

    pub fn peek_next(&self) -> Option<&T> {
        self.peek_nth(1)
    }

    pub fn peek_prev(&self) -> Option<&T> {
        self.peek_nth(-1)
    }
}

pub struct Parser<'a> {
    tokens: Iterator<'a, Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            tokens: Iterator::new(tokens),
        }
    }

    fn check(&self, types: Vec<TokenType<'a>>) -> bool {
        let mut cur = self.tokens.clone();
        for t in types {
            match (cur.next(), t) {
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
        for token in types {
            let val = &self.tokens.peek_next();
            match (val, &token) {
                (None, _) => panic!(
                    "expected: {:?} @ {}:{}",
                    &token,
                    self.tokens
                        .peek_next()
                        .unwrap_or(&Token::new(TokenType::Semicolon, 0, 0))
                        .row, // FIXME:
                    self.tokens
                        .peek_next()
                        .unwrap_or(&Token::new(TokenType::Semicolon, 0, 0))
                        .col,
                ),
                (Some(lhs), rhs) if lhs.value != *rhs => panic!(
                    "expected: {:?}, received: {:?}, @ {}:{}",
                    &token, lhs.value, lhs.row, lhs.col
                ),
                _ => {
                    self.tokens.next();
                }
            }
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
        let name_token = self.tokens.peek_next();
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
        while !self.tokens.peek_next().is_none() && !self.check_one(TokenType::RightParen) {
            if !is_first {
                self.advance_one(TokenType::Comma);
            }
            // parse parameters as variable declarations in function scope
            statements.push(Statement::Expression(Expression::Binary(
                BinaryOperator::Equal,
                Box::new(self.expression(ExpressionTrace::new())),
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
        let expression: Expression;
        if self.check_one(TokenType::LeftCurly) {
            expression = Expression::Empty;
        } else {
            expression = self.expression(ExpressionTrace::new());
        }
        self.advance_one(TokenType::LeftCurly);
        let statements = self.statements();
        self.advance_one(TokenType::RightCurly);
        dest.push((expression, statements));
    }

    fn skip_newlines_and_semicolons(&mut self) {
        // handle trailing semicolons
        while self.check_one(TokenType::Semicolon) | self.check_one(TokenType::Newline) {
            self.tokens.next();
        }
    }

    fn skip_by(&mut self, num: usize) {
        for _ in 0..num {
            self.tokens.next();
        }
    }

    fn statement(&mut self) -> Statement<'a> {
        self.skip_newlines_and_semicolons();
        let ret = match self.tokens.peek_next() {
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
                    Statement::Return(self.expression(ExpressionTrace::new()))
                }
                TokenType::Exit => {
                    self.skip_by(1);
                    Statement::Exit(self.expression(ExpressionTrace::new()))
                }
                TokenType::Print => {
                    self.skip_by(1);
                    let mut args = Vec::<Expression>::new();
                    let mut is_first = true;
                    while !self.tokens.peek_next().is_none()
                        && !self.check_one(TokenType::Semicolon)
                        && !self.check_one(TokenType::Newline)
                        && !self.check_one(TokenType::RightCurly)
                    {
                        if !is_first {
                            self.advance_one(TokenType::Comma);
                        }
                        is_first = false;
                        args.push(self.expression(ExpressionTrace::new()));
                    }
                    Statement::Print(args)
                }
                TokenType::Delete => {
                    self.skip_by(1);
                    Statement::Delete(self.expression(ExpressionTrace::new()))
                }
                TokenType::If => {
                    self.advance(vec![TokenType::If, TokenType::LeftParen]);
                    let cond = self.expression(ExpressionTrace::new());
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
                    let cond = self.expression(ExpressionTrace::new());
                    self.advance_one(TokenType::RightParen);
                    Statement::DoWhile(statements, cond)
                }
                TokenType::While => {
                    self.advance(vec![TokenType::While, TokenType::LeftParen]);
                    let cond = self.expression(ExpressionTrace::new());
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
                    let expression1 = self.expression(ExpressionTrace::new());
                    let expression2: Expression;
                    let mut expression3: Option<Expression> = None;
                    if self.check_one(TokenType::Semicolon) {
                        self.advance_one(TokenType::Semicolon);
                        expression2 = self.expression(ExpressionTrace::new());
                        self.advance_one(TokenType::Semicolon);
                        expression3 = Some(self.expression(ExpressionTrace::new()));
                    } else if self.check_one(TokenType::In) {
                        self.advance_one(TokenType::In);
                        expression2 = self.expression(ExpressionTrace::new());
                    } else {
                        panic!(
                            "expected: `;` or `in` @ {}:{}",
                            self.tokens.clone().current().unwrap().row,
                            self.tokens.clone().current().unwrap().col
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
                _ => Statement::Expression(self.expression(ExpressionTrace::new())), // FIXME: io expressions ?
            },
        };
        self.skip_newlines_and_semicolons();
        ret
    }

    fn statements(&mut self) -> Vec<Statement<'a>> {
        // parse statements either until the end of the tokens or until we encounter a right curly
        self.skip_newlines_and_semicolons();
        let mut ret = Vec::<Statement<'a>>::new();
        while self.tokens.current().is_some() && !self.check_one(TokenType::RightCurly) {
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
        while self.tokens.current().is_some() && !self.check_one(TokenType::RightCurly) {
            ret.push(Box::new(self.statement()));
        }
        ret
    }

    fn extended_expression(
        &mut self,
        expression: Expression<'a>,
        trace: &mut ExpressionTrace<'a>,
    ) -> Expression<'a> {
        trace.push(ExpressionItem::Expression(expression));
        match self.tokens.peek_next() {
            None => trace.reduce(255).last().unwrap(),
            Some(t) => match t.value {
                TokenType::Semicolon | TokenType::Newline => {
                    self.skip_by(1);
                    trace.reduce(255).last().unwrap()
                }
                // FIXME: things like (a++ + 5) probably won't work
                TokenType::PlusPlus => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(lhs.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Plus,
                            Box::new(lhs.clone()),
                            Box::new(Expression::Literal(PrimitiveType::Integer(1))),
                        )),
                    )
                }
                TokenType::MinusMinus => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(lhs.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Minus,
                            Box::new(lhs.clone()),
                            Box::new(Expression::Literal(PrimitiveType::Integer(1))),
                        )),
                    )
                }
                TokenType::Equal => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(lhs),
                        Box::new(self.expression(ExpressionTrace::new())),
                    )
                }
                // FIXME:: x= should be creating their own stack, so
                //         a /= 5^42
                //         is computed correctly
                TokenType::MinusEqual => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(lhs.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Minus,
                            Box::new(lhs.clone()),
                            Box::new(self.expression(ExpressionTrace::new())),
                        )),
                    )
                }
                TokenType::PlusEqual => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(lhs.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Plus,
                            Box::new(lhs.clone()),
                            Box::new(self.expression(ExpressionTrace::new())),
                        )),
                    )
                }
                TokenType::StarEqual => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(lhs.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Multiply,
                            Box::new(lhs.clone()),
                            Box::new(self.expression(ExpressionTrace::new())),
                        )),
                    )
                }
                TokenType::SlashEqual => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(lhs.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Divide,
                            Box::new(lhs.clone()),
                            Box::new(self.expression(ExpressionTrace::new())),
                        )),
                    )
                }
                TokenType::PercentEqual => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(lhs.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Divide,
                            Box::new(lhs.clone()),
                            Box::new(self.expression(ExpressionTrace::new())),
                        )),
                    )
                }
                TokenType::CarrotEqual => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Power,
                        Box::new(lhs.clone()),
                        Box::new(Expression::Binary(
                            BinaryOperator::Divide,
                            Box::new(lhs.clone()),
                            Box::new(self.expression(ExpressionTrace::new())),
                        )),
                    )
                }
                TokenType::Question => {
                    self.skip_by(1);
                    let cond = trace.reduce(255).last().unwrap();
                    let truthy = self.expression(ExpressionTrace::new());
                    self.advance_one(TokenType::Colon);
                    let falsy = self.expression(ExpressionTrace::new());
                    Expression::Ternary(Box::new(cond), Box::new(truthy), Box::new(falsy))
                }
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Star
                | TokenType::Slash
                | TokenType::Carrot
                | TokenType::Percent
                | TokenType::LeftParen
                | TokenType::Identifier(_)
                | TokenType::Literal(_)
                | TokenType::LessThan
                | TokenType::LessEqual
                | TokenType::GreaterThan
                | TokenType::GreaterEqual
                | TokenType::EqualEqual
                | TokenType::NotEqual
                | TokenType::And
                | TokenType::Or
                | TokenType::Tilde
                | TokenType::NotTilde => {
                    let op = BinaryOperator::convert(&t.value);
                    trace.push(ExpressionItem::IncompleteBinary(op));
                    if op != BinaryOperator::Concat {
                        self.skip_by(1);
                    }
                    self.expression(trace.clone())
                }
                _ => trace.reduce(255).last().unwrap(),
            },
        }
    }

    fn expression(&mut self, mut trace: ExpressionTrace<'a>) -> Expression<'a> {
        let expression: Expression<'a>;
        match self.tokens.peek_next() {
            None => Expression::Empty,
            Some(e) => match &e.value {
                TokenType::Literal(value) => {
                    let v = value.clone();
                    self.skip_by(1);
                    self.extended_expression(Expression::Literal(v), &mut trace)
                }
                TokenType::LeftParen => {
                    self.skip_by(1);
                    expression =
                        Expression::Grouping(Box::new(self.expression(ExpressionTrace::new())));
                    self.extended_expression(expression, &mut trace)
                }
                TokenType::Dollar => {
                    self.skip_by(1);
                    expression = Expression::FieldVariable(Box::new(
                        self.expression(ExpressionTrace::new()),
                    ));
                    self.extended_expression(expression, &mut trace)
                }
                TokenType::Identifier(name)
                    if self.check(vec![TokenType::Identifier(""), TokenType::LeftParen]) =>
                {
                    let n = *name;
                    self.skip_by(2);
                    let mut args = Vec::<Expression>::new();
                    let mut is_first = true;
                    while self.tokens.current().is_some() && !self.check_one(TokenType::RightParen)
                    {
                        if !is_first {
                            self.advance_one(TokenType::Comma);
                        }
                        args.push(self.expression(ExpressionTrace::new()));
                        is_first = false;
                    }
                    let mut ret = Expression::Function(n, Box::new(args));
                    ret = self.extended_expression(ret, &mut trace);
                    self.advance_one(TokenType::RightParen);
                    ret
                }
                TokenType::Identifier(_)
                    if self.check(vec![TokenType::Identifier(""), TokenType::LeftBracket]) =>
                {
                    self.skip_by(2);
                    let mut ret = Expression::ArrayVariable(Box::new(
                        self.expression(ExpressionTrace::new()),
                    ));
                    ret = self.extended_expression(ret, &mut trace);
                    self.advance_one(TokenType::RightBracket);
                    ret
                }
                TokenType::Identifier(name) => {
                    let n = *name;
                    self.skip_by(1);
                    self.extended_expression(Expression::Variable(n), &mut trace)
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
                        Box::new(self.expression(ExpressionTrace::new())),
                    );
                    self.extended_expression(expression, &mut trace)
                }
                TokenType::Semicolon | TokenType::Eof => Expression::Empty,
                t => panic!(
                    "expected: expression, received {:?} @ {}:{}",
                    t, e.row, e.col
                ),
            },
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut functions = Vec::<(&'a str, Vec<Statement<'a>>)>::new();
        let mut begin = Vec::<Statement<'a>>::new();
        let mut end = Vec::<Statement<'a>>::new();
        let mut actions = Vec::<(Expression<'a>, Vec<Statement<'a>>)>::new();
        while self.tokens.peek_next().is_some() {
            match self.tokens.peek_next().unwrap().value {
                TokenType::Eof => break,
                TokenType::Begin => self.begin(&mut begin),
                TokenType::End => self.end(&mut end),
                TokenType::Function => self.function(&mut functions),
                _ => self.action(&mut actions),
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
    #[should_panic(expected = "expected: expression, received Equal @ 1:1")]
    fn unfinished_expr() {
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::Equal, 1, 1),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let _prog = p.parse();
    }

    #[test]
    fn print() {
        let tokens = vec![
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::Comma, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("world")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.actions,
            vec![(
                Expression::Empty,
                vec![Statement::Print(vec![
                    Expression::Literal(PrimitiveType::String("hello")),
                    Expression::Literal(PrimitiveType::String("world"))
                ]),]
            )],
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

    #[test]
    fn func1() {
        // BEGIN { emitop("jump", "_"tag2) }
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Identifier("emitop"), 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("jump")), 0, 0),
            Token::new(TokenType::Comma, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("_")), 0, 0),
            Token::new(TokenType::Identifier("tag2"), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![Statement::Expression(Expression::Function(
                "emitop",
                Box::new(vec![
                    Expression::Literal(PrimitiveType::String("jump")),
                    Expression::Binary(
                        BinaryOperator::Concat,
                        Box::new(Expression::Literal(PrimitiveType::String("_"))),
                        Box::new(Expression::Variable("tag2"))
                    )
                ])
            ))]
        )
    }

    #[test]
    fn func2() {
        // BEGIN { a - b * c ^ 4 / 6.9 } => ( a - ( ( b * (c^4) ) / 6.9 ) )
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::Minus, 0, 0),
            Token::new(TokenType::Identifier("b"), 0, 0),
            Token::new(TokenType::Star, 0, 0),
            Token::new(TokenType::Identifier("c"), 0, 0),
            Token::new(TokenType::Carrot, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(4)), 0, 0),
            Token::new(TokenType::Slash, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Float(6.9)), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![Statement::Expression(Expression::Binary(
                BinaryOperator::Minus,
                Box::new(Expression::Variable("a")),
                Box::new(Expression::Binary(
                    BinaryOperator::Divide,
                    Box::new(Expression::Binary(
                        BinaryOperator::Multiply,
                        Box::new(Expression::Variable("b")),
                        Box::new(Expression::Binary(
                            BinaryOperator::Power,
                            Box::new(Expression::Variable("c")),
                            Box::new(Expression::Literal(PrimitiveType::Integer(4))),
                        ))
                    )),
                    Box::new(Expression::Literal(PrimitiveType::Float(6.9)))
                ))
            ))]
        )
    }
}
