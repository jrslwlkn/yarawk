use crate::{
    parser_helpers::{
        BinaryOperator, Expression, ExpressionItem, ExpressionTrace, Iterator, Statement,
        UnaryOperator,
    },
    token::{PrimitiveType, Token, TokenType},
};

pub struct Program<'a> {
    functions: Vec<(&'a str, Vec<Statement<'a>>)>,
    begin: Vec<Statement<'a>>,
    end: Vec<Statement<'a>>,
    actions: Vec<(Vec<Expression<'a>>, Vec<Statement<'a>>)>,
}

pub struct Parser<'a> {
    tokens: Iterator<'a, Token<'a>>,
}

// FIXME: strings are regexes within ~ and !~ binary expressions
//        strings are only requiring escaping the backslash
//        however, only regexes are allowed as filters for actions
impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            tokens: Iterator::new(tokens),
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut functions = Vec::<(&'a str, Vec<Statement<'a>>)>::new();
        let mut begin = Vec::<Statement<'a>>::new();
        let mut end = Vec::<Statement<'a>>::new();
        let mut actions = Vec::<(Vec<Expression<'a>>, Vec<Statement<'a>>)>::new();
        while self.tokens.peek_next().is_some() {
            self.skip_newlines_and_semicolons();
            match self.tokens.peek_next().unwrap().value {
                TokenType::Eof => break,
                TokenType::Begin => self.begin(&mut begin),
                TokenType::End => self.end(&mut end),
                TokenType::Function => self.function(&mut functions),
                _ => self.action(&mut actions),
            }
        }
        Program {
            functions,
            begin,
            end,
            actions,
        }
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

    fn action(&mut self, dest: &mut Vec<(Vec<Expression<'a>>, Vec<Statement<'a>>)>) {
        let mut expressions = vec![];
        while !self.check_one(TokenType::LeftCurly) {
            expressions.push(self.expression(ExpressionTrace::new()));
            if self.check_one(TokenType::Comma) {
                self.advance_one(TokenType::Comma);
            }
        }
        let mut statements = vec![];
        if self.check_one(TokenType::LeftCurly) {
            self.advance_one(TokenType::LeftCurly);
            statements = self.statements();
            self.advance_one(TokenType::RightCurly);
        } else {
            self.skip_newlines_and_semicolons();
        }
        dest.push((expressions, statements));
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

    fn statement(&mut self) -> Statement<'a> {
        self.skip_newlines();
        let ret = match self.tokens.peek_next() {
            None => Statement::Empty,
            Some(token) => match token.value {
                TokenType::Semicolon => Statement::Empty,
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
                    while self.tokens.peek_next().is_some()
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
                    self.skip_newlines_and_semicolons();
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
                        self.skip_by(1);
                        ifs.extend(self.boxed_statements());
                        self.advance_one(TokenType::RightCurly);
                    } else {
                        ifs.push(Box::new(self.statement()));
                    }
                    if self.check_one(TokenType::Else) {
                        self.skip_by(1);
                        if self.check_one(TokenType::LeftCurly) {
                            self.skip_by(1);
                            elses.extend(self.boxed_statements());
                            self.advance_one(TokenType::RightCurly);
                        } else {
                            elses.push(Box::new(self.statement()));
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
                        statements.push(Box::new(self.statement()));
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
                        statements.push(Box::new(self.statement()));
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
                _ => Statement::Expression(self.expression(ExpressionTrace::new())),
            },
        };
        self.skip_newlines_and_semicolons();
        ret
    }

    fn expression(&mut self, mut trace: ExpressionTrace<'a>) -> Expression<'a> {
        let expression: Expression<'a>;
        match self.tokens.peek_next() {
            None => Expression::Empty,
            Some(e) => {
                match &e.value {
                    TokenType::Literal(value) => {
                        let val = value.clone();
                        self.skip_by(1);
                        self.extended_expression(Expression::Literal(val), &mut trace)
                    }
                    TokenType::LeftParen => {
                        self.skip_by(1);
                        expression = Expression::Grouping(self.boxed_comma_separated_expressions());
                        let ret = self.extended_expression(expression, &mut trace);
                        self.advance_one(TokenType::RightParen);
                        self.extended_expression(ret, &mut trace)
                    }
                    TokenType::Dollar => {
                        match self.tokens.peek_nth(2) {
                            None => panic!(),
                            Some(t) => match &t.value {
                                TokenType::LeftParen => {
                                    // to ensure precedence of $(..) over anything else
                                    self.advance(vec![TokenType::Dollar, TokenType::LeftParen]);
                                    let e = self.expression(ExpressionTrace::new());
                                    self.advance_one(TokenType::RightParen);
                                    self.extended_expression(
                                        Expression::FieldVariable(Box::new(e)),
                                        &mut trace,
                                    )
                                }
                                TokenType::Literal(PrimitiveType::Integer(value)) => {
                                    let v = value.clone();
                                    self.skip_by(2); // $ plus Literal
                                    self.extended_expression(
                                    Expression::FieldVariable(Box::new(Expression::Literal(PrimitiveType::Integer(v)))),
                                        &mut trace,
                                    )
                                }
                                _ => panic!("expected: int field or group expression, received: {:?} @ {}:{}", t, e.row, e.col),
                            },
                        }
                    }
                    TokenType::Getline => {
                        self.skip_by(1);
                        let ret =
                            Expression::Getline(Box::new(self.expression(ExpressionTrace::new())));
                        self.extended_expression(ret, &mut trace)
                    }
                    TokenType::Identifier(name)
                        if self.check(vec![TokenType::Identifier(""), TokenType::LeftParen]) =>
                    {
                        // parse function call
                        let n = *name;
                        self.skip_by(2);
                        let mut ret =
                            Expression::Function(n, Box::new(self.comma_separated_expressions()));
                        ret = self.extended_expression(ret, &mut trace);
                        self.advance_one(TokenType::RightParen);
                        ret
                    }
                    TokenType::Identifier(_)
                        if self.check(vec![TokenType::Identifier(""), TokenType::LeftBracket]) =>
                    {
                        // parse (associative/multidimentional/regular) array access
                        self.skip_by(2);
                        let mut ret =
                            Expression::ArrayVariable(self.boxed_comma_separated_expressions());
                        ret = self.extended_expression(ret, &mut trace);
                        self.advance_one(TokenType::RightBracket);
                        ret
                    }
                    TokenType::Identifier(name) => {
                        // parse variable
                        let n = *name;
                        self.skip_by(1);
                        self.extended_expression(Expression::Variable(n), &mut trace)
                    }
                    TokenType::Plus
                    | &TokenType::Minus
                    | &TokenType::Not
                    | &TokenType::PlusPlus
                    | &TokenType::MinusMinus => {
                        // parse unary expression
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
                    TokenType::Semicolon | TokenType::Newline | TokenType::Eof => Expression::Empty,
                    t => panic!(
                        "expected: expression, received {:?} @ {}:{}",
                        t, e.row, e.col
                    ),
                }
            }
        }
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
                    // semicolon or newline end statements,
                    // so expressions are terminated too
                    trace.reduce(255).last().unwrap()
                }
                TokenType::PlusPlus | TokenType::MinusMinus => {
                    let e = trace.reduce(255).last().unwrap();
                    let u = Expression::Unary(
                        match t.value {
                            TokenType::PlusPlus => UnaryOperator::PostPlusPlus,
                            TokenType::MinusMinus => UnaryOperator::PostMinusMinus,
                            _ => unreachable!(),
                        },
                        Box::new(e),
                    );
                    self.skip_by(1);
                    let mut x = ExpressionTrace::new();
                    let t = x.push(ExpressionItem::Expression(u.clone()));
                    self.extended_expression(u, &mut t.clone())
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
                            BinaryOperator::Modulo,
                            Box::new(lhs.clone()),
                            Box::new(self.expression(ExpressionTrace::new())),
                        )),
                    )
                }
                TokenType::CarrotEqual => {
                    self.skip_by(1);
                    let lhs = trace.reduce(255).last().unwrap();
                    Expression::Binary(
                        BinaryOperator::Exponent,
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
                | TokenType::In
                | TokenType::Pipe
                | TokenType::RightRight
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

    fn comma_separated_expressions(&mut self) -> Vec<Expression<'a>> {
        let mut ret = Vec::<Expression<'a>>::new();
        let mut is_first = true;
        while self.tokens.current().is_some()
            && !self.check_one(TokenType::RightParen)
            && !self.check_one(TokenType::RightBracket)
        {
            if !is_first {
                self.advance_one(TokenType::Comma);
            }
            ret.push(self.expression(ExpressionTrace::new()));
            is_first = false;
        }
        ret
    }

    fn boxed_comma_separated_expressions(&mut self) -> Vec<Box<Expression<'a>>> {
        let mut ret = Vec::<Box<Expression<'a>>>::new();
        let mut is_first = true;
        while self.tokens.current().is_some()
            && !self.check_one(TokenType::RightParen)
            && !self.check_one(TokenType::RightBracket)
        {
            if !is_first {
                self.advance_one(TokenType::Comma);
            }
            ret.push(Box::new(self.expression(ExpressionTrace::new())));
            is_first = false;
        }
        ret
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
                        .current()
                        .unwrap_or(&Token::new(TokenType::Eof, 0, 0))
                        .row,
                    self.tokens
                        .current()
                        .unwrap_or(&Token::new(TokenType::Eof, 0, 0))
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

    fn skip_by(&mut self, num: usize) {
        for _ in 0..num {
            self.tokens.next();
        }
    }

    fn skip_newlines(&mut self) {
        while self.tokens.peek_next().is_some() && self.check_one(TokenType::Newline) {
            self.tokens.next();
        }
    }

    fn skip_newlines_and_semicolons(&mut self) {
        while self.tokens.peek_next().is_some()
            && (self.check_one(TokenType::Semicolon) | self.check_one(TokenType::Newline))
        {
            self.tokens.next();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;
    use std::str::FromStr;

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
                vec![Expression::Binary(
                    BinaryOperator::Equal,
                    Box::new(Expression::Variable("a")),
                    Box::new(Expression::Literal(PrimitiveType::Integer(1)))
                )],
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
    fn multi_action() {
        // {}
        // { print }
        // (1) { print }
        // /hello/, "world" {
        //      print
        //      print
        // }
        let tokens = vec![
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
            //
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
            //
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
            //
            Token::new(TokenType::Newline, 0, 2),
            Token::new(
                TokenType::Literal(PrimitiveType::Pattern(Regex::from_str("hello").unwrap())),
                0,
                0,
            ),
            Token::new(TokenType::Comma, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("world")), 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Print, 1, 0),
            Token::new(TokenType::Newline, 0, 3),
            Token::new(TokenType::Print, 2, 0),
            Token::new(TokenType::Newline, 0, 4),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.actions,
            vec![
                (vec![], vec![]),
                (vec![], vec![Statement::Print(vec![])]),
                (
                    vec![Expression::Grouping(vec![Box::new(Expression::Literal(
                        PrimitiveType::Integer(1)
                    ))])],
                    vec![Statement::Print(vec![])]
                ),
                (
                    vec![
                        Expression::Literal(PrimitiveType::Pattern(
                            Regex::from_str("hello").unwrap()
                        )),
                        Expression::Literal(PrimitiveType::String("world"))
                    ],
                    vec![Statement::Print(vec![]), Statement::Print(vec![])]
                )
            ]
        )
    }

    #[test]
    fn print() {
        // {
        // print "hello", "world"
        //  }
        let tokens = vec![
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
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
                vec![],
                vec![Statement::Print(vec![
                    Expression::Literal(PrimitiveType::String("hello")),
                    Expression::Literal(PrimitiveType::String("world"))
                ]),]
            )],
        )
    }

    #[test]
    fn print2() {
        // {
        // print
        //  }
        let tokens = vec![
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.actions,
            vec![(vec![], vec![Statement::Print(vec![]),])],
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
    fn function2() {
        // function f1() {
        //
        // }
        // function f2() {
        //
        // }
        let tokens = vec![
            Token::new(TokenType::Function, 0, 0),
            Token::new(TokenType::Identifier("f1"), 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
            Token::new(TokenType::Function, 0, 0),
            Token::new(TokenType::Identifier("f2"), 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(prog.functions, vec![("f1", vec![]), ("f2", vec![]),])
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
                            BinaryOperator::Exponent,
                            Box::new(Expression::Variable("c")),
                            Box::new(Expression::Literal(PrimitiveType::Integer(4))),
                        ))
                    )),
                    Box::new(Expression::Literal(PrimitiveType::Float(6.9)))
                ))
            ))]
        )
    }

    #[test]
    fn unary_expr() {
        // END { print (a++ + 42) }
        let tokens = vec![
            Token::new(TokenType::End, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::PlusPlus, 0, 0),
            Token::new(TokenType::Plus, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(42)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.end,
            vec![Statement::Print(vec![Expression::Grouping(vec![
                Box::new(Expression::Binary(
                    BinaryOperator::Plus,
                    Box::new(Expression::Unary(
                        UnaryOperator::PostPlusPlus,
                        Box::new(Expression::Variable("a"))
                    )),
                    Box::new(Expression::Literal(PrimitiveType::Integer(42)))
                ))
            ])])]
        )
    }

    #[test]
    fn binary_expr() {
        // END { a = 4 + 5 * 2 - a ^ 3 * (a-1) - 1 } ---> [ [4 + 5*2] - (a^3 * (a-1)) ] - 1
        let tokens = vec![
            Token::new(TokenType::End, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::Equal, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(4)), 0, 0),
            Token::new(TokenType::Plus, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(5)), 0, 0),
            Token::new(TokenType::Star, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(2)), 0, 0),
            Token::new(TokenType::Minus, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::Carrot, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(3)), 0, 0),
            Token::new(TokenType::Star, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::Minus, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Minus, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.end,
            vec![Statement::Expression(Expression::Binary(
                BinaryOperator::Equal,
                Box::new(Expression::Variable("a")),
                Box::new(Expression::Binary(
                    BinaryOperator::Minus,
                    Box::new(Expression::Binary(
                        BinaryOperator::Minus,
                        Box::new(Expression::Binary(
                            BinaryOperator::Plus,
                            Box::new(Expression::Literal(PrimitiveType::Integer(4))),
                            Box::new(Expression::Binary(
                                BinaryOperator::Multiply,
                                Box::new(Expression::Literal(PrimitiveType::Integer(5))),
                                Box::new(Expression::Literal(PrimitiveType::Integer(2)))
                            ))
                        )),
                        Box::new(Expression::Binary(
                            BinaryOperator::Multiply,
                            Box::new(Expression::Binary(
                                BinaryOperator::Exponent,
                                Box::new(Expression::Variable("a")),
                                Box::new(Expression::Literal(PrimitiveType::Integer(3)))
                            )),
                            Box::new(Expression::Grouping(vec![Box::new(Expression::Binary(
                                BinaryOperator::Minus,
                                Box::new(Expression::Variable("a")),
                                Box::new(Expression::Literal(PrimitiveType::Integer(1)))
                            ))]))
                        ))
                    )),
                    Box::new(Expression::Literal(PrimitiveType::Integer(1)))
                ))
            ))]
        )
    }

    #[test]
    fn x_eq_expr() {
        // END { a += b - 2 ^ c }
        let tokens = vec![
            Token::new(TokenType::End, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::PlusEqual, 0, 0),
            Token::new(TokenType::Identifier("b"), 0, 0),
            Token::new(TokenType::Minus, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(2)), 0, 0),
            Token::new(TokenType::Carrot, 0, 0),
            Token::new(TokenType::Identifier("c"), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.end,
            vec![Statement::Expression(Expression::Binary(
                BinaryOperator::Equal,
                Box::new(Expression::Variable("a")),
                Box::new(Expression::Binary(
                    BinaryOperator::Plus,
                    Box::new(Expression::Variable("a")),
                    Box::new(Expression::Binary(
                        BinaryOperator::Minus,
                        Box::new(Expression::Variable("b")),
                        Box::new(Expression::Binary(
                            BinaryOperator::Exponent,
                            Box::new(Expression::Literal(PrimitiveType::Integer(2))),
                            Box::new(Expression::Variable("c"))
                        ))
                    ))
                ))
            ))]
        )
    }

    #[test]
    fn x_eq_expr2() {
        // END { a %= b - 2 }
        let tokens = vec![
            Token::new(TokenType::End, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::PercentEqual, 0, 0),
            Token::new(TokenType::Identifier("b"), 0, 0),
            Token::new(TokenType::Minus, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(2)), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.end,
            vec![Statement::Expression(Expression::Binary(
                BinaryOperator::Equal,
                Box::new(Expression::Variable("a")),
                Box::new(Expression::Binary(
                    BinaryOperator::Modulo,
                    Box::new(Expression::Variable("a")),
                    Box::new(Expression::Binary(
                        BinaryOperator::Minus,
                        Box::new(Expression::Variable("b")),
                        Box::new(Expression::Literal(PrimitiveType::Integer(2))),
                    ))
                ))
            ))]
        )
    }

    #[test]
    fn x_eq_expr3() {
        // END { a %= b -= 2 }
        let tokens = vec![
            Token::new(TokenType::End, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::PercentEqual, 0, 0),
            Token::new(TokenType::Identifier("b"), 0, 0),
            Token::new(TokenType::MinusEqual, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(2)), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.end,
            vec![Statement::Expression(Expression::Binary(
                BinaryOperator::Equal,
                Box::new(Expression::Variable("a")),
                Box::new(Expression::Binary(
                    BinaryOperator::Modulo,
                    Box::new(Expression::Variable("a")),
                    Box::new(Expression::Binary(
                        BinaryOperator::Equal,
                        Box::new(Expression::Variable("b")),
                        Box::new(Expression::Binary(
                            BinaryOperator::Minus,
                            Box::new(Expression::Variable("b")),
                            Box::new(Expression::Literal(PrimitiveType::Integer(2))),
                        ))
                    ))
                ))
            ))]
        )
    }

    #[test]
    fn dollar() {
        // END { $1 = "hello" }
        let tokens = vec![
            Token::new(TokenType::End, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Dollar, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::Equal, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.end,
            vec![Statement::Expression(Expression::Binary(
                BinaryOperator::Equal,
                Box::new(Expression::FieldVariable(Box::new(Expression::Literal(
                    PrimitiveType::Integer(1)
                )))),
                Box::new(Expression::Literal(PrimitiveType::String("hello"))),
            ))]
        )
    }

    #[test]
    fn dollar_group() {
        // END { $(4+$(NF)) = "hello" }
        let tokens = vec![
            Token::new(TokenType::End, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Dollar, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(4)), 0, 0),
            Token::new(TokenType::Plus, 0, 0),
            Token::new(TokenType::Dollar, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Identifier("NF"), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Equal, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.end,
            vec![Statement::Expression(Expression::Binary(
                BinaryOperator::Equal,
                Box::new(Expression::FieldVariable(Box::new(Expression::Binary(
                    BinaryOperator::Plus,
                    Box::new(Expression::Literal(PrimitiveType::Integer(4))),
                    Box::new(Expression::FieldVariable(Box::new(Expression::Variable(
                        "NF"
                    ))))
                )))),
                Box::new(Expression::Literal(PrimitiveType::String("hello")))
            ))]
        )
    }

    #[test]
    #[should_panic(
        expected = "expected: int field or group expression, received: Token { value: Literal(Float(9.6)), row: 0, col: 0 } @ 0:0"
    )]
    fn dollar_float() {
        // END { $9.6 }
        let tokens = vec![
            Token::new(TokenType::End, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Dollar, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Float(9.6)), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let _prog = p.parse();
    }

    #[test]
    fn if1() {
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            // if (1) print
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            // if (1)
            //      print
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            // if (1) { print }
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
            //
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![
                Statement::If(
                    Expression::Literal(PrimitiveType::Integer(1)),
                    vec![Box::new(Statement::Print(vec![]))],
                    vec![]
                ),
                Statement::If(
                    Expression::Literal(PrimitiveType::Integer(1)),
                    vec![Box::new(Statement::Print(vec![]))],
                    vec![]
                ),
                Statement::If(
                    Expression::Literal(PrimitiveType::Integer(1)),
                    vec![Box::new(Statement::Print(vec![]))],
                    vec![]
                ),
            ]
        )
    }

    #[test]
    fn if_else1() {
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            // if (1) print 1
            // else print 2
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::Else, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(2)), 0, 0),
            //
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![Statement::If(
                Expression::Literal(PrimitiveType::Integer(1)),
                vec![Box::new(Statement::Print(vec![Expression::Literal(
                    PrimitiveType::Integer(1)
                ),]))],
                vec![Box::new(Statement::Print(vec![Expression::Literal(
                    PrimitiveType::Integer(2)
                ),]))],
            ),]
        )
    }

    #[test]
    fn if2() {
        // BEGIN {
        // if ("1") print "1"
        // else if ("2") print "2"
        // else if ("3") print "3"
        // }
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("1")), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("1")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::Else, 0, 0),
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("2")), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("2")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::Else, 0, 0),
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("3")), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("3")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![Statement::If(
                Expression::Literal(PrimitiveType::String("1")),
                vec![Box::new(Statement::Print(vec![Expression::Literal(
                    PrimitiveType::String("1")
                ),]))],
                vec![Box::new(Statement::If(
                    Expression::Literal(PrimitiveType::String("2")),
                    vec![Box::new(Statement::Print(vec![Expression::Literal(
                        PrimitiveType::String("2")
                    )]))],
                    vec![Box::new(Statement::If(
                        Expression::Literal(PrimitiveType::String("3")),
                        vec![Box::new(Statement::Print(vec![Expression::Literal(
                            PrimitiveType::String("3")
                        )]))],
                        vec![]
                    ))]
                ))],
            ),]
        )
    }

    #[test]
    fn if3() {
        // BEGIN {
        // if ("1") print "1"
        // else if ("2") print "2"
        // else if ("3") print "3"
        // else print "4"
        // }
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("1")), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("1")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::Else, 0, 0),
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("2")), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("2")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::Else, 0, 0),
            Token::new(TokenType::If, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("3")), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("3")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::Else, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("4")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![Statement::If(
                Expression::Literal(PrimitiveType::String("1")),
                vec![Box::new(Statement::Print(vec![Expression::Literal(
                    PrimitiveType::String("1")
                ),]))],
                vec![Box::new(Statement::If(
                    Expression::Literal(PrimitiveType::String("2")),
                    vec![Box::new(Statement::Print(vec![Expression::Literal(
                        PrimitiveType::String("2")
                    )]))],
                    vec![Box::new(Statement::If(
                        Expression::Literal(PrimitiveType::String("3")),
                        vec![Box::new(Statement::Print(vec![Expression::Literal(
                            PrimitiveType::String("3")
                        )]))],
                        vec![Box::new(Statement::Print(vec![Expression::Literal(
                            PrimitiveType::String("4")
                        )]))],
                    ))]
                ))],
            ),]
        )
    }

    #[test]
    fn ternary() {
        // BEGIN { print x > y ? a <= b ? c + 1 : d : e }
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Identifier("x"), 0, 0),
            Token::new(TokenType::GreaterThan, 0, 0),
            Token::new(TokenType::Identifier("y"), 0, 0),
            Token::new(TokenType::Question, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::LessEqual, 0, 0),
            Token::new(TokenType::Identifier("b"), 0, 0),
            Token::new(TokenType::Question, 0, 0),
            Token::new(TokenType::Identifier("c"), 0, 0),
            Token::new(TokenType::Plus, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::Colon, 0, 0),
            Token::new(TokenType::Identifier("d"), 0, 0),
            Token::new(TokenType::Colon, 0, 0),
            Token::new(TokenType::Identifier("e"), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![Statement::Print(vec![Expression::Ternary(
                Box::new(Expression::Binary(
                    BinaryOperator::GreaterThan,
                    Box::new(Expression::Variable("x")),
                    Box::new(Expression::Variable("y"))
                )),
                Box::new(Expression::Ternary(
                    Box::new(Expression::Binary(
                        BinaryOperator::LessEqual,
                        Box::new(Expression::Variable("a")),
                        Box::new(Expression::Variable("b"))
                    )),
                    Box::new(Expression::Binary(
                        BinaryOperator::Plus,
                        Box::new(Expression::Variable("c")),
                        Box::new(Expression::Literal(PrimitiveType::Integer(1)))
                    )),
                    Box::new(Expression::Variable("d"))
                )),
                Box::new(Expression::Variable("e"))
            )])]
        )
    }

    #[test]
    fn do_while() {
        // BEGIN {
        //
        // do print "hello"
        // while (1)
        //
        // do print "hello"; while (1)
        //
        // do {
        //  print "hello"
        // } while (1)
        //
        // }
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            //
            Token::new(TokenType::Do, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::While, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            Token::new(TokenType::Do, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::Semicolon, 0, 0),
            Token::new(TokenType::While, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            Token::new(TokenType::Do, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
            Token::new(TokenType::While, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![
                Statement::DoWhile(
                    vec![Box::new(Statement::Print(vec![Expression::Literal(
                        PrimitiveType::String("hello")
                    )]))],
                    Expression::Literal(PrimitiveType::Integer(1))
                ),
                Statement::DoWhile(
                    vec![Box::new(Statement::Print(vec![Expression::Literal(
                        PrimitiveType::String("hello")
                    )]))],
                    Expression::Literal(PrimitiveType::Integer(1))
                ),
                Statement::DoWhile(
                    vec![Box::new(Statement::Print(vec![Expression::Literal(
                        PrimitiveType::String("hello")
                    )]))],
                    Expression::Literal(PrimitiveType::Integer(1))
                ),
            ]
        )
    }

    #[test]
    fn while1() {
        // BEGIN {
        //
        // while (1) print "hello"
        //
        // while (1);
        //
        // while (1) {
        //      print "hello"
        // }
        //
        // }
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            //
            Token::new(TokenType::While, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            Token::new(TokenType::While, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::Semicolon, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            Token::new(TokenType::While, 0, 0),
            Token::new(TokenType::LeftParen, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 0, 0),
            Token::new(TokenType::RightParen, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            Token::new(TokenType::RightCurly, 0, 0),
            //
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![
                Statement::While(
                    Expression::Literal(PrimitiveType::Integer(1)),
                    vec![Box::new(Statement::Print(vec![Expression::Literal(
                        PrimitiveType::String("hello")
                    )]))],
                ),
                Statement::While(
                    Expression::Literal(PrimitiveType::Integer(1)),
                    vec![Box::new(Statement::Empty)],
                ),
                Statement::While(
                    Expression::Literal(PrimitiveType::Integer(1)),
                    vec![Box::new(Statement::Print(vec![Expression::Literal(
                        PrimitiveType::String("hello")
                    )]))],
                ),
            ]
        )
    }

    #[test]
    fn pipe_and_append() {
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            //
            // $0 | getline
            Token::new(TokenType::Dollar, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::Integer(0)), 0, 0),
            Token::new(TokenType::Pipe, 0, 0),
            Token::new(TokenType::Getline, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            // "hello" "world" | "filename"
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("world")), 0, 0),
            Token::new(TokenType::Pipe, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("filename")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            //
            // print "hello" "world" >> "filename"
            Token::new(TokenType::Print, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hello")), 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("world")), 0, 0),
            Token::new(TokenType::RightRight, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("filename")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![
                Statement::Expression(Expression::Binary(
                    BinaryOperator::Pipe,
                    Box::new(Expression::FieldVariable(Box::new(Expression::Literal(
                        PrimitiveType::Integer(0)
                    )))),
                    Box::new(Expression::Getline(Box::new(Expression::Empty)))
                )),
                Statement::Expression(Expression::Binary(
                    BinaryOperator::Pipe,
                    Box::new(Expression::Binary(
                        BinaryOperator::Concat,
                        Box::new(Expression::Literal(PrimitiveType::String("hello"))),
                        Box::new(Expression::Literal(PrimitiveType::String("world"))),
                    )),
                    Box::new(Expression::Literal(PrimitiveType::String("filename")))
                )),
                Statement::Print(vec![Expression::Binary(
                    BinaryOperator::Append,
                    Box::new(Expression::Binary(
                        BinaryOperator::Concat,
                        Box::new(Expression::Literal(PrimitiveType::String("hello"))),
                        Box::new(Expression::Literal(PrimitiveType::String("world"))),
                    )),
                    Box::new(Expression::Literal(PrimitiveType::String("filename")))
                )]),
            ]
        )
    }

    #[test]
    fn getline() {
        let tokens = vec![
            Token::new(TokenType::Begin, 0, 0),
            Token::new(TokenType::LeftCurly, 0, 0),
            //
            Token::new(TokenType::Getline, 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            Token::new(TokenType::Getline, 0, 0),
            Token::new(TokenType::Identifier("NF"), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            Token::new(TokenType::Getline, 0, 0),
            Token::new(TokenType::Identifier("a"), 0, 0),
            Token::new(TokenType::LessThan, 0, 0),
            Token::new(TokenType::Literal(PrimitiveType::String("hey")), 0, 0),
            Token::new(TokenType::Newline, 0, 0),
            //
            Token::new(TokenType::RightCurly, 0, 0),
        ];
        let mut p = Parser::new(&tokens);
        let prog = p.parse();
        assert_eq!(
            prog.begin,
            vec![
                Statement::Expression(Expression::Getline(Box::new(Expression::Empty))),
                Statement::Expression(Expression::Getline(Box::new(Expression::Variable("NF")))),
                Statement::Expression(Expression::Getline(Box::new(Expression::Binary(
                    BinaryOperator::LessThan,
                    Box::new(Expression::Variable("a")),
                    Box::new(Expression::Literal(PrimitiveType::String("hey")))
                )))),
            ]
        )
    }
}
