use crate::token::{PrimitiveType, TokenType};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Statement<'a> {
    Empty,
    Break,
    Continue,
    Next,
    Expression(Expression<'a>),
    IoStatement,
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
        Option<Expression<'a>>,
        Vec<Box<Statement<'a>>>,
    ),
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum UnaryOperator {
    PrePlus,
    PrePlusPlus,
    PreMinus,
    PreMinusMinus,
    PostPlusPlus,
    PostMinusMinus,
    Not,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum BinaryOperator {
    Concat,
    EqualEqual,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Match,
    NotMatch,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Exponent,
    Equal,
    Or,
    And,
    In,
}

impl BinaryOperator {
    pub fn convert(t: &TokenType) -> Self {
        match t {
            TokenType::Carrot => Self::Exponent,
            TokenType::Star => Self::Multiply,
            TokenType::Slash => Self::Divide,
            TokenType::Percent => Self::Modulo,
            TokenType::Plus => Self::Plus,
            TokenType::Minus => Self::Minus,
            TokenType::In => Self::In,
            TokenType::LeftParen | TokenType::Literal(_) | TokenType::Identifier(_) => Self::Concat,
            TokenType::EqualEqual => Self::EqualEqual,
            TokenType::NotEqual => Self::NotEqual,
            TokenType::LessThan => Self::LessThan,
            TokenType::LessEqual => Self::LessEqual,
            TokenType::GreaterThan => Self::GreaterThan,
            TokenType::GreaterEqual => Self::GreaterEqual,
            TokenType::Tilde => Self::Match,
            TokenType::NotTilde => Self::NotMatch,
            TokenType::And => Self::And,
            TokenType::Or => Self::Or,
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

    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Exponent => 4,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => 5,
            BinaryOperator::Plus | BinaryOperator::Minus => 7,
            BinaryOperator::Concat => 8,
            BinaryOperator::EqualEqual
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterEqual => 9,
            BinaryOperator::Match | BinaryOperator::NotMatch => 10,
            BinaryOperator::In => 11,
            BinaryOperator::And => 12,
            BinaryOperator::Or => 13,
            BinaryOperator::Equal => 14,
        }
    }
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
    Ternary(
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ),
}

#[derive(Clone, Debug)]
pub enum ExpressionItem<'a> {
    Expression(Expression<'a>),
    IncompleteBinary(BinaryOperator),
}

#[derive(Clone, Debug)]
pub struct ExpressionTrace<'a> {
    stack: Vec<ExpressionItem<'a>>,
}

impl<'a> ExpressionTrace<'a> {
    pub fn new() -> Self {
        Self { stack: vec![] }
    }

    fn highest_precedence(&self) -> u8 {
        for i in 0..self.stack.len() {
            let cur = self.stack.get(self.stack.len() - 1 - i);
            match cur {
                None => return 255,
                Some(ExpressionItem::IncompleteBinary(op)) => return op.precedence(),
                Some(_) => {}
            }
        }
        255
    }

    pub fn last(&self) -> Option<Expression<'a>> {
        match self.stack.last() {
            None => None,
            Some(ExpressionItem::IncompleteBinary(t)) => {
                panic!("{:?} is on top of expression stack", t)
            }
            Some(ExpressionItem::Expression(e)) => Some(e.clone()),
        }
    }

    pub fn reduce(&mut self, lowest_precedence: u8) -> &Self {
        while self.stack.len() >= 3 {
            let last_op = self.stack.get(self.stack.len() - 2).unwrap();
            match last_op {
                ExpressionItem::IncompleteBinary(op) => {
                    if op.precedence() > lowest_precedence {
                        break;
                    }
                }
                _ => panic!(
                    "why isn't {:?} an operation?",
                    self.stack.get(self.stack.len() - 2)
                ),
            }
            let rhs = match self.stack.pop() {
                Some(ExpressionItem::Expression(e)) => e,
                Some(ExpressionItem::IncompleteBinary(t)) => {
                    panic!("received {:?} instead of expression", t)
                }
                None => unreachable!(),
            };
            let op = match self.stack.pop() {
                Some(ExpressionItem::Expression(e)) => {
                    panic!("received {:?} instead of binary operator", e)
                }
                Some(ExpressionItem::IncompleteBinary(op)) => op,
                None => unreachable!(),
            };
            let lhs = match self.stack.pop() {
                Some(ExpressionItem::Expression(e)) => e,
                Some(ExpressionItem::IncompleteBinary(t)) => {
                    panic!("received {:?} instead of expression", t)
                }
                None => unreachable!(),
            };
            let expression = Expression::Binary(op, Box::new(lhs), Box::new(rhs));
            self.stack.push(ExpressionItem::Expression(expression));
        }
        self
    }

    pub fn push(&mut self, item: ExpressionItem<'a>) -> &Self {
        match &item {
            ExpressionItem::IncompleteBinary(op) => {
                if op.precedence() >= self.highest_precedence() {
                    self.reduce(op.precedence());
                }
            }
            _ => {}
        }
        self.stack.push(item);
        self
    }
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
