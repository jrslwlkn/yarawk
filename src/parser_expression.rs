use crate::token::{PrimitiveType, TokenType};

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
    In,
}

impl BinaryOperator {
    pub fn convert(t: &TokenType) -> Self {
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
            BinaryOperator::Power => 4,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => 5,
            BinaryOperator::Plus | BinaryOperator::Minus => 7,
            // TokenType::Identifier(v) if v == &"in" => 11,
            BinaryOperator::Concat => 8,
            BinaryOperator::EqualEqual
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterEqual => 9,
            BinaryOperator::Tilde | BinaryOperator::NotTilde => 10,
            BinaryOperator::And => 12,
            BinaryOperator::Or => 13,
            BinaryOperator::Equal => 14,
            _ => panic!("how did {:?} get here?", self),
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

    pub fn push(&mut self, item: ExpressionItem<'a>) {
        match &item {
            ExpressionItem::IncompleteBinary(op) => {
                if op.precedence() >= self.highest_precedence() {
                    self.reduce(op.precedence());
                }
            }
            _ => {}
        }
        self.stack.push(item);
    }
}

impl<'a> Expression<'a> {
    fn precedence(&self) -> u8 {
        match self {
            Expression::Empty => 255,
            Expression::Literal(_) => 255,
            Expression::Variable(_) => 0,
            Expression::Function(_, _) => 0,
            Expression::Grouping(_) => 0,
            Expression::Ternary(_, _, _) => 0,
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
                BinaryOperator::Equal => 15,
            },
        }
    }

    // FIXME: precedence should be calculated by operator, so
    //        if current expr is literal or variable,
    //        we should go back until op is encountered (or other string or var -> concat op)
    //        and compare against the precedence of that op instead
    pub fn is_precedent_to(&self, t: &TokenType) -> Option<bool> {
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
