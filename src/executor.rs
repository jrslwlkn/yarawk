use std::{collections::HashMap, str::FromStr};

use regex::Regex;

use crate::{
    parser::Program,
    parser_helpers::{
        BinaryOperator, Expression, ExpressionItem, ExpressionTrace, Iterator, Statement,
        UnaryOperator,
    },
    token::{PrimitiveType, Token, TokenType},
};

pub struct Environment<'a> {
    program: Program<'a>,
    variables: HashMap<String, Value<'a>>,
    functions: HashMap<String, Box<dyn Fn(Vec<Value<'a>>) -> Value<'a>>>,
}

#[derive(Debug)]
pub enum Value<'a> {
    PrimitiveType(PrimitiveType<'a>),
    ArrayType(HashMap<String, PrimitiveType<'a>>),
}

// FIXME: strings are regexes within ~ and !~ binary expressions
//        strings are only requiring escaping the backslash
//        however, only regexes are allowed as filters for actions
impl<'a> Environment<'a> {
    pub fn new(program: Program<'a>) -> Self {
        let mut variables = HashMap::new(); // FIXME: set global variables
        let mut functions = HashMap::new(); // FIXME: set built-in functions
        Self {
            program,
            variables,
            functions,
        }
    }

    pub fn set_variable(&mut self, name: String, val: Value<'a>) {
        self.variables.insert(name, val);
    }

    pub fn execute(&mut self) {
        todo!()
    }

    fn visit(&mut self, statement: &Statement<'a>) {
        todo!()
    }

    fn evaluate(&mut self, expression: &Expression<'a>) -> Value {
        todo!()
    }

    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::PrimitiveType(PrimitiveType::Integer(v)) => *v != 0,
            Value::PrimitiveType(PrimitiveType::Float(v)) => *v != 0.0,
            Value::PrimitiveType(PrimitiveType::String(v)) => *v != "",
            Value::PrimitiveType(PrimitiveType::Pattern(_)) => true, // FIXME: ?
            Value::ArrayType(_) => true,
        }
    }
}

impl<'a> Value<'a> {
    pub fn from_str(val: &'a str) -> Self {
        Self::PrimitiveType(PrimitiveType::String(val))
    }

    pub fn from_int(val: i64) -> Self {
        Self::PrimitiveType(PrimitiveType::Integer(val))
    }

    pub fn from_float(val: f64) -> Self {
        Self::PrimitiveType(PrimitiveType::Float(val))
    }

    pub fn from_array(val: HashMap<String, PrimitiveType<'a>>) -> Self {
        Self::ArrayType(val)
    }
}
