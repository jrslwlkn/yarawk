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
    program: &'a Program<'a>,
    variables: HashMap<String, Value<'a>>,
    functions: HashMap<String, Box<fn(Vec<Value<'a>>) -> Value<'a>>>,
}

#[derive(Debug)]
pub enum Value<'a> {
    Empty,
    PrimitiveType(PrimitiveType<'a>),
    ArrayType(HashMap<String, PrimitiveType<'a>>),
}

// FIXME: strings are regexes within ~ and !~ binary expressions
//        strings are only requiring escaping the backslash
//        however, only regexes are allowed as filters for actions
impl<'a> Environment<'a> {
    pub fn new(program: &'a Program<'a>) -> Self {
        let variables = HashMap::new(); // FIXME: set global variables
        let mut functions = HashMap::new(); // FIXME: set built-in functions before user-defined ones
        for (name, statements) in &program.functions {
            if functions.contains_key(*name) {
                panic!("function {} already exists", name)
            }
            functions.insert(name.to_string(), Environment::make_function(&statements));
        }
        Self {
            program,
            variables,
            functions,
        }
    }

    pub fn set_variable(&mut self, name: String, val: Value<'a>) {
        self.variables.insert(name, val);
    }

    pub fn execute_begin(&mut self) {
        for s in &self.program.begin {
            self.visit(&s);
        }
    }

    pub fn execute_end(&mut self) {
        for s in &self.program.end {
            self.visit(&s);
        }
    }

    pub fn execute_actions(&mut self, line: &String) {
        for (expressions, statements) in &self.program.actions {
            todo!()
        }
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
            Value::Empty => false,
        }
    }

    fn make_function(statements: &Vec<Statement<'a>>) -> Box<fn(Vec<Value<'a>>) -> Value<'a>> {
        todo!()
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
