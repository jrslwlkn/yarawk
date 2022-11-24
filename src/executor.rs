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
    functions: HashMap<String, (u8, Vec<Statement<'a>>)>,
}

#[derive(Clone, Debug)]
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
        let mut ret = Self {
            program,
            variables,
            functions: HashMap::new(), // FIXME: set built-in functions before user-defined ones
        };
        for (name, arity, statements) in &program.functions {
            if ret.functions.contains_key(*name) {
                panic!("function {} already exists", name)
            }
            ret.functions
                .insert(name.to_string(), (*arity, statements.clone()));
        }
        ret
    }

    pub fn set_variable(&mut self, name: String, val: Value<'a>) {
        self.variables.insert(name, val);
    }

    pub fn execute_begin(&mut self) {
        for s in &self.program.begin {
            self.execute_one(&s);
        }
    }

    pub fn execute_end(&mut self) {
        for s in &self.program.end {
            self.execute_one(&s);
        }
    }

    pub fn execute_actions(&mut self, line: &String) {
        for (expressions, statements) in &self.program.actions {
            todo!()
        }
    }

    fn execute(&mut self, statements: &Vec<Box<Statement<'a>>>) -> Value<'a> {
        let mut ret = Value::Empty;
        for s in statements {
            let val = self.execute_one(s);
            match val {
                Value::Empty => {}
                val => {
                    // here we assume that if a statement returns a value, it must be a return or exit.
                    ret = val;
                    break;
                }
            }
        }
        ret
    }

    fn execute_one(&mut self, statement: &Statement<'a>) -> Value<'a> {
        match statement {
            Statement::Empty => Value::Empty,
            Statement::Exit(e) => {
                std::process::exit(self.evaluate(&e).as_int().try_into().unwrap_or(-69420));
            }
            Statement::Return(e) => self.evaluate(e),
            Statement::If(cond, truthy, falsy) => {
                if (Environment::is_truthy(&self.evaluate(cond))) {
                    self.execute(truthy)
                } else {
                    self.execute(falsy)
                }
            }
            Statement::DoWhile(statements, cond) => {
                todo!()
            }
            Statement::While(cond, statemens) => {
                todo!()
            }
            Statement::For(cond, Some(e1), Some(e2), statements) => {
                todo!()
            }
            Statement::For(cond, None, None, statements) => {
                todo!()
            }
            Statement::Expression(e) => {
                self.evaluate(e);
                Value::Empty
            }
            Statement::Print(_) => todo!(),
            _ => panic!("unexpected statement: {:?}", statement),
        }
    }

    fn evaluate(&mut self, expression: &Expression<'a>) -> Value<'a> {
        todo!()
    }

    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::PrimitiveType(PrimitiveType::Integer(v)) => *v != 0,
            Value::PrimitiveType(PrimitiveType::Float(v)) => *v != 0.0,
            Value::PrimitiveType(PrimitiveType::String(v)) => *v != "",
            Value::PrimitiveType(PrimitiveType::Pattern(_)) => true,
            Value::ArrayType(_) => true,
            Value::Empty => false,
        }
    }
}

impl<'a> Value<'a> {
    pub fn from_str(val: &'a str) -> Self {
        Self::PrimitiveType(PrimitiveType::String(val))
    }

    pub fn as_str(&self) -> String {
        match self {
            Self::Empty => "".to_string(),
            Self::PrimitiveType(val) => val.to_string(),
            Self::ArrayType(elements) => {
                let mut ret = String::new();
                ret.push_str("[");
                for (k, v) in elements {
                    ret.push_str(k);
                    ret.push_str(": ");
                    ret.push_str(v.to_string().as_str());
                    ret.push_str(", ");
                }
                if !elements.is_empty() {
                    ret = ret[0..ret.len() - 2].to_string();
                }
                ret.push_str("]");
                ret
            }
        }
    }

    pub fn from_int(val: i64) -> Self {
        Self::PrimitiveType(PrimitiveType::Integer(val))
    }

    pub fn as_int(&self) -> i64 {
        match self {
            Self::Empty => 0,
            Self::PrimitiveType(val) => match val {
                PrimitiveType::Integer(v) => *v,
                PrimitiveType::Float(v) => *v as i64,
                PrimitiveType::String(v) => match v.parse::<i64>() {
                    Ok(ret) => ret,
                    Err(_) => 0,
                },
                PrimitiveType::Pattern(_) => 0,
            },
            Self::ArrayType(_) => 0,
        }
    }

    pub fn from_float(val: f64) -> Self {
        Self::PrimitiveType(PrimitiveType::Float(val))
    }

    pub fn from_array(val: HashMap<String, PrimitiveType<'a>>) -> Self {
        Self::ArrayType(val)
    }
}
