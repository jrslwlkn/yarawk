use core::panic;
use std::collections::HashMap;

use regex::Regex;

use crate::{
    parser::Program,
    parser_helpers::{BinaryOperator, Expression, Statement, UnaryOperator},
    token::PrimitiveType,
};

pub struct Environment<'a> {
    program: &'a Program<'a>,
    variables: HashMap<String, Value>,
    functions: HashMap<String, (Vec<&'a str>, Vec<Statement<'a>>)>,
    max_field: usize,
    ranges_flags: Vec<bool>, // if true for a given idx, then we're currently in this running range
    local_scope: HashMap<String, Value>,
}

#[derive(Clone, Eq, Debug)]
pub enum Value {
    Empty,
    PrimitiveType(PrimitiveType),
    ArrayType(HashMap<String, PrimitiveType>),
}

impl<'a> Environment<'a> {
    pub fn new(program: &'a Program<'a>) -> Self {
        let mut ret = Self {
            program,
            variables: Self::default_variables(),
            functions: Self::default_functions(),
            max_field: 0,
            ranges_flags: vec![],
            local_scope: HashMap::new(),
        };
        for (name, params, statements) in &program.functions {
            if ret.functions.contains_key(*name) {
                panic!("function {} already exists", name)
            }
            ret.functions
                .insert(name.to_string(), (params.clone(), statements.clone()));
        }
        ret
    }

    pub fn get_rs(&self) -> u8 {
        *self
            .get_variable(&"RS".to_string())
            .to_string()
            .as_bytes()
            .first()
            .unwrap_or(&b'\n')
    }

    pub fn set_variable(&mut self, name: String, val: Value) {
        match self.local_scope.get(&name.to_string()) {
            None => {
                self.variables.insert(name, val);
            }
            Some(_) => {
                self.local_scope.insert(name, val);
            }
        }
    }

    pub fn get_array_variable(&mut self, name: String, accessors: &[Expression<'a>]) -> Value {
        let variable = match self.get_variable(&name) {
            Value::ArrayType(val) => val,
            _ => HashMap::new(),
        };
        let prev_subsep = self.get_variable(&"SUBSEP".to_string());
        self.set_variable("SUBSEP".to_string(), Value::from_string(",".to_string()));
        let key = accessors
            .iter()
            .map(|e| self.evaluate(e).to_string())
            .reduce(|mut acc, x| {
                acc.push_str(x.as_str());
                acc.push(',');
                acc
            })
            .unwrap();
        let ret = match variable.get(&key) {
            None => Value::Empty,
            Some(v) => Value::from_primitive(v),
        };
        self.set_variable("SUBSEP".to_string(), prev_subsep);
        ret
    }

    // TODO: The same name shall not be used within the same scope both
    //       as a scalar variable and as an array.
    fn set_array_variable(&mut self, name: String, accessors: &Vec<Expression<'a>>, value: Value) {
        let mut variable = match self.get_variable(&name) {
            Value::ArrayType(val) => val,
            _ => HashMap::new(),
        };
        let prev_subsep = self.get_variable(&"SUBSEP".to_string());
        self.set_variable("SUBSEP".to_string(), Value::from_string(",".to_string()));
        let key = accessors
            .iter()
            .map(|e| self.evaluate(e).to_string())
            .reduce(|mut acc, x| {
                acc.push_str(x.as_str());
                acc.push(',');
                acc
            })
            .unwrap();
        variable.insert(key, value.to_primitive());
        self.set_variable("SUBSEP".to_string(), prev_subsep);
        self.set_variable(name, Value::ArrayType(variable));
    }

    pub fn execute_begin(&mut self) {
        for s in &self.program.begin {
            self.execute_one(s);
        }
    }

    pub fn execute_end(&mut self) {
        for s in &self.program.end {
            self.execute_one(s);
        }
    }

    pub fn execute_actions(&mut self, record: &str) {
        // parse and set field variables
        self.set_variable("0".to_string(), Value::from_string(record.to_string()));
        let fs = self.get_variable(&"FS".to_string()).to_regex();
        let mut i = 1;
        for val in fs.split(&record) {
            let val = val.trim();
            if !val.is_empty() {
                self.set_variable(i.to_string(), Value::from_string(val.to_string()));
                i += 1;
            }
        }
        for n in i..=self.max_field {
            // remove fields from the previous record
            self.variables.remove(&n.to_string());
        }
        self.max_field = i;
        self.set_variable("NF".to_string(), Value::from_int(i as i64 - 1));
        // execute actions
        let mut range_idx = 0;
        for (expressions, statements) in &self.program.actions {
            match expressions.len() {
                0 => {
                    self.execute_in_action(statements);
                }
                1 => match self.evaluate(expressions.get(0).unwrap()) {
                    Value::Empty => {
                        self.execute_in_action(statements);
                    }
                    Value::PrimitiveType(PrimitiveType::Pattern(re)) => {
                        if re.is_match(record) {
                            self.execute_in_action(statements);
                        }
                    }
                    val => {
                        if val.is_truthy() {
                            self.execute_in_action(statements);
                        }
                    }
                },
                2 => {
                    if self.ranges_flags.is_empty() || self.ranges_flags.len() - 1 < range_idx {
                        // init
                        self.ranges_flags.push(false);
                    }
                    if self.ranges_flags[range_idx] {
                        // we're in running range, test if should stop
                        match self.evaluate(expressions.get(1).unwrap()) {
                            Value::PrimitiveType(PrimitiveType::Pattern(p)) => {
                                if self.match_first(record, &p).0 > 0 {
                                    self.ranges_flags[range_idx] = false;
                                    self.execute_in_action(statements);
                                }
                            }
                            val => {
                                if val.is_truthy() {
                                    self.execute_in_action(statements);
                                }
                            }
                        }
                    } else {
                        // we're not in running range, test if should start
                        let mut cur_record = record;
                        match self.evaluate(expressions.get(0).unwrap()) {
                            Value::PrimitiveType(PrimitiveType::Pattern(p)) => {
                                let (match_start, match_len) = self.match_first(cur_record, &p);
                                if match_start > 0 {
                                    cur_record = &cur_record[(match_start + match_len - 1)..];
                                    self.ranges_flags[range_idx] = true;
                                    self.execute_in_action(statements);
                                    if self.match_first(cur_record, &p).0 > 0 {
                                        // range ends within record
                                        self.ranges_flags[range_idx] = false;
                                    }
                                }
                            }
                            val => {
                                if val.is_truthy() {
                                    self.execute_in_action(statements);
                                }
                            }
                        }
                    }
                    range_idx += 1;
                }
                _ => panic!(
                    "expected: expression or range expression(2), received {} expressions",
                    expressions.len()
                ),
            }
        }
    }

    fn execute(&mut self, statements: &Vec<Statement<'a>>) -> Value {
        let mut ret = Value::Empty;
        for s in statements {
            match self.execute_one(s) {
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

    fn execute_in_action(&mut self, statements: &Vec<Statement<'a>>) -> Value {
        let mut ret = Value::Empty;
        for s in statements {
            match self.execute_one(s) {
                Value::Empty => {}
                val => {
                    // here we assume that if a statement returns a value, it must be a return or exit.
                    ret = val;
                    break;
                }
            }
        }
        if statements.is_empty()
            || statements.len() == 1 && *statements.get(0).unwrap() == Statement::Empty
        {
            // default statement in actions is print current record
            self.execute_one(&Statement::Print(vec![]));
        }
        ret
    }

    fn execute_in_loop(&mut self, statements: &Vec<Statement<'a>>) -> Value {
        let mut ret = Value::Empty;
        for s in statements {
            match s {
                Statement::Continue => break,
                Statement::Break => {
                    return Value::Empty;
                }
                s => match self.execute_one(s) {
                    Value::Empty => {}
                    val => {
                        ret = val;
                        break;
                    }
                },
            }
        }
        ret
    }

    fn execute_one(&mut self, statement: &Statement<'a>) -> Value {
        match statement {
            Statement::Empty => Value::Empty,
            Statement::Exit(e) => {
                std::process::exit(self.evaluate(e).to_int().try_into().unwrap_or(-69420));
            }
            Statement::Return(e) => self.evaluate(e),
            Statement::Next => todo!(),
            Statement::If(cond, truthy, falsy) => {
                if self.evaluate(cond).is_truthy() {
                    self.execute(truthy)
                } else {
                    self.execute(falsy)
                }
            }
            Statement::DoWhile(statements, cond) => {
                match self.execute_in_loop(statements) {
                    Value::Empty => {}
                    val => {
                        return val;
                    }
                }
                while self.evaluate(cond).is_truthy() {
                    match self.execute_in_loop(statements) {
                        Value::Empty => {}
                        val => {
                            return val;
                        }
                    }
                }
                Value::Empty
            }
            Statement::While(cond, statements) => {
                while self.evaluate(cond).is_truthy() {
                    match self.execute_in_loop(statements) {
                        Value::Empty => {}
                        val => {
                            return val;
                        }
                    }
                }
                Value::Empty
            }
            Statement::For(e1, Some(cond), Some(e2), statements) => {
                while self.evaluate(cond).is_truthy() {
                    self.evaluate(e1);
                    self.evaluate(e2);
                    match self.execute_in_loop(statements) {
                        Value::Empty => {}
                        val => {
                            return val;
                        }
                    }
                }
                Value::Empty
            }
            Statement::For(expression, None, None, statements) => match expression {
                Expression::Binary(BinaryOperator::In, lhs, rhs) => {
                    todo!()
                }
                _ => {
                    panic!(
                        "expected: [iter_variable] [in] [array_variable], received: {:?}",
                        expression
                    ) // FIXME: store positions
                }
            },
            Statement::Expression(e) => {
                self.evaluate(e);
                Value::Empty
            }
            Statement::Print(expressions) => {
                let record = self.get_variable(&"0".to_string()).to_string();
                let ors = self.get_variable(&"ORS".to_string()).to_string();
                let ofs = self.get_variable(&"OFS".to_string()).to_string();
                match expressions.len() {
                    // {
                    // print
                    //
                    // print > "filename"
                    //  }
                    0 => {
                        print!("{}{}", record, ors);
                    }
                    1 => match expressions.get(0).unwrap() {
                        Expression::Binary(BinaryOperator::GreaterThan, lhs, rhs) => todo!(),
                        Expression::Binary(BinaryOperator::Append, lhs, rhs) => todo!(),
                        Expression::Binary(BinaryOperator::Pipe, lhs, rhs) => todo!(),
                        e => print!("{}{}", self.evaluate(e).to_string(), ors),
                    },
                    _ => {
                        let mut out = "".to_string();
                        let mut need_sep = false;
                        for e in expressions {
                            if need_sep {
                                out.push_str(&ofs);
                            }
                            need_sep = true;
                            out.push_str(&self.evaluate(e).to_string());
                        }
                        out.push_str(&ors);
                        print!("{}", out);
                    }
                };
                Value::Empty
            }
            _ => panic!("unexpected statement: {:?}", statement),
        }
    }

    fn evaluate(&mut self, expression: &Expression<'a>) -> Value {
        match expression {
            Expression::Empty => Value::Empty,
            Expression::Literal(val) => Value::from_primitive(val),
            Expression::Variable(name) => self.get_variable(&name.to_string()),
            Expression::ArrayVariable(name, expressions) => {
                self.get_array_variable(name.to_string(), expressions)
            }
            Expression::FieldVariable(e) => {
                let name = self.evaluate(e).to_string();
                self.get_variable(&name)
            }
            Expression::Grouping(expressions) => {
                if expressions.len() != 1 {
                    panic!(
                        "expected 1 expression, received: {}, {:?}",
                        expressions.len(),
                        *expressions
                    )
                }
                self.evaluate(expressions.get(0).unwrap())
            }
            Expression::Unary(op, val) => match op {
                UnaryOperator::PrePlus => match self.evaluate(val) {
                    Value::PrimitiveType(PrimitiveType::Integer(val)) => Value::from_int(val),
                    Value::PrimitiveType(PrimitiveType::Float(val)) => Value::from_float(val),
                    _ => Value::default(),
                },
                UnaryOperator::PreMinus => match self.evaluate(val) {
                    Value::PrimitiveType(PrimitiveType::Integer(val)) => Value::from_int(-val),
                    Value::PrimitiveType(PrimitiveType::Float(val)) => Value::from_float(-val),
                    _ => Value::default(),
                },
                UnaryOperator::PrePlusPlus => match *val.clone() {
                    Expression::Variable(name) => {
                        let ret = Self::add_int(self.evaluate(val), 1);
                        self.set_variable(name.to_string(), ret.clone());
                        ret
                    }
                    Expression::ArrayVariable(name, accessors) => {
                        let val = self.get_array_variable(name.to_string(), &accessors);
                        let ret = Self::add_int(val, 1);
                        self.set_array_variable(name.to_string(), &accessors, ret.clone());
                        ret
                    }
                    Expression::FieldVariable(expression) => {
                        let name = self.evaluate(&*expression).to_string();
                        let ret = Self::add_int(self.evaluate(val), 1);
                        self.set_variable(name, ret.clone());
                        ret
                    }
                    _ => panic!("expected: [++][variable], received: {:?}", val),
                },
                UnaryOperator::PreMinusMinus => match *val.clone() {
                    Expression::Variable(name) => {
                        let ret = Self::add_int(self.evaluate(val), -1);
                        self.set_variable(name.to_string(), ret.clone());
                        ret
                    }
                    Expression::ArrayVariable(name, accessors) => {
                        let val = self.get_array_variable(name.to_string(), &accessors);
                        let ret = Self::add_int(val, -1);
                        self.set_array_variable(name.to_string(), &accessors, ret.clone());
                        ret
                    }
                    Expression::FieldVariable(expression) => {
                        let name = self.evaluate(&*expression).to_string();
                        let ret = Self::add_int(self.evaluate(val), -1);
                        self.set_variable(name, ret.clone());
                        ret
                    }
                    _ => panic!("expected: [--][variable], received: {:?}", val),
                },
                UnaryOperator::PostPlusPlus => match *val.clone() {
                    Expression::Variable(name) => {
                        let ret = self.evaluate(val);
                        self.set_variable(name.to_string(), Self::add_int(ret.clone(), 1));
                        ret
                    }
                    Expression::ArrayVariable(name, accessors) => {
                        let ret = self.get_array_variable(name.to_string(), &accessors);
                        self.set_array_variable(
                            name.to_string(),
                            &accessors,
                            Self::add_int(ret.clone(), 1),
                        );
                        ret
                    }
                    Expression::FieldVariable(expression) => {
                        let name = self.evaluate(&*expression).to_string();
                        let ret = self.evaluate(val);
                        self.set_variable(name, Self::add_int(ret.clone(), 1));
                        ret
                    }
                    _ => panic!("expected: [variable][++] , received: {:?}", val),
                },
                UnaryOperator::PostMinusMinus => match *val.clone() {
                    Expression::Variable(name) => {
                        let ret = self.evaluate(val);
                        self.set_variable(name.to_string(), Self::add_int(ret.clone(), -1));
                        ret
                    }
                    Expression::ArrayVariable(name, accessors) => {
                        let ret = self.get_array_variable(name.to_string(), &accessors);
                        self.set_array_variable(
                            name.to_string(),
                            &accessors,
                            Self::add_int(ret.clone(), -1),
                        );
                        ret
                    }
                    Expression::FieldVariable(expression) => {
                        let name = self.evaluate(&*expression).to_string();
                        let ret = self.evaluate(val);
                        self.set_variable(name, Self::add_int(ret.clone(), -1));
                        ret
                    }
                    _ => panic!("expected: [variable][--], received: {:?}", val),
                },
                UnaryOperator::Not => {
                    Value::from_int(if self.evaluate(val).is_truthy() { 0 } else { 1 })
                }
            },
            Expression::Binary(op, lhs_expression, rhs_expression) => {
                let lhs = self.evaluate(lhs_expression);
                let rhs = self.evaluate(rhs_expression);
                match op {
                    BinaryOperator::Concat => {
                        let val = format!("{}{}", lhs.to_string(), rhs.to_string());
                        Value::from_string(val)
                    }
                    BinaryOperator::EqualEqual => Value::from_int(if lhs == rhs { 1 } else { 0 }),
                    BinaryOperator::NotEqual => Value::from_int(if lhs != rhs { 1 } else { 0 }),
                    BinaryOperator::LessThan => {
                        let val = match (&lhs, &rhs) {
                            (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                            | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                                if lhs.to_float() < rhs.to_float() {
                                    1
                                } else {
                                    0
                                }
                            }
                            (Value::PrimitiveType(PrimitiveType::Integer(_)), _)
                            | (_, Value::PrimitiveType(PrimitiveType::Integer(_))) => {
                                if lhs.to_int() < rhs.to_int() {
                                    1
                                } else {
                                    0
                                }
                            }
                            _ => 0,
                        };
                        Value::from_int(val)
                    }
                    BinaryOperator::GreaterThan => {
                        let val = match (&lhs, &rhs) {
                            (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                            | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                                if lhs.to_float() > rhs.to_float() {
                                    1
                                } else {
                                    0
                                }
                            }
                            (Value::PrimitiveType(PrimitiveType::Integer(_)), _)
                            | (_, Value::PrimitiveType(PrimitiveType::Integer(_))) => {
                                if lhs.to_int() > rhs.to_int() {
                                    1
                                } else {
                                    0
                                }
                            }
                            _ => 0,
                        };
                        Value::from_int(val)
                    }
                    BinaryOperator::LessEqual => {
                        let val = match (&lhs, &rhs) {
                            (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                            | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                                if lhs.to_float() <= rhs.to_float() {
                                    1
                                } else {
                                    0
                                }
                            }
                            (Value::PrimitiveType(PrimitiveType::Integer(_)), _)
                            | (_, Value::PrimitiveType(PrimitiveType::Integer(_))) => {
                                if lhs.to_int() <= rhs.to_int() {
                                    1
                                } else {
                                    0
                                }
                            }
                            _ => 0,
                        };
                        Value::from_int(val)
                    }
                    BinaryOperator::GreaterEqual => {
                        let val = match (&lhs, &rhs) {
                            (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                            | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                                if lhs.to_float() >= rhs.to_float() {
                                    1
                                } else {
                                    0
                                }
                            }
                            (Value::PrimitiveType(PrimitiveType::Integer(_)), _)
                            | (_, Value::PrimitiveType(PrimitiveType::Integer(_))) => {
                                if lhs.to_int() >= rhs.to_int() {
                                    1
                                } else {
                                    0
                                }
                            }
                            _ => 0,
                        };
                        Value::from_int(val)
                    }
                    BinaryOperator::Match => Value::from_int(
                        if self.match_first(&lhs.to_string(), &rhs.to_regex()).0 > 0 {
                            1
                        } else {
                            0
                        },
                    ),
                    BinaryOperator::NotMatch => Value::from_int(
                        if self.match_first(&lhs.to_string(), &rhs.to_regex()).0 == 0 {
                            1
                        } else {
                            0
                        },
                    ),
                    BinaryOperator::Plus => match (&lhs, &rhs) {
                        (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                        | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                            Value::from_float(lhs.to_float() + rhs.to_float())
                        }
                        _ => Value::from_int(lhs.to_int() + rhs.to_int()),
                    },
                    BinaryOperator::Minus => match (&lhs, &rhs) {
                        (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                        | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                            Value::from_float(lhs.to_float() - rhs.to_float())
                        }
                        _ => Value::from_int(lhs.to_int() - rhs.to_int()),
                    },
                    BinaryOperator::Multiply => match (&lhs, &rhs) {
                        (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                        | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                            Value::from_float(lhs.to_float() * rhs.to_float())
                        }
                        _ => Value::from_int(lhs.to_int() * rhs.to_int()),
                    },
                    BinaryOperator::Divide => Value::from_float(lhs.to_float() / rhs.to_float()),
                    BinaryOperator::Modulo => Value::from_int(lhs.to_int() % rhs.to_int()),
                    BinaryOperator::Exponent => match (&lhs, &rhs) {
                        (
                            Value::PrimitiveType(PrimitiveType::Integer(_)),
                            Value::PrimitiveType(PrimitiveType::Integer(_)),
                        ) => Value::from_int(lhs.to_int().pow(rhs.to_int() as u32)),
                        _ => Value::from_float(lhs.to_float().powf(rhs.to_float())),
                    },
                    BinaryOperator::Equal => {
                        let val = rhs;
                        match &**lhs_expression {
                            Expression::Variable(name) => {
                                self.set_variable(name.to_string(), val.clone());
                            }
                            Expression::ArrayVariable(name, expressions) => {
                                // TODO: References to nonexistent fields (that is,
                                //       fields after $NF), shall evaluate to the uninitialized value. Such references shall not create new fields. However, assigning to a nonexis-
                                //       tent field (for example, $(NF+2)=5) shall increase the value of NF; create any intervening fields with the uninitialized value;	and  cause
                                //       the value of $0 to be recomputed, with the fields being separated by the value of OFS.  Each field variable shall have a string value or an
                                //       uninitialized value when created. Field variables shall have the uninitialized value when created from $0 using FS and  the  variable  does
                                //       not contain any characters.
                                let mut var = match self.get_variable(&name.to_string()) {
                                    Value::ArrayType(val) => val,
                                    _ => {
                                        let val = Value::ArrayType(HashMap::new());
                                        self.set_variable(name.to_string(), val);
                                        HashMap::new()
                                    }
                                };
                                let prev_subsep = self.get_variable(&"SUBSEP".to_string());
                                self.set_variable(
                                    "SUBSEP".to_string(),
                                    Value::from_string(",".to_string()),
                                );
                                let key = expressions
                                    .iter()
                                    .map(|e| self.evaluate(e).to_string())
                                    .reduce(|mut acc, x| {
                                        acc.push_str(x.as_str());
                                        acc.push(',');
                                        acc
                                    })
                                    .unwrap();
                                var.insert(key, val.to_primitive());
                                self.set_variable(name.to_string(), Value::ArrayType(var));
                                self.set_variable("SUBSEP".to_string(), prev_subsep);
                            }
                            Expression::FieldVariable(expression) => {
                                let name = self.evaluate(expression);
                                if name == Value::Empty {
                                    // TODO
                                    //
                                }
                                self.set_variable(name.to_string(), val.clone());
                            }
                            _ => panic!(
                                "expected variable assignment, received: {:?}",
                                **lhs_expression
                            ),
                        }
                        val
                    }
                    BinaryOperator::Or => Value::from_int(if lhs.is_truthy() || rhs.is_truthy() {
                        1
                    } else {
                        0
                    }),
                    BinaryOperator::And => Value::from_int(if lhs.is_truthy() && rhs.is_truthy() {
                        1
                    } else {
                        0
                    }),
                    BinaryOperator::In => todo!(),
                    BinaryOperator::Pipe => todo!(),
                    BinaryOperator::Append => todo!(),
                }
            }

            Expression::Ternary(cond, truthy, falsy) => {
                if self.evaluate(cond).is_truthy() {
                    self.evaluate(truthy)
                } else {
                    self.evaluate(falsy)
                }
            }
            Expression::Function(name, args) => {
                let function = self
                    .functions
                    .get(&name.to_string())
                    .expect(format!("function {} does not exist", name).as_str())
                    .clone();
                if args.len() > function.0.len() {
                    panic!("function {} does not take {} arguments", name, args.len())
                }

                let prev_scope = self.local_scope.clone();
                self.local_scope = HashMap::new();
                for i in 0..function.0.len() {
                    let val = match args.get(i) {
                        None => Value::Empty,
                        Some(e) => self.evaluate(e),
                    };
                    self.local_scope
                        .insert(function.0.get(i).unwrap().to_string(), val);
                }
                let ret = self.execute_in_action(&function.1.to_vec());
                self.local_scope = prev_scope;
                ret
            }
            Expression::Getline(_) => panic!("unexpected getline"),
        }
    }

    fn match_first(&mut self, lhs: &str, rhs: &Regex) -> (usize, usize) {
        // ([idx where match started, 1-based], [match length])
        let mut captures = rhs.captures_iter(lhs);
        match captures.next() {
            None => (0, 0),
            Some(c) => (
                c.get(0).unwrap().start() + 1,
                c.get(0).unwrap().range().len(),
            ),
        }
    }

    fn get_variable(&self, name: &String) -> Value {
        match self.local_scope.get(name) {
            None => self.variables.get(name).unwrap_or(&Value::Empty).clone(),
            Some(val) => val.clone(),
        }
    }

    fn add_int(lhs: Value, rhs: i64) -> Value {
        match lhs {
            Value::PrimitiveType(PrimitiveType::Integer(v)) => Value::from_int(v + rhs),
            Value::PrimitiveType(PrimitiveType::Float(v)) => Value::from_float(v + rhs as f64),
            _ => Value::from_int(rhs),
        }
    }

    fn default_variables() -> HashMap<String, Value> {
        let mut ret = HashMap::new();
        ret.insert("ARGC".to_string(), Value::from_string("".to_string()));
        ret.insert("ARGV".to_string(), Value::from_int(0));
        ret.insert(
            "CONVFMT".to_string(),
            Value::from_string("%.6g".to_string()),
        );
        ret.insert("FILENAME".to_string(), Value::from_string("".to_string()));
        ret.insert("FNR".to_string(), Value::from_int(0));
        ret.insert("FS".to_string(), Value::from_string(" ".to_string()));
        ret.insert("NF".to_string(), Value::from_int(0));
        ret.insert("NR".to_string(), Value::from_int(0));
        ret.insert("OFMT".to_string(), Value::from_string("%.6g".to_string()));
        ret.insert("OFS".to_string(), Value::from_string(" ".to_string()));
        ret.insert("ORS".to_string(), Value::from_string("\n".to_string()));
        ret.insert("RLENGTH".to_string(), Value::from_int(0));
        ret.insert("RS".to_string(), Value::from_string("\n".to_string()));
        ret.insert("RSTART".to_string(), Value::from_int(0));
        ret.insert("SUBSEP".to_string(), Value::from_string(",".to_string()));
        ret
    }

    fn default_functions() -> HashMap<String, (Vec<&'a str>, Vec<Statement<'a>>)> {
        let mut ret = HashMap::new();
        ret.insert("atan2".to_string(), (vec![], vec![])); // TODO
        ret.insert("cos".to_string(), (vec![], vec![])); // TODO
        ret.insert("sin".to_string(), (vec![], vec![])); // TODO
        ret.insert("exp".to_string(), (vec![], vec![])); // TODO
        ret.insert("log".to_string(), (vec![], vec![])); // TODO
        ret.insert("sqrt".to_string(), (vec![], vec![])); // TODO
        ret.insert("int".to_string(), (vec![], vec![])); // TODO
        ret.insert("rand".to_string(), (vec![], vec![])); // TODO
        ret.insert("srand".to_string(), (vec![], vec![])); // TODO
        ret.insert("gsub".to_string(), (vec![], vec![])); // TODO
        ret.insert("index".to_string(), (vec![], vec![])); // TODO
        ret.insert("length".to_string(), (vec![], vec![])); // TODO
        ret.insert("match".to_string(), (vec![], vec![])); // TODO
        ret.insert("split".to_string(), (vec![], vec![])); // TODO
        ret.insert("sprintf".to_string(), (vec![], vec![])); // TODO
        ret.insert("sub".to_string(), (vec![], vec![])); // TODO
        ret.insert("substr".to_string(), (vec![], vec![])); // TODO
        ret.insert("tolower".to_string(), (vec![], vec![])); // TODO
        ret.insert("toupper".to_string(), (vec![], vec![])); // TODO
        ret.insert("close".to_string(), (vec![], vec![])); // TODO
        ret.insert("system".to_string(), (vec![], vec![])); // TODO
        ret
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PrimitiveType(lhs), Self::PrimitiveType(rhs)) => lhs == rhs,
            (Self::ArrayType(lhs), Self::ArrayType(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Value {
    pub fn default() -> Self {
        Self::from_primitive(&PrimitiveType::Integer(0))
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    fn is_truthy(&self) -> bool {
        match self {
            Self::PrimitiveType(PrimitiveType::Integer(v)) => *v != 0,
            Self::PrimitiveType(PrimitiveType::Float(v)) => *v != 0.0,
            Self::PrimitiveType(PrimitiveType::String(v)) => !v.is_empty(),
            Self::PrimitiveType(PrimitiveType::Pattern(_)) => true,
            Self::ArrayType(_) => true,
            Self::Empty => false,
        }
    }

    pub fn from_string(val: String) -> Self {
        Self::PrimitiveType(PrimitiveType::String(val))
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::Empty => "".to_string(),
            Self::PrimitiveType(val) => val.to_string(),
            Self::ArrayType(elements) => {
                let mut ret = String::new();
                ret.push('[');
                for (k, v) in elements {
                    ret.push_str(k);
                    ret.push_str(": ");
                    ret.push_str(v.to_string().as_str());
                    ret.push_str(", ");
                }
                if !elements.is_empty() {
                    ret = ret[0..ret.len() - 2].to_string();
                }
                ret.push(']');
                ret
            }
        }
    }

    pub fn to_regex(&self) -> Regex {
        match self {
            Value::Empty => Regex::new("").unwrap(),
            Self::PrimitiveType(val) => val.to_regex(),
            val => panic!("{:?} cannot be used as a pattern", val),
        }
    }

    pub fn from_int(val: i64) -> Self {
        Self::PrimitiveType(PrimitiveType::Integer(val))
    }

    pub fn to_int(&self) -> i64 {
        match self {
            Self::Empty => 0,
            Self::PrimitiveType(val) => match val {
                PrimitiveType::Integer(v) => *v,
                PrimitiveType::Float(v) => *v as i64,
                PrimitiveType::String(v) => v.parse::<i64>().unwrap_or(0),
                PrimitiveType::Pattern(_) => 0,
            },
            Self::ArrayType(_) => 0,
        }
    }

    pub fn from_float(val: f64) -> Self {
        Self::PrimitiveType(PrimitiveType::Float(val))
    }

    pub fn to_float(&self) -> f64 {
        match self {
            Self::Empty => 0.0,
            Self::PrimitiveType(val) => match val {
                PrimitiveType::Integer(v) => *v as f64,
                PrimitiveType::Float(v) => *v,
                PrimitiveType::String(v) => v.parse::<f64>().unwrap_or(0.0),
                PrimitiveType::Pattern(_) => 0.0,
            },
            Self::ArrayType(_) => 0.0,
        }
    }

    pub fn from_array(val: HashMap<String, PrimitiveType>) -> Self {
        Self::ArrayType(val)
    }

    pub fn from_primitive(val: &PrimitiveType) -> Self {
        Self::PrimitiveType(val.clone())
    }

    pub fn to_primitive(&self) -> PrimitiveType {
        match &self {
            Self::PrimitiveType(val) => val.clone(),
            _ => PrimitiveType::String("".to_string()),
        }
    }
}
