use core::panic;
use std::{cmp::Ordering, collections::HashMap};

use regex::Regex;

use crate::{
    parser::Program,
    parser_helpers::{BinaryOperator, Expression, Statement, UnaryOperator},
    token::PrimitiveType,
};

pub struct Environment<'a> {
    program: &'a Program<'a>,
    variables: HashMap<String, Value>,
    functions: HashMap<String, (u8, Vec<Statement<'a>>)>,
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
        for (name, arity, statements) in &program.functions {
            if ret.functions.contains_key(*name) {
                panic!("function {} already exists", name)
            }
            ret.functions
                .insert(name.to_string(), (*arity, statements.clone()));
        }
        ret
    }

    pub fn get_rs(&self) -> u8 {
        *self
            .get_variable(&"RS".to_string())
            .to_string()
            .as_bytes()
            .get(0)
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

    pub fn execute_actions(&mut self, record: &str) {
        // parse and set field variables
        self.set_variable("0".to_string(), Value::from_string(record.to_string()));
        let fs = self.get_variable(&"FS".to_string()).to_string();
        let mut i = 1;
        for val in record.split(&fs) {
            let val = val.trim();
            if val.len() > 0 {
                self.set_variable(i.to_string(), Value::from_string(val.to_string()));
                i += 1;
            }
        }
        for n in i..=self.max_field {
            // remove fields from the previous record
            self.variables.remove(&n.to_string());
        }
        self.max_field = i;
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
                                if self.match_first(&record.to_string(), &p).0 > 0 {
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
                                let (match_start, match_len) =
                                    self.match_first(&cur_record.to_string(), &p);
                                if match_start > 0 {
                                    cur_record = &cur_record[(match_start + match_len - 1)..];
                                    self.ranges_flags[range_idx] = true;
                                    self.execute_in_action(statements);
                                    if self.match_first(&cur_record.to_string(), &p).0 > 0 {
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

    fn execute(&mut self, statements: &Vec<Box<Statement<'a>>>) -> Value {
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
        if statements.len() == 0
            || statements.len() == 1 && *statements.get(0).unwrap() == Statement::Empty
        {
            // default statement in actions is print current record
            self.execute_one(&Statement::Print(vec![]));
        }
        ret
    }

    fn execute_in_loop(&mut self, statements: &Vec<Box<Statement<'a>>>) -> Value {
        let mut ret = Value::Empty;
        for s in statements {
            match *s.clone() {
                Statement::Continue => break,
                Statement::Break => {
                    return Value::Empty;
                }
                s => match self.execute_one(&s) {
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
                std::process::exit(self.evaluate(&e).to_int().try_into().unwrap_or(-69420));
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
            Expression::ArrayVariable(name, expressions) => todo!(),
            Expression::FieldVariable(e) => {
                let name = self.evaluate(&e).to_string();
                self.get_variable(&name)
            }
            Expression::Grouping(expressions) => self.evaluate(expressions.get(0).unwrap()), //FIXME: should we handle (x,y,z) thingys here?
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
                    Expression::Variable(name) => todo!(),
                    Expression::ArrayVariable(name, expressions) => todo!(),
                    Expression::FieldVariable(expression) => todo!(),
                    _ => panic!("expected: [++][variable], received: {:?}", val),
                },
                UnaryOperator::PreMinusMinus => match *val.clone() {
                    Expression::Variable(name) => todo!(),
                    Expression::ArrayVariable(name, expressions) => todo!(),
                    Expression::FieldVariable(expression) => todo!(),
                    _ => panic!("expected: [--][variable], received: {:?}", val),
                },
                UnaryOperator::PostPlusPlus => match *val.clone() {
                    Expression::Variable(name) => todo!(),
                    Expression::ArrayVariable(name, expressions) => todo!(),
                    Expression::FieldVariable(expression) => todo!(),
                    _ => panic!("expected: [variable][++] , received: {:?}", val),
                },
                UnaryOperator::PostMinusMinus => match *val.clone() {
                    Expression::Variable(name) => todo!(),
                    Expression::ArrayVariable(name, expressions) => todo!(),
                    Expression::FieldVariable(expression) => todo!(),
                    _ => panic!("expected: [variable][--], received: {:?}", val),
                },
                UnaryOperator::Not => Value::from_int(if self.evaluate(&val).is_truthy() {
                    0
                } else {
                    1
                }),
            },
            Expression::Binary(op, lhs, rhs) => match op {
                BinaryOperator::Concat => {
                    let lhs = self.evaluate(lhs).to_string();
                    let rhs = self.evaluate(rhs).to_string();
                    let val = format!("{} {}", lhs, rhs);
                    Value::from_string(val)
                }
                BinaryOperator::EqualEqual => {
                    Value::from_int(if self.evaluate(lhs) == self.evaluate(rhs) {
                        1
                    } else {
                        0
                    })
                }
                BinaryOperator::NotEqual => {
                    Value::from_int(if self.evaluate(lhs) != self.evaluate(rhs) {
                        1
                    } else {
                        0
                    })
                }
                BinaryOperator::LessThan => Value::from_int(
                    if self.evaluate(lhs).cmp(&self.evaluate(rhs)) == Ordering::Less {
                        1
                    } else {
                        0
                    },
                ),
                BinaryOperator::GreaterThan => Value::from_int(
                    if self.evaluate(lhs).cmp(&self.evaluate(rhs)) == Ordering::Greater {
                        1
                    } else {
                        0
                    },
                ),
                BinaryOperator::LessEqual => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    Value::from_int(match lhs.cmp(&rhs) {
                        Ordering::Greater => 0,
                        _ => 1,
                    })
                }
                BinaryOperator::GreaterEqual => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    Value::from_int(match lhs.cmp(&rhs) {
                        Ordering::Less => 0,
                        _ => 1,
                    })
                }
                BinaryOperator::Match => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    Value::from_int(
                        if self.match_first(&lhs.to_string(), &rhs.to_regex()).0 > 0 {
                            1
                        } else {
                            0
                        },
                    )
                }
                BinaryOperator::NotMatch => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    Value::from_int(
                        if self.match_first(&lhs.to_string(), &rhs.to_regex()).0 == 0 {
                            1
                        } else {
                            0
                        },
                    )
                }
                BinaryOperator::Plus => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    match (&lhs, &rhs) {
                        (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                        | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                            Value::from_float(lhs.to_float() + rhs.to_float())
                        }
                        _ => Value::from_int(lhs.to_int() + rhs.to_int()),
                    }
                }
                BinaryOperator::Minus => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    match (&lhs, &rhs) {
                        (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                        | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                            Value::from_float(lhs.to_float() - rhs.to_float())
                        }
                        _ => Value::from_int(lhs.to_int() - rhs.to_int()),
                    }
                }
                BinaryOperator::Multiply => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    match (&lhs, &rhs) {
                        (Value::PrimitiveType(PrimitiveType::Float(_)), _)
                        | (_, Value::PrimitiveType(PrimitiveType::Float(_))) => {
                            Value::from_float(lhs.to_float() * rhs.to_float())
                        }
                        _ => Value::from_int(lhs.to_int() * rhs.to_int()),
                    }
                }
                BinaryOperator::Divide => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    Value::from_float(lhs.to_float() / rhs.to_float())
                }
                BinaryOperator::Modulo => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    Value::from_int(lhs.to_int() % rhs.to_int())
                }
                BinaryOperator::Exponent => {
                    let lhs = self.evaluate(lhs);
                    let rhs = self.evaluate(rhs);
                    match (&lhs, &rhs) {
                        (
                            Value::PrimitiveType(PrimitiveType::Integer(_)),
                            Value::PrimitiveType(PrimitiveType::Integer(_)),
                        ) => Value::from_int(lhs.to_int().pow(rhs.to_int() as u32)),
                        _ => Value::from_float(lhs.to_float().powf(rhs.to_float())),
                    }
                }
                BinaryOperator::Equal => {
                    let val = self.evaluate(rhs);
                    match **lhs {
                        Expression::Variable(name) => {
                            self.set_variable(name.to_string(), val.clone());
                        }
                        Expression::ArrayVariable(_, _) => {
                            todo!()
                        }
                        Expression::FieldVariable(_) => {
                            todo!()
                        }
                        _ => panic!("expected variable assignment, received: {:?}", **lhs),
                    }
                    val
                }
                BinaryOperator::Or => Value::from_int(
                    if self.evaluate(lhs).is_truthy() || self.evaluate(rhs).is_truthy() {
                        1
                    } else {
                        0
                    },
                ),
                BinaryOperator::And => Value::from_int(
                    if self.evaluate(lhs).is_truthy() && self.evaluate(rhs).is_truthy() {
                        1
                    } else {
                        0
                    },
                ),
                BinaryOperator::In => todo!(),
                BinaryOperator::Pipe => todo!(),
                BinaryOperator::Append => todo!(),
            },
            Expression::Ternary(cond, truthy, falsy) => {
                if self.evaluate(cond).is_truthy() {
                    self.evaluate(truthy)
                } else {
                    self.evaluate(falsy)
                }
            }
            Expression::Function(name, arity, args) => {
                let mut function = self
                    .functions
                    .get(&name.to_string())
                    .expect(format!("function {} does not exist", name).as_str())
                    .clone();
                if *arity > function.0 {
                    panic!("function {} does not take {} arguments", name, *arity);
                }

                let prev_scope = self.local_scope.clone();
                self.local_scope = HashMap::new();

                // first <arity> statements are arg declarations
                for _ in 0..function.0 {
                    match function.1.get(0).unwrap() {
                        Statement::Expression(Expression::Binary(BinaryOperator::Equal, e, _)) => {
                            match **e {
                                Expression::Variable(n) => {
                                    let val = match args.get(0) {
                                        None => Value::Empty,
                                        Some(val) => self.evaluate(val),
                                    };
                                    self.local_scope.insert(n.to_string(), val);
                                    function.1.remove(0);
                                }
                                _ => unreachable!(),
                            }
                        }
                        s => {
                            panic!("unexpected {:?}", s)
                        }
                    };
                }
                let ret = self.execute_in_action(&function.1.to_vec());
                self.local_scope = prev_scope;
                ret
            }
            Expression::Getline(_) => panic!("unexpected getline"),
        }
    }

    fn match_first(&mut self, lhs: &String, rhs: &Regex) -> (usize, usize) {
        // ([idx where match started, 1-based], [match length])
        let mut captures = rhs.captures_iter(lhs);
        match captures.nth(0) {
            None => (0, 0),
            Some(c) => (
                c.get(0).unwrap().start() + 1,
                c.get(0).unwrap().range().len(),
            ),
        }
    }

    fn get_variable(&self, name: &String) -> Value {
        match self.local_scope.get(name) {
            None => self.variables.get(name).unwrap_or(&&Value::Empty).clone(),
            Some(val) => val.clone(),
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

    fn default_functions() -> HashMap<String, (u8, Vec<Statement<'a>>)> {
        let mut ret = HashMap::new();
        ret.insert("atan2".to_string(), (2, vec![])); // TODO
        ret.insert("cos".to_string(), (1, vec![])); // TODO
        ret.insert("sin".to_string(), (1, vec![])); // TODO
        ret.insert("exp".to_string(), (1, vec![])); // TODO
        ret.insert("log".to_string(), (1, vec![])); // TODO
        ret.insert("sqrt".to_string(), (1, vec![])); // TODO
        ret.insert("int".to_string(), (1, vec![])); // TODO
        ret.insert("rand".to_string(), (1, vec![])); // TODO
        ret.insert("srand".to_string(), (1, vec![])); // TODO
        ret.insert("gsub".to_string(), (3, vec![])); // TODO
        ret.insert("index".to_string(), (2, vec![])); // TODO
        ret.insert("length".to_string(), (1, vec![])); // TODO
        ret.insert("match".to_string(), (2, vec![])); // TODO
        ret.insert("split".to_string(), (3, vec![])); // TODO
        ret.insert("sprintf".to_string(), (2, vec![])); // TODO
        ret.insert("sub".to_string(), (3, vec![])); // TODO
        ret.insert("substr".to_string(), (3, vec![])); // TODO
        ret.insert("tolower".to_string(), (1, vec![])); // TODO
        ret.insert("toupper".to_string(), (1, vec![])); // TODO
        ret.insert("close".to_string(), (1, vec![])); // TODO
        ret.insert("system".to_string(), (1, vec![])); // TODO
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

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::PrimitiveType(lhs), Self::PrimitiveType(rhs)) => lhs.cmp(rhs),
            _ => Ordering::Equal,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::PrimitiveType(lhs), Self::PrimitiveType(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

impl<'a> Value {
    pub fn default() -> Self {
        Self::from_primitive(&PrimitiveType::Integer(0))
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Self::Empty => true,
            _ => false,
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Self::PrimitiveType(PrimitiveType::Integer(v)) => *v != 0,
            Self::PrimitiveType(PrimitiveType::Float(v)) => *v != 0.0,
            Self::PrimitiveType(PrimitiveType::String(v)) => *v != "",
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

    pub fn to_float(&self) -> f64 {
        match self {
            Self::Empty => 0.0,
            Self::PrimitiveType(val) => match val {
                PrimitiveType::Integer(v) => *v as f64,
                PrimitiveType::Float(v) => *v,
                PrimitiveType::String(v) => match v.parse::<f64>() {
                    Ok(ret) => ret,
                    Err(_) => 0.0,
                },
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
}
