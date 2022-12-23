use core::panic;
use regex::Regex;
use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader, BufWriter, Cursor, Write},
    process::{Command, Stdio},
};

use crate::{
    parser::Program,
    parser_helpers::{BinaryOperator, Expression, Statement, UnaryOperator},
    token::PrimitiveType,
};

pub struct Environment<'a> {
    input_filepaths: &'a [&'a str],
    file_idx: usize,
    last_record: String,
    file_reader: Option<BufReader<File>>,
    program: &'a Program<'a>,
    variables: HashMap<String, Value>,
    functions: HashMap<String, (Vec<&'a str>, Vec<Statement<'a>>)>,
    max_field: usize,
    ranges_flags: Vec<bool>, // if true for a given idx, then we're currently in this running range
    local_scope: HashMap<String, Value>,
    read_streams: HashMap<String, ReadStream>,
    write_streams: HashMap<String, WriteStream>,
}

pub struct ReadStream {
    reader: Box<dyn BufRead>,
    last_record: String,
}

pub struct WriteStream {
    filepath: String,
    command: String,
    input: String,
    truncate: bool,
}

#[derive(Clone, Eq, Debug)]
pub enum Value {
    Empty,
    PrimitiveType(PrimitiveType),
    ArrayType(HashMap<String, PrimitiveType>),
}

enum ActionResult {
    None,
    Next,
    Exit,
}

impl<'a> Environment<'a> {
    pub fn new(input_filepaths: &'a [&'a str], program: &'a Program<'a>) -> Self {
        let mut ret = Self {
            input_filepaths,
            file_idx: 0,
            last_record: String::from(""),
            file_reader: None,
            program,
            variables: Self::default_variables(),
            functions: HashMap::new(),
            max_field: 0,
            ranges_flags: vec![],
            local_scope: HashMap::new(),
            read_streams: HashMap::new(),
            write_streams: HashMap::new(),
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

    // returns whether file was opened
    fn open_next_file(&mut self, idx: usize) -> bool {
        if idx >= self.input_filepaths.len() {
            return false;
        }
        self.file_idx = idx;
        let filepath = self.input_filepaths[idx];
        let file =
            File::open(filepath).unwrap_or_else(|_| panic!("unable to open file: {}", filepath));
        self.file_reader = Some(BufReader::new(file));
        self.set_variable(
            "FILENAME".to_string(),
            Value::from_string(filepath.to_string()),
        );
        self.set_variable("FNR".to_string(), Value::from_int(0));
        true
    }

    fn next_record(&mut self) -> Option<String> {
        if self.file_idx >= self.input_filepaths.len()
            || self.file_reader.is_none() && !self.open_next_file(self.file_idx)
        {
            return None;
        }

        let rs = self.get_rs();
        let mut record = vec![];
        match self
            .file_reader
            .as_mut()
            .unwrap()
            .read_until(rs, &mut record)
        {
            Err(_) => {
                panic!(
                    "unable to read file: {}",
                    self.input_filepaths[self.file_idx]
                );
            }
            Ok(v) => {
                if v == 0 && !self.open_next_file(self.file_idx + 1) {
                    return None;
                }
                if v == 0 {
                    // new file just got opened, rerun the function
                    return self.next_record();
                }
                record.pop(); // remove record separator at the end
                let ret = std::str::from_utf8(record.as_slice())
                    .unwrap_or(
                        format!(
                            "unable to read file: {}",
                            self.input_filepaths[self.file_idx]
                        )
                        .trim(),
                    )
                    .to_string();
                self.last_record = ret.clone();
                Some(ret)
            }
        }
    }

    fn next_stream_record(&mut self, stream_name: &String) -> (String, bool) {
        let rs = self.get_rs();
        if !self.read_streams.contains_key(stream_name) {
            let (s, exists) = ReadStream::new(stream_name);
            if !exists {
                return ("".to_string(), false);
            }
            self.read_streams.insert(stream_name.clone(), s);
        };
        let stream: &mut ReadStream = self.read_streams.get_mut(stream_name).unwrap();
        let mut record = vec![];
        match stream.reader.as_mut().read_until(rs, &mut record) {
            Err(_) => {
                panic!(
                    "unable to read stream: {}",
                    self.input_filepaths[self.file_idx]
                );
            }
            Ok(v) => {
                if v == 0 {
                    return (stream.last_record.to_string(), false);
                }
                record.pop(); // remove record separator at the end
                let ret = std::str::from_utf8(record.as_slice())
                    .unwrap_or(format!("unable to read file: {}", stream_name).trim())
                    .to_string();
                (ret, true)
            }
        }
    }

    fn get_rs(&self) -> u8 {
        *self
            .get_variable("RS")
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

    fn get_array_accessor(&mut self, accessors: &[Expression<'a>]) -> String {
        let prev_subsep = self.get_variable("SUBSEP");
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
        self.set_variable("SUBSEP".to_string(), prev_subsep);
        key
    }

    fn get_array_variable(&mut self, name: &str, accessors: &[Expression<'a>]) -> Value {
        let variable = match self.get_variable(name) {
            Value::ArrayType(val) => val,
            Value::Empty => HashMap::new(),
            Value::PrimitiveType(v) => panic!("{} has type {:?} but was used as array", name, v),
        };
        let key = self.get_array_accessor(accessors);
        let ret = match variable.get(&key) {
            None => Value::Empty,
            Some(v) => Value::from_primitive(v),
        };
        ret
    }

    fn set_array_variable(&mut self, name: String, accessors: &[Expression<'a>], value: Value) {
        let mut variable = match self.get_variable(&name) {
            Value::ArrayType(val) => val,
            Value::Empty => HashMap::new(),
            Value::PrimitiveType(v) => panic!("{} has type {:?} but was used as array", name, v),
        };
        let key = self.get_array_accessor(accessors);
        variable.insert(key, value.to_primitive());
        self.set_variable(name, Value::ArrayType(variable));
    }

    pub fn execute_begin(&mut self) {
        for s in &self.program.begin {
            self.execute_one(s);
        }
    }

    pub fn execute_end(&mut self, should_run: bool) -> Option<i32> {
        let mut ret = None;
        if should_run {
            if let (v, ActionResult::Exit) = self.execute_in_action(&self.program.end) {
                ret = Some(v.to_int() as i32);
            }
        }
        for (_, s) in self.write_streams.iter_mut() {
            s.close();
        }
        ret
    }

    fn execute_getline(
        &mut self,
        var_name: String,
        record: &str,
        set_nf: bool,
        inc_nr: bool,
        inc_fnr: bool,
    ) {
        self.set_variable(var_name, Value::from_string(record.to_string()));

        if set_nf {
            // parse and set field variables
            let fs = self.get_variable("FS").to_regex();
            let mut i = 1;
            for val in fs.split(record) {
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
        }

        if inc_nr {
            self.set_variable("NR".to_string(), Self::add_int(self.get_variable("NR"), 1));
        }

        if inc_fnr {
            self.set_variable(
                "FNR".to_string(),
                Self::add_int(self.get_variable("FNR"), 1),
            );
        }
    }

    pub fn execute_actions(&mut self) -> Option<i32> {
        'main: loop {
            let record = self.next_record();
            if record.is_none() {
                break;
            }
            let record = record.unwrap();
            self.execute_getline("0".to_string(), &record, true, true, true);
            // execute actions
            let mut range_idx = 0;
            for (expressions, statements) in &self.program.actions {
                match expressions.len() {
                    0 => match self.execute_in_action(statements) {
                        (_, ActionResult::Next) => continue 'main,
                        (v, ActionResult::Exit) => return Some(v.to_int() as i32),
                        (_, ActionResult::None) => {}
                    },
                    1 => match self.evaluate(expressions.get(0).unwrap()) {
                        Value::Empty => match self.execute_in_action(statements) {
                            (_, ActionResult::Next) => continue 'main,
                            (v, ActionResult::Exit) => return Some(v.to_int() as i32),
                            (_, ActionResult::None) => {}
                        },
                        Value::PrimitiveType(PrimitiveType::Pattern(re)) => {
                            if re.is_match(record.as_str()) {
                                match self.execute_in_action(statements) {
                                    (_, ActionResult::Next) => continue 'main,
                                    (v, ActionResult::Exit) => return Some(v.to_int() as i32),
                                    (_, ActionResult::None) => {}
                                }
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
                                    if self.match_first(record.as_str(), &p).0 > 0 {
                                        self.ranges_flags[range_idx] = false;
                                        match self.execute_in_action(statements) {
                                            (_, ActionResult::Next) => continue 'main,
                                            (v, ActionResult::Exit) => {
                                                return Some(v.to_int() as i32)
                                            }
                                            (_, ActionResult::None) => {}
                                        }
                                    }
                                }
                                val => {
                                    if val.is_truthy() {
                                        match self.execute_in_action(statements) {
                                            (_, ActionResult::Next) => continue 'main,
                                            (v, ActionResult::Exit) => {
                                                return Some(v.to_int() as i32)
                                            }
                                            (_, ActionResult::None) => {}
                                        }
                                    }
                                }
                            }
                        } else {
                            // we're not in running range, test if should start
                            let mut cur_record = record.as_str();
                            match self.evaluate(expressions.get(0).unwrap()) {
                                Value::PrimitiveType(PrimitiveType::Pattern(p)) => {
                                    let (match_start, match_len) = self.match_first(cur_record, &p);
                                    if match_start > 0 {
                                        cur_record = &cur_record[(match_start + match_len - 1)..];
                                        self.ranges_flags[range_idx] = true;
                                        match self.execute_in_action(statements) {
                                            (_, ActionResult::Next) => continue 'main,
                                            (v, ActionResult::Exit) => {
                                                return Some(v.to_int() as i32)
                                            }
                                            (_, ActionResult::None) => {}
                                        }
                                        if self.match_first(cur_record, &p).0 > 0 {
                                            // range ends within record
                                            self.ranges_flags[range_idx] = false;
                                        }
                                    }
                                }
                                val => {
                                    if val.is_truthy() {
                                        match self.execute_in_action(statements) {
                                            (_, ActionResult::Next) => continue 'main,
                                            (v, ActionResult::Exit) => {
                                                return Some(v.to_int() as i32)
                                            }
                                            (_, ActionResult::None) => {}
                                        }
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
        None
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

    fn execute_in_action(&mut self, statements: &Vec<Statement<'a>>) -> (Value, ActionResult) {
        for s in statements {
            match self.execute_one(s) {
                Value::Empty => {}
                val => match s {
                    Statement::Next => return (val, ActionResult::Next),
                    Statement::Exit(_) => return (val, ActionResult::Exit),
                    Statement::Return(_) => return (val, ActionResult::None),
                    _ => unreachable!(),
                },
            }
            if statements.is_empty()
                || statements.len() == 1 && *statements.get(0).unwrap() == Statement::Empty
            {
                // default statement in actions is print current record
                self.execute_one(&Statement::Print(vec![]));
            }
        }
        (Value::Empty, ActionResult::None)
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
            Statement::Exit(e) => self.evaluate(e),
            Statement::Return(e) => self.evaluate(e),
            Statement::Next => Value::Empty,
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
                match expressions.len() {
                    0 => {
                        print!("{}", self.get_inner_print_string(&Expression::Empty));
                    }
                    1 => match expressions.get(0).unwrap() {
                        Expression::Binary(BinaryOperator::GreaterThan, lhs, rhs) => {
                            let record = &self.get_inner_print_string(lhs);
                            let filename = &self.evaluate(rhs).to_string();
                            self.add_write_stream_if_absent(filename, true, true)
                                .write(record);
                        }
                        Expression::Binary(BinaryOperator::Append, lhs, rhs) => {
                            let record = &self.get_inner_print_string(lhs);
                            let filename = &self.evaluate(rhs).to_string();
                            self.add_write_stream_if_absent(filename, true, false)
                                .write(record);
                        }
                        Expression::Binary(BinaryOperator::Pipe, lhs, rhs) => {
                            let record = &self.get_inner_print_string(lhs);
                            let command = &self.evaluate(rhs).to_string();
                            self.add_write_stream_if_absent(command, false, false)
                                .pipe(record);
                        }
                        e => print!("{}", self.get_inner_print_string(e)),
                    },
                    _ => {
                        print!("{}", self.get_print_string(expressions));
                    }
                };
                Value::Empty
            }
            Statement::Delete(e) => {
                match e {
                    Expression::Empty => panic!("nothing to delete"), // TODO: add line/position
                    Expression::Variable(name) => {
                        self.variables.remove(*name);
                    }
                    Expression::ArrayVariable(name, accessors) => {
                        let var = self.get_variable(name);
                        match var {
                            Value::ArrayType(mut v) => {
                                v.remove(&self.get_array_accessor(accessors));
                                self.set_variable(name.to_string(), Value::from_array(v));
                            }
                            _ => unreachable!(),
                        }
                    }
                    e => panic!("cannot delete expression that is not a variable: {:?}", e),
                }
                Value::Empty
            }
            _ => panic!("unexpected statement: {:?}", statement),
        }
    }

    fn get_inner_print_string(&mut self, expression: &Expression<'a>) -> String {
        match expression {
            Expression::Empty => {
                self.get_variable("0").to_string() + self.get_variable("ORS").to_string().as_str()
            }
            Expression::Grouping(expressions) => self.get_print_string(expressions),
            e => self.get_print_string(&vec![e.clone()]),
        }
    }

    fn get_print_string(&mut self, expressions: &Vec<Expression<'a>>) -> String {
        let ors = self.get_variable("ORS").to_string();
        let ofs = self.get_variable("OFS").to_string();
        let mut ret = String::from("");
        let mut need_sep = false;
        for e in expressions {
            if need_sep {
                ret.push_str(&ofs);
            }
            need_sep = true;
            ret.push_str(&self.evaluate(e).to_string());
        }
        ret.push_str(&ors);
        ret
    }

    fn evaluate(&mut self, expression: &Expression<'a>) -> Value {
        match expression {
            Expression::Empty => Value::Empty,
            Expression::Literal(val) => Value::from_primitive(val),
            Expression::Variable(name) => self.get_variable(name),
            Expression::ArrayVariable(name, expressions) => {
                self.get_array_variable(name, expressions)
            }
            Expression::FieldVariable(e) => {
                let name = self.evaluate(e).to_string();
                self.get_variable(&name)
            }
            Expression::Grouping(expressions) => {
                if expressions.len() > 1 {
                    // assume this is a check before the In operator
                    Value::from_string(self.get_array_accessor(expressions))
                } else {
                    self.evaluate(expressions.get(0).unwrap())
                }
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
                        let val = self.get_array_variable(name, &accessors);
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
                        let val = self.get_array_variable(name, &accessors);
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
                        let ret = self.get_array_variable(name, &accessors);
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
                        let ret = self.get_array_variable(name, &accessors);
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
                let lhs = match **lhs_expression {
                    Expression::Function("printf", _)
                        if *op == BinaryOperator::GreaterThan
                            || *op == BinaryOperator::Append
                            || *op == BinaryOperator::Pipe =>
                    {
                        Value::Empty
                    }
                    _ => self.evaluate(lhs_expression),
                };
                let rhs = match **rhs_expression {
                    Expression::Getline(_)
                        if *op == BinaryOperator::Pipe || *op == BinaryOperator::LessThan =>
                    {
                        Value::Empty
                    }
                    Expression::Function("printf", _)
                        if *op == BinaryOperator::GreaterThan
                            || *op == BinaryOperator::Append
                            || *op == BinaryOperator::Pipe =>
                    {
                        Value::Empty
                    }
                    _ => self.evaluate(rhs_expression),
                };
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
                        match &**lhs_expression {
                            Expression::Function("printf", args) => {
                                let record = &self.get_print_string(&args);
                                let filename = &self.evaluate(&rhs_expression).to_string();
                                self.add_write_stream_if_absent(filename, true, true)
                                    .write(record);
                            }
                            _ => {}
                        }

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
                                let mut var = match self.get_variable(name) {
                                    Value::ArrayType(val) => val,
                                    _ => {
                                        let val = Value::ArrayType(HashMap::new());
                                        self.set_variable(name.to_string(), val);
                                        HashMap::new()
                                    }
                                };
                                let prev_subsep = self.get_variable("SUBSEP");
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
                                    //       References to nonexistent fields (that is, fields after $NF),
                                    //       shall evaluate to the uninitialized value. Such references shall not create new fields.
                                    //
                                    // TODO:
                                    //       However, assigning to a nonexistent field (for example, $(NF+2)=5) shall
                                    //       increase the value of NF; create any intervening fields with the uninitialized value;
                                    //       and  cause the value of $0 to be recomputed, with the fields being separated by the value of OFS.
                                    //
                                    //       Each field variable shall have a string value or an uninitialized value when created.
                                    //       Field variables shall have the uninitialized value when created from $0 using FS and the variable does
                                    //       not contain any characters.
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
                    BinaryOperator::In => Value::from_int(match (lhs, rhs) {
                        (v, Value::ArrayType(val)) => {
                            if val.contains_key(&v.to_string()) {
                                1
                            } else {
                                0
                            }
                        }
                        _ => 0,
                    }),
                    BinaryOperator::Pipe => {
                        match (&**lhs_expression, &**rhs_expression) {
                            (Expression::Function("printf", args), _) => {
                                let command = self.get_print_string(&args);
                                self.add_write_stream_if_absent(&command, false, false)
                                    .pipe(&rhs.to_string());
                            }
                            (_, Expression::Getline(e)) => match &**e {
                                Expression::Empty => {
                                    let cmd_def = &lhs.to_string();
                                    if !self.add_command_read_stream_if_absent(cmd_def) {
                                        return Value::Empty;
                                    }
                                    let (record, _) = self.next_stream_record(cmd_def);
                                    self.execute_getline(
                                        "0".to_string(),
                                        record.as_str(),
                                        true,
                                        false,
                                        false,
                                    );
                                }
                                Expression::Variable(v) => {
                                    let cmd_def = &lhs.to_string();
                                    if !self.add_command_read_stream_if_absent(cmd_def) {
                                        return Value::Empty;
                                    }
                                    let (record, _) = self.next_stream_record(cmd_def);
                                    self.execute_getline(
                                        v.to_string(),
                                        record.as_str(),
                                        true,
                                        false,
                                        false,
                                    );
                                }
                                e => panic!("unexpected expression in pipe/getline: {:?}", e),
                            },
                            p => panic!("unexpected pipe between: {:?}", p),
                        };
                        Value::Empty
                    }
                    BinaryOperator::Append => match (&**lhs_expression, &**rhs_expression) {
                        (Expression::Function("printf", args), _) => {
                            let record = &self.get_print_string(args);
                            let filename = &self.evaluate(&rhs_expression).to_string();
                            self.add_write_stream_if_absent(filename, true, false)
                                .write(record);
                            Value::Empty
                        }
                        p => panic!("unexpected append between: {:?}", p),
                    },
                }
            }
            Expression::Getline(e) => {
                // it returns 1 if there was a record present,
                //            0 if end-of-file was encountered,
                //       and -1 if some error occurred (such as failure to open a file).
                let mut return_value = 0;
                let mut should_inc_counters = true;
                match &**e {
                    Expression::Empty => {
                        let record = match self.next_record() {
                            None => {
                                should_inc_counters = false;
                                self.last_record.clone()
                            }
                            Some(r) => {
                                return_value = 1;
                                r
                            }
                        };
                        self.execute_getline(
                            "0".to_string(),
                            &record,
                            true,
                            should_inc_counters,
                            should_inc_counters,
                        );
                    }
                    Expression::Variable(v) => {
                        let record = match self.next_record() {
                            None => {
                                should_inc_counters = false;
                                self.last_record.clone()
                            }
                            Some(r) => {
                                return_value = 1;
                                r
                            }
                        };
                        self.execute_getline(
                            v.to_string(),
                            &record,
                            false,
                            should_inc_counters,
                            should_inc_counters,
                        );
                    }
                    Expression::Binary(BinaryOperator::LessThan, lhs, rhs) => {
                        let filepath = self.evaluate(rhs).to_string();
                        match **lhs {
                            Expression::Empty => {
                                let (record, has_more) = self.next_stream_record(&filepath);
                                if self.read_streams.get(&filepath).is_some() {
                                    if has_more {
                                        return_value = 1;
                                    }
                                } else {
                                    return_value = -1;
                                };
                                self.execute_getline("0".to_string(), &record, true, false, false);
                            }
                            Expression::Variable(v) => {
                                let (record, has_more) = self.next_stream_record(&filepath);
                                if self.read_streams.get(&filepath).is_some() {
                                    if has_more {
                                        return_value = 1;
                                    }
                                } else {
                                    return_value = -1;
                                };
                                self.execute_getline(v.to_string(), &record, false, false, false);
                            }
                            _ => panic!("unexpected expression: {:?}", **lhs),
                        };
                    }
                    _ => panic!("unexpected expression: {:?}", **e),
                };
                Value::from_int(return_value)
            }
            Expression::Ternary(cond, truthy, falsy) => {
                if self.evaluate(cond).is_truthy() {
                    self.evaluate(truthy)
                } else {
                    self.evaluate(falsy)
                }
            }
            Expression::Function(name, args) => {
                let mut evaluated_args: Vec<Value> = if *name == "printf" {
                    vec![]
                } else {
                    args.iter().map(|a| self.evaluate(a)).collect()
                };
                match *name {
                    "atan2" => return crate::standard_functions::atan2(&evaluated_args),
                    "cos" => return crate::standard_functions::cos(&evaluated_args),
                    "sin" => return crate::standard_functions::sin(&evaluated_args),
                    "exp" => return crate::standard_functions::exp(&evaluated_args),
                    "log" => return crate::standard_functions::log(&evaluated_args),
                    "sqrt" => return crate::standard_functions::sqrt(&evaluated_args),
                    "int" => return crate::standard_functions::int(&evaluated_args),
                    "rand" => return crate::standard_functions::rand(&evaluated_args),
                    "srand" => return crate::standard_functions::srand(&evaluated_args),
                    "sub" => {
                        match args.get(2) {
                            None => {
                                evaluated_args.push(self.get_variable("0"));
                                let val = crate::standard_functions::sub(&evaluated_args);
                                self.set_variable("0".to_string(), val);
                            }
                            Some(e) => match e {
                                Expression::FieldVariable(v) => {
                                    let name = self.evaluate(v).to_string();
                                    let val = crate::standard_functions::sub(&evaluated_args);
                                    self.set_variable(name, val);
                                }
                                Expression::Variable(name) => {
                                    let val = crate::standard_functions::sub(&evaluated_args);
                                    self.set_variable(name.to_string(), val);
                                }
                                _ => return Value::Empty,
                            },
                        }
                        Value::Empty
                    }
                    "gsub" => {
                        match args.get(2) {
                            None => {
                                evaluated_args.push(self.get_variable("0"));
                                let val = crate::standard_functions::gsub(&evaluated_args);
                                self.set_variable("0".to_string(), val);
                            }
                            Some(e) => match e {
                                Expression::FieldVariable(v) => {
                                    let name = self.evaluate(v).to_string();
                                    let val = crate::standard_functions::gsub(&evaluated_args);
                                    self.set_variable(name, val);
                                }
                                Expression::Variable(name) => {
                                    let val = crate::standard_functions::gsub(&evaluated_args);
                                    self.set_variable(name.to_string(), val);
                                }
                                _ => return Value::Empty,
                            },
                        }
                        Value::Empty
                    }
                    "index" => return crate::standard_functions::index(&evaluated_args),
                    "length" => return crate::standard_functions::length(&evaluated_args),
                    // TODO:
                    // "match" => return crate::standard_functions::matchf(&evaluated_args),
                    // "split" => return crate::standard_functions::split(&evaluated_args),
                    "printf" => {
                        print!("{}", self.get_print_string(args));
                        Value::Empty
                    }
                    // "sprintf" => return crate::standard_functions::sprintf(&evaluated_args),
                    "substr" => return crate::standard_functions::substr(&evaluated_args),
                    "tolower" => return crate::standard_functions::tolower(&evaluated_args),
                    "toupper" => return crate::standard_functions::toupper(&evaluated_args),
                    // TODO:
                    // "close" => return crate::standard_functions::close(&evaluated_args),
                    // "system" => return crate::standard_functions::system(&evaluated_args),
                    _ => {
                        let function = self
                            .functions
                            .get(&name.to_string())
                            .expect(&("function ".to_string() + name + " does not exist."))
                            .clone();

                        let prev_scope = self.local_scope.clone();
                        self.local_scope = HashMap::new();

                        for i in 0..function.0.len() {
                            let val = evaluated_args.get(i).unwrap_or(&Value::Empty);
                            self.local_scope
                                .insert(function.0.get(i).unwrap().to_string(), val.clone());
                        }

                        let (ret, _) = self.execute_in_action(&function.1.to_vec());
                        self.local_scope = prev_scope;
                        ret
                    }
                }
            }
        }
    }

    fn add_write_stream_if_absent(
        &mut self,
        def: &str,
        is_file: bool,
        truncate: bool,
    ) -> &mut WriteStream {
        if !self.write_streams.contains_key(def) {
            self.write_streams.insert(
                def.to_string(),
                WriteStream::new(def.to_string(), is_file, truncate),
            );
        }
        self.write_streams.get_mut(def).unwrap()
    }

    fn add_command_read_stream_if_absent(&mut self, cmd_def: &str) -> bool {
        if !self.read_streams.contains_key(cmd_def) {
            let (cmd_name, args) = Self::get_command_args(cmd_def);
            let output = match Command::new(&cmd_name).args(args).output() {
                Ok(v) => v,
                Err(e) => {
                    println!("failed to execute `{}`, {}", &cmd_name, e);
                    return false;
                }
            };
            let output = std::str::from_utf8(&output.stdout).unwrap();
            self.read_streams.insert(
                cmd_def.to_string(),
                ReadStream {
                    reader: Box::new(BufReader::new(Cursor::new(output.to_string()))),
                    last_record: String::from(""),
                },
            );
        }
        true
    }

    pub fn get_command_args(cmd: &str) -> (String, Vec<String>) {
        let mut tokens = vec![];
        let mut span_start = 0;
        let mut i = 0;
        let mut quote: Option<char> = None;
        while cmd.chars().nth(i).is_some() {
            match cmd.chars().nth(i).unwrap() {
                c if c.is_whitespace() => {
                    if span_start < i {
                        tokens.push(cmd[span_start..i].to_string());
                    }
                    span_start = i + 1;
                }
                c if (c == '"' || c == '\'') && quote.is_some() && quote.unwrap() == c => {
                    tokens.push(cmd[span_start..i].to_string());
                    span_start = i + 1;
                    quote = None;
                }
                c if (c == '\"' || c == '\'') && quote.is_none() => {
                    if span_start < i {
                        tokens.push(cmd[span_start..i].to_string());
                    }
                    span_start = i;
                    quote = Some(c);
                }
                c if c == '\\' => {
                    // this is an escape, so ignore next char
                    i += 1;
                }
                _ => {}
            }
            i += 1;
        }
        if span_start < i {
            tokens.push(cmd[span_start..i].to_string());
        }

        (
            tokens.first().expect("command string is empty").to_string(),
            tokens[1..].to_owned(),
        )
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

    fn get_variable(&self, name: &str) -> Value {
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
        ret.insert("ARGC".to_string(), Value::from_int(0));
        ret.insert("ARGV".to_string(), Value::Empty);
        ret.insert(
            "CONVFMT".to_string(),
            Value::from_string("%.6g".to_string()),
        );
        ret.insert("FILENAME".to_string(), Value::Empty);
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

impl ReadStream {
    pub fn new(filepath: &String) -> (Self, bool) {
        let file = match File::open(filepath) {
            Ok(f) => f,
            Err(_) => {
                return (
                    Self {
                        reader: Box::new(Cursor::new("".to_string())),
                        last_record: String::from(""),
                    },
                    false,
                )
            }
        };
        (
            Self {
                reader: Box::new(BufReader::new(file)),
                last_record: String::from(""),
            },
            true,
        )
    }
}

impl WriteStream {
    pub fn new(def: String, is_file: bool, truncate: bool) -> Self {
        if is_file {
            Self {
                filepath: def,
                command: String::from(""),
                input: String::from(""),
                truncate,
            }
        } else {
            Self {
                filepath: String::from(""),
                command: def,
                input: String::from(""),
                truncate,
            }
        }
    }

    pub fn write(&mut self, s: &str) {
        if !self.filepath.is_empty() {
            self.input.push_str(s);
        }
    }

    pub fn pipe(&mut self, s: &str) {
        if !self.command.is_empty() {
            self.input.push_str(s);
        }
    }

    pub fn close(&mut self) {
        // commands are executed on close
        if !self.command.is_empty() {
            let (cmd_name, args) = Environment::get_command_args(&self.command);
            let mut cmd = Command::new(cmd_name)
                .args(args)
                .stdin(Stdio::piped())
                .spawn()
                .unwrap();
            cmd.stdin
                .as_mut()
                .unwrap()
                .write_all(self.input.as_bytes())
                .unwrap();
        }
        if !self.filepath.is_empty() {
            let file = File::options()
                .create(true)
                .write(true)
                .truncate(self.truncate)
                .append(!self.truncate)
                .open(&self.filepath)
                .unwrap();
            let mut writer = BufWriter::new(file);
            writer.write_all(self.input.as_bytes()).unwrap();
            writer.flush().unwrap();
            self.input = String::from("");
        }
    }
}
