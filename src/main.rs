use crate::executor::{Environment, Value};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::PrimitiveType;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};

pub mod executor;
pub mod lexer;
pub mod parser;
pub mod parser_helpers;
pub mod token;

fn main() {
    let args: Vec<String> = env::args().collect();
    // for a in &args {
    //     println!("-> {}", a);
    // }

    let mut source = String::from("");
    let mut preset_vars = Vec::<(&str, &str)>::new();
    let mut argv: Vec<&str> = Vec::new();
    match &args.len() {
        0 | 1 => panic!("usage: yarawk [-F fs] [-v var=value] [-f progfile | 'prog'] [file ...]"),
        _ => {
            let mut i = 1;
            loop {
                match (args.get(i), args.get(i + 1)) {
                    (None, _) => break,
                    (Some(val), None) => {
                        if source == "" {
                            source = val.clone();
                        } else {
                            argv.push(val);
                        }
                        break;
                    }
                    (Some(a1), Some(a2)) => {
                        match (a1.as_str(), a2.as_str()) {
                            ("-F", val) => {
                                preset_vars.push(("FS", val));
                                i += 2;
                            }
                            ("-f", val) => {
                                let mut buf = String::new();
                                File::open(val)
                                    .expect(format!("unable to open file: {}", val).as_str())
                                    .read_to_string(&mut buf)
                                    .expect(format!("unable to read file: {}", val).as_str());
                                source.push_str(buf.as_str());
                                i += 2;
                            }
                            ("-v", val) => {
                                match get_kv_pair(val) {
                                    None => panic!("expected a key-value pair, received: {}", val),
                                    Some((k, v)) => preset_vars.push((k, v)),
                                };
                                i += 2;
                            }
                            (f1, f2) => {
                                argv.push(f1);
                                argv.push(f2);
                                i += 2;
                            }
                        };
                    }
                }
            }
        }
    }

    let mut lexer = Lexer::new(&source);
    // println!("source: {:#?}", source);
    let tokens = lexer.lex();
    // println!("tokens: {:#?}", tokens);
    let mut parser = Parser::new(&tokens);
    let prog = parser.parse();
    // println!("{:#?}", prog);

    let mut env = Environment::new(&prog);
    for (name, val) in preset_vars {
        env.set_variable(name.to_string(), Value::from_string(val.to_string()));
    }
    env.set_variable("ARGC".to_string(), Value::from_int(argv.len() as i64 + 1));
    let mut argv_var = HashMap::<String, PrimitiveType>::new();
    argv_var.insert(
        format!("{}", 0),
        PrimitiveType::String("yarawk".to_string()),
    );
    for (i, val) in argv.iter().enumerate() {
        argv_var.insert(format!("{}", i + 1), PrimitiveType::String(val.to_string()));
    }
    env.set_variable("ARGV".to_string(), Value::from_array(argv_var));

    env.execute_begin();

    for filepath in argv {
        let file =
            File::open(filepath).expect(format!("unable to open file: {}", filepath).as_str());
        env.set_variable(
            "FILENAME".to_string(),
            Value::from_string(filepath.to_string()),
        );
        let mut reader = BufReader::new(file);
        loop {
            let mut record = vec![];
            match reader.read_until(env.get_rs(), &mut record) {
                Err(_) => {
                    panic!("unable to read file: {}", filepath);
                }
                Ok(v) if v == 0 => break,
                Ok(_) => {
                    record.pop(); // remove record separator at the end
                    env.execute_actions(
                        std::str::from_utf8(record.as_slice())
                            .unwrap_or(format!("unable to read file: {}", filepath).as_str())
                            .trim(),
                    );
                }
            }
        }
    }

    env.execute_end();
}

fn get_kv_pair(input: &str) -> Option<(&str, &str)> {
    match input.find('=') {
        None => None,
        Some(i) => Some((&input[0..i], &input[i + 1..input.len()])),
    }
}
