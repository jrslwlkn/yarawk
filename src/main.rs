use crate::executor::{Environment, Value};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::PrimitiveType;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;

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
                        if source.is_empty() {
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
                                    .unwrap_or_else(|_| panic!("unable to open file: {}", val))
                                    .read_to_string(&mut buf)
                                    .unwrap_or_else(|_| panic!("unable to read file: {}", val));
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

    let mut env = Environment::new(&argv, &prog);

    for (name, val) in preset_vars {
        env.set_variable(name.to_string(), Value::from_string(val.to_string()));
    }
    let mut argv_var = HashMap::<String, PrimitiveType>::new();
    argv_var.insert(
        format!("{}", 0),
        PrimitiveType::String("yarawk".to_string()),
    );
    for (i, val) in argv.iter().enumerate() {
        argv_var.insert(format!("{}", i + 1), PrimitiveType::String(val.to_string()));
    }

    env.set_variable("ARGC".to_string(), Value::from_int(argv.len() as i64 + 1));
    env.set_variable("ARGV".to_string(), Value::from_array(argv_var));

    let mut code = 0;
    env.execute_begin();
    let mut result = env.execute_actions();
    if let Some(r) = result {
        code = r;
    }
    result = env.execute_end(result.is_none());
    if let Some(r) = result {
        code = r;
    }

    std::process::exit(code)
}

fn get_kv_pair(input: &str) -> Option<(&str, &str)> {
    input
        .find('=')
        .map(|i| (&input[0..i], &input[i + 1..input.len()]))
}
