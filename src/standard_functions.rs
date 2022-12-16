use crate::executor::Value;
use rand::{Rng, SeedableRng};
use std::{
    cmp,
    time::{Duration, SystemTime},
};

fn ensure_args_count(name: &str, args: &[Value], min_count: i64, max_count: i64) -> bool {
    if (args.len() as i64) < min_count || (max_count > 0 && max_count < (args.len() as i64)) {
        println!("function {} does not take {} parameters", name, args.len());
        return false;
    }
    true
}

pub fn atan2(args: &[Value]) -> Value {
    //  Return arctangent of y/x in radians in the range [-n,n].
    ensure_args_count("atan2", args, 2, 2);
    Value::from_float(args[0].to_float().atan2(args[1].to_float()))
}

pub fn cos(args: &[Value]) -> Value {
    // Return cosine of x, where x is in radians.
    ensure_args_count("cos", args, 1, -1);
    Value::from_float(args[0].to_float().cos())
}

pub fn sin(args: &[Value]) -> Value {
    // Return sine of x, where x is in radians.
    ensure_args_count("sin", args, 1, -1);
    Value::from_float(args[0].to_float().sin())
}

pub fn exp(args: &[Value]) -> Value {
    // Return the exponential function of x.
    ensure_args_count("exp", args, 1, -1);
    Value::from_float(args[0].to_float().exp())
}

pub fn log(args: &[Value]) -> Value {
    // Return the natural logarithm of x.
    ensure_args_count("log", args, 1, -1);
    Value::from_float(args[0].to_float().ln())
}

pub fn sqrt(args: &[Value]) -> Value {
    // Return the square root of x.
    ensure_args_count("sqrt", args, 1, -1);
    Value::from_float(args[0].to_float().sqrt())
}

pub fn int(args: &[Value]) -> Value {
    // Return the argument truncated to an integer. Truncation shall be toward 0 when x>0.
    ensure_args_count("int", args, 1, -1);
    Value::from_int(args[0].to_int())
}

pub fn rand(args: &[Value]) -> Value {
    // Return a random number n, such that 0<=n<1.
    ensure_args_count("rand", args, -1, -1);
    Value::from_float(rand::Rng::gen(&mut rand::thread_rng()))
}

pub fn srand(args: &[Value]) -> Value {
    //  Set the seed value for rand to expr or use the time of day if expr is omitted. The previous seed value shall be returned.
    ensure_args_count("srand", args, 0, 1);
    let ret = rand::rngs::StdRng::seed_from_u64(
        args.get(0)
            .unwrap_or(&Value::from_int(
                SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap_or(Duration::from_secs(0))
                    .as_secs() as i64,
            ))
            .to_float()
            .to_bits(),
    )
    .gen();
    Value::from_int(ret)
}

pub fn substr(args: &[Value]) -> Value {
    // Return the at most n-character substring of s that begins at position m, numbering from 1.
    ensure_args_count("substr", args, 2, -1);
    let s = args[0].to_string();
    let m = (args[1].to_int() - 1) as usize;
    let n = (args[2].to_int() - 1) as usize;
    let ret = &s
        .get(
            m..=(if n >= 0 {
                cmp::min(m + n, s.len() - 1)
            } else {
                s.len() - 1
            }),
        )
        .unwrap_or("");
    Value::from_string(ret.to_string())
}

pub fn index(args: &[Value]) -> Value {
    //  Return  the  position, in characters, numbering from 1, in string s where string t first occurs,
    // or zero if it does not occur at all.
    ensure_args_count("index", args, 2, -1);
    let s = args[0].to_string();
    let t = args[1].to_string();
    Value::from_int((s.find(&t).unwrap_or(0) + 1) as i64)
}

pub fn length(args: &[Value]) -> Value {
    ensure_args_count("length", args, -1, -1);
    Value::from_int(args[0].to_string().len() as i64)
}

pub fn tolower(args: &[Value]) -> Value {
    ensure_args_count("tolower", args, -1, -1);
    Value::from_string(args[0].to_string().to_lowercase())
}

pub fn toupper(args: &[Value]) -> Value {
    ensure_args_count("toupper", args, -1, -1);
    Value::from_string(args[0].to_string().to_uppercase())
}
