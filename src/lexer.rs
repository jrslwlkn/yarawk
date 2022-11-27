use std::str::FromStr;

use regex::Regex;

use crate::token::{PrimitiveType, Token, TokenType};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a String,
    chars: Vec<char>,
    pos: usize,
    row: usize,
    col: usize,
    tokens: Vec<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a String) -> Self {
        Self {
            input,
            chars: input.chars().collect(),
            pos: 0,
            row: 1,
            col: 1,
            tokens: Vec::<Token>::new(),
        }
    }

    fn peek(&self, value: &str) -> bool {
        if self.pos + value.len() > self.input.len() {
            return false;
        }
        &self.input[self.pos..self.pos + value.len()] == value
    }

    fn peek_ws(&self, value: &str) -> bool {
        if self.pos + value.len() > self.input.len() {
            return false;
        }
        match self.chars.get(self.pos + value.len()) {
            None => {}
            Some(c) if c.is_whitespace() => {}
            Some(c) => match *c {
                ';' | '(' | '{' => {}
                _ => return false,
            },
        }
        &self.input[self.pos..self.pos + value.len()] == value
    }

    fn peek_next(&self) -> char {
        let val = self.chars.get(self.pos + 1);
        match val {
            Some(ret) => ret.to_owned(),
            None => '\0',
        }
    }

    fn advance(&mut self, len: usize, newrow: bool) {
        if newrow {
            self.row += 1;
            self.col = 1;
        } else {
            self.col += len;
        }
        self.pos += len;
    }

    fn emit(&mut self, value: TokenType<'a>, col: usize, len: usize) {
        self.tokens.push(Token::new(value, self.row, col));
        self.advance(len, false);
    }

    pub fn lex(&mut self) -> Vec<Token> {
        'main: loop {
            if self.pos >= self.input.len() {
                break 'main;
            }
            let cur = self.chars.get(self.pos).unwrap().to_owned();
            match cur {
                '\\' if self.peek_next() == '\n' => {
                    self.advance(2, true);
                }
                '{' => self.emit(TokenType::LeftCurly, self.col, 1),
                '}' => self.emit(TokenType::RightCurly, self.col, 1),
                '(' => self.emit(TokenType::LeftParen, self.col, 1),
                ')' => self.emit(TokenType::RightParen, self.col, 1),
                '[' => self.emit(TokenType::LeftBracket, self.col, 1),
                ']' => self.emit(TokenType::RightBracket, self.col, 1),
                ';' => {
                    if !self.tokens.is_empty()
                        && self.tokens.last().unwrap().value != TokenType::Semicolon
                    {
                        self.emit(TokenType::Semicolon, self.col, 1);
                    } else {
                        self.advance(1, false);
                    }
                }
                '?' => self.emit(TokenType::Question, self.col, 1),
                ':' => self.emit(TokenType::Colon, self.col, 1),
                '+' if self.peek("++") => self.emit(TokenType::PlusPlus, self.col, 2),
                '+' => self.emit(TokenType::Plus, self.col, 1),
                '-' if self.peek("--") => self.emit(TokenType::MinusMinus, self.col, 2),
                '-' => self.emit(TokenType::Minus, self.col, 1),
                '=' if self.peek("==") => self.emit(TokenType::EqualEqual, self.col, 2),
                '=' => self.emit(TokenType::Equal, self.col, 1),
                '!' if self.peek("!=") => self.emit(TokenType::NotEqual, self.col, 2),
                '!' if self.peek("!~") => self.emit(TokenType::NotTilde, self.col, 2),
                '!' => self.emit(TokenType::Not, self.col, 1),
                '~' => self.emit(TokenType::Tilde, self.col, 1),
                '*' if self.peek("*=") => self.emit(TokenType::StarEqual, self.col, 2),
                '*' => self.emit(TokenType::Star, self.col, 1),
                '%' if self.peek("%=") => self.emit(TokenType::PercentEqual, self.col, 2),
                '%' => self.emit(TokenType::Percent, self.col, 1),
                '^' if self.peek("^=") => self.emit(TokenType::CarrotEqual, self.col, 2),
                '^' => self.emit(TokenType::Carrot, self.col, 1),
                '$' => self.emit(TokenType::Dollar, self.col, 1),
                '/' if self.peek("/=") && !self.peek("/==") => {
                    self.emit(TokenType::SlashEqual, self.col, 2)
                }
                '/' => {
                    // when matched first Slash, go till the end of the line
                    let mut pair_idx = -1;
                    let mut cur_pos = self.pos + 1;
                    let mut newlines = 0;
                    let mut cur_col = 1;
                    loop {
                        match self.chars.get(cur_pos) {
                            None => break,
                            Some(val) => match val {
                                '\\' if !self.chars.get(cur_pos + 1).is_none()
                                    && *self.chars.get(cur_pos + 1).unwrap() == '\n' =>
                                {
                                    cur_pos += 2;
                                    newlines += 1;
                                    cur_col = 1;
                                }
                                '\\' if !self.chars.get(cur_pos + 1).is_none()
                                    && *self.chars.get(cur_pos + 1).unwrap() == '/' =>
                                {
                                    cur_pos += 2;
                                    cur_col += 2;
                                }
                                '\n' => {
                                    cur_pos += 1;
                                    cur_col = 1;
                                    newlines += 1;
                                    break;
                                }
                                '/' => {
                                    pair_idx = cur_pos as i64;
                                    cur_pos += 1;
                                    cur_col += 1;
                                    break;
                                }
                                _ => {
                                    cur_pos += 1;
                                    cur_col += 1;
                                }
                            },
                        }
                    }
                    if pair_idx > 0 {
                        // if found another unescaped Slash, try following to determine if this is Pattern:
                        // before: LeftParen, Equal, EqualEqual, NotEqual, Tilde, NotTilde, Comma, Nothing
                        match self.tokens.last() {
                            None => {
                                // remove line splits within the regex
                                let val =
                                    &self.input[self.pos + 1..cur_pos - 1].replace("\\\n", "");
                                self.emit(
                                    TokenType::Literal(PrimitiveType::Pattern(
                                        Regex::from_str(val).unwrap(),
                                    )),
                                    self.col,
                                    pair_idx as usize - self.pos + 1,
                                );
                                self.row += newlines;
                                self.col = if newlines > 0 { cur_col } else { cur_col + 1 };
                            }
                            Some(t) => match t.value {
                                TokenType::LeftParen
                                | TokenType::Comma
                                | TokenType::Equal
                                | TokenType::EqualEqual
                                | TokenType::NotEqual
                                | TokenType::Tilde
                                | TokenType::NotTilde => {
                                    self.emit(
                                        TokenType::Literal(PrimitiveType::Pattern(
                                            Regex::from_str(&self.input[self.pos + 1..cur_pos - 1])
                                                .unwrap(),
                                        )),
                                        self.col,
                                        pair_idx as usize - self.pos + 1,
                                    );
                                    self.row += newlines;
                                    self.col = if newlines > 0 { cur_col } else { cur_col + 1 };
                                }
                                _ => self.emit(TokenType::Slash, self.col, 1),
                            },
                        }
                    } else {
                        // this is Slash
                        self.emit(TokenType::Slash, self.col, 1);
                    }
                }
                '<' if self.peek("<=") => self.emit(TokenType::LessEqual, self.col, 2),
                '<' => self.emit(TokenType::LessThan, self.col, 1),
                '>' if self.peek(">>") => self.emit(TokenType::RightRight, self.col, 2),
                '>' if self.peek(">=") => self.emit(TokenType::GreaterEqual, self.col, 2),
                '>' => self.emit(TokenType::GreaterThan, self.col, 1),
                '&' if self.peek("&&") => self.emit(TokenType::And, self.col, 2),
                '|' if self.peek("||") => self.emit(TokenType::Or, self.col, 2),
                '|' => self.emit(TokenType::Pipe, self.col, 1),
                'B' if self.peek_ws("BEGIN") => self.emit(TokenType::Begin, self.col, 5),
                'E' if self.peek_ws("END") => self.emit(TokenType::End, self.col, 3),
                'd' if self.peek_ws("delete") => self.emit(TokenType::Delete, self.col, 6),
                'e' if self.peek_ws("exit") => self.emit(TokenType::Exit, self.col, 4),
                'f' if self.peek_ws("function") => self.emit(TokenType::Function, self.col, 8),
                'g' if self.peek_ws("getline") => self.emit(TokenType::Getline, self.col, 7),
                'i' if self.peek_ws("if") => self.emit(TokenType::If, self.col, 2),
                'e' if self.peek_ws("else") => self.emit(TokenType::Else, self.col, 4),
                'n' if self.peek_ws("next") => self.emit(TokenType::Next, self.col, 4),
                'b' if self.peek_ws("break") => self.emit(TokenType::Break, self.col, 5),
                'c' if self.peek_ws("continue") => self.emit(TokenType::Continue, self.col, 8),
                'r' if self.peek_ws("return") => self.emit(TokenType::Return, self.col, 6),
                'd' if self.peek_ws("do") => self.emit(TokenType::Do, self.col, 2),
                'w' if self.peek_ws("while") => self.emit(TokenType::While, self.col, 5),
                'f' if self.peek_ws("for") => self.emit(TokenType::For, self.col, 3),
                'i' if self.peek_ws("in") => self.emit(TokenType::In, self.col, 2),
                'p' if self.peek_ws("printf") => self.emit(TokenType::Printf, self.col, 6),
                'p' if self.peek_ws("print") => self.emit(TokenType::Print, self.col, 5),
                '\n' => {
                    match self.tokens.last() {
                        Some(t) => match t.value {
                            TokenType::Semicolon
                            | TokenType::Comma
                            | TokenType::LeftCurly
                            | TokenType::Question
                            | TokenType::Colon
                            | TokenType::Or
                            | TokenType::And
                            | TokenType::Do
                            | TokenType::Else => {}
                            _ => {
                                self.emit(TokenType::Newline, self.col, 1);
                                self.advance(0, true);
                                continue 'main;
                            }
                        },
                        _ => {}
                    }
                    self.advance(1, true);
                }
                '#' => {
                    // ignore comments
                    loop {
                        match self.peek_next() {
                            '\n' => {
                                self.advance(2, true);
                                break;
                            }
                            _ => self.advance(1, false),
                        }
                    }
                }
                '\'' | '\"' => {
                    // string literals
                    let quote = cur.to_owned();
                    let start = self.pos + 1;
                    let col = self.col;
                    loop {
                        let next_char = self.peek_next();
                        match next_char {
                            _ if next_char == quote || next_char == '\0' => {
                                self.emit(
                                    TokenType::Literal(PrimitiveType::String(
                                        self.input[start..=self.pos].to_string(),
                                    )),
                                    col,
                                    1,
                                );
                                self.advance(1, false);
                                break;
                            }
                            _ => self.advance(1, false),
                        }
                    }
                }
                '_' | 'a'..='z' | 'A'..='Z' => {
                    let start = self.pos;
                    let col = self.col;
                    loop {
                        match self.peek_next() {
                            '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' => {
                                self.advance(1, false);
                            }
                            _ => {
                                self.emit(
                                    TokenType::Identifier(&self.input[start..=self.pos]),
                                    col,
                                    0,
                                );
                                self.advance(1, false);
                                break;
                            }
                        }
                    }
                }
                '0'..='9' => {
                    let start = self.pos;
                    let col = self.col;
                    let mut had_dot = cur == '.';
                    loop {
                        let next_digit = self.peek_next();
                        match next_digit {
                            '.' if !had_dot => {
                                had_dot = true;
                                self.advance(1, false);
                            }
                            '0'..='9' => {
                                self.advance(1, false);
                            }
                            _ => {
                                if had_dot {
                                    let val = self.input[start..=self.pos].parse::<f64>().unwrap();
                                    self.emit(
                                        TokenType::Literal(PrimitiveType::Float(val)),
                                        col,
                                        1,
                                    );
                                } else {
                                    let val = self.input[start..=self.pos].parse::<i64>().unwrap();
                                    self.emit(
                                        TokenType::Literal(PrimitiveType::Integer(val)),
                                        col,
                                        1,
                                    );
                                }
                                break;
                            }
                        }
                    }
                }
                ',' => self.emit(TokenType::Comma, self.col, 1),
                _ => self.advance(1, false),
            }
        }
        self.emit(TokenType::Eof, self.col, 0);
        self.tokens.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample() {
        let source = r#"BEGIN {
    print "hello world"
}"#
        .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(TokenType::Begin, 1, 1),
            Token::new(TokenType::LeftCurly, 1, 7),
            Token::new(TokenType::Print, 2, 5),
            Token::new(
                TokenType::Literal(PrimitiveType::String("hello world".to_string())),
                2,
                11,
            ),
            Token::new(TokenType::Newline, 2, 24),
            Token::new(TokenType::RightCurly, 3, 1),
            Token::new(TokenType::Eof, 3, 2),
        ];
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn function() {
        let source = r#"# Returns minimum number
function find_min(num1, num2) {
    if (num1 < num2)
        return num1
    return num2;
}"#
        .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(TokenType::Function, 2, 1),
            Token::new(TokenType::Identifier("find_min"), 2, 10),
            Token::new(TokenType::LeftParen, 2, 18),
            Token::new(TokenType::Identifier("num1"), 2, 19),
            Token::new(TokenType::Comma, 2, 23),
            Token::new(TokenType::Identifier("num2"), 2, 25),
            Token::new(TokenType::RightParen, 2, 29),
            Token::new(TokenType::LeftCurly, 2, 31),
            Token::new(TokenType::If, 3, 5),
            Token::new(TokenType::LeftParen, 3, 8),
            Token::new(TokenType::Identifier("num1"), 3, 9),
            Token::new(TokenType::LessThan, 3, 14),
            Token::new(TokenType::Identifier("num2"), 3, 16),
            Token::new(TokenType::RightParen, 3, 20),
            Token::new(TokenType::Newline, 3, 21),
            Token::new(TokenType::Return, 4, 9),
            Token::new(TokenType::Identifier("num1"), 4, 16),
            Token::new(TokenType::Newline, 4, 20),
            Token::new(TokenType::Return, 5, 5),
            Token::new(TokenType::Identifier("num2"), 5, 12),
            Token::new(TokenType::Semicolon, 5, 16),
            Token::new(TokenType::RightCurly, 6, 1),
            Token::new(TokenType::Eof, 6, 2),
        ];
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn statement1() {
        let source = r#"#
result = find_min(10, 0.2)"#
            .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(TokenType::Identifier("result"), 2, 1),
            Token::new(TokenType::Equal, 2, 8),
            Token::new(TokenType::Identifier("find_min"), 2, 10),
            Token::new(TokenType::LeftParen, 2, 18),
            Token::new(TokenType::Literal(PrimitiveType::Integer(10)), 2, 19),
            Token::new(TokenType::Comma, 2, 21),
            Token::new(TokenType::Literal(PrimitiveType::Float(0.2)), 2, 23),
            Token::new(TokenType::RightParen, 2, 26),
            Token::new(TokenType::Eof, 2, 27),
        ];
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn statement2() {
        let source = r#"#
db[n, "return"] = st"#
            .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(TokenType::Identifier("db"), 2, 1),
            Token::new(TokenType::LeftBracket, 2, 3),
            Token::new(TokenType::Identifier("n"), 2, 4),
            Token::new(TokenType::Comma, 2, 5),
            Token::new(
                TokenType::Literal(PrimitiveType::String("return".to_string())),
                2,
                7,
            ),
            Token::new(TokenType::RightBracket, 2, 15),
            Token::new(TokenType::Equal, 2, 17),
            Token::new(TokenType::Identifier("st"), 2, 19),
            Token::new(TokenType::Eof, 2, 21),
        ];
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn statement3() {
        let source = r#"#
a++ && b-- || c != d"#
            .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(TokenType::Identifier("a"), 2, 1),
            Token::new(TokenType::PlusPlus, 2, 2),
            Token::new(TokenType::And, 2, 5),
            Token::new(TokenType::Identifier("b"), 2, 8),
            Token::new(TokenType::MinusMinus, 2, 9),
            Token::new(TokenType::Or, 2, 12),
            Token::new(TokenType::Identifier("c"), 2, 15),
            Token::new(TokenType::NotEqual, 2, 17),
            Token::new(TokenType::Identifier("d"), 2, 20),
            Token::new(TokenType::Eof, 2, 21),
        ];
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn statement4() {
        let source = r#"#
while (1) {
    break
    continue;
}"#
        .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(TokenType::While, 2, 1),
            Token::new(TokenType::LeftParen, 2, 7),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 2, 8),
            Token::new(TokenType::RightParen, 2, 9),
            Token::new(TokenType::LeftCurly, 2, 11),
            Token::new(TokenType::Break, 3, 5),
            Token::new(TokenType::Newline, 3, 10),
            Token::new(TokenType::Continue, 4, 5),
            Token::new(TokenType::Semicolon, 4, 13),
            Token::new(TokenType::RightCurly, 5, 1),
            Token::new(TokenType::Eof, 5, 2),
        ];
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn re_vs_div() {
        let source = r#"#
/hello/ 2/4"#
            .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(
                TokenType::Literal(PrimitiveType::Pattern(Regex::new("hello").unwrap())),
                2,
                1,
            ),
            Token::new(TokenType::Literal(PrimitiveType::Integer(2)), 2, 9),
            Token::new(TokenType::Slash, 2, 10),
            Token::new(TokenType::Literal(PrimitiveType::Integer(4)), 2, 11),
            Token::new(TokenType::Eof, 2, 12),
        ];
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn line_cont_regex() {
        let source = r#"/a\
 b\
 c/"#
        .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(
                TokenType::Literal(PrimitiveType::Pattern(Regex::new("a b c").unwrap())),
                1,
                1,
            ),
            Token::new(TokenType::Eof, 3, 4),
        ];
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn line_cont_expr() {
        let source = r#"function a( \
\
)"#
        .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(TokenType::Function, 1, 1),
            Token::new(TokenType::Identifier("a"), 1, 10),
            Token::new(TokenType::LeftParen, 1, 11),
            Token::new(TokenType::RightParen, 3, 1),
            Token::new(TokenType::Eof, 3, 2),
        ];
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn ternary_expr() {
        let source = r#"
print 1 > 0 ? "yes" : "no"
"#
        .to_string();
        let mut lexer = Lexer::new(&source);
        let lhs = lexer.lex();
        let rhs = vec![
            Token::new(TokenType::Print, 2, 1),
            Token::new(TokenType::Literal(PrimitiveType::Integer(1)), 2, 7),
            Token::new(TokenType::GreaterThan, 2, 9),
            Token::new(TokenType::Literal(PrimitiveType::Integer(0)), 2, 11),
            Token::new(TokenType::Question, 2, 13),
            Token::new(
                TokenType::Literal(PrimitiveType::String("yes".to_string())),
                2,
                15,
            ),
            Token::new(TokenType::Colon, 2, 21),
            Token::new(
                TokenType::Literal(PrimitiveType::String("no".to_string())),
                2,
                23,
            ),
            Token::new(TokenType::Newline, 2, 27),
            Token::new(TokenType::Eof, 3, 1),
        ];
        assert_eq!(lhs, rhs);
    }
}
