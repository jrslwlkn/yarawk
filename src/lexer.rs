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

    fn add(&mut self, value: TokenType<'a>, col: usize, len: usize) {
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
                '{' => self.add(TokenType::LeftCurly, self.col, 1),
                '}' => self.add(TokenType::RightCurly, self.col, 1),
                '(' => self.add(TokenType::LeftParen, self.col, 1),
                ')' => self.add(TokenType::RightParen, self.col, 1),
                '[' => self.add(TokenType::LeftBracket, self.col, 1),
                ']' => self.add(TokenType::RightBracket, self.col, 1),
                ';' => self.add(TokenType::Semicolon, self.col, 1),
                ':' => self.add(TokenType::Colon, self.col, 1),
                '+' if self.peek("++") => self.add(TokenType::PlusPlus, self.col, 2),
                '+' => self.add(TokenType::Plus, self.col, 1),
                '-' if self.peek("--") => self.add(TokenType::MinusMinus, self.col, 2),
                '-' => self.add(TokenType::Minus, self.col, 1),
                '=' if self.peek("==") => self.add(TokenType::EqualEqual, self.col, 2),
                '=' => self.add(TokenType::Equal, self.col, 1),
                '!' if self.peek("!=") => self.add(TokenType::NotEqual, self.col, 2),
                '!' if self.peek("!~") => self.add(TokenType::NotTilde, self.col, 2),
                '!' => self.add(TokenType::Not, self.col, 1),
                '~' => self.add(TokenType::Tilde, self.col, 1),
                '*' => self.add(TokenType::Star, self.col, 1),
                '/' => self.add(TokenType::Slash, self.col, 1),
                '<' if self.peek("<=") => self.add(TokenType::LessEqual, self.col, 2),
                '<' => self.add(TokenType::LessThan, self.col, 1),
                '>' if self.peek(">=") => self.add(TokenType::GreaterEqual, self.col, 2),
                '>' => self.add(TokenType::GreaterThan, self.col, 1),
                '&' if self.peek("&&") => self.add(TokenType::And, self.col, 2),
                '|' if self.peek("||") => self.add(TokenType::Or, self.col, 2),
                'B' if self.peek("BEGIN") => self.add(TokenType::Begin, self.col, 5),
                'E' if self.peek("END") => self.add(TokenType::End, self.col, 3),
                'f' if self.peek("function") => self.add(TokenType::Function, self.col, 8),
                'i' if self.peek("if") => self.add(TokenType::If, self.col, 2),
                'e' if self.peek("else") => self.add(TokenType::Else, self.col, 4),
                'b' if self.peek("break") => self.add(TokenType::Break, self.col, 5),
                'c' if self.peek("continue") => self.add(TokenType::Continue, self.col, 8),
                'r' if self.peek("return") => self.add(TokenType::Return, self.col, 6),
                'w' if self.peek("while") => self.add(TokenType::While, self.col, 5),
                'f' if self.peek("for") => self.add(TokenType::For, self.col, 3),
                'i' if self.peek("in") => self.add(TokenType::In, self.col, 2),
                '\n' => {
                    // treating all newlines as semicolons because semicolons are optional in awk
                    self.add(TokenType::Semicolon, self.col, 1);
                    self.advance(0, true);
                }
                '#' | '\\' => {
                    // ignore comments and line splits
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
                                self.add(
                                    TokenType::Literal(PrimitiveType::String(
                                        &self.input[start..=self.pos],
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
                                self.add(
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
                                    self.add(TokenType::Literal(PrimitiveType::Float(val)), col, 1);
                                } else {
                                    let val = self.input[start..=self.pos].parse::<i64>().unwrap();
                                    self.add(
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
                '.' => self.add(TokenType::Dot, self.col, 1),
                ',' => self.add(TokenType::Comma, self.col, 1),
                _ => self.advance(1, false),
            }
        }
        self.add(TokenType::Eof, self.col, 0);
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
            Token::new(TokenType::Semicolon, 1, 8),
            Token::new(TokenType::Identifier("print"), 2, 5),
            Token::new(
                TokenType::Literal(PrimitiveType::String("hello world")),
                2,
                11,
            ),
            Token::new(TokenType::Semicolon, 2, 24),
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
    return num2
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
            Token::new(TokenType::Semicolon, 2, 32),
            Token::new(TokenType::If, 3, 5),
            Token::new(TokenType::LeftParen, 3, 8),
            Token::new(TokenType::Identifier("num1"), 3, 9),
            Token::new(TokenType::LessThan, 3, 14),
            Token::new(TokenType::Identifier("num2"), 3, 16),
            Token::new(TokenType::RightParen, 3, 20),
            Token::new(TokenType::Semicolon, 3, 21),
            Token::new(TokenType::Return, 4, 9),
            Token::new(TokenType::Identifier("num1"), 4, 16),
            Token::new(TokenType::Semicolon, 4, 20),
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
            Token::new(TokenType::Literal(PrimitiveType::String("return")), 2, 7),
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
            Token::new(TokenType::Semicolon, 2, 12),
            Token::new(TokenType::Break, 3, 5),
            Token::new(TokenType::Semicolon, 3, 10),
            Token::new(TokenType::Continue, 4, 5),
            Token::new(TokenType::Semicolon, 4, 13),
            Token::new(TokenType::Semicolon, 4, 14),
            Token::new(TokenType::RightCurly, 5, 1),
            Token::new(TokenType::Eof, 5, 2),
        ];
        assert_eq!(lhs, rhs);
    }
}
