#![allow(dead_code)]

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Keyword,
    Identifier,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Int,
    Add,
    Subtract,
    Multiply,
    Divide,
    SemiColon,
    EOF,
    Error,
    BitwiseComplement,
    LogicalNegation,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub literal: String,
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}


impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok = match self.ch {
            '(' => {
                self.read_char();
                Token { kind: TokenType::LParen, literal: "".to_string() }
            },
            ')' => {
                self.read_char();
                Token { kind: TokenType::RParen, literal: "".to_string() }
            },
            '{' => {
                self.read_char();
                Token { kind: TokenType::LBrace, literal: "".to_string() }
            },
            '}' => {
                self.read_char();
                Token { kind: TokenType::RBrace, literal: "".to_string() }
            },
            '+' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    Token { kind: TokenType::AddAssign, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::Add, literal: "".to_string() }
                }
            },
            '-' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    Token { kind: TokenType::SubtractAssign, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::Subtract, literal: "".to_string() }
                }
            },
            '*' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    Token { kind: TokenType::MultiplyAssign, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::Multiply, literal: "".to_string() }
                }
            },
            '/' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    Token { kind: TokenType::DivideAssign, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::Divide, literal: "".to_string() }
                }
            },
            ';' => {
                self.read_char();
                Token { kind: TokenType::SemiColon, literal: "".to_string() }
            },
            '~' => {
                self.read_char();
                Token { kind: TokenType::BitwiseComplement, literal: "".to_string() }
            },
            '!' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    Token { kind: TokenType::NotEqual, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::LogicalNegation, literal: "".to_string() }
                }
            },
            '&' => {
                self.read_char();
                if self.ch == '&' {
                    self.read_char();
                    Token { kind: TokenType::And, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::Error, literal: format!("unexpected character: {}", self.ch) }
                }
            },
            '|' => {
                self.read_char();
                if self.ch == '|' {
                    self.read_char();
                    Token { kind: TokenType::Or, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::Error, literal: format!("unexpected character: {}", self.ch) }
                }
            },
            '=' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    Token { kind: TokenType::Equal, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::Assign, literal: "".to_string() }
                }
            },
            '<' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    Token { kind: TokenType::LessThanEqual, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::LessThan, literal: "".to_string() }
                }
            },
            '>' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    Token { kind: TokenType::GreaterThanEqual, literal: "".to_string() }
                } else {
                    Token { kind: TokenType::GreaterThan, literal: "".to_string() }
                }
            },
            '\0' => Token { kind: TokenType::EOF, literal: "".to_string() },
            _ => {
                if self.ch.is_alphabetic() {
                    self.read_identifier()
                } else if self.ch.is_numeric() {
                    Token { kind: TokenType::Int, literal: self.read_number() }
                } else {
                    Token { kind: TokenType::Error, literal: format!("unexpected character: {}", self.ch) }
                }
            }
        };
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> Token {
        let position = self.position;
        while self.ch.is_alphabetic() {
            self.read_char();
        }

        let keywords = vec!["int", "return", "if", "else"];

        let literal = self.input[position..self.position].to_string();

        let kind = if keywords.contains(&&literal[..]) {
            TokenType::Keyword
        } else {
            TokenType::Identifier
        };

        Token { kind, literal }
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_numeric() {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}