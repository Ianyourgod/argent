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
    Colon,
    QuestionMark,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub literal: String,
    pub line: usize, //technically not needed but its easier to do this shit in the lexer
    pub pos: usize,
    pub length: usize,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        if self.literal != String::new() {
            format!("{:#?}: {}", self.kind, self.literal)
        } else {
            format!("{:#?}", self.kind)
        }
    }
}

pub struct Lexer {
    pub input: String,
    position: usize,
    read_position: usize,
    line: usize,
    pos: usize,
    ch: char,
}


impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            line: 0,
            pos: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    fn create_token(&mut self, kind: TokenType, literal: String, length: usize) -> Token {
        Token { kind, literal, line: self.line, pos: (if self.pos < 2 {self.pos} else {self.pos-2}), length }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        const BLANK: String = String::new();
        let tok = match self.ch {
            '(' => {
                self.read_char();
                self.create_token(TokenType::LParen, BLANK, 1)
            },
            ')' => {
                self.read_char();
                self.create_token(TokenType::RParen, BLANK, 1)
            },
            '{' => {
                self.read_char();
                self.create_token(TokenType::LBrace, BLANK, 1)
            },
            '}' => {
                self.read_char();
                self.create_token(TokenType::RBrace, BLANK, 1)
            },
            '+' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    self.create_token(TokenType::AddAssign, BLANK, 2)
                } else {
                    self.create_token(TokenType::Add, BLANK, 1)
                }
            },
            '-' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    self.create_token(TokenType::SubtractAssign, BLANK, 2)
                } else {
                    self.create_token(TokenType::Subtract, BLANK, 1)
                }
            },
            '*' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    self.create_token(TokenType::MultiplyAssign, BLANK, 2)
                } else {
                    self.create_token(TokenType::Multiply, BLANK, 1)
                }
            },
            '/' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    self.create_token(TokenType::DivideAssign, BLANK, 2)
                } else {
                    self.create_token(TokenType::Divide, BLANK, 1)
                }
            },
            ';' => {
                self.read_char();
                self.create_token(TokenType::SemiColon, BLANK, 1)
            },
            '~' => {
                self.read_char();
                self.create_token(TokenType::BitwiseComplement, BLANK, 1)
            },
            '!' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    self.create_token(TokenType::NotEqual, BLANK, 2)
                } else {
                    self.create_token(TokenType::LogicalNegation, BLANK, 1)
                }
            },
            '&' => {
                self.read_char();
                if self.ch == '&' {
                    self.read_char();
                    self.create_token(TokenType::And, BLANK, 2)
                } else {
                    self.create_token(TokenType::Error, "unexpected character, expected &".to_string(), 1)
                }
            },
            '|' => {
                self.read_char();
                if self.ch == '|' {
                    self.read_char();
                    self.create_token(TokenType::Or, BLANK, 2)
                } else {
                    self.create_token(TokenType::Error, "unexpected character, expected |".to_string(), 1)
                }
            },
            '=' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    self.create_token(TokenType::Equal, BLANK, 2)
                } else {
                    self.create_token(TokenType::Assign, BLANK, 1)
                }
            },
            '<' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    self.create_token(TokenType::LessThanEqual, BLANK, 2)
                } else {
                    self.create_token(TokenType::LessThan, BLANK, 1)
                }
            },
            '>' => {
                self.read_char();
                if self.ch == '=' {
                    self.read_char();
                    self.create_token(TokenType::GreaterThanEqual, BLANK, 2)
                } else {
                    self.create_token(TokenType::GreaterThan, BLANK, 1)
                }
            },
            ':' => {
                self.read_char();
                self.create_token(TokenType::Colon, BLANK, 1)
            },
            '?' => {
                self.read_char();
                self.create_token(TokenType::QuestionMark, BLANK, 1)
            },
            '\0' => self.create_token(TokenType::EOF, BLANK, 1),
            _ => {
                if self.ch.is_alphabetic() || self.ch == '_' {
                    self.read_identifier()
                } else if self.ch.is_numeric() {
                    let numb = self.read_number();
                    let numb_len = numb.len();
                    self.create_token(TokenType::Int, numb, numb_len)
                } else {
                    self.create_token(TokenType::Error, "unexpected character".to_string(), 1)
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

        if self.ch == '\n' {
            self.line += 1;
            self.pos = 0;
        } else {
            self.pos += 1;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> Token {
        let position = self.position;
        while self.ch.is_alphabetic() || self.ch == '_' {
            self.read_char();
        }

        let keywords = vec!["int", "return", "if", "else"];

        let literal = self.input[position..self.position].to_string();

        let kind = if keywords.contains(&&literal[..]) {
            TokenType::Keyword
        } else {
            TokenType::Identifier
        };

        let literal_len = literal.len();

        self.create_token(kind, literal, literal_len)
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