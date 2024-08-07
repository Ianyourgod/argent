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
    Star,
    Divide,
    Modulus,
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
    ModulusAssign,
    Colon,
    QuestionMark,
    Comma,
    Arrow,
    Pointer,
}

#[derive(Debug, Clone, PartialEq)]
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
    line_pos: usize,
    ch: char,
}


impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            line: 0,
            line_pos: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    fn create_token(&mut self, kind: TokenType, literal: String, line: usize, pos: usize, length: usize) -> Token {
        Token { kind, literal, line, pos: pos-1, length }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        const BLANK: String = String::new();
        let tok = match self.ch {
            '(' => {
                let tok = self.create_token(TokenType::LParen, BLANK, self.line, self.line_pos, 1);
                self.read_char();
                tok  
            },
            ')' => {
                let tok = self.create_token(TokenType::RParen, BLANK, self.line, self.line_pos, 1);
                self.read_char();
                tok
            },
            '{' => {
                let tok = self.create_token(TokenType::LBrace, BLANK, self.line, self.line_pos, 1);
                self.read_char();
                tok
            },
            '}' => {
                let tok = self.create_token(TokenType::RBrace, BLANK, self.line, self.line_pos, 1);
                self.read_char();
                tok
            },
            '+' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '=' {
                    let tok = self.create_token(TokenType::AddAssign, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::Add, BLANK, start_line, start_pos, 1)
                }
            },
            '-' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '=' {
                    let tok = self.create_token(TokenType::SubtractAssign, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else if self.ch == '>' {
                    let tok = self.create_token(TokenType::Arrow, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::Subtract, BLANK, start_line, start_pos, 1)
                }
            },
            '*' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '=' {
                    let tok = self.create_token(TokenType::MultiplyAssign, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::Star, BLANK, start_line, start_pos, 1)
                }
            },
            '/' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '=' {
                    let tok = self.create_token(TokenType::DivideAssign, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else if self.ch == '/' {
                    while self.ch != '\n' {
                        self.read_char();
                    }
                    self.next_token() // todo: this is a hack, fix it
                } else {
                    self.create_token(TokenType::Divide, BLANK, start_line, start_pos, 1)
                }
            },
            ';' => {
                let tok = self.create_token(TokenType::SemiColon, BLANK, self.line, self.line_pos, 1);
                self.read_char();
                tok
            },
            '~' => {
                let tok = self.create_token(TokenType::BitwiseComplement, BLANK, self.line, self.line_pos, 1);
                self.read_char();
                tok
            },
            '!' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '=' {
                    let tok = self.create_token(TokenType::NotEqual, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::LogicalNegation, BLANK, start_line, start_pos, 1)
                }
            },
            '&' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '&' {
                    let tok = self.create_token(TokenType::And, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::Pointer, BLANK, start_line, start_pos, 1)
                }
            },
            '|' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '|' {
                    let tok = self.create_token(TokenType::Or, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::Error, "unexpected character, expected |".to_string(), start_line, start_pos, 1)
                }
            },
            '=' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '=' {
                    let tok = self.create_token(TokenType::Equal, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::Assign, BLANK, start_line, start_pos, 1)
                }
            },
            '<' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '=' {
                    let tok = self.create_token(TokenType::LessThanEqual, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::LessThan, BLANK, start_line, start_pos, 1)
                }
            },
            '>' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '=' {
                    let tok = self.create_token(TokenType::GreaterThanEqual, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::GreaterThan, BLANK, start_line, start_pos, 1)
                }
            },
            ':' => {
                let tok = self.create_token(TokenType::Colon, BLANK, self.line, self.line_pos, 1);
                self.read_char();
                tok
            },
            '?' => {
                let tok = self.create_token(TokenType::QuestionMark, BLANK, self.line, self.line_pos, 1);
                self.read_char();
                tok
            },
            ',' => {
                let tok = self.create_token(TokenType::Comma, BLANK, self.line, self.line_pos, 1);
                self.read_char();
                tok
            }
            '%' => {
                let start_line = self.line;
                let start_pos = self.line_pos;
                self.read_char();
                if self.ch == '=' {
                    let tok = self.create_token(TokenType::ModulusAssign, BLANK, self.line, start_pos, 2);
                    self.read_char();
                    tok
                } else {
                    self.create_token(TokenType::Modulus, BLANK, start_line, start_pos, 1)
                }
            },
            '\0' => self.create_token(TokenType::EOF, BLANK, self.line, self.line_pos, 1),
            _ => {
                if self.ch.is_alphabetic() || self.ch == '_' {
                    self.read_identifier()
                } else if self.ch.is_numeric() {
                    let start_line = self.line;
                    let start_pos = self.line_pos;
                    let numb = self.read_number();
                    let numb_len = numb.len();
                    self.create_token(TokenType::Int, numb, start_line, start_pos, numb_len)
                } else {
                    self.create_token(TokenType::Error, "unexpected character".to_string(), self.line, self.line_pos, 1)
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
            self.line_pos = 0;
        } else {
            self.line_pos += 1;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> Token {
        let start_line = self.line;
        let start_pos = self.line_pos;
        let position = self.position;
        while self.ch.is_alphanumeric() || self.ch == '_' {
            self.read_char();
        }

        let keywords = vec!["fn", "let", "return", "if", "else", "while", "break", "continue", "int", "i32", "i64", "uint", "u32", "u64", "bool", "true", "false"];
        // todo: remove int from keywords and improve the type system

        let literal = self.input[position..self.position].to_string();

        let kind = if keywords.contains(&&literal[..]) {
            TokenType::Keyword
        } else {
            TokenType::Identifier
        };

        let literal_len = literal.len();

        self.create_token(kind, literal, start_line, start_pos, literal_len)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from("let x: int = 5;");
        let mut l = Lexer::new(input);
        let tests = vec![
            Token { kind: TokenType::Keyword, literal: String::from("let"), line: 0, pos: 0, length: 3 },
            Token { kind: TokenType::Identifier, literal: String::from("x"), line: 0, pos: 4, length: 1 },
            Token { kind: TokenType::Colon, literal: String::from(""), line: 0, pos: 5, length: 1 },
            Token { kind: TokenType::Keyword, literal: String::from("int"), line: 0, pos: 7, length: 3 },
            Token { kind: TokenType::Assign, literal: String::from(""), line: 0, pos: 11, length: 1 },
            Token { kind: TokenType::Int, literal: String::from("5"), line: 0, pos: 13, length: 1 },
            Token { kind: TokenType::SemiColon, literal: String::from(""), line: 0, pos: 14, length: 1 },
            Token { kind: TokenType::EOF, literal: String::from(""), line: 0, pos: 15, length: 1 },
        ];

        for test in tests {
            let tok = l.next_token();
            assert_eq!(tok, test);
        }
    }
}