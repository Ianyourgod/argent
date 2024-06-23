use std::process::exit;

use crate::lexer;
pub mod nodes;

pub struct Parser {
    lexer: lexer::Lexer,
    cur_token: lexer::Token,
    peek_token: lexer::Token,
}

impl Parser {
    pub fn new(mut lexer: lexer::Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            cur_token,
            peek_token,
        }
    }

    fn error(&mut self, error_message: String, line: usize, position: usize, length: usize, error_code: Option<i32>) {
        let lines = self.lexer.input.split('\n').collect::<Vec<&str>>();

        let error_line = lines[line];
        let trimmed_line = error_line.trim_start();
        let top_line = if line > 0 {
            lines[line - 1]
        } else {
            ""
        };

        let split = top_line.split_at(error_line.len()-trimmed_line.len());
        let mut error_text = split.1.to_string();

        error_text.push_str("\n");
        error_text.push_str(trimmed_line);

        let diff = error_line.len() - error_line.trim_start().len();

        let mut arrows = String::new();
        for _ in 0..(position - diff - 1) {
            arrows.push_str(" ");
        }
        for _ in position..(position+length) {
            arrows.push_str("^")
        }

        let position = format!("--> {}:{}", line + 1, position);
        
        println!("{}\n{}\n{}\n{}",
            error_message,
            position,
            error_text,
            arrows
        );

        let code = if error_code.is_some() {
            error_code.unwrap()
        } else { 1 };

        exit(code)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
        if self.peek_token.kind == lexer::TokenType::Error {
            self.error(self.peek_token.to_string(), self.peek_token.line, self.peek_token.pos, self.peek_token.length, Some(1));
        }
    }

    pub fn parse_program(&mut self) -> nodes::CompoundStatement {
        let mut program = nodes::CompoundStatement { statements: vec![] };
        while self.cur_token.kind != lexer::TokenType::EOF {
            let stmt = self.parse_statement();
            program.statements.push(stmt);
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Box<nodes::Statement> {
        match self.cur_token.kind {
            lexer::TokenType::Keyword => {
                match self.cur_token.literal.as_str() {
                    "return" => self.parse_return_statement(),
                    "fn" => self.parse_function_declaration(),
                    "let" => self.parse_variable_declaration(),
                    "if" => self.parse_if_statement(),
                    "while" => self.parse_while_statement(),
                    "break" => self.parse_break_statement(),
                    "continue" => self.parse_continue_statement(),
                    // todo: for loop
                    _ => self.parse_expression_statement(),
                }
            },
            lexer::TokenType::LBrace => {
                self.parse_block_statement()
            },
            lexer::TokenType::SemiColon => {
                Box::new(nodes::Statement::Empty)
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_return_statement(&mut self) -> Box<nodes::Statement> {
        self.next_token();
        let return_value = self.parse_expression();
        if self.cur_token.kind != lexer::TokenType::SemiColon {
            self.error("unexpected character, expected semicolon".to_string(), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }

        Box::new(nodes::Statement::ReturnStatement(nodes::ReturnStatement {
            return_value
        }))
    }

    fn parse_variable_declaration(&mut self) -> Box<nodes::Statement> {
        self.next_token();

        if self.cur_token.kind != lexer::TokenType::Identifier {
            self.error(format!("Expected identifier, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }
        let ident = self.cur_token.literal.clone();

        self.next_token();

        if self.cur_token.kind != lexer::TokenType::Colon {
            self.error(format!("Expected colon, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }

        self.next_token();

        if self.cur_token.kind != lexer::TokenType::Identifier && self.cur_token.literal != "int" {
            self.error(format!("Expected identifier, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }

        let kind = self.cur_token.literal.clone();

        self.next_token();

        if self.cur_token.kind == lexer::TokenType::Assign {
            self.next_token();
            let expr = self.parse_expression();
            if self.cur_token.kind != lexer::TokenType::SemiColon {
                self.error("unexpected character, expected semicolon".to_string(), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
            }
            Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                kind,
                ident: nodes::Identifier { value: ident },
                expr,
            }))
        } else if self.cur_token.kind == lexer::TokenType::SemiColon {
            Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                kind,
                ident: nodes::Identifier { value: ident },
                expr: Box::new(nodes::Expression::Literal(nodes::Literal::Int(0))), // todo: add some kind of undefined value
            }))
        } else {
            self.error(format!("expected = or ;, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
            panic!();
        }
    }

    fn parse_if_statement(&mut self) -> Box<nodes::Statement> {
        self.next_token();

        let condition = self.parse_expression();

        // todo: require braces
        let consequence = self.parse_statement();
        self.next_token();
        let alternative = if self.cur_token.kind == lexer::TokenType::Keyword && self.cur_token.literal == "else" {
            self.next_token();
            Some(self.parse_statement())
        } else {
            None
        };
        Box::new(nodes::Statement::IfStatement(nodes::IfStatement {
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_while_statement(&mut self) -> Box<nodes::Statement> {
        self.next_token();

        let condition = self.parse_expression();

        // todo: require braces
        let body = self.parse_statement();

        Box::new(nodes::Statement::WhileStatement(nodes::WhileStatement {
            condition,
            body,
        }))
    }

    fn parse_break_statement(&mut self) -> Box<nodes::Statement> {
        self.next_token();
        if self.cur_token.kind != lexer::TokenType::SemiColon {
            self.error("unexpected character, expected semicolon".to_string(), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }
        Box::new(nodes::Statement::BreakStatement)
    }

    fn parse_continue_statement(&mut self) -> Box<nodes::Statement> {
        self.next_token();
        if self.cur_token.kind != lexer::TokenType::SemiColon {
            self.error("unexpected character, expected semicolon".to_string(), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }
        Box::new(nodes::Statement::ContinueStatement)
    }

    fn parse_function_declaration(&mut self) -> Box<nodes::Statement> {
        self.next_token();

        if self.cur_token.kind != lexer::TokenType::Identifier {
            self.error("unexpected token".to_string(), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }
        let function_name = self.cur_token.literal.clone();

        self.next_token();

        if self.cur_token.kind != lexer::TokenType::LParen {
            self.error(format!("expected LParen, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }
        
        self.next_token();

        // parse args
        let mut args: Vec<nodes::FunctionArg> = vec![];

        if self.cur_token.kind != lexer::TokenType::RParen {
            loop  {
                if self.cur_token.kind != lexer::TokenType::Identifier {
                    self.error(format!("Expected identifier, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                }

                let ident = nodes::Identifier { value: self.cur_token.literal.clone() };
                self.next_token();

                if self.cur_token.kind != lexer::TokenType::Colon {
                    self.error(format!("Expected Colon, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                }

                self.next_token();

                if self.cur_token.kind != lexer::TokenType::Identifier || self.cur_token.literal != "int" {
                    self.error(format!("Expected type, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                }

                let kind = self.cur_token.literal.clone();

                self.next_token();

                args.push(nodes::FunctionArg {
                    kind,
                    ident,
                });
                
                if self.cur_token.kind == lexer::TokenType::RParen {
                    break;
                }
                if self.cur_token.kind != lexer::TokenType::Comma {
                    self.error(format!("Expected RParen, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                }
            }
        }
        self.next_token();

        // get type
        if self.cur_token.kind != lexer::TokenType::Arrow {
            self.error(format!("expected Arrow, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }

        self.next_token();

        if self.cur_token.kind != lexer::TokenType::Identifier || self.cur_token.literal != "int" {
            self.error(format!("expected int, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }

        let kind = self.cur_token.literal.clone();

        self.next_token();

        if self.cur_token.kind == lexer::TokenType::SemiColon {
            return Box::new(nodes::Statement::FunctionDeclaration(nodes::FunctionDeclaration {
                function_name,
                params: args,
                body: Box::new(nodes::Statement::Empty),
                return_type: kind,
            }));
        }
        if self.cur_token.kind != lexer::TokenType::LBrace {
            self.error(format!("expected LBrace, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
        }

        let body = self.parse_statement();

        Box::new(nodes::Statement::FunctionDeclaration(nodes::FunctionDeclaration {
            function_name,
            params: args,
            body,
            return_type: kind,
        }))
    }

    fn parse_block_statement(&mut self) -> Box<nodes::Statement> {
        let mut block = nodes::CompoundStatement { statements: vec![] };
        self.next_token();
        while self.cur_token.kind != lexer::TokenType::RBrace  {
            let stmt = self.parse_statement();
            block.statements.push(stmt);
            self.next_token();
            if self.cur_token.kind == lexer::TokenType::EOF {
                self.error(format!("Expected RBrace, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
            }
        }
        Box::new(nodes::Statement::Compound(block))
    }

    fn parse_expression_statement(&mut self) -> Box<nodes::Statement> {
        let expression = self.parse_expression();
        while self.cur_token.kind != lexer::TokenType::SemiColon {
            self.next_token();
        }

        Box::new(nodes::Statement::ExpressionStatement(nodes::ExpressionStatement {
            expression
        }))
    }

    fn parse_expression(&mut self) -> Box<nodes::Expression> {
        let mut node = self.parse_conditional();
        while   self.cur_token.kind == lexer::TokenType::Assign         ||
                self.cur_token.kind == lexer::TokenType::AddAssign      ||
                self.cur_token.kind == lexer::TokenType::SubtractAssign ||
                self.cur_token.kind == lexer::TokenType::MultiplyAssign ||
                self.cur_token.kind == lexer::TokenType::DivideAssign    {
            let identifier = match *node {
                nodes::Expression::Identifier(ref ident) => ident.clone(),
                _ => {
                    self.error(format!("Expected identifier, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                    panic!();
                },
            }; // just doing that so we can panic :bleh:
            let kind = self.cur_token.kind.clone();
            self.next_token();
            let right = self.parse_expression();
            if kind == lexer::TokenType::Assign {
                node = Box::new(nodes::Expression::Assignment(identifier, right));
            } else {
                let op = match kind {
                    lexer::TokenType::AddAssign => nodes::BinOp::Add,
                    lexer::TokenType::SubtractAssign => nodes::BinOp::Subtract,
                    lexer::TokenType::MultiplyAssign => nodes::BinOp::Multiply,
                    lexer::TokenType::DivideAssign => nodes::BinOp::Divide,
                    _ => {
                        self.error(format!("Expected BinOp, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                        panic!();
                    },
                };
                node = Box::new(nodes::Expression::BinOp(Box::new(nodes::Expression::Identifier(identifier.clone())), op, right));
                node = Box::new(nodes::Expression::Assignment(identifier, node));
            }
        }
        node
    }

    fn parse_conditional(&mut self) -> Box<nodes::Expression> {
        let mut node = self.parse_logical_or();
        while self.cur_token.kind == lexer::TokenType::QuestionMark {
            self.next_token();
            let left = self.parse_expression();
            if self.cur_token.kind != lexer::TokenType::Colon {
                self.error(format!("Expected Colon, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                panic!();
            }
            self.next_token();
            let right = self.parse_conditional();
            node = Box::new(nodes::Expression::Conditional(node, left, right))
        }
        node
    }
    
    fn parse_logical_or(&mut self) -> Box<nodes::Expression> {
        let mut node = self.parse_logical_and();
        while self.cur_token.kind == lexer::TokenType::Or {
            let op = nodes::BinOp::Or;
            self.next_token();
            let right = self.parse_logical_and();
            node = Box::new(nodes::Expression::BinOp(node, op, right));
        }
        node
    }

    fn parse_logical_and(&mut self) -> Box<nodes::Expression> {
        let mut node = self.parse_equalities();
        while self.cur_token.kind == lexer::TokenType::And {
            let op = nodes::BinOp::And;
            self.next_token();
            let right = self.parse_equalities();
            node = Box::new(nodes::Expression::BinOp(node, op, right));
        }
        node
    }

    fn parse_equalities(&mut self) -> Box<nodes::Expression> {
        let mut node = self.parse_relational();
        while self.cur_token.kind == lexer::TokenType::Equal || self.cur_token.kind == lexer::TokenType::NotEqual {
            let op = match self.cur_token.kind {
                lexer::TokenType::Equal => nodes::BinOp::Equal,
                lexer::TokenType::NotEqual => nodes::BinOp::NotEqual,
                _ => panic!("how the fuck"),
            };
            self.next_token();
            let right = self.parse_relational();
            node = Box::new(nodes::Expression::BinOp(node, op, right));
        }
        node
    }

    fn parse_relational(&mut self) -> Box<nodes::Expression> {
        let mut node = self.parse_additive();
        while self.cur_token.kind == lexer::TokenType::LessThan          ||
                self.cur_token.kind == lexer::TokenType::LessThanEqual   ||
                self.cur_token.kind == lexer::TokenType::GreaterThan     ||
                self.cur_token.kind == lexer::TokenType::GreaterThanEqual {
            let op = match self.cur_token.kind {
                lexer::TokenType::LessThan => nodes::BinOp::LessThan,
                lexer::TokenType::LessThanEqual => nodes::BinOp::LessThanEqual,
                lexer::TokenType::GreaterThan => nodes::BinOp::GreaterThan,
                lexer::TokenType::GreaterThanEqual => nodes::BinOp::GreaterThanEqual,
                _ => {
                    self.error(format!("Unexpected token, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                    panic!();
                },
            };
            self.next_token();
            let right = self.parse_additive();
            node = Box::new(nodes::Expression::BinOp(node, op, right));
        }
        node
    }

    fn parse_additive(&mut self) -> Box<nodes::Expression> {
        let mut node = self.parse_term();
        while self.cur_token.kind == lexer::TokenType::Add || self.cur_token.kind == lexer::TokenType::Subtract {
            let op = match self.cur_token.kind {
                lexer::TokenType::Add => nodes::BinOp::Add,
                lexer::TokenType::Subtract => nodes::BinOp::Subtract,
                _ => {
                    self.error(format!("Unexpected token, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                    panic!();
                },
            };
            self.next_token();
            let right = self.parse_term();
            node = Box::new(nodes::Expression::BinOp(node, op, right));
        }
        node
    }

    fn parse_term(&mut self) -> Box<nodes::Expression> {
        let mut node = self.parse_factor();
        while self.cur_token.kind == lexer::TokenType::Multiply || self.cur_token.kind == lexer::TokenType::Divide {
            let op = match self.cur_token.kind {
                lexer::TokenType::Multiply => nodes::BinOp::Multiply,
                lexer::TokenType::Divide => nodes::BinOp::Divide,
                _ => {
                    self.error(format!("Unexpected token, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                    panic!();
                },
            };
            self.next_token();
            let right = self.parse_factor();
            node = Box::new(nodes::Expression::BinOp(node, op, right));
        }
        node
    }

    fn parse_factor(&mut self) -> Box<nodes::Expression> {
        match self.cur_token.kind {
            lexer::TokenType::Int => {
                let int = self.cur_token.literal.parse::<i32>().unwrap();
                self.next_token();
                Box::new(nodes::Expression::Literal(nodes::Literal::Int(int)))
            },
            lexer::TokenType::Identifier => {
                self.parse_identifier()
            },
            lexer::TokenType::Subtract => {
                self.next_token();
                let expr = self.parse_factor();
                Box::new(nodes::Expression::UnaryOp(nodes::UnaryOp::Negation, expr))
            },
            lexer::TokenType::LogicalNegation => {
                self.next_token();
                let right = self.parse_factor();
                Box::new(nodes::Expression::UnaryOp(nodes::UnaryOp::LogicalNegation, right))
            },
            lexer::TokenType::BitwiseComplement => {
                self.next_token();
                let right = self.parse_factor();
                Box::new(nodes::Expression::UnaryOp(nodes::UnaryOp::BitwiseComplement, right))
            },
            lexer::TokenType::LParen => {
                self.next_token();
                let node = self.parse_expression();
                if self.cur_token.kind != lexer::TokenType::RParen {
                    self.error(format!("Expected RParen, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                    panic!();
                }
                self.next_token();
                node
            },
            _ => {
                self.error(format!("Unexpected token, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                panic!();
            },
        }
    }

    fn parse_identifier(&mut self) -> Box<nodes::Expression> {
        let ident = self.cur_token.literal.clone();
        self.next_token();

        if self.cur_token.kind != lexer::TokenType::LParen {
            return Box::new(nodes::Expression::Identifier(nodes::Identifier {
                value: ident,
            }));
        }

        self.next_token();

        let mut args: Vec<Box<nodes::Expression>> = vec![];

        if self.cur_token.kind != lexer::TokenType::RParen {
            loop  {
                args.push(self.parse_expression());
                
                if self.cur_token.kind == lexer::TokenType::RParen {
                    break;
                }
                if self.cur_token.kind != lexer::TokenType::Comma {
                    self.error(format!("Expected RParen, found {:#?}", self.cur_token.kind), self.cur_token.line, self.cur_token.pos, self.cur_token.length, Some(1));
                }
            }
        }

        self.next_token();

        Box::new(nodes::Expression::FunctionCall(ident, args))
    }


}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_parse_program() {
        let input = r#"
        fn main() -> int {
            return 5;
        }
        "#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_function_declaration() {
        let input = r#"
        fn main() -> int {
            return 5;
        }
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let stmt = &program.statements[0];
        match **stmt {
            nodes::Statement::FunctionDeclaration(ref f) => {
                assert_eq!(f.function_name, "main");
                assert_eq!(f.return_type, "int");
            },
            _ => panic!("expected function declaration"),
        };
    }

    #[test]
    fn test_parse_variable_declaration() {
        let input = r#"
        let x: int = 5;
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let stmt = &program.statements[0];
        match **stmt {
            nodes::Statement::VariableDeclaration(ref v) => {
                assert_eq!(v.kind, "int");
                assert_eq!(v.ident.value, "x");
            },
            _ => panic!("expected variable declaration"),
        }
    }

    #[test]
    fn test_parse_if_statement() {
        let input = r#"
        if x < 5 {
            return 5;
        } else {
            return 10;
        }
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let stmt = &program.statements[0];
        match **stmt {
            nodes::Statement::IfStatement(ref i) => {
                assert_eq!(*i.condition, nodes::Expression::BinOp(
                    Box::new(nodes::Expression::Identifier(nodes::Identifier { value: "x".to_string() })),
                    nodes::BinOp::LessThan,
                    Box::new(nodes::Expression::Literal(nodes::Literal::Int(5)))
                ));
            },
            _ => panic!("expected if statement"),
        }
    }

    #[test]
    fn test_parse_while_statement() {
        let input = r#"
        while x < 5 {
            return 5;
        }
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let stmt = &program.statements[0];
        match **stmt {
            nodes::Statement::WhileStatement(ref w) => {
                assert_eq!(*w.condition, nodes::Expression::BinOp(
                    Box::new(nodes::Expression::Identifier(nodes::Identifier { value: "x".to_string() })),
                    nodes::BinOp::LessThan,
                    Box::new(nodes::Expression::Literal(nodes::Literal::Int(5)))
                ));
            },
            _ => panic!("expected while statement"),
        }
    }

    #[test]
    fn test_parse_break_statement() {
        let input = r#"
        break;
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let stmt = &program.statements[0];
        match **stmt {
            nodes::Statement::BreakStatement => {},
            _ => panic!("expected break statement"),
        }
    }

    #[test]
    fn test_parse_continue_statement() {
        let input = r#"
        continue;
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let stmt = &program.statements[0];
        match **stmt {
            nodes::Statement::ContinueStatement => {},
            _ => panic!("expected continue statement"),
        }
    }

    #[test]
    fn test_parse_expression_statement() {
        let input = r#"
        x = 5;
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let stmt = &program.statements[0];
        match **stmt {
            nodes::Statement::ExpressionStatement(ref e) => {
                assert_eq!(*e.expression, nodes::Expression::Assignment(
                    nodes::Identifier { value: "x".to_string() },
                    Box::new(nodes::Expression::Literal(nodes::Literal::Int(5)))
                ));
            },
            _ => panic!("expected expression statement"),
        }
    }

    #[test]
    fn test_parse_block_statement() {
        let input = r#"
        {
            x = 5;
            y = 10;
        }
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let stmt = &program.statements[0];
        match **stmt {
            nodes::Statement::Compound(ref c) => {
                assert_eq!(c.statements.len(), 2);
            },
            _ => panic!("expected compound statement"),
        }
    }
}

// todo: we should prob test more thoroughly but i am lazy