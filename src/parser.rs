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

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> nodes::StatementList {
        let mut program = nodes::StatementList { statements: vec![] };

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
                    "int" => self.parse_declaration(),
                    _ => self.parse_expression_statement(),
                }
            },
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_return_statement(&mut self) -> Box<nodes::Statement> {
        self.next_token();

        let return_value = self.parse_expression();
        if self.cur_token.kind != lexer::TokenType::SemiColon {
            panic!("expected semicolon, got {:?}", self.cur_token.kind);
        }

        Box::new(nodes::Statement::ReturnStatement(nodes::ReturnStatement {
            return_value
        }))
    }

    fn parse_declaration(&mut self) -> Box<nodes::Statement> {
        self.next_token();
        let ident = self.cur_token.literal.clone();
        self.next_token();
        if self.cur_token.kind == lexer::TokenType::Assign {
            self.next_token();
            let expr = self.parse_expression();
            Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                kind: "int".to_string(),
                ident: nodes::Identifier { value: ident },
                expr,
            }))
        } else if self.cur_token.kind == lexer::TokenType::SemiColon {
            Box::new(nodes::Statement::VariableDeclaration(nodes::VariableDeclaration {
                kind: "int".to_string(),
                ident: nodes::Identifier { value: ident },
                expr: Box::new(nodes::Expression::Literal(nodes::Literal::Int(0))), // todo: add some kind of undefined value
            }))
        } else if self.cur_token.kind == lexer::TokenType::LParen {
            self.parse_function_declaration(ident)
        } else {
            panic!("unexpected token: {:?}", self.cur_token);
        }
    }

    fn parse_function_declaration(&mut self, function_name: String) -> Box<nodes::Statement> {
        // TODO: parse function arguments
        
        while self.cur_token.kind != lexer::TokenType::LBrace {
            self.next_token();
        }
        let body = self.parse_block_statement();
        Box::new(nodes::Statement::FunctionDeclaration(nodes::FunctionDeclaration {
            function_name,
            body,
        }))
    }

    fn parse_block_statement(&mut self) -> nodes::StatementList {
        let mut block = nodes::StatementList { statements: vec![] };
        self.next_token();
        while self.cur_token.kind != lexer::TokenType::RBrace {
            let stmt = self.parse_statement();
            block.statements.push(stmt);
            self.next_token();
        }
        block
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
        let mut node = self.parse_logical_or();
        while   self.cur_token.kind == lexer::TokenType::Assign         ||
                self.cur_token.kind == lexer::TokenType::AddAssign      ||
                self.cur_token.kind == lexer::TokenType::SubtractAssign ||
                self.cur_token.kind == lexer::TokenType::MultiplyAssign ||
                self.cur_token.kind == lexer::TokenType::DivideAssign    {
            let identifier = match *node {
                nodes::Expression::Identifier(ref ident) => ident.clone(),
                _ => panic!("expected identifier, got {:?}", node),
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
                    _ => panic!("unexpected token: {:?}", kind),
                };
                node = Box::new(nodes::Expression::BinOp(Box::new(nodes::Expression::Identifier(identifier.clone())), op, right));
                node = Box::new(nodes::Expression::Assignment(identifier, node));
            }
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
                _ => panic!("unexpected token: {:?}", self.cur_token),
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
                _ => panic!("unexpected token: {:?}", self.cur_token),
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
                _ => panic!("unexpected token: {:?}", self.cur_token),
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
                _ => panic!("unexpected token: {:?}", self.cur_token),
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
                let ident = self.cur_token.literal.clone();
                self.next_token();
                Box::new(nodes::Expression::Identifier(nodes::Identifier {
                    value: ident,
                }))
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
                self.next_token();
                node
            },
            lexer::TokenType::LBrace => {
                self.next_token();
                let node = self.parse_block_statement();
                self.next_token();
                Box::new(nodes::Expression::StatementList(node))
            }
            _ => panic!("unexpected token: {:?}", self.cur_token),
        }
    }


}