use crate::parser;

pub struct CodeGen {
    pub program: parser::nodes::StatementList,
}

impl CodeGen {
    pub fn new(program: parser::nodes::StatementList) -> CodeGen {
        CodeGen { program }
    }

    pub fn generate_code(&self) -> String {
        let mut code = String::new();

        for stmt in &self.program.statements {
            code.push_str(&self.generate_statement(stmt));
        }

        code
    }

    fn generate_statement(&self, stmt: &Box<parser::nodes::Statement>) -> String {
        match **stmt {
            parser::nodes::Statement::ReturnStatement(ref return_stmt) => {
                self.generate_return_statement(return_stmt)
            },
            parser::nodes::Statement::ExpressionStatement(ref expr_stmt) => {
                self.generate_expression_statement(expr_stmt)
            },
            parser::nodes::Statement::FunctionDeclaration(ref func_decl) => {
                self.generate_function_declaration(func_decl)
            },
        }
    }

    fn generate_return_statement(&self, stmt: &parser::nodes::ReturnStatement) -> String {
        format!("{}\t# no movqing needed as return value should be in rax\n\tret", self.generate_expression(&stmt.return_value))
    }

    fn generate_expression_statement(&self, stmt: &parser::nodes::ExpressionStatement) -> String {
        format!("\t{}\n", self.generate_expression(&stmt.expression))
    }

    fn generate_function_declaration(&self, stmt: &parser::nodes::FunctionDeclaration) -> String {
        let mut code = format!("\t.globl {}\n{}:\n ", stmt.function_name, stmt.function_name);
        // we push rbp to the stack and then move rsp to rbp to create a new stack frame
        // whatever the fuck that means

        for stmt in &stmt.body.statements {
            code.push_str(&self.generate_statement(stmt));
        }

        code
    }

    fn generate_expression(&self, expr: &parser::nodes::Expression) -> String {
        match *expr {
            parser::nodes::Expression::Literal(ref lit) => self.generate_literal(lit),
            parser::nodes::Expression::Identifier(ref ident) => ident.value.clone(),
            parser::nodes::Expression::BinOp(ref left, ref op, ref right) => self.generate_bin_op(op, left, right),
            parser::nodes::Expression::UnaryOp(ref op, ref expr) => self.generate_unary_op(op, expr),
            parser::nodes::Expression::StatementList(ref stmt_list) => {
                let mut code = String::new();
                for stmt in &stmt_list.statements {
                    code.push_str(&self.generate_statement(stmt));
                }
                code
            }
        }
    }

    fn generate_bin_op(&self, op: &parser::nodes::BinOp, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>) -> String {
        let left = self.generate_expression(left);
        let right = self.generate_expression(right);

        let out = format!("{}\tpushq %rax\n{}\tpopq %rcx\n", left, right);

        match *op {
            parser::nodes::BinOp::Add => format!("{}\taddq %rcx, %rax\n", out),
            parser::nodes::BinOp::Subtract => format!("{}\tsubq %rcx, %rax\n", out),
            parser::nodes::BinOp::Multiply => format!("{}\timulq %rcx, %rax\n", out),
            parser::nodes::BinOp::Divide => format!("{}\tmovq $0, %rdx\n{} idivq %rcx\n", out, out),
        }
    }

    fn generate_unary_op(&self, op: &parser::nodes::UnaryOp, expr: &Box<parser::nodes::Expression>) -> String {
        let expr = self.generate_expression(expr);

        match *op {
            parser::nodes::UnaryOp::BitwiseComplement => format!("{}\tnegq %rax\n", expr),
            parser::nodes::UnaryOp::LogicalNegation => format!("{}\tcmpq $0, %rax\n\tmovq $0, %rax\n\tsete %al\n", expr), // mov command is used as xor will set flags
        }
    }

    fn generate_literal(&self, lit: &parser::nodes::Literal) -> String {
        match *lit {
            parser::nodes::Literal::Int(ref i) => format!("\tmovq ${}, %rax\n", i),
            parser::nodes::Literal::Bool(ref b) => format!("\tmovq ${}, %rax\n", if *b { 1 } else { 0 })
        }
    }
}