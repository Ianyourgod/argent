use crate::parser;

pub struct CodeGen {
    pub program: parser::nodes::StatementList,
    label_count: i32,
}

impl CodeGen {
    pub fn new(program: parser::nodes::StatementList) -> CodeGen {
        CodeGen { program, label_count: 0 }
    }

    pub fn generate_code(&mut self) -> String {
        let mut code = ".globl main\n".to_string();

        for stmt in self.program.statements.clone() { // clone to avoid borrowing issues
            code.push_str(&(self.generate_statement(&stmt)));
        }

        code
    }

    fn generate_statement(&mut self, stmt: &Box<parser::nodes::Statement>) -> String {
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

    fn generate_return_statement(&mut self, stmt: &parser::nodes::ReturnStatement) -> String {
        format!("{}\t# no movqing needed as return value should be in rax\n\tret", self.generate_expression(&stmt.return_value))
    }

    fn generate_expression_statement(&mut self, stmt: &parser::nodes::ExpressionStatement) -> String {
        format!("\t{}\n", self.generate_expression(&stmt.expression))
    }

    fn generate_function_declaration(&mut self, stmt: &parser::nodes::FunctionDeclaration) -> String {
        let mut code = format!("{}:\n ", stmt.function_name);
        // we push rbp to the stack and then move rsp to rbp to create a new stack frame
        // whatever the fuck that means

        for stmt in &stmt.body.statements {
            code.push_str(&self.generate_statement(stmt));
        }

        code
    }

    fn generate_expression(&mut self, expr: &parser::nodes::Expression) -> String {
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

    fn generate_bin_op(&mut self, op: &parser::nodes::BinOp, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>) -> String {

        match *op {
            parser::nodes::BinOp::Add => self.generate_addition(left, right),
            parser::nodes::BinOp::Subtract => self.generate_subtraction(left, right),
            parser::nodes::BinOp::Multiply => self.generate_multiplication(left, right),
            parser::nodes::BinOp::Divide => self.generate_division(left, right),
            parser::nodes::BinOp::And => self.generate_and(left, right),
            parser::nodes::BinOp::Or => self.generate_or(left, right),
            parser::nodes::BinOp::Equal => self.generate_comparison("sete", left, right),
            parser::nodes::BinOp::NotEqual => self.generate_comparison("setne", left, right),
            parser::nodes::BinOp::LessThan => self.generate_comparison("setl", left, right),
            parser::nodes::BinOp::LessThanEqual => self.generate_comparison("setle", left, right),
            parser::nodes::BinOp::GreaterThan => self.generate_comparison("setg", left, right),
            parser::nodes::BinOp::GreaterThanEqual => self.generate_comparison("setge", left, right),
        }
    }

    fn generate_addition(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>) -> String {
        let left = self.generate_expression(left);
        let right = self.generate_expression(right);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * popq %rcx
         * addq %rcx, %rax
         */

        format!("{}\tpushq %rax\n{}popq %rcx\n\taddq %rax, %rcx\n", left, right)
    }

    fn generate_subtraction(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>) -> String {
        let left = self.generate_expression(left);
        let right = self.generate_expression(right);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * popq %rcx
         * subq %rcx, %rax
         */

        format!("{}\tpushq %rax\n{}popq %rcx\n\tsubq %rax, %rcx\n", left, right)
    }

    fn generate_multiplication(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>) -> String {
        let left = self.generate_expression(left);
        let right = self.generate_expression(right);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * popq %rcx
         * imulq %rcx, %rax
         */

        format!("{}\tpushq %rax\n{}popq %rcx\n\timulq %rax, %rcx\n", left, right)
    }

    fn generate_division(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>) -> String {
        let left = self.generate_expression(left);
        let right = self.generate_expression(right);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * popq %rcx
         * movq $0, %rdx
         * idivq %rcx
         */

        format!("{}\tpushq %rax\n{}popq %rcx\n\tmovq $0, %rdx\n\tidivq %rcx\n", left, right)
    }

    fn generate_and(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>) -> String {
        let left = self.generate_expression(left);
        let right = self.generate_expression(right);

        /*/
         * {left}
         * cmpq $0, %rax
         * je .L{label_count}
         * {right}
         * cmpq $0, %rax
         * je .L{label_count}
         * movq $1, %rax
         * jmp .L{label_count + 1}
         * .L{label_count}:
         * movq $0, %rax
         * .L{label_count + 1}:
         * // code continues from here
         */

        self.label_count += 2;

        format!("{}\tcmpq $0, %rax\n\tje .L{}\n{}\tcmpq $0, %rax\n\tje .L{}\n\tmovq $1, %rax\n\tjmp .L{}\n.L{}:\n\tmovq $0, %rax\n.L{}:\n", left, self.label_count - 2, right, self.label_count - 1, self.label_count - 1, self.label_count - 2, self.label_count -1)
    }

    fn generate_or(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>) -> String {
        let left = self.generate_expression(left);
        let right = self.generate_expression(right);

        /*/
         * {left}
         * cmpq $0, %rax
         * jne .L{label_count}
         * {right}
         * cmpq $0, %rax
         * jne .L{label_count}
         * movq $0, %rax
         * jmp .L{label_count + 1}
         * .L{label_count}:
         * movq $1, %rax
         * .L{label_count + 1}:
         * // code continues from here
         */

        self.label_count += 2;

        format!("{}\tcmpq $0, %rax\n\tjne .L{}\n{}\tcmpq $0, %rax\n\tjne .L{}\n\tmovq $0, %rax\n\tjmp .L{}\n.L{}:\n\tmovq $1, %rax\n.L{}:\n", left, self.label_count - 2, right, self.label_count - 1, self.label_count - 1, self.label_count - 2, self.label_count -1)
    }

    fn generate_comparison(&mut self, set: &str, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>) -> String {
        let left = self.generate_expression(left);
        let right = self.generate_expression(right);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * popq %rcx
         * cmpq %rcx, %rax
         * {set} %al
         */

        format!("{}\tpushq %rax\n{}popq %rcx\n\tcmpq %rcx, %rax\n\t{} %al\n", left, right, set)
    }

    fn generate_unary_op(&mut self, op: &parser::nodes::UnaryOp, expr: &Box<parser::nodes::Expression>) -> String {
        let expr = self.generate_expression(expr);

        match *op {
            parser::nodes::UnaryOp::BitwiseComplement => format!("{}\tnegq %rax\n", expr),
            parser::nodes::UnaryOp::LogicalNegation => format!("{}\tcmpq $0, %rax\n\tmovq $0, %rax\n\tsete %al\n", expr), // mov command is used as xor will set flags
            parser::nodes::UnaryOp::Negation => format!("{}\tnegq %rax\n", expr),
        }
    }

    fn generate_literal(&self, lit: &parser::nodes::Literal) -> String {
        match *lit {
            parser::nodes::Literal::Int(ref i) => format!("\tmovq ${}, %rax\n", i),
            parser::nodes::Literal::Bool(ref b) => format!("\tmovq ${}, %rax\n", if *b { 1 } else { 0 })
        }
    }
}