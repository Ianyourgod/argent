use std::collections::HashMap;
use crate::parser;

pub struct CodeGen {
    pub program: parser::nodes::StatementList,
    label_count: i32,
}

struct VarMap {
    hashmap: HashMap<String, i32>,
    stack_index: i32,
}

impl CodeGen {
    pub fn new(program: parser::nodes::StatementList) -> CodeGen {
        CodeGen { program, label_count: 0 }
    }

    pub fn generate_code(&mut self) -> String {
        let mut code = ".globl main\n".to_string();
        let mut var_map = VarMap { hashmap: HashMap::new(), stack_index: -8, };

        for stmt in self.program.statements.clone() { // clone to avoid borrowing issues
            code.push_str(&(self.generate_statement(&stmt, &mut var_map)));
        }

        code
    }

    fn generate_statement(&mut self, stmt: &Box<parser::nodes::Statement>, var_map: &mut VarMap) -> String {
        match **stmt {
            parser::nodes::Statement::ReturnStatement(ref return_stmt) => {
                self.generate_return_statement(return_stmt, var_map)
            },
            parser::nodes::Statement::ExpressionStatement(ref expr_stmt) => {
                self.generate_expression_statement(expr_stmt, var_map)
            },
            parser::nodes::Statement::IfStatement(ref if_stmt) => {
                self.generate_if_statement(if_stmt, var_map)
            },
            parser::nodes::Statement::FunctionDeclaration(ref func_decl) => {
                self.generate_function_declaration(func_decl) // doesnt currently need var_map
            },
            parser::nodes::Statement::VariableDeclaration(ref var_decl) => {
                self.generate_variable_declaration(var_decl, var_map)
            }
        }
    }

    fn generate_return_statement(&mut self, stmt: &parser::nodes::ReturnStatement, var_map: &mut VarMap) -> String {
        /*/
         * {return_value}
         * movq %rbp, %rsp // restore old stack pointer
         * pop %rbp // restore old base pointer
         * ret
         */

        format!("{}\tmovq %rbp, %rsp\n\tpop %rbp\n\tret\n", self.generate_expression(&stmt.return_value, var_map))
    }

    fn generate_expression_statement(&mut self, stmt: &parser::nodes::ExpressionStatement, var_map: &mut VarMap) -> String {
        self.generate_expression(&stmt.expression, var_map)
    }

    fn generate_if_statement(&mut self, stmt: &parser::nodes::IfStatement, var_map: &mut VarMap) -> String {
        /*/
         * {condition}
         * cmpq $0, %rax
         * je .L{label_count}
         * {consequence}
         * jmp .L{label_count + 1}
         * .L{label_count}:
         * {alternative}
         * .L{label_count + 1}:
         */

        let mut code = String::new();

        let condition = self.generate_expression(&stmt.condition, var_map);
        let consequence = self.generate_statement_list(&stmt.consequence, var_map);
        
        code.push_str(&format!("{}\tcmpq $0, %rax\n\tje .L{}\n{}", condition, self.label_count, consequence));

        if stmt.alternative.is_some() {
            let label_count = self.label_count;
            self.label_count += 2;
            let alternative = self.generate_statement_list(stmt.alternative.as_ref().unwrap(), var_map);
            code.push_str(&format!("\tjmp .L{}\n.L{}:\n{}\n.L{}:\n", label_count + 1, label_count, alternative, label_count + 1));
        } else {
            code.push_str(&format!(".L{}:\n", self.label_count));
            self.label_count += 1;
        }
        code
    }

    fn generate_function_declaration(&mut self, stmt: &parser::nodes::FunctionDeclaration) -> String {
        /*/
         * {function_name}:
         * push %rbp // save old base pointer
         * mov %rsp, %rbp // set new base pointer
         * {body}
         * movq %rbp, %rsp // restore old stack pointer
         * pop %rbp // restore old base pointer
         * movq $0, %rax // return 0
         * ret
         */

        
        let mut code = format!("{}:\n\tpush %rbp\n\tmov %rsp, %rbp\n", stmt.function_name);
        let mut var_map = VarMap { hashmap: HashMap::new(), stack_index: -8 };

        for stmt in &stmt.body.statements {
            code.push_str(&self.generate_statement(stmt, &mut var_map));
        }

        code.push_str("\tmovq %rbp, %rsp\n\tpop %rbp\n\tmovq $0, %rax\n\tret\n");

        code
    }

    fn generate_variable_declaration(&mut self, stmt: &parser::nodes::VariableDeclaration, var_map: &mut VarMap) -> String {
        if var_map.hashmap.contains_key(&stmt.ident.value) {
            panic!("variable {} already declared", stmt.ident.value);
        }

        var_map.hashmap.insert(stmt.ident.value.clone(), var_map.stack_index);

        var_map.stack_index -= 8;

        /*/
         * {expr}
         * pushq %rax
         */

        format!("{}\tpushq %rax\n", self.generate_expression(&stmt.expr, var_map))
    }

    fn generate_expression(&mut self, expr: &parser::nodes::Expression, var_map: &mut VarMap) -> String {
        match *expr {
            parser::nodes::Expression::Literal(ref lit) => self.generate_literal(lit),
            parser::nodes::Expression::Identifier(ref ident) => self.generate_identifier(ident, var_map),
            parser::nodes::Expression::BinOp(ref left, ref op, ref right) => self.generate_bin_op(op, left, right, var_map),
            parser::nodes::Expression::UnaryOp(ref op, ref expr) => self.generate_unary_op(op, expr, var_map),
            parser::nodes::Expression::StatementList(ref stmt_list) => self.generate_statement_list(stmt_list, var_map),
            parser::nodes::Expression::Assignment(ref ident, ref expr) => self.generate_assignment(ident, expr, var_map),
            parser::nodes::Expression::Conditional(ref flag, ref left, ref right) => self.generate_conditional(flag, left, right, var_map),
        }
    }

    fn generate_statement_list(&mut self, stmt_list: &parser::nodes::StatementList, var_map: &mut VarMap) -> String {
        let mut code = String::new();
        for stmt in &stmt_list.statements {
            code.push_str(&self.generate_statement(stmt, var_map));
        }
        code
    }

    fn generate_assignment(&mut self, ident: &parser::nodes::Identifier, expr: &parser::nodes::Expression, var_map: &mut VarMap) -> String {
        let expr = self.generate_expression(expr, var_map);

        /*/
         * {expr}
         * movq %rax, {index}(%rbp)
         */

        let index = var_map.hashmap[&ident.value];

        format!("{}\tmovq %rax, {}(%rbp)\n", expr, index)
    }

    fn generate_conditional(&mut self, flag: &parser::nodes::Expression, left: &parser::nodes::Expression, right: &parser::nodes::Expression, var_map: &mut VarMap) -> String {
        /*/
         * {condition}
         * cmpq $0, %rax
         * je .L{label_count}
         * {consequence}
         * jmp .L{label_count + 1}
         * .L{label_count}:
         * {alternative}
         * .L{label_count + 1}:
         */

        let mut code = String::new();

        let condition = self.generate_expression(flag, var_map);
        let consequence = self.generate_expression(left, var_map);
        
        code.push_str(&format!("{}\tcmpq $0, %rax\n\tje .L{}\n{}", condition, self.label_count, consequence));

        let label_count = self.label_count;
        self.label_count += 2;
        let alternative = self.generate_expression(right, var_map);
        code.push_str(&format!("\tjmp .L{}\n.L{}:\n{}\n.L{}:\n", label_count + 1, label_count, alternative, label_count + 1));
        
        code
    }

    fn generate_identifier(&self, ident: &parser::nodes::Identifier, var_map: &mut VarMap) -> String {
        // check that variable is declared
        if !var_map.hashmap.contains_key(&ident.value) {
            panic!("variable {} not declared", ident.value);
        }

        let index = var_map.hashmap[&ident.value];

        /*/
         * movq {index}(%rbp), %rax
         */

        format!("\tmovq {}(%rbp), %rax\n", index)
    }

    fn generate_bin_op(&mut self, op: &parser::nodes::BinOp, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, var_map: &mut VarMap) -> String {

        match *op {
            parser::nodes::BinOp::Add => self.generate_addition(left, right, var_map),
            parser::nodes::BinOp::Subtract => self.generate_subtraction(left, right, var_map),
            parser::nodes::BinOp::Multiply => self.generate_multiplication(left, right, var_map),
            parser::nodes::BinOp::Divide => self.generate_division(left, right, var_map),
            parser::nodes::BinOp::And => self.generate_and(left, right, var_map),
            parser::nodes::BinOp::Or => self.generate_or(left, right, var_map),
            parser::nodes::BinOp::Equal => self.generate_comparison("sete", left, right, var_map),
            parser::nodes::BinOp::NotEqual => self.generate_comparison("setne", left, right, var_map),
            parser::nodes::BinOp::LessThan => self.generate_comparison("setl", left, right, var_map),
            parser::nodes::BinOp::LessThanEqual => self.generate_comparison("setle", left, right, var_map),
            parser::nodes::BinOp::GreaterThan => self.generate_comparison("setg", left, right, var_map),
            parser::nodes::BinOp::GreaterThanEqual => self.generate_comparison("setge", left, right, var_map),
        }
    }

    fn generate_addition(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, var_map: &mut VarMap) -> String {
        let left = self.generate_expression(left, var_map);
        let right = self.generate_expression(right, var_map);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * popq %rcx
         * addq %rcx, %rax
         */

        format!("{}\tpushq %rax\n{}\tpopq %rcx\n\taddq %rcx, %rax\n", left, right)
    }

    fn generate_subtraction(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, var_map: &mut VarMap) -> String {
        let left = self.generate_expression(left, var_map);
        let right = self.generate_expression(right, var_map);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * movq %rax, %rcx
         * popq %rax
         * subq %rcx, %rax
         */

        format!("{}\tpushq %rax\n{}\tmovq %rax, %rcx\n\tpopq %rax\n\tsubq %rcx, %rax\n", left, right)
    }

    fn generate_multiplication(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, var_map: &mut VarMap) -> String {
        let left = self.generate_expression(left, var_map);
        let right = self.generate_expression(right, var_map);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * popq %rcx
         * imulq %rcx, %rax
         */

        format!("{}\tpushq %rax\n{}\tpopq %rcx\n\timulq %rcx, %rax\n", left, right)
    }

    fn generate_division(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, var_map: &mut VarMap) -> String {
        let left = self.generate_expression(left, var_map);
        let right = self.generate_expression(right, var_map);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * movq %rax, %rcx
         * popq %rax
         * cqto
         * idivq %rcx
         */

        format!("{}\tpushq %rax\n{}\tmovq %rax, %rcx\n\tpopq %rax\n\tcqto\n\tidivq %rcx\n", left, right)
    }

    fn generate_and(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, var_map: &mut VarMap) -> String {
        let left = self.generate_expression(left, var_map);
        let right = self.generate_expression(right, var_map);

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

    fn generate_or(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, var_map: &mut VarMap) -> String {
        let left = self.generate_expression(left, var_map);
        let right = self.generate_expression(right, var_map);

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

    fn generate_comparison(&mut self, set: &str, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, var_map: &mut VarMap) -> String {
        let left = self.generate_expression(left, var_map);
        let right = self.generate_expression(right, var_map);

        /*/
         * {right}
         * pushq %rax
         * {left}
         * popq %rcx
         * cmpq %rcx, %rax
         * {set} %al
         */

        // right and left are swapped because comparisons are done in reverse order and im lazy

        format!("{}\tpushq %rax\n{}\tpopq %rcx\n\tcmpq %rcx, %rax\n\t{} %al\n", right, left, set)
    }

    fn generate_unary_op(&mut self, op: &parser::nodes::UnaryOp, expr: &Box<parser::nodes::Expression>, var_map: &mut VarMap) -> String {
        let expr = self.generate_expression(expr, var_map);

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