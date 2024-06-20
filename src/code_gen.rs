#![allow(dead_code)]

use std::collections::HashMap;

use crate::parser;

pub struct CodeGen {
    pub program: parser::nodes::CompoundStatement,
    label_count: i32,
    source: String
}

#[derive(Clone, Debug)]
struct Function {
    name: String,
    params: Vec<parser::nodes::FunctionArg>,
}

#[derive(Clone)]
struct Context {
    var_map: VarMap,
    current_scope: VarMap,
    loop_start: Option<usize>,
    loop_end: Option<usize>,
    functions: HashMap<String, Function>,
}

impl Context {
    fn new() -> Context {
        Context { var_map: VarMap::new(), loop_start: None, loop_end: None, current_scope: VarMap::new(), functions: HashMap::new()}
    }
}

#[derive(Clone)]
struct VarMap {
    hashmap: HashMap<String, i32>,
    stack_index: i32,
}

impl VarMap {
    fn new() -> VarMap {
        VarMap { hashmap: HashMap::new(), stack_index: -8 }
    }
}

impl CodeGen {
    pub fn new(program: parser::nodes::CompoundStatement, source: Option<String>) -> CodeGen {
        let src: String;
        if source.is_some() {
            src = source.unwrap();
        } else {
            src = String::new();
        }
        
        CodeGen { program, label_count: 0, source: src }
    }

    pub fn generate_code(&mut self) -> String {
        let mut code = String::new();
        let mut context = Context::new();

        for stmt in self.program.statements.clone() { // clone to avoid borrowing issues
            code.push_str(&(self.generate_statement(&stmt, &mut context)));
        }

        code
    }

    fn error(&mut self, error_message: String, line: usize, position: usize, length: usize, error_code: Option<i32>) {
        let lines = self.source.split('\n').collect::<Vec<&str>>();
        let error_text = lines[line].trim_start();

        let diff = lines[line].len() - error_text.len();

        let mut arrows = String::new();
        for _ in 0..(position - diff - length + 1) {
            arrows.push_str(" ");
        }
        for _ in position..(position+length) {
            arrows.push_str("^")
        }

        let position = format!("--> {}:{}", line+1, position+1);
        
        println!("{}\n{}\n{}\n{}",
            error_message,
            position,
            error_text,
            arrows
        );

        let code = if error_code.is_some() {
            error_code.unwrap()
        } else { 1 };

        std::process::exit(code)
    }

    fn generate_statement(&mut self, stmt: &Box<parser::nodes::Statement>, context: &mut Context) -> String {
        match **stmt {
            parser::nodes::Statement::ReturnStatement(ref return_stmt) => self.generate_return_statement(return_stmt, context),
            parser::nodes::Statement::ExpressionStatement(ref expr_stmt) => self.generate_expression_statement(expr_stmt, context),
            parser::nodes::Statement::IfStatement(ref if_stmt) => self.generate_if_statement(if_stmt, context),
            parser::nodes::Statement::WhileStatement(ref while_stmt) => self.generate_while_statement(while_stmt, context),
            parser::nodes::Statement::BreakStatement => self.generate_break_statement(context),
            parser::nodes::Statement::ContinueStatement => self.generate_continue_statement(context),
            parser::nodes::Statement::FunctionDeclaration(ref func_decl) =>self.generate_function_declaration(func_decl, &mut context.functions), // doesnt currently need context
            parser::nodes::Statement::VariableDeclaration(ref var_decl) => self.generate_variable_declaration(var_decl, context),
            parser::nodes::Statement::Compound(ref block_stmt) => self.generate_block_statement(block_stmt, context),
            parser::nodes::Statement::Empty => String::new(),
        }
    }

    fn generate_variables_clear(&mut self, var_map: &VarMap) -> String {
        // clears variables (just by moving rsp to the top of the stack (where it was at the start of the var_map))

        /*/
         * addq {-vm.stack_index - 8}, %rsp
         */

        format!("\taddq ${}, %rsp\n", -var_map.stack_index - 8)
    }

    fn generate_return_statement(&mut self, stmt: &parser::nodes::ReturnStatement, context: &mut Context) -> String {
        /*/
         * {return_value}
         * mov %rbp, %rsp // restore stack pointer
         * pop %rbp // restore old base pointer
         * ret
         */

        format!("{}\tmov %rbp, %rsp\n\tpop %rbp\n\tret\n", self.generate_expression(&stmt.return_value, context))
    }

    fn generate_expression_statement(&mut self, stmt: &parser::nodes::ExpressionStatement, context: &mut Context) -> String {
        self.generate_expression(&stmt.expression, context)
    }

    fn generate_if_statement(&mut self, stmt: &parser::nodes::IfStatement, context: &mut Context) -> String {
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

        let condition = self.generate_expression(&stmt.condition, context);
        let consequence = self.generate_statement(&stmt.consequence, context);
        
        code.push_str(&format!("{}\tcmpq $0, %rax\n\tje .L{}\n{}", condition, self.label_count, consequence));

        if stmt.alternative.is_some() {
            let label_count = self.label_count;
            self.label_count += 2;
            let alternative = self.generate_statement(stmt.alternative.as_ref().unwrap(), context);
            code.push_str(&format!("\tjmp .L{}\n.L{}:\n{}\n.L{}:\n", label_count + 1, label_count, alternative, label_count + 1));
        } else {
            code.push_str(&format!(".L{}:\n", self.label_count));
            self.label_count += 1;
        }
        code
    }

    fn generate_while_statement(&mut self, stmt: &parser::nodes::WhileStatement, context: &mut Context) -> String {
        /*/
         * .L{label_count}:
         * {condition}
         * cmpq $0, %rax
         * je .L{label_count + 1}
         * {body}
         * jmp .L{label_count}
         * .L{label_count + 1}:
         */

        let mut code = format!(".L{}:\n", self.label_count);
        let label_count = self.label_count;
        self.label_count += 2;

        context.loop_start = Some(label_count as usize);
        context.loop_end = Some((label_count + 1) as usize);

        let condition = self.generate_expression(&stmt.condition, context);
        let body = self.generate_statement(&stmt.body, context);

        code.push_str(&format!("{}\tcmpq $0, %rax\n\tje .L{}\n{}\tjmp .L{}\n.L{}:\n", condition, label_count + 1, body, label_count, label_count + 1));

        code
    }

    fn generate_break_statement(&mut self, context: &Context) -> String {
        /*/
         * jmp .L{loop end label}
         */

        if context.loop_end.is_none() {
            panic!("break statement outside of loop");
        }

        format!("\tjmp .L{}\n", context.loop_end.unwrap())
    }

    fn generate_continue_statement(&mut self, context: &Context) -> String {
        /*/
         * jmp .L{loop start label}
         */

        if context.loop_start.is_none() {
            panic!("continue statement outside of loop");
        }

        format!("\tjmp .L{}\n", context.loop_start.unwrap())
    }

    fn generate_function_declaration(&mut self, stmt: &parser::nodes::FunctionDeclaration, functions: &mut HashMap<String, Function>) -> String {
        /*/
         * .globl {function_name}
         * {function_name}:
         * {var setup}
         * push %rbp ; save old base pointer
         * mov %rsp, %rbp ; set new base pointer
         * {body}
         * mov %rbp, %rsp // restore stack pointer
         * pop %rbp // restore old base pointer
         * movq $0, %rax // return 0
         * ret
         */

        if functions.contains_key(&stmt.function_name) {
            panic!("function {} already declared", stmt.function_name);
        }

        functions.insert(
            stmt.function_name.clone(),
            Function {
                name: stmt.function_name.clone(),
                params: stmt.params.clone(),
            }
        );
    
        let mut context = Context::new();
        context.functions = functions.clone();

        // args

        let stack_index = stmt.params.len() as i32 * 8 - 8 - (if stmt.params.len() > 6 { 6 } else { stmt.params.len() } as i32 * 8);

        let mut code = format!("\t.globl {}\n{}:\n\tpush %rbp\n\tmov %rsp, %rbp\n", stmt.function_name, stmt.function_name);
        let mut arg_count = 0;
        let arg_regs = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

        for arg in &stmt.params {
            if arg_count < 6 {
                context.var_map.hashmap.insert(arg.ident.value.clone(), context.var_map.stack_index);
                code.push_str(&format!("\tmovq {}, {}(%rbp)\n", arg_regs[arg_count], context.var_map.stack_index));
                context.var_map.stack_index -= 8;
                arg_count += 1;
            } else {
                context.var_map.stack_index -= 8;
                context.var_map.hashmap.insert(arg.ident.value.clone(), stack_index + 24); // +24 because of the return address and base pointer and the stack grows downwards
            }
        }

        context.current_scope = context.var_map.clone();

        code.push_str(&self.generate_statement(&stmt.body, &mut context));

        code.push_str("\tmov %rbp, %rsp\n\tpop %rbp\n\tmovq $0, %rax\n\tret\n");

        code
    }

    fn generate_variable_declaration(&mut self, stmt: &parser::nodes::VariableDeclaration, context: &mut Context) -> String {
        if context.current_scope.hashmap.contains_key(&stmt.ident.value) {
            panic!("variable {} already declared", stmt.ident.value);
        }

        // TODO: allow variables defined in higher up scopes to be redeclared
        context.var_map.hashmap.insert(stmt.ident.value.clone(), context.var_map.stack_index);
        context.current_scope.hashmap.insert(stmt.ident.value.clone(), context.var_map.stack_index);

        context.var_map.stack_index -= 8;
        context.current_scope.stack_index -= 8;

        /*/
         * {expr}
         * pushq %rax
         */

        format!("{}\tpushq %rax\n", self.generate_expression(&stmt.expr, context))
    }

    fn generate_expression(&mut self, expr: &parser::nodes::Expression, context: &mut Context) -> String {
        match *expr {
            parser::nodes::Expression::Literal(ref lit) => self.generate_literal(lit),
            parser::nodes::Expression::Identifier(ref ident) => self.generate_identifier(ident, context),
            parser::nodes::Expression::BinOp(ref left, ref op, ref right) => self.generate_bin_op(op, left, right, context),
            parser::nodes::Expression::UnaryOp(ref op, ref expr) => self.generate_unary_op(op, expr, context),
            parser::nodes::Expression::Assignment(ref ident, ref expr) => self.generate_assignment(ident, expr, context),
            parser::nodes::Expression::Conditional(ref flag, ref left, ref right) => self.generate_conditional(flag, left, right, context),
            parser::nodes::Expression::FunctionCall(ref name, ref args) => self.generate_function_call(name, args, context),
        }
    }

    fn generate_block_statement(&mut self, stmt_list: &parser::nodes::CompoundStatement, context: &Context) -> String {
        let mut code = String::new();

        /*/
         * {code}
         * {generate_variables_clear}
         */

        let mut new_context = context.clone();
        new_context.current_scope = VarMap::new();
        for stmt in &stmt_list.statements {
            code.push_str(&self.generate_statement(stmt, &mut new_context));
        }

        code.push_str(&self.generate_variables_clear(&new_context.current_scope));
    
        code
    }

    fn generate_assignment(&mut self, ident: &parser::nodes::Identifier, expr: &parser::nodes::Expression, context: &mut Context) -> String {
        let expr = self.generate_expression(expr, context);

        /*/
         * {expr}
         * movq %rax, {index}(%rbp)
         */

        let index = context.var_map.hashmap[&ident.value];

        format!("{}\tmovq %rax, {}(%rbp)\n", expr, index)
    }

    fn generate_function_call(&mut self, name: &String, args: &Vec<Box<parser::nodes::Expression>>, context: &mut Context) -> String {
        if !context.functions.contains_key(name) {
            //panic!("function {} not declared", name);
        }

        /*
        let function = &context.functions[name];

        if function.params.len() != args.len() {
            panic!("function {} expected {} arguments, got {}", name, function.params.len(), args.len());
        }
        */

        let mut code = String::new();

        /*/
         * {args stuff}
         * call {name}
         * {clear args}
         */

        let mut arg_count = 0;
        let arg_regs = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

        for arg in args.iter().rev() {
            code.push_str(&self.generate_expression(arg, context));
            if arg_count < 6 {
                code.push_str(&format!("\tmovq %rax, {}\n", arg_regs[arg_count]));
                arg_count += 1;
            } else {
                code.push_str("\tpushq %rax\n");
            }
        }

        code.push_str(&format!("\tcall {}\n", name));

        for _ in 6..args.len() {
            code.push_str("\tpopq %rcx\n")
        }

        code
    }

    fn generate_conditional(&mut self, flag: &parser::nodes::Expression, left: &parser::nodes::Expression, right: &parser::nodes::Expression, context: &mut Context) -> String {
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

        let condition = self.generate_expression(flag, context);
        let consequence = self.generate_expression(left, context);
        
        code.push_str(&format!("{}\tcmpq $0, %rax\n\tje .L{}\n{}", condition, self.label_count, consequence));

        let label_count = self.label_count;
        self.label_count += 2;
        let alternative = self.generate_expression(right, context);
        code.push_str(&format!("\tjmp .L{}\n.L{}:\n{}\n.L{}:\n", label_count + 1, label_count, alternative, label_count + 1));
        
        code
    }

    fn generate_identifier(&self, ident: &parser::nodes::Identifier, context: &mut Context) -> String {
        // check that variable is declared
        if !context.var_map.hashmap.contains_key(&ident.value) {
            panic!("variable {} not declared", ident.value);
        }

        let index = context.var_map.hashmap[&ident.value];

        /*/
         * movq {index}(%rbp), %rax
         */

        format!("\tmovq {}(%rbp), %rax\n", index)
    }

    fn generate_bin_op(&mut self, op: &parser::nodes::BinOp, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, context: &mut Context) -> String {

        match *op {
            parser::nodes::BinOp::Add => self.generate_addition(left, right, context),
            parser::nodes::BinOp::Subtract => self.generate_subtraction(left, right, context),
            parser::nodes::BinOp::Multiply => self.generate_multiplication(left, right, context),
            parser::nodes::BinOp::Divide => self.generate_division(left, right, context),
            parser::nodes::BinOp::And => self.generate_and(left, right, context),
            parser::nodes::BinOp::Or => self.generate_or(left, right, context),
            parser::nodes::BinOp::Equal => self.generate_comparison("sete", left, right, context),
            parser::nodes::BinOp::NotEqual => self.generate_comparison("setne", left, right, context),
            parser::nodes::BinOp::LessThan => self.generate_comparison("setl", left, right, context),
            parser::nodes::BinOp::LessThanEqual => self.generate_comparison("setle", left, right, context),
            parser::nodes::BinOp::GreaterThan => self.generate_comparison("setg", left, right, context),
            parser::nodes::BinOp::GreaterThanEqual => self.generate_comparison("setge", left, right, context),
        }
    }

    fn generate_addition(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, context: &mut Context) -> String {
        let left = self.generate_expression(left, context);
        let right = self.generate_expression(right, context);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * popq %rcx
         * addq %rcx, %rax
         */

        format!("{}\tpushq %rax\n{}\tpopq %rcx\n\taddq %rcx, %rax\n", left, right)
    }

    fn generate_subtraction(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, context: &mut Context) -> String {
        let left = self.generate_expression(left, context);
        let right = self.generate_expression(right, context);

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

    fn generate_multiplication(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, context: &mut Context) -> String {
        let left = self.generate_expression(left, context);
        let right = self.generate_expression(right, context);

        /*/
         * {left}
         * pushq %rax
         * {right}
         * popq %rcx
         * imulq %rcx, %rax
         */

        format!("{}\tpushq %rax\n{}\tpopq %rcx\n\timulq %rcx, %rax\n", left, right)
    }

    fn generate_division(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, context: &mut Context) -> String {
        let left = self.generate_expression(left, context);
        let right = self.generate_expression(right, context);

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

    fn generate_and(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, context: &mut Context) -> String {
        let left = self.generate_expression(left, context);
        let right = self.generate_expression(right, context);

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

    fn generate_or(&mut self, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, context: &mut Context) -> String {
        let left = self.generate_expression(left, context);
        let right = self.generate_expression(right, context);

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

    fn generate_comparison(&mut self, set: &str, left: &Box<parser::nodes::Expression>, right: &Box<parser::nodes::Expression>, context: &mut Context) -> String {
        let left = self.generate_expression(left, context);
        let right = self.generate_expression(right, context);

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

    fn generate_unary_op(&mut self, op: &parser::nodes::UnaryOp, expr: &Box<parser::nodes::Expression>, context: &mut Context) -> String {
        let expr = self.generate_expression(expr, context);

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