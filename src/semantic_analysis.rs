use crate::parser;
mod identifier_resolution_pass;
mod type_checking_pass;
mod loop_labeling_pass;

pub mod symbol_table;

pub struct Analysis {
    pub ast: parser::nodes::Program,
}

impl Analysis {
    pub fn new(ast: parser::nodes::Program) -> Self {
        Self {
            ast,
        }
    }

    pub fn run(&mut self) -> parser::nodes::Program {
        let first_pass = identifier_resolution_pass::Pass::new(&self.ast);

        let program = first_pass.run();

        let second_pass = loop_labeling_pass::Pass::new(&program);

        let program = second_pass.run();

        let third_pass = type_checking_pass::Pass::new(&program);

        let sym_tbl = third_pass.run();

        program
    }
}