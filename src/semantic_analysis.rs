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

    pub fn run(&mut self) -> (parser::nodes::Program, symbol_table::SymbolTable) {
        let mut program: parser::nodes::Program;

        let mut first_pass = identifier_resolution_pass::Pass::new(&self.ast);

        program = first_pass.run();

        let mut second_pass = loop_labeling_pass::Pass::new(&program);

        program = second_pass.run();

        let third_pass = type_checking_pass::Pass::new(&program);

        let sym_tbl: symbol_table::SymbolTable;
        (program, sym_tbl) = third_pass.run();

        (program, sym_tbl)
    }
}