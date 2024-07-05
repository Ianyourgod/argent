#![allow(dead_code)]

use crate::tacky;

pub mod nodes;

mod convert_pass;
mod pseudo_replace_pass;
mod inst_fixup_pass;
mod inst_fixn2_pass;

pub struct CodeGen {
    pub program: tacky::nodes::Program,
    symbol_table: tacky::nodes::SymbolTable,
    source: String
}

impl CodeGen {
    pub fn new(program: tacky::nodes::Program, symbol_table: tacky::nodes::SymbolTable, source: Option<String>) -> CodeGen {
        let src: String;
        if source.is_some() {
            src = source.unwrap();
        } else {
            src = String::new();
        }
        
        CodeGen { program, source: src, symbol_table }
    }

    pub fn generate_code(&mut self) -> nodes::Program {
        let first_pass = convert_pass::Pass::new(&self.program, self.symbol_table.clone());
        let first_pass_output = first_pass.run();

        let mut second_pass = pseudo_replace_pass::Pass::new(&first_pass_output, self.symbol_table.clone());
        let second_pass_output = second_pass.run();

        let third_pass = inst_fixup_pass::Pass::new(&second_pass_output);
        let third_pass_output = third_pass.run();

        let fourth_pass = inst_fixn2_pass::Pass::new(&third_pass_output);
        let fourth_pass_output = fourth_pass.run();

        fourth_pass_output
    }
}