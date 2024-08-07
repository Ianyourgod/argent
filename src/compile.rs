use std::process::exit;
use colored::Colorize;
use crate::preprocessor;
use crate::lexer;
use crate::parser;
use crate::semantic_analysis;
use crate::tacky;
use crate::code_gen;
use crate::emitter;

// TODO: move tag parsing to a separate function

pub fn compile_program(input: String, input_name: &String, outfile_name: &String, tags: Vec<String>, include_output: bool) -> String {
    let include_output = include_output || tags.contains(&String::from("-v")) || tags.contains(&String::from("--verbose"));
    let keep_asm = tags.contains(&String::from("-a")) || tags.contains(&String::from("--asm"));

    if include_output {
        println!("{} {}", "Compiling".bright_green(), input_name);
    }

    let input = preprocessor::preprocess(input, input_name, outfile_name, include_output);

    let lexer = lexer::Lexer::new(input.clone());
    let mut parser = parser::Parser::new(lexer);

    parser.input_name = input_name.clone();
    parser.error_func = Some(error);

    let program = parser.parse_program();

    if include_output {
        println!("{} {}", "Parsed".bright_green(), input_name);
    }

    let mut resolver = semantic_analysis::Analysis::new(program);
    let (program, symbol_table) = resolver.run();

    if include_output {
        println!("{} {}", "Resolved".bright_green(), input_name);
    }

    let mut tacky = tacky::Tacky::new(program);
    let program = tacky.generate();

    if include_output {
        println!("{} {}", "Tacky".bright_green(), input_name);
    }

    let mut compiler = code_gen::CodeGen::new(program, tacky.symbol_table, Some(input));
    let data_asm = compiler.generate_code();

    if include_output {
        println!("{} {}", "Generated".bright_green(), input_name);
    }
    
    let emitter = emitter::Emitter::new(data_asm);
    let code = emitter.emit();

    if include_output {
        println!("{} {}", "Emitted".bright_green(), input_name);
    }

    return code;
}

fn error(filename: String, input: String, error_message: String, line: usize, position: usize, length: usize, error_code: Option<i32>) {
    let lines = input.split('\n').collect::<Vec<&str>>();

    let error_line = lines[line];

    let trimmed_line = error_line.trim_start();
    let error_text = if line > 0 {
        let mut out = lines[line - 1].split_at(error_line.len()-trimmed_line.len()).1.to_string();
        out.push_str("\n");
        out.push_str(trimmed_line);
        out
    } else {
        trimmed_line.to_string()
    };

    let diff = error_line.len() - error_line.trim_start().len();

    let mut arrows = String::new();
    for _ in 0..(position - diff) {
        arrows.push_str(" ");
    }
    for _ in position..(position+length) {
        arrows.push_str("^")
    }

    let position = format!("--> {}:{}:{}", filename, line + 1, position + 1);
    
    println!("{}\n{}\n{}\n{}",
        error_message,
        position,
        error_text,
        arrows
    );

    let code = if error_code.is_some() {
        error_code.unwrap()
    } else { 1 };

    exit(code);
}