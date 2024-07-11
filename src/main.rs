#![allow(unused_variables)]

use nix::unistd::execvp;
use std::{ffi::{CStr, CString}, process::exit};
use colored::Colorize;

mod lexer;
mod parser;
mod semantic_analysis;
mod tacky;
mod code_gen;
mod emitter;

fn help(err_code: i32) {
    println!(
"Usage: 
    argent run <filename> - to run the file
    argent build <filename> - to compile the file
    argent help - to show this message");

    std::process::exit(err_code);
}

fn compile_program(input: String, input_name: String, outfile_name: &String, tags: Vec<String>) {
    let include_output = tags.contains(&String::from("-v")) || tags.contains(&String::from("--verbose"));
    let keep_asm = tags.contains(&String::from("-a")) || tags.contains(&String::from("--asm"));

    println!("{} {}", "Compiling".bright_green(), input_name);


    let lexer = lexer::Lexer::new(input.clone());
    let mut parser = parser::Parser::new(lexer);

    parser.input_name = input_name.clone();
    parser.error_func = Some(error);

    let program = parser.parse_program();

    if include_output {
        println!("{} {}", "Parsed".bright_green(), input_name);
    }

    let mut resolver = semantic_analysis::Analysis::new(program.clone());
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
    let assembly_asm = compiler.generate_code();

    if include_output {
        println!("{} {}", "Generated".bright_green(), input_name);
    }
    
    let emitter = emitter::Emitter::new(assembly_asm);
    let code = emitter.emit();

    if include_output {
        println!("{} {}", "Emitted".bright_green(), input_name);
    }

    // write to file
    let dir = std::fs::create_dir("output");
    let asm_write_res = std::fs::write("output/temp.s", code);

    if asm_write_res.is_err() {
        eprintln!("Failed to write to file: {}", asm_write_res.err().unwrap());
        std::process::exit(1);
    }

    // assemble
    let output = std::process::Command::new("gcc")
        .arg("output/temp.s")
        .arg("-o")
        .arg(outfile_name)
        .output()
        .expect("failed to assemble");

    if output.status.code().unwrap() != 0 {
        eprintln!("Failed to assemble: {}", output.status.code().unwrap());
        std::process::exit(1);
    }

    if keep_asm {
        // remove temp file
        let remove_res = std::fs::remove_file("output/temp.s");
    }

    println!("{} {}", "Finished".bright_green(), input_name);

    let stdout = std::str::from_utf8(&output.stdout).unwrap();
    let stderr = std::str::from_utf8(&output.stderr).unwrap();

    if stdout.len() > 0 {
        println!("{}", std::str::from_utf8(&output.stdout).unwrap());
    }
    if stderr.len() > 0 {
        println!("{}", std::str::from_utf8(&output.stderr).unwrap());
        std::process::exit(output.status.code().unwrap());
    }
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

fn main() {
    // collect all the "tags" (-a, --abc)
    let args: Vec<_> = std::env::args().collect();

    let mut tags: Vec<String> = Vec::new();

    // get tags
    for arg in args.iter() {
        if arg.starts_with("-") {
            if arg.starts_with("--") {
                tags.push(arg.clone());
            } else {
                let mut chars = arg.chars();
                chars.next();
                for c in chars {
                    tags.push(format!("-{}", c));
                }
            }
        }
    }

    println!("{} {}", "Argent".bright_green(), "v0.2.2");
    
    let mut iter = args.iter().skip(1);
    let mut i = 0;
    while i < iter.len() {
        let op_arg = iter.next();
        if op_arg.is_none() {
            break;
        }
        let arg = op_arg.unwrap();
        match arg.as_str() {
            "help" => {
                help(0);
                panic!();
            },
            "run" => {
                let op_filename = args.get(i + 2);
                if op_filename.is_none() {
                    println!("No file specified");
                    help(2);
                    panic!();
                }

                let filename = op_filename.unwrap();
                let copied_filename = filename.clone();

                let outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
                let op_input = std::fs::read_to_string(filename);

                if op_input.is_err() {
                    println!("Failed to read file: {}", filename);
                    help(2);
                    panic!();
                }

                let input = op_input.unwrap();

                compile_program(input, copied_filename, &outfile_name, tags.clone());

                println!("{} {}", "Running".bright_green(), outfile_name);

                let outfile_name_cstr = CString::new(outfile_name).expect("CString::new failed");
                let err = execvp::<&CStr>(&outfile_name_cstr, &[]);

                match err {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("failed to exec: {}", e);
                        std::process::exit(1);
                    }
                }
            },
            "build" => {
                let op_filename = std::env::args().nth(2);
                if op_filename.is_none() {
                    println!("No file specified");
                    help(2);
                    panic!();
                }

                let filename = op_filename.unwrap();
                let copied_filename = filename.clone();

                let outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
                let op_input = std::fs::read_to_string(filename.clone());

                if op_input.is_err() {
                    println!("Failed to read file: {}", filename);
                    help(2);
                    panic!();
                }

                let input = op_input.unwrap();

                compile_program(input, copied_filename, &outfile_name, tags.clone());
            },
            "new" => {
                let op_filename = std::env::args().nth(2);
                let filename = if op_filename.is_none() {
                    "main.ag".to_string()
                } else {
                    op_filename.unwrap()
                };

                let new_file_contents = 
"fn main(argc: i32) -> i32 {
    return 0;
}";
                
                // create new file
                let new_file = format!("{}\n", new_file_contents);
                std::fs::write(filename.clone(), new_file).unwrap();

                println!("{} {}", "Created".bright_green(), filename);
                println!("Run '{}{}' to compile and run the file", "argent run ".bright_black(), filename.green());
            }
            _ => {
                println!("Unknown command: {}", arg);
                help(2);
                panic!();
            },
        }
        i += 1;
    }
}
