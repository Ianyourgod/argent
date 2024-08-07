#![allow(unused_variables)]

use colored::Colorize;
mod lexer;
mod parser;
mod semantic_analysis;
mod tacky;
mod code_gen;
mod emitter;
mod compile;
mod preprocessor;

fn help(err_code: i32) {
    println!(
"Usage: 
    argent run <filename> - to run the file
    argent build <filename> - to compile the file
    argent help - to show this message");

    std::process::exit(err_code);
}

fn main() {
    // collect all the "tags" (-a, --abc)
    let args: Vec<String> = std::env::args().collect();

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

                let outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
                let op_input = std::fs::read_to_string(filename);

                if op_input.is_err() {
                    println!("Failed to read file: {}", filename);
                    help(2);
                    panic!();
                }

                let input = op_input.unwrap();

                let code = compile::compile_program(input, filename, &outfile_name, tags.clone(), true);

                let dir = std::fs::create_dir("output");
                let asm_write_res = std::fs::write("output/temp.as", code).expect("Failed to write to file");
            },
            "build" => {
                let op_filename = std::env::args().nth(2);
                if op_filename.is_none() {
                    println!("No file specified");
                    help(2);
                    panic!();
                }

                let filename = op_filename.unwrap();

                let outfile_name = "output/".to_string() + &filename.split('.').collect::<Vec<&str>>()[0].to_string();
                let op_input = std::fs::read_to_string(&filename);

                if op_input.is_err() {
                    println!("Failed to read file: {}", filename);
                    help(2);
                    panic!();
                }

                let input = op_input.unwrap();

                let code = compile::compile_program(input, &filename, &outfile_name, tags.clone(), true);

                let dir = std::fs::create_dir("output");
                let asm_write_res = std::fs::write("output/temp.as", code).expect("Failed to write to file");
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
                std::fs::write(&filename, new_file).unwrap();

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
