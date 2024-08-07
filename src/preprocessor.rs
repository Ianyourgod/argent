use colored::Colorize;

pub fn preprocess(input: String, input_name: &String, outfile_name: &String, include_output: bool) -> String {
    let mut output = String::new();
    let mut lines = input.lines();
    while let Some(line) = lines.next() {
        if line.starts_with("#include") {
            let mut parts = line.split_whitespace();
            parts.next();
            let file = parts.next().unwrap();
            let file = file.trim_matches(|c| c == '"' || c == '\'');
            let file = std::fs::read_to_string(file).expect("Failed to read file");
            output.push_str(&file);
            output.push('\n');
        } else {
            output.push_str(line);
            output.push('\n');
        }
    }

    if include_output {
        println!("{} {}", "Preprocessed".bright_green(), input_name);
    }

    return output;
}