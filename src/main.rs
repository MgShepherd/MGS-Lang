use core::fmt;
use std::{env, fs};

mod lexer;
mod token;

enum InputError {
    NotEnoughArgs,
    FileNotFound(String),
}

impl fmt::Display for InputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InputError::NotEnoughArgs => write!(
                f,
                "Not enough arguments provided, use -h flag to see expected usage",
            ),
            InputError::FileNotFound(x) => write!(f, "Unable to find file path: {}", x),
        }
    }
}

struct CmdArgs {
    file_name: String,
}

fn main() {
    let cmd_args = match process_cmd_args() {
        Ok(cmd_args) => match cmd_args {
            Some(args) => args,
            None => return,
        },
        Err(error) => panic!("Problem processing arguments: {error}"),
    };
    let contents = match read_file(&cmd_args.file_name) {
        Ok(contents) => contents,
        Err(error) => panic!("Problem reading input file: {error}"),
    };

    let tokens = lexer::parse_text(&contents);
    lexer::display_tokens(&tokens);
}

fn process_cmd_args() -> Result<Option<CmdArgs>, InputError> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return Err(InputError::NotEnoughArgs);
    }

    if args[1] == "-h" {
        println!("Usage: ./mgs_lang [filename]");
        Ok(None)
    } else {
        Ok(Some(CmdArgs {
            file_name: args[1].clone(),
        }))
    }
}

fn read_file(file_path: &str) -> Result<String, InputError> {
    fs::read_to_string(file_path).or(Err(InputError::FileNotFound(file_path.to_string())))
}
