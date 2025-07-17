use core::fmt;
use std::{env, fs, str::FromStr};

use crate::target::Target;

mod generator;
mod lexer;
mod parser;
mod target;
mod token;

#[derive(Debug)]
enum InputError {
    NotEnoughArgs,
    InvalidArg(String),
    FileNotFound(String),
    InvalidTarget(String),
}

impl std::error::Error for InputError {}

impl fmt::Display for InputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InputError::NotEnoughArgs => write!(
                f,
                "Not enough arguments provided, use -h flag to see expected usage",
            ),
            InputError::InvalidArg(x) => write!(
                f,
                "Invalid argument {}, use -h flag to see expected usage",
                x
            ),
            InputError::FileNotFound(x) => write!(f, "Unable to find file path: {}", x),
            InputError::InvalidTarget(x) => write!(
                f,
                "Invalid target argument {}, available values are: [{}]",
                x,
                Target::get_values_string()
            ),
        }
    }
}

struct CmdArgs {
    file_name: String,
    target: Target,
}

fn main() {
    if let Err(e) = run() {
        println!("{}", e);
        std::process::exit(1)
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cmd_args = match process_cmd_args()? {
        Some(args) => args,
        None => return Ok(()),
    };
    let contents = read_file(&cmd_args.file_name)?;

    let tokens = lexer::parse_text(&contents);
    let program = parser::parse_program(tokens)?;
    let out_assembly = generator::generate(cmd_args.target, program);
    println!("Output Assembly:\n{}", out_assembly);
    Ok(())
}

fn process_cmd_args() -> Result<Option<CmdArgs>, InputError> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return Err(InputError::NotEnoughArgs);
    }

    if args[1] == "-h" {
        println!("Usage: ./mgs_lang [filename] -t ASSEMBLY_TARGET");
        Ok(None)
    } else {
        read_to_cmd_args(&args[1..]).map(|x| Some(x))
    }
}

fn read_to_cmd_args(args: &[String]) -> Result<CmdArgs, InputError> {
    let mut file_name: Option<String> = None;
    let mut target: Target = Target::ARM64;
    let mut provided_target = false;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-t" => {
                provided_target = true;
                if i == args.len() - 1 {
                    return Err(InputError::InvalidArg(args[i].clone()));
                }
                i += 1;
                target = Target::from_str(args[i].as_str())
                    .map_err(|_| InputError::InvalidTarget(args[i].clone()))?;
            }
            _ if file_name.is_none() => file_name = Some(args[i].clone()),
            _ => return Err(InputError::InvalidArg(args[i].clone())),
        }

        i += 1;
    }

    if !provided_target {
        println!("No target provided, using default {}", target);
    }

    if file_name.is_none() {
        Err(InputError::NotEnoughArgs)
    } else {
        Ok(CmdArgs {
            file_name: file_name.unwrap(),
            target: target,
        })
    }
}

fn read_file(file_path: &str) -> Result<String, InputError> {
    fs::read_to_string(file_path).or(Err(InputError::FileNotFound(file_path.to_string())))
}
