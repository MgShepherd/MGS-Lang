use core::fmt;
use std::{
    env,
    fs::{self, File},
    io::Write,
    str::FromStr,
};

use crate::target::Target;

#[derive(Debug)]
pub enum InputError {
    NotEnoughArgs,
    InvalidArg(String),
    FileNameParseError(String),
    FileNotFound(String),
    InvalidTarget(String),
    ContentWriteFailure(std::io::Error),
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
            InputError::FileNameParseError(x) => write!(f, "Unable to split file path: {}", x),
            InputError::InvalidTarget(x) => write!(
                f,
                "Invalid target argument {}, available values are: [{}]",
                x,
                Target::get_values_string()
            ),
            InputError::ContentWriteFailure(e) => {
                write!(f, "Failed to write output to file, caused by: {}", e)
            }
        }
    }
}

pub struct CmdArgs {
    pub file_name: String,
    pub target: Target,
}

impl CmdArgs {
    pub fn get_output_file(&self) -> Result<String, InputError> {
        let mut split_idx = 0;

        for (idx, char) in self.file_name.chars().rev().enumerate() {
            if char == '.' {
                split_idx = self.file_name.len() - 1 - idx;
                break;
            }
        }

        if split_idx > 1 {
            Ok(format!("{}.s", &self.file_name[0..split_idx]))
        } else {
            Err(InputError::FileNameParseError(self.file_name.clone()))
        }
    }
}

pub fn read_file(file_path: &str) -> Result<String, InputError> {
    fs::read_to_string(file_path).or(Err(InputError::FileNotFound(file_path.to_string())))
}

pub fn write_file(file_path: &str, content: &str) -> Result<(), InputError> {
    let mut file = File::create(file_path).map_err(|e| InputError::ContentWriteFailure(e))?;
    file.write_all(content.as_bytes())
        .map_err(|e| InputError::ContentWriteFailure(e))?;
    println!("Successfully wrote Assembly file {}", file_path);
    Ok(())
}

pub fn process_cmd_args() -> Result<Option<CmdArgs>, InputError> {
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
