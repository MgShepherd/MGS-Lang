use core::fmt;
use std::{
    env,
    fs::{self, File},
    io::Write,
    str::FromStr,
};

use crate::target::Target;

const BUILD_FOLDER: &'static str = "./build";

#[derive(Debug)]
pub enum InputError {
    NotEnoughArgs,
    InvalidArg(String),
    FileNameParseError(String),
    FileNotFound(String),
    InvalidTarget(String),
    ContentWriteFailure(Box<dyn std::error::Error>),
    ExecutableGenerationFailure(String),
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
            InputError::ExecutableGenerationFailure(x) => {
                write!(f, "Failed to generate executable file due to {}", x)
            }
        }
    }
}

pub struct CmdArgs {
    pub file_name: String,
    pub target: Target,
}

impl CmdArgs {
    pub fn get_file_name(&self) -> Result<String, InputError> {
        let mut split_idx = 0;
        let mut last_slash = self.file_name.len();

        for (idx, char) in self.file_name.chars().rev().enumerate() {
            if char == '.' {
                split_idx = self.file_name.len() - 1 - idx;
            }
            if last_slash == self.file_name.len() && char == '/' {
                last_slash = self.file_name.len() - 1 - idx;
            }
        }

        if split_idx > 1 {
            let start_idx = if last_slash != self.file_name.len() {
                last_slash + 1
            } else {
                0
            };

            Ok(format!("{}", &self.file_name[start_idx..split_idx]))
        } else {
            Err(InputError::FileNameParseError(self.file_name.clone()))
        }
    }
}

pub fn read_file(file_path: &str) -> Result<String, InputError> {
    fs::read_to_string(file_path).or(Err(InputError::FileNotFound(file_path.to_string())))
}

pub fn write_program(file_name: &str, content: &str) -> Result<(), InputError> {
    if !std::path::Path::new(BUILD_FOLDER).exists() {
        fs::create_dir(BUILD_FOLDER).map_err(|e| InputError::ContentWriteFailure(e.into()))?;
    }

    let assembly_path = format!("{}/{}.s", BUILD_FOLDER, file_name);
    let mut file =
        File::create(&assembly_path).map_err(|e| InputError::ContentWriteFailure(e.into()))?;
    file.write_all(content.as_bytes())
        .map_err(|e| InputError::ContentWriteFailure(e.into()))?;

    let object_path = format!("{}/{}.o", BUILD_FOLDER, file_name);
    generate_object_file(&object_path, assembly_path)?;
    generate_executable_file(file_name, object_path)?;
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

fn generate_object_file(object_path: &str, assembly_path: String) -> Result<(), InputError> {
    let result = std::process::Command::new("aarch64-linux-gnu-as")
        .args([
            assembly_path,
            "-o".to_string(),
            object_path.to_string(),
            "-g".to_string(),
        ])
        .output()
        .map_err(|e| InputError::ContentWriteFailure(e.into()))?;
    let stderr_str =
        String::from_utf8(result.stderr).map_err(|e| InputError::ContentWriteFailure(e.into()))?;
    if stderr_str.len() > 0 {
        Err(InputError::ExecutableGenerationFailure(stderr_str))
    } else {
        Ok(())
    }
}

fn generate_executable_file(file_name: &str, object_path: String) -> Result<(), InputError> {
    let executable_path = format!("{}/{}", BUILD_FOLDER, file_name);
    let result = std::process::Command::new("aarch64-linux-gnu-ld")
        .args([object_path, "-o".to_string(), executable_path])
        .output()
        .map_err(|e| InputError::ContentWriteFailure(e.into()))?;
    let stderr_str =
        String::from_utf8(result.stderr).map_err(|e| InputError::ContentWriteFailure(e.into()))?;
    if stderr_str.len() > 0 {
        Err(InputError::ExecutableGenerationFailure(stderr_str))
    } else {
        Ok(())
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
