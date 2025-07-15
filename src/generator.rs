use crate::{parser::Program, target::Target};

pub fn generate(target: Target, program: Program) {
    match target {
        Target::ARM64 => generate_arm(program),
    }
}

fn generate_arm(_program: Program) {
    println!("Generating ARM Code");
}
