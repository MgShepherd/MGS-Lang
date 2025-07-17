use crate::{parser::Program, target::Target};

pub fn generate(target: Target, program: Program) -> String {
    match target {
        Target::ARM64 => generate_arm(program),
    }
}

fn generate_arm(_program: Program) -> String {
    let mut output = String::new();
    output.push_str(generate_prog_prelude());

    output.push_str(generate_prog_postlude());
    output
}

fn generate_prog_prelude() -> &'static str {
    "section .text\nglobal .start\n_start:\n"
}

fn generate_prog_postlude() -> &'static str {
    "  mov x0, #0\n  mov x8, #93\n  svc #0\n"
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn should_generate_empty_assembly_program() {
        let output = generate(Target::ARM64, Program { statements: vec![] });
        starts_with_prelude(&output);
        ends_with_postlude(&output)
    }

    fn starts_with_prelude(output: &str) {
        let prelude = "section .text\nglobal .start\n_start:\n";
        assert_eq!(&output[0..prelude.len()], prelude);
    }

    fn ends_with_postlude(output: &str) {
        let postlude = "  mov x0, #0\n  mov x8, #93\n  svc #0\n";
        assert_eq!(&output[(output.len() - postlude.len())..], postlude);
    }
}
