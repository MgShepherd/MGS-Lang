use crate::{
    parser::{Program, Statement},
    target::Target,
};

const PROG_PRELUDE: &str = "section .text\nglobal .start\n_start:\n";
const PROG_POSTLUDE: &str = "  mov x0, #0\n  mov x8, #93\n  svc #0\n";

pub fn generate(target: Target, program: Program) -> String {
    match target {
        Target::ARM64 => generate_arm(program),
    }
}

fn generate_arm(program: Program) -> String {
    let mut output = String::new();
    output.push_str(PROG_PRELUDE);

    output.push_str(&process_statements(program.statements));

    output.push_str(PROG_POSTLUDE);
    output
}

fn process_statements(statements: Vec<Statement>) -> String {
    let mut output = String::new();

    for statement in statements {
        let processed = match statement {
            Statement::AssignmentStatement { v_name, value } => {
                process_assignment_statement(v_name, value)
            }
        };
        output.push_str(&processed);
    }

    output
}

fn process_assignment_statement(_v_name: String, value: String) -> String {
    format!("  mov x0, #{}\n", value)
}

#[cfg(test)]
mod tests {
    use super::*;

    const PRELUDE: &str = "section .text\nglobal .start\n_start:\n";
    const POSTLUDE: &str = "  mov x0, #0\n  mov x8, #93\n  svc #0\n";

    #[test]
    fn should_generate_empty_assembly_program() {
        let output = generate(Target::ARM64, Program { statements: vec![] });
        starts_with_prelude(&output);
        ends_with_postlude(&output)
    }

    #[test]
    fn should_generate_assignment_statement() {
        let output = generate(
            Target::ARM64,
            Program {
                statements: vec![Statement::AssignmentStatement {
                    v_name: String::from("x"),
                    value: String::from("10"),
                }],
            },
        );

        starts_with_prelude(&output);
        contains_body(&output, "  mov x0, #10\n");
        ends_with_postlude(&output)
    }

    #[test]
    fn should_generate_multiple_statements() {
        let output = generate(
            Target::ARM64,
            Program {
                statements: vec![
                    Statement::AssignmentStatement {
                        v_name: String::from("x"),
                        value: String::from("10"),
                    },
                    Statement::AssignmentStatement {
                        v_name: String::from("y"),
                        value: String::from("32"),
                    },
                ],
            },
        );

        starts_with_prelude(&output);
        contains_body(&output, "  mov x0, #10\n  mov x0, #32\n");
        ends_with_postlude(&output)
    }

    fn starts_with_prelude(output: &str) {
        assert_eq!(&output[0..PRELUDE.len()], PRELUDE);
    }

    fn ends_with_postlude(output: &str) {
        assert_eq!(&output[(output.len() - POSTLUDE.len())..], POSTLUDE);
    }

    fn contains_body(output: &str, expected_body: &str) {
        assert_eq!(
            &output[PRELUDE.len()..(output.len() - POSTLUDE.len())],
            expected_body
        );
    }
}
