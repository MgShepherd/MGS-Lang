use std::collections::HashMap;

use crate::{
    parser::{Program, Statement},
    target::Target,
};

const PROG_PRELUDE: &str = ".section .text\n.global _start\n_start:\n  mov x29, sp\n";
const PROG_POSTLUDE: &str = "  mov x0, #0\n  mov x8, #93\n  svc #0\n";
const STACK_VAR_OFFSET: usize = 16;

struct GenState {
    var_locs: HashMap<String, usize>,
}

impl GenState {
    fn new() -> GenState {
        return GenState {
            var_locs: HashMap::new(),
        };
    }
}

pub fn generate(target: &Target, program: Program) -> String {
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
    let mut state = GenState::new();

    for statement in statements {
        let processed = match statement {
            Statement::DeclarationStatement { v_name, value } => {
                process_declaration_statement(&mut state, v_name, value)
            }
            Statement::AssignmentStatement { v_name, value } => {
                process_assignment_statement(&state, v_name, value)
            }
        };
        output.push_str(&processed);
    }

    output
}

fn process_declaration_statement(state: &mut GenState, v_name: String, value: String) -> String {
    state.var_locs.insert(v_name, state.var_locs.len() + 1);
    // TODO: Properly handle value types here, since currently any string is accepted
    format!(
        "  mov x0, #{}\n  str x0, [sp, #-{}]!\n",
        value,
        state.var_locs.len() * STACK_VAR_OFFSET
    )
}

fn process_assignment_statement(state: &GenState, v_name: String, value: String) -> String {
    // TODO: Handle variable not found here, should be impossible due to parser but need to check
    let offset = state.var_locs.get(&v_name).unwrap_or(&0) * STACK_VAR_OFFSET;
    // TODO: Properly handle value types here, since currently any string is accepted
    format!("  mov x0, #{}\n  str x0, [x29, #-{}]\n", value, offset)
}

#[cfg(test)]
mod tests {
    use super::*;

    const PRELUDE: &str = ".section .text\n.global _start\n_start:\n  mov x29, sp\n";
    const POSTLUDE: &str = "  mov x0, #0\n  mov x8, #93\n  svc #0\n";

    #[test]
    fn should_generate_empty_assembly_program() {
        let output = generate(&Target::ARM64, Program { statements: vec![] });
        starts_with_prelude(&output);
        ends_with_postlude(&output)
    }

    #[test]
    fn should_generate_declaration_statement() {
        let output = generate(
            &Target::ARM64,
            Program {
                statements: vec![Statement::DeclarationStatement {
                    v_name: String::from("x"),
                    value: String::from("10"),
                }],
            },
        );

        starts_with_prelude(&output);
        contains_body(&output, "  mov x0, #10\n  str x0, [sp, #-16]!\n");
        ends_with_postlude(&output)
    }

    #[test]
    fn should_generate_multiple_declarations() {
        let output = generate(
            &Target::ARM64,
            Program {
                statements: vec![
                    Statement::DeclarationStatement {
                        v_name: String::from("x"),
                        value: String::from("10"),
                    },
                    Statement::DeclarationStatement {
                        v_name: String::from("y"),
                        value: String::from("32"),
                    },
                ],
            },
        );

        starts_with_prelude(&output);
        contains_body(
            &output,
            "  mov x0, #10\n  str x0, [sp, #-16]!\n  mov x0, #32\n  str x0, [sp, #-32]!\n",
        );
        ends_with_postlude(&output)
    }

    #[test]
    fn should_generate_assignment_statement() {
        let output = generate(
            &Target::ARM64,
            Program {
                statements: vec![
                    Statement::DeclarationStatement {
                        v_name: String::from("x"),
                        value: String::from("10"),
                    },
                    Statement::AssignmentStatement {
                        v_name: String::from("x"),
                        value: String::from("32"),
                    },
                ],
            },
        );

        starts_with_prelude(&output);
        contains_body(
            &output,
            "  mov x0, #10\n  str x0, [sp, #-16]!\n  mov x0, #32\n  str x0, [x29, #-16]\n",
        );
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
