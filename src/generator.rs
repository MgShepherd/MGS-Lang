use std::collections::HashMap;

use crate::{
    parser::{Expression, Program, Statement},
    target::Target,
};

const PROG_PRELUDE: &str = ".section .text\n.global _start\n_start:\n  mov x29, sp\n";
const PROG_POSTLUDE: &str = "  mov x0, #0\n  mov x8, #93\n  svc #0\n";
const STACK_VAR_OFFSET: usize = 16;

#[derive(Debug)]
pub enum GenInternalError {
    UndefinedVariable(String),
}

#[derive(Debug)]
pub enum GenError {
    UnexpectedInternalError(GenInternalError),
}

impl std::error::Error for GenInternalError {}
impl std::error::Error for GenError {}

impl std::fmt::Display for GenInternalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenInternalError::UndefinedVariable(x) => {
                write!(f, "Undefined variable: {}", x)
            }
        }
    }
}

impl std::fmt::Display for GenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenError::UnexpectedInternalError(x) => {
                write!(
                    f,
                    "Unexpected error occurred during program generation: {}",
                    x
                )
            }
        }
    }
}

impl GenError {
    fn from_undefined_var(v_name: String) -> Self {
        GenError::UnexpectedInternalError(GenInternalError::UndefinedVariable(v_name))
    }
}

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

pub fn generate(target: &Target, program: Program) -> Result<String, GenError> {
    match target {
        Target::ARM64 => generate_arm(program),
    }
}

fn generate_arm(program: Program) -> Result<String, GenError> {
    let mut output = String::new();
    output.push_str(PROG_PRELUDE);

    output.push_str(&process_statements(program.statements)?);

    output.push_str(PROG_POSTLUDE);
    Ok(output)
}

fn process_statements(statements: Vec<Statement>) -> Result<String, GenError> {
    let mut output = String::new();
    let mut state = GenState::new();

    for statement in statements {
        let processed = match statement {
            Statement::DeclarationStatement { v_name, expr } => {
                process_declaration_statement(&mut state, v_name, expr)?
            }
            Statement::AssignmentStatement { v_name, expr } => {
                process_assignment_statement(&state, v_name, expr)?
            }
        };
        output.push_str(&processed);
    }

    Ok(output)
}

fn process_declaration_statement(
    state: &mut GenState,
    v_name: String,
    expr: Expression,
) -> Result<String, GenError> {
    state.var_locs.insert(v_name, state.var_locs.len() + 1);
    Ok(format!(
        "  mov x0, #{}\n  str x0, [sp, #-{}]!\n",
        expr,
        state.var_locs.len() * STACK_VAR_OFFSET
    ))
}

fn process_assignment_statement(
    state: &GenState,
    v_name: String,
    expr: Expression,
) -> Result<String, GenError> {
    let location = state
        .var_locs
        .get(&v_name)
        .ok_or(GenError::from_undefined_var(v_name.clone()))?;
    let offset = location * STACK_VAR_OFFSET;
    Ok(format!(
        "  mov x0, #{}\n  str x0, [x29, #-{}]\n",
        expr, offset
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    const PRELUDE: &str = ".section .text\n.global _start\n_start:\n  mov x29, sp\n";
    const POSTLUDE: &str = "  mov x0, #0\n  mov x8, #93\n  svc #0\n";

    #[test]
    fn should_generate_empty_assembly_program() {
        let output = generate(&Target::ARM64, Program { statements: vec![] }).unwrap();
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
                    expr: Expression::ValExpr(String::from("10")),
                }],
            },
        )
        .unwrap();

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
                        expr: Expression::ValExpr(String::from("10")),
                    },
                    Statement::DeclarationStatement {
                        v_name: String::from("y"),
                        expr: Expression::ValExpr(String::from("32")),
                    },
                ],
            },
        )
        .unwrap();

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
                        expr: Expression::ValExpr(String::from("10")),
                    },
                    Statement::AssignmentStatement {
                        v_name: String::from("x"),
                        expr: Expression::ValExpr(String::from("32")),
                    },
                ],
            },
        )
        .unwrap();

        starts_with_prelude(&output);
        contains_body(
            &output,
            "  mov x0, #10\n  str x0, [sp, #-16]!\n  mov x0, #32\n  str x0, [x29, #-16]\n",
        );
        ends_with_postlude(&output)
    }

    #[test]
    fn should_return_err_when_assigning_to_undefined_var() {
        let output = generate(
            &Target::ARM64,
            Program {
                statements: vec![Statement::AssignmentStatement {
                    v_name: String::from("x"),
                    expr: Expression::ValExpr(String::from("32")),
                }],
            },
        )
        .unwrap_err();

        assert_eq!(
            output.to_string(),
            "Unexpected error occurred during program generation: Undefined variable: x"
        );
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
