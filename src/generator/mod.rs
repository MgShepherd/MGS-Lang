#[cfg(test)]
mod tests;

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
        "{}\n  str x0, [sp, #-{}]!\n",
        generate_expression(state, &expr, 0)?,
        STACK_VAR_OFFSET
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
        "{}\n  str x0, [x29, #-{}]\n",
        generate_expression(state, &expr, 0)?,
        offset
    ))
}

fn generate_expression(
    state: &GenState,
    expr: &Expression,
    target_reg: usize,
) -> Result<String, GenError> {
    match expr {
        Expression::ValExpr(x) => Ok(format!("  mov x{}, #{}", target_reg, x)),
        Expression::VarExpr(x) => {
            let location = state
                .var_locs
                .get(x)
                .ok_or(GenError::from_undefined_var(x.clone()))?;
            let offset = location * STACK_VAR_OFFSET;
            Ok(format!("  ldr x{}, [x29, #-{}]", target_reg, offset))
        }
        Expression::ArithmeticExpr(x, op, y) => {
            let x_expr = generate_expression(state, &x, target_reg + 1)?;
            let y_expr = generate_expression(state, &y, target_reg + 2)?;
            Ok(format!(
                "{}\n{}\n  {} x{}, x{}, x{}",
                x_expr,
                y_expr,
                op.to_arm_command(),
                target_reg,
                target_reg + 1,
                target_reg + 2
            ))
        }
        Expression::BooleanExpr(_x, _op, _y) => Ok(format!("Boolean expression not implemented")),
    }
}
