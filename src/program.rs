//! Parsing and executing dice expressions.

use std::rc::Rc;

use codespan_reporting::files::SimpleFiles;
use rand::RngCore;

use crate::{
    dice::{Die, FateDie, SimpleDie, Value},
    errors::ProgramError,
    output::Output,
    spans::{FileName, Files, ProgramDiagnostics, Span},
};

pub enum Expression {
    Die(Rc<dyn Die>),
}

impl Expression {
    fn execute(&self, rng: &mut dyn RngCore) -> Result<Output, ProgramError> {
        match self {
            Expression::Die(die) => Ok(Output::Roll(die.clone().roll(rng))),
        }
    }
}

/// A dice program, which can be executed to produce output.
pub struct Program {
    files: Files,
    expression: Expression,
}

impl Program {
    /// Parse a string, returning a program.
    pub fn parse(file_name: FileName, dice_expr: &str) -> Result<Self, ProgramDiagnostics> {
        let mut files = SimpleFiles::new();
        let file_id = files.add(file_name, dice_expr.to_owned());

        let result = program_parser::program(dice_expr);
        let files = Rc::new(files);
        match result {
            Ok(expression) => Ok(Program { files, expression }),
            Err(err) => {
                let offset = err.location.offset;
                Err(ProgramDiagnostics::from_program_error(
                    files.clone(),
                    ProgramError::ParseError {
                        span: Span::new(file_id, offset..offset),
                        message: format!("expected {}", err.expected),
                    },
                ))
            }
        }
    }

    /// Execute this program.
    pub(crate) fn execute(&self, rng: &mut dyn RngCore) -> Result<Output, ProgramDiagnostics> {
        match self.expression.execute(rng) {
            Ok(output) => Ok(output),
            Err(err) => {
                let files = self.files.clone();
                Err(ProgramDiagnostics::from_program_error(files, err))
            }
        }
    }
}

peg::parser! {
    grammar program_parser() for str {
        pub rule program() -> Expression = expression:expression();

        rule expression() -> Expression = die:die() { Expression::Die(die) };

        rule die() -> Rc<dyn Die>
            = "dF" { FateDie::new() }
            / "d" faces:number() {? SimpleDie::new(faces).map_err(|_| "the number of faces on a die must be greater than 0") }

        rule number() -> Value
            = quiet! { n:$(['0'..='9']+) {? n.parse().or(Err(concat!("expected an integer no larger than ", stringify!(Value::MAX)))) } }
            / expected!("a number")
    }
}
