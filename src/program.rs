//! Parsing and executing dice expressions.

use std::{fmt, rc::Rc};

use codespan_reporting::files::SimpleFiles;
use rand::RngCore;

use crate::{
    dice::{Die, FateDie, SimpleDie, Value},
    errors::ProgramError,
    output::Output,
    spans::{FileName, Files, ProgramDiagnostics, Span},
};

pub enum Expression {
    Dice { count: u64, die: Rc<dyn Die> },
    Constant(Value),
    Add(Rc<Expression>, Rc<Expression>),
}

impl Expression {
    fn execute(self: &Rc<Expression>, rng: &mut dyn RngCore) -> Result<Output, ProgramError> {
        match &**self {
            Expression::Dice { count, die } => {
                let mut rolls = vec![];
                for _ in 0..*count {
                    rolls.push(die.to_owned().roll(rng));
                }
                Ok(Output::Rolls {
                    expression: self.to_owned(),
                    rolls,
                })
            }
            Expression::Constant(value) => Ok(Output::Constant(*value)),
            Expression::Add(e1, e2) => {
                let o1 = e1.execute(rng)?;
                let o2 = e2.execute(rng)?;
                Ok(Output::Add(Rc::new(o1), Rc::new(o2)))
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Dice { count, die } => {
                if *count != 1 {
                    write!(f, "{}", count)?;
                }
                write!(f, "{}", die)?;
            }
            Expression::Constant(value) => write!(f, "{}", value)?,
            Expression::Add(e1, e2) => {
                write!(f, "{} + {}", e1, e2)?;
            }
        }
        Ok(())
    }
}

/// A dice program, which can be executed to produce output.
pub struct Program {
    files: Files,
    expression: Rc<Expression>,
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
        pub rule program() -> Rc<Expression> = expression:expression()

        rule expression() -> Rc<Expression> = precedence!{
            e1:(@) "+" e2:@ { Rc::new(Expression::Add(e1, e2)) }
            --
            dice:dice() { dice }
            value:value() { Rc::new(Expression::Constant(value)) }
        }

        rule dice() -> Rc<Expression>
            = die:die() { Rc::new(Expression::Dice { count: 1, die }) }
            / count:count() die:die() { Rc::new(Expression::Dice { count, die }) }

        rule die() -> Rc<dyn Die>
            = "dF" { FateDie::new() }
            / "d" faces:value() {? SimpleDie::new(faces).map_err(|_| "the number of faces on a die must be greater than 0") }

        rule value() -> Value
            = quiet! { n:$(['0'..='9']+) {? n.parse().or(Err(concat!("expected an integer no larger than ", stringify!(Value::MAX)))) } }
            / expected!("a number")

        rule count() -> u64
            = quiet! { n:$(['0'..='9']+) {? n.parse().or(Err(concat!("expected an integer no larger than ", stringify!(u64::MAX)))) } }
            / expected!("a number")
    }
}

#[cfg(test)]
mod tests {
    use rand::SeedableRng;
    use rand_chacha::ChaCha8Rng;

    use super::*;
    use crate::markdown_writer::MarkdownWriter;

    #[test]
    fn programs_print_expected_results() {
        let examples = &[
            ("d6", "d6 (5) = 5"),
            ("d12", "d12 (10) = 10"),
            ("d20", "d20 (19) = 19"),
            ("dF", "dF (+) = 1"),
            ("2d6", "2d6 (3 3) = 6"),
            ("2d6+3", "2d6 (4 6) + 3 = 13"),
        ][..];

        let mut rng = ChaCha8Rng::seed_from_u64(28);
        for (idx, &(program, expected)) in examples.iter().enumerate() {
            let file_name = FileName::Arg(idx + 1);
            let program = Program::parse(file_name, program).unwrap();
            let output = program.execute(&mut rng).unwrap();
            let mut wtr = MarkdownWriter::new(vec![]);
            output.pretty_format_with_value(&mut wtr).unwrap();
            assert_eq!(wtr.into_string_lossy(), expected);
        }
    }
}
