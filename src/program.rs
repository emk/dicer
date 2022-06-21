//! Parsing and executing dice expressions.

use std::{fmt, io, rc::Rc};

use codespan_reporting::files::SimpleFiles;
use rand::RngCore;

use crate::{
    dice::{Die, FateDie, SimpleDie, Value},
    errors::{ProgramDiagnostics, ProgramError},
    expressions::{Binop, DiceExpr, Evaluate, Expr, RollAll, RollsExpr},
    pretty::{PrettyFormat, WriteColor},
    spans::{FileName, Files, Span},
};

/// A dice program, which can be executed to produce output.
#[derive(Debug)]
pub struct Program<D: fmt::Debug + Eq + 'static> {
    files: Files,
    expr: Rc<Expr<D>>,
}

impl Program<DiceExpr> {
    /// Parse a string, returning a program.
    pub fn parse(file_name: FileName, dice_expr: &str) -> Result<Self, ProgramDiagnostics> {
        let mut files = SimpleFiles::new();
        let file_id = files.add(file_name, dice_expr.to_owned());

        let result = program_parser::program(dice_expr);
        let files = Rc::new(files);
        match result {
            Ok(expr) => Ok(Program { files, expr }),
            Err(err) => {
                let offset = err.location.offset;
                Err(ProgramDiagnostics::from_program_error(
                    files.clone(),
                    ProgramError::Parse {
                        span: Span::new(file_id, offset..offset),
                        message: format!("expected {}", err.expected),
                    },
                ))
            }
        }
    }

    /// Roll all the dice specified by our program.
    pub fn roll_all(&self, rng: &mut dyn RngCore) -> Program<RollsExpr> {
        let expr = self.expr.clone().roll_all(rng);
        Program::<RollsExpr> {
            files: self.files.clone(),
            expr: Rc::new(expr),
        }
    }
}

impl Program<RollsExpr> {
    /// Return the result of our program.
    pub fn evaluate(&self) -> Result<Value, ProgramDiagnostics> {
        self.expr
            .evaluate()
            .map_err(|err| ProgramDiagnostics::from_program_error(self.files.clone(), err))
    }

    /// The "evaluate" and "print" portions of a classic read-eval-print loop.
    pub fn evaluate_and_pretty_format(
        &self,
        writer: &mut dyn WriteColor,
    ) -> Result<(), ProgramDiagnostics> {
        let value = self.evaluate()?;
        self.pretty_format(writer)
            .map_err(ProgramDiagnostics::from_error)?;
        write!(writer, " = {value}").map_err(ProgramDiagnostics::from_error)?;
        Ok(())
    }
}

impl<D: fmt::Debug + Eq + PrettyFormat + 'static> PrettyFormat for Program<D> {
    fn pretty_format(&self, writer: &mut dyn WriteColor) -> Result<(), io::Error> {
        self.expr.pretty_format(writer)
    }
}

peg::parser! {
    grammar program_parser() for str {
        pub rule program() -> Rc<Expr<DiceExpr>> = _? expression:expression() _? {
            expression
        }

        rule expression() -> Rc<Expr<DiceExpr>> = precedence!{
            e1:(@) _? "+" _? e2:@ { Rc::new(Expr::Binop(Binop::Add, e1, e2)) }
            e1:(@) _? "-" _? e2:@ { Rc::new(Expr::Binop(Binop::Sub, e1, e2)) }
            --
            "(" _? e:expression() _? ")" { e }
            dice:dice() { Rc::new(Expr::Dice(dice)) }
            value:value() { Rc::new(Expr::Constant(value)) }
        }

        rule dice() -> Rc<DiceExpr>
            = die:die() { Rc::new(DiceExpr::Dice { count: 1, die }) }
            / count:count() die:die() { Rc::new(DiceExpr::Dice { count, die }) }

        rule die() -> Rc<dyn Die>
            = "dF" { FateDie::new() }
            / "d" faces:value() {? SimpleDie::new(faces).map_err(|_| "the number of faces on a die must be greater than 0") }

        rule value() -> Value
            = quiet! { n:$("-"? ['0'..='9']+) {? n.parse().or(Err(concat!("expected an integer no larger than ", stringify!(Value::MAX)))) } }
            / expected!("a number")

        rule count() -> u64
            = quiet! { n:$(['0'..='9']+) {? n.parse().or(Err(concat!("expected an integer no larger than ", stringify!(u64::MAX)))) } }
            / expected!("a number")

        rule _ = quiet!{[' ' | '\n' | '\t']+}
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rand::SeedableRng;
    use rand_chacha::ChaCha8Rng;

    use super::*;
    use crate::markdown_writer::MarkdownWriter;

    #[test]
    fn programs_print_expected_results() {
        let examples = &[
            // These tests can't be reordered without changing the rolls and
            // sums, because they share a seeded RNG.
            ("d6", "d6 (5) = 5"),
            ("d12", "d12 (10) = 10"),
            ("d20", "d20 (19) = 19"),
            ("dF", "dF (+) = 1"),
            ("2d6", "2d6 (3 3) = 6"),
            ("2d6+3", "2d6 (4 6) + 3 = 13"),
            ("4dF", "4dF (0 0 + +) = 2"),
            (" 1d6 + 1 ", "d6 (1) + 1 = 2"),
            ("2d6 - 2", "2d6 (2 3) - 2 = 3"),
            ("-1", "-1 = -1"),                  // Found by proptest!
            ("0 + (0 + 0)", "0 + (0 + 0) = 0"), // Found by proptest!
            ("(0 + 0) + 0", "0 + 0 + 0 = 0"),   // No parens required.
        ][..];

        let mut rng = ChaCha8Rng::seed_from_u64(28);
        for (idx, &(program, expected)) in examples.iter().enumerate() {
            let file_name = FileName::Arg(idx + 1);
            let program = Program::parse(file_name, program).unwrap();
            let rolled = program.roll_all(&mut rng);
            let mut wtr = MarkdownWriter::new(vec![]);
            rolled.evaluate_and_pretty_format(&mut wtr).unwrap();
            assert_eq!(wtr.into_string_lossy(), expected);
        }
    }

    proptest! {
        /// This is a very powerful test case which makes sure that our parser
        /// and our formatter work together well. It has generated several test
        /// cases for `programs_print_expected_results`.
        #[test]
        fn formatting_and_parsing_returns_original_expression(expr in any::<Rc<Expr<DiceExpr>>>()) {
            let mut wtr = MarkdownWriter::new(vec![]);
            expr.pretty_format(&mut wtr).unwrap();
            let src = wtr.into_string_lossy();
            let parsed = Program::parse(FileName::Arg(1), &src).unwrap();
            assert_eq!(parsed.expr, expr);
        }
    }
}
