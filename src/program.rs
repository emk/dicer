//! Parsing and executing dice expressions.

use std::{fmt, io, rc::Rc};

use codespan_reporting::files::SimpleFiles;
use rand::RngCore;

use crate::{
    annotations::{Annotation, Annotations},
    dice::{Die, FateDie, SimpleDie},
    errors::{ProgramDiagnostics, ProgramError},
    expressions::{Binop, DiceExpr, Evaluate, Expr, RollAll, RollsExpr},
    pretty::{PrettyFormat, WriteColor},
    spans::{FileId, FileName, Files, Span},
    values::{Number, Values},
};

/// A dice program, which can be executed to produce output.
#[derive(Debug)]
pub struct Program<D: fmt::Debug + Eq + 'static> {
    /// Source code for this program, for use in error reporting.
    files: Files,
    /// The `Expr` making up this program.
    expr: Rc<Expr<D>>,
}

impl Program<DiceExpr> {
    /// Parse a string, returning a program.
    pub fn parse(file_name: FileName, source: &str) -> Result<Self, ProgramDiagnostics> {
        let mut files = SimpleFiles::new();
        let file_id = files.add(file_name, source.to_owned());

        let result = program_parser::program(source, file_id);
        let files = Files::new(files);
        match result {
            Ok(expr) => Ok(Program { files, expr }),
            Err(err) => {
                let offset = err.location.offset;
                Err(ProgramDiagnostics::from_program_error(
                    files,
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
    pub fn evaluate(&self) -> Result<Values, ProgramDiagnostics> {
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
        pub rule program(file_id: FileId) -> Rc<Expr<DiceExpr>> = _? expression:expression(file_id) _? {
            expression
        }

        rule expression(file_id: FileId) -> Rc<Expr<DiceExpr>> = precedence!{
            // We cannot use `position!` here at all, so we need to get tricky.
            e1:(@) _? "+" _? e2:@ {
                Rc::new(Expr::Binop(e1.combined_span_for_parser(&e2), Binop::Add, e1, e2))
            }
            e1:(@) _? "-" _? e2:@ {
                Rc::new(Expr::Binop(e1.combined_span_for_parser(&e2), Binop::Sub, e1, e2))
            }
            --
            e:@ _? annotations:annotations() r:position!() {
                Rc::new(Expr::Annotations(Span::new(file_id, e.span().range.start..r), e, annotations))
            }
            --
            "(" _? e:expression(file_id) _? ")" { e }
            span_and_dice:spanned(file_id, <dice(file_id)>) {
                let (span, dice) = span_and_dice;
                Rc::new(Expr::Dice(span, dice))
            }
            span_and_value:spanned(file_id, <value()>) {
                let (span, value) = span_and_value;
                Rc::new(Expr::Constant(span, value))
            }
        }

        /// Helper rule for `expression`, which can't use `position!` normally.
        rule spanned<T>(file_id: FileId, expr: rule<T>) -> (Span, T)
            = l:position!() e:expr() r:position!() { (Span { file_id, range: l..r }, e) }

        rule annotations() -> Annotations
            = "[" _? annotations:(annotation() ++ (_? "," _?)) _? "]" {
                Annotations::from_iter(annotations)
            }

        rule annotation() -> Annotation
            = annotation:$(['_' | 'a'..='z'] ['_' | 'a'..='z' | '0'..='9']*) {
                Annotation::new(annotation)
            }

        rule dice(file_id: FileId) -> Rc<DiceExpr>
            = l:position!() die:die() r:position!() {
                let span = Span { file_id, range: l..r };
                Rc::new(DiceExpr::Dice { span, count: 1, die })
            }
            / l:position!() count:count() die:die() r:position!() {
                let span = Span { file_id, range: l..r };
                Rc::new(DiceExpr::Dice { span, count, die })
            }

        rule die() -> Rc<Die>
            = "dF" { Rc::new(Die::Fate(FateDie::new())) }
            / "d" faces:value() {?
                Ok(Rc::new(Die::Simple(SimpleDie::new(faces)
                    .map_err(|_| "the number of faces on a die must be greater than 0")?)))
            }

        rule value() -> Number
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
            ("d6", "1d6 (5) = 5"),
            ("d12", "1d12 (10) = 10"),
            ("d20", "1d20 (19) = 19"),
            ("dF", "1dF (+) = 1"),
            ("2d6", "2d6 (3 3) = 6"),
            ("2d6+3", "2d6 (4 6) + 3 = 13"),
            ("4dF", "4dF (0 0 + +) = 2"),
            (" 1d6 + 1 ", "1d6 (1) + 1 = 2"),
            ("2d6 - 2", "2d6 (2 3) - 2 = 3"),
            ("-1", "-1 = -1"),                  // Found by proptest!
            ("0 + (0 + 0)", "0 + (0 + 0) = 0"), // Found by proptest!
            ("(0 + 0) + 0", "0 + 0 + 0 = 0"),   // No parens required.
            // Level 5, 20 STR, longsword (flame tongue). Unlike Avrae, we
            // include annotations in our actual calculations.
            (
                "(1d8 + 5) [piercing] + 2d6 [fire]",
                "(1d8 (8) + 5) [piercing] + 2d6 (4 5) [fire] = 9 [fire] + 13 [piercing]",
            ),
            // Multiple annotations.
            (
                "1d8 [mark, piercing]",
                "1d8 (5) [mark, piercing] = 5 [mark, piercing]",
            ),
        ][..];

        let mut rng = ChaCha8Rng::seed_from_u64(28);
        for &(source, expected) in examples {
            let program = Program::parse(FileName::Test, source).unwrap();
            let rolled = program.roll_all(&mut rng);
            let mut wtr = MarkdownWriter::new(vec![]);
            rolled.evaluate_and_pretty_format(&mut wtr).unwrap();
            assert_eq!(wtr.into_string_lossy(), expected);
        }
    }

    #[test]
    fn errors_are_reported() {
        let examples = vec![format!("{max} + {max}", max = Number::MAX)];

        let mut rng = ChaCha8Rng::seed_from_u64(28);
        for source in examples {
            let program = Program::parse(FileName::Test, &source).unwrap();
            let rolled = program.roll_all(&mut rng);
            let mut wtr = MarkdownWriter::new(vec![]);
            let result = rolled.evaluate_and_pretty_format(&mut wtr);
            assert!(result.is_err());
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
            let parsed = Program::parse(FileName::Test, &src).unwrap();
            assert_eq!(parsed.expr, expr);
        }
    }
}
