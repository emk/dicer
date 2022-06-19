//! An algebra of die rolls.

use std::{io, rc::Rc};

use codespan_reporting::term::termcolor::WriteColor;

use crate::{dice::Roll, pretty::PrettyFormat, program::Expression};

// pub struct Rolls(Vec<Rc<Roll>>);

pub enum Output {
    Rolls {
        expression: Rc<Expression>,
        rolls: Vec<Rc<Roll>>,
    },
    // Constant(Value),
    // Sum(Rc<Output>, Rc<Output>),
}

impl PrettyFormat for Output {
    fn pretty_format(&self, writer: &mut dyn WriteColor) -> Result<(), io::Error> {
        match self {
            Output::Rolls { expression, rolls } => {
                write!(writer, "{} (", expression)?;
                for (idx, roll) in rolls.iter().enumerate() {
                    if idx > 0 {
                        write!(writer, " ")?;
                    }
                    roll.pretty_format(writer)?;
                }
                write!(writer, ")")?;
            }
        }
        Ok(())
    }
}
