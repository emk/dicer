//! An algebra of die rolls.

use std::{io, rc::Rc};

use codespan_reporting::term::termcolor::WriteColor;

use crate::dice::Roll;

// pub struct Rolls(Vec<Rc<Roll>>);

pub enum Output {
    Roll(Rc<Roll>),
    // Constant(Value),
    // Rolls(Rolls),
    // Sum(Rc<Output>, Rc<Output>),
}

impl Output {
    pub fn write(&self, writer: &mut dyn WriteColor) -> Result<(), io::Error> {
        match self {
            Output::Roll(roll) => write!(writer, "{}", roll.face)?,
        }
        Ok(())
    }
}
