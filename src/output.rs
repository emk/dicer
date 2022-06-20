//! An algebra of die rolls.

use std::{io, rc::Rc};

use codespan_reporting::term::termcolor::WriteColor;

use crate::{
    dice::{Roll, Value},
    errors::{OverflowErr, ProgramDiagnostics},
    pretty::PrettyFormat,
    program::Expression,
};

fn add_values(v1: Value, v2: Value) -> Result<Value, OverflowErr> {
    Ok(v1.checked_add(v2).ok_or(OverflowErr)?)
}

pub enum Output {
    Rolls {
        expression: Rc<Expression>,
        rolls: Vec<Rc<Roll>>,
    },
    Constant(Value),
    Add(Rc<Output>, Rc<Output>),
}

impl Output {
    pub fn value(&self) -> Result<Value, OverflowErr> {
        match self {
            Output::Rolls { rolls, .. } => {
                let mut sum = 0;
                for roll in rolls {
                    sum = add_values(sum, roll.face.value())?;
                }
                Ok(sum)
            }
            Output::Constant(value) => Ok(*value),
            Output::Add(o1, o2) => {
                let v1 = o1.value()?;
                let v2 = o2.value()?;
                add_values(v1, v2)
            }
        }
    }

    pub fn pretty_format_with_value(
        &self,
        writer: &mut dyn WriteColor,
    ) -> Result<(), ProgramDiagnostics> {
        self.pretty_format(writer)
            .map_err(ProgramDiagnostics::from_error)?;
        let value = self.value().map_err(ProgramDiagnostics::from_error)?;
        write!(writer, " = {}", value).map_err(ProgramDiagnostics::from_error)?;
        Ok(())
    }
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
            Output::Constant(value) => write!(writer, "{}", value)?,
            Output::Add(o1, o2) => {
                o1.pretty_format(writer)?;
                write!(writer, " + ")?;
                o2.pretty_format(writer)?;
            }
        }
        Ok(())
    }
}
