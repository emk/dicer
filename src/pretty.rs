use std::io;

pub use codespan_reporting::term::termcolor::{ColorSpec, WriteColor};

/// Write using colors and other formatting.
pub trait PrettyFormat {
    fn pretty_format(&self, writer: &mut dyn WriteColor) -> Result<(), io::Error>;
}
