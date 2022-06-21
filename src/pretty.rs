//! Colored printing utilities.

use std::io;

pub use codespan_reporting::term::termcolor::{ColorSpec, WriteColor};

/// Write using colors and other formatting.
pub trait PrettyFormat {
    /// Write this valuen to `writer`, optionally using color.
    fn pretty_format(&self, writer: &mut dyn WriteColor) -> Result<(), io::Error>;
}
