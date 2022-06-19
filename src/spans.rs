//! Source code location information.

use std::{error, fmt, ops::Range, rc::Rc};

use codespan_reporting::{
    diagnostic::Diagnostic,
    files::{Error as CodeSpanError, SimpleFiles},
    term::{
        self,
        termcolor::{NoColor, WriteColor},
        Config,
    },
};
use log::error;

use crate::errors::ProgramError;

/// Unique identifier for a file.
pub type FileId = usize;

/// A span in our source code, use to report where errors occurred.
#[derive(Debug)]
pub struct Span {
    pub file_id: FileId,
    pub range: Range<usize>,
}

impl Span {
    pub fn new(file_id: FileId, range: Range<usize>) -> Self {
        Self { file_id, range }
    }
}

/// Format this `Span` for display as best we can without access to [`Files`].
impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<file {}[{}..{}]>",
            self.file_id, self.range.start, self.range.end
        )
    }
}

/// Names of our input files.
#[derive(Clone, Debug)]
pub enum FileName {
    /// Command-line argument.
    Arg(usize),
}

impl fmt::Display for FileName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FileName::Arg(i) => write!(f, "<argument {i}>"),
        }
    }
}

/// Convert a single, normal error into a `[Diagnostic]`.
pub fn diagnostic_from_error<E: error::Error>(err: E) -> Diagnostic<FileId> {
    let mut notes = vec![];
    let mut source = err.source();
    while let Some(current) = source {
        notes.push(format!("caused by: {current}"));
        source = current.source();
    }
    Diagnostic::error()
        .with_message(err.to_string())
        .with_notes(notes)
}

pub type Files = Rc<SimpleFiles<FileName, String>>;

/// A collection of multiple diagnostics, which can be displayed on a terminal.
pub struct ProgramDiagnostics {
    /// Source code.
    files: Files,

    /// Diagnostics to display.
    diagnostics: Vec<Diagnostic<FileId>>,
}

impl ProgramDiagnostics {
    pub fn from_program_error(files: Files, err: ProgramError) -> Self {
        Self {
            files,
            diagnostics: vec![err.to_diagnostic()],
        }
    }

    /// Convert a single, normal error into a diagnostic report.
    pub fn from_error<E: error::Error>(err: E) -> Self {
        Self {
            files: Rc::new(SimpleFiles::new()),
            diagnostics: vec![diagnostic_from_error(err)],
        }
    }

    /// Write a complete diagnostic reporting, using color wherever possible.
    pub fn write(&self, writer: &mut dyn WriteColor) -> Result<(), CodeSpanError> {
        let config = Config::default();
        for diagnostic in &self.diagnostics {
            term::emit(writer, &config, &*self.files, diagnostic)?;
        }
        Ok(())
    }
}

/// We override `Debug` to return the same as `Display`, because Rust shows the
/// `Debug` output when we return an error from `main`.
impl fmt::Debug for ProgramDiagnostics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

/// Display plain text version of our error. Prefer
/// [`ProgramDiagnostics::write`], which supports pretty colors.
impl fmt::Display for ProgramDiagnostics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut wtr = NoColor::new(Vec::new());
        self.write(&mut wtr).map_err(|err| {
            error!("error formatting source: {}", err);
            fmt::Error
        })?;
        let buf = wtr.into_inner();
        write!(f, "{}", String::from_utf8_lossy(&buf))
    }
}

impl error::Error for ProgramDiagnostics {}
