//! Our basic error types.

use std::{error, fmt, io, rc::Rc};

use codespan_reporting::{
    diagnostic::{self, Diagnostic, Label},
    files::{Error as CodeSpanError, SimpleFiles},
    term::{
        self,
        termcolor::{NoColor, WriteColor},
        Config,
    },
};
use log::error;
use thiserror::Error;

use crate::{
    dice::Value,
    expressions::Binop,
    spans::{FileId, Files, Span},
};

/// An error occurred while doing math.
#[derive(Debug, Error)]
pub enum MathError {
    #[error("arithmetic overflow in {v1} {op} {v2}")]
    Overflow { op: Binop, v1: Value, v2: Value },
}

/// An error occurred while parsing or executing a dice-related "program". This
/// will normally include a source location, and it can be formatted as a
/// user-friendly diagnostic.
#[derive(Debug, Error)]
pub enum ProgramError {
    /// We found a die with the wrong number of faces.
    #[error("a die must have at least one face, found: {faces}")]
    InvalidFaceCount { faces: Value },

    /// We had a problem doing I/O. We don't provide a lot of extra information,
    /// because our language doesn't really do I/O. So this mostly applies to
    /// `write!`.
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),

    /// An error occurred while performing basic math.
    #[error("math error: {source}")]
    Math { source: MathError },

    /// We encountered an error parsing.
    #[error("{span}: {message}")]
    Parse { span: Span, message: String },
}

impl ProgramError {
    /// Generate a user-friendly diagnostic for this error.
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        let mut diagnostic = Diagnostic::error();
        match self {
            ProgramError::InvalidFaceCount { .. } => {
                diagnostic = diagnostic.with_message(self.to_string());
            }
            ProgramError::Io(source) => {
                diagnostic = diagnostic.with_message(source.to_string());
            }
            ProgramError::Math { source } => {
                diagnostic = diagnostic.with_message(source.to_string());
            }
            ProgramError::Parse { span, message } => {
                diagnostic = diagnostic
                    .with_message(message.to_owned())
                    .with_labels(vec![Label::new(
                        diagnostic::LabelStyle::Primary,
                        span.file_id,
                        span.range.clone(),
                    )]);
            }
        }
        diagnostic
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
