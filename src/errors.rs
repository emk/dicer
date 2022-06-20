//! Our basic error types.

use std::{error, fmt, rc::Rc};

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
    spans::{FileId, Files, Span},
};

/// Reported when an arithmetic overflow occurs. This should never happen in
/// normal operation, so we don't bother to include any useful information.
#[derive(Debug, Error)]
#[error("arithmetic overflow")]
pub struct OverflowErr;

/// Errors involving fundemantal dice operations. These errors do not include
/// source code locations.
#[derive(Debug, Error)]
pub enum DieError {
    #[error("a die must have at least one face, found: {faces}")]
    InvalidFaceCount { faces: Value },
}

/// An error occurred while parsing or executing a dice-related "program". This
/// will normally include a source location, and it can be formatted as a
/// user-friendly diagnostic.
#[derive(Debug, Error)]
pub enum ProgramError {
    #[error("{span}: {message}")]
    ParseError { span: Span, message: String },
}

impl ProgramError {
    /// Generate a user-friendly diagnostic for this error.
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        let mut diagnostic = Diagnostic::error();
        match self {
            ProgramError::ParseError { span, message } => {
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
