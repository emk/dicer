//! Our basic error types.

use codespan_reporting::diagnostic::{self, Diagnostic, Label};
use thiserror::Error;

use crate::{
    dice::Value,
    spans::{FileId, Span},
};

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
