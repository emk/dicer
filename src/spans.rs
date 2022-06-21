//! Source code location information.

use std::{fmt, ops::Range, rc::Rc};

use codespan_reporting::files::SimpleFiles;

/// Unique identifier for a file.
pub type FileId = usize;

/// A span in our source code, use to report where errors occurred.
#[derive(Debug)]
pub struct Span {
    /// The file in which this span appears.
    pub file_id: FileId,
    /// The range of bytes in the source code included in this span.
    pub range: Range<usize>,
}

impl Span {
    /// Create a new [`Span`].
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

/// A collection of source files, used to report diagnostics.
pub type Files = Rc<SimpleFiles<FileName, String>>;
