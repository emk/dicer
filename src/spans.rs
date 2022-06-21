//! Source code location information.

use std::{fmt, ops::Range, rc::Rc};

use codespan_reporting::files::{self, SimpleFiles};

/// Unique identifier for a file.
pub type FileId = usize;

/// A span in our source code, use to report where errors occurred.
///
/// Spans are _ignored_ during comparison. All `Span` objects are treated as
/// equal to other [`Span`] objects.
#[derive(Clone, Debug)]
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

impl PartialEq for Span {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for Span {}

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
    /// Used for test cases.
    #[cfg(test)]
    Test,

    /// Command-line argument.
    Arg(usize),
}

impl fmt::Display for FileName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            #[cfg(test)]
            FileName::Test => write!(f, "<test>"),
            FileName::Arg(i) => write!(f, "<argument {i}>"),
        }
    }
}

/// A collection of source files, used to report diagnostics.
///
/// This type is very cheap to clone. But once created, it is impossible
/// to add any more files.
#[derive(Clone, Debug)]
pub struct Files {
    /// Wrapped set of files.
    files: Rc<SimpleFiles<FileName, String>>,
}

impl Files {
    /// Wrap a [`SimpleFiles`].
    pub fn new(files: SimpleFiles<FileName, String>) -> Self {
        Self {
            files: Rc::new(files),
        }
    }

    /// Create a [`SimpleFiles`] with no source files in it. This is used when
    /// reporting errors that did not have an associated source location.
    pub fn empty() -> Files {
        Self {
            files: Rc::new(SimpleFiles::new()),
        }
    }
}

impl<'a> files::Files<'a> for Files {
    type FileId = FileId;

    type Name = FileName;

    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, files::Error> {
        self.files.name(id)
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, files::Error> {
        self.files.source(id)
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, files::Error> {
        self.files.line_index(id, byte_index)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, files::Error> {
        self.files.line_range(id, line_index)
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    impl Arbitrary for Span {
        type Parameters = ();

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            // When generating spans, always assume this is at byte 0 in an
            // empty test file.
            Just(Span::new(0, 0..0)).boxed()
        }

        type Strategy = BoxedStrategy<Span>;
    }

    impl Arbitrary for Files {
        type Parameters = ();

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            let mut files = SimpleFiles::new();
            assert_eq!(0, files.add(FileName::Test, String::new()));
            Just(Files::new(files)).boxed()
        }

        type Strategy = BoxedStrategy<Files>;
    }
}
