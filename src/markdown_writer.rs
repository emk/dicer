//! A somewhat simplistic implementation of [`WriteColor`] that outputs
//! something like Markdown.

use std::io::{self, Write};

pub use codespan_reporting::term::termcolor::{ColorSpec, WriteColor};

/// Text styles we can represent with paired delimiters.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum MarkdownTextStyle {
    Bold,
    Italic,
    StrikeThrough,
}

impl MarkdownTextStyle {
    /// The delimiter associated with this text style.
    fn delimiter(self) -> &'static str {
        match self {
            MarkdownTextStyle::Bold => "**",
            MarkdownTextStyle::Italic => "_",
            MarkdownTextStyle::StrikeThrough => "~~",
        }
    }
}

/// A somewhat simplistic implementation of [`WriteColor`] that outputs
/// something like Markdown. This will break if you try to do anything fancy.
pub struct MarkdownWriter<W> {
    writer: W,
    style_stack: Vec<MarkdownTextStyle>,
}

impl<W> MarkdownWriter<W> {
    /// Create a new MarkdownWriter.
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            style_stack: vec![],
        }
    }

    /// Recover our inner writer object.
    pub fn into_inner(self) -> W {
        self.writer
    }
}

impl MarkdownWriter<Vec<u8>> {
    /// Recover our inner writer object.
    pub fn into_string_lossy(self) -> String {
        String::from_utf8_lossy(&self.into_inner()).into_owned()
    }
}

impl<W: Write> MarkdownWriter<W> {
    /// Attempt to set a Markdown text style, as best as we can.
    fn set_markdown_text_style(
        &mut self,
        style: MarkdownTextStyle,
        active: bool,
    ) -> io::Result<()> {
        if active {
            if !self.style_stack.contains(&style) {
                self.style_stack.push(style);
                write!(self.writer, "{}", style.delimiter())?;
            }
        } else {
            if self.style_stack.last() == Some(&style) {
                write!(self.writer, "{}", style.delimiter())?;
                self.style_stack.pop();
            } else if self.style_stack.contains(&style) {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "can't convert non-hierarchical styling to Markdown",
                ));
            }
        }
        Ok(())
    }
}

impl<W: Write> Write for MarkdownWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

impl<W: Write> WriteColor for MarkdownWriter<W> {
    fn supports_color(&self) -> bool {
        true
    }

    fn set_color(&mut self, spec: &ColorSpec) -> io::Result<()> {
        self.set_markdown_text_style(MarkdownTextStyle::Bold, spec.bold())?;
        self.set_markdown_text_style(MarkdownTextStyle::Italic, spec.italic())?;
        self.set_markdown_text_style(MarkdownTextStyle::StrikeThrough, spec.dimmed())?;
        Ok(())
    }

    fn reset(&mut self) -> io::Result<()> {
        while let Some(&style) = self.style_stack.last() {
            let stack_size = self.style_stack.len();
            self.set_markdown_text_style(style, false)?;
            assert_eq!(self.style_stack.len(), stack_size - 1);
        }
        Ok(())
    }
}
