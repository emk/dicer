//! A simple die-rolling utility.

#![warn(missing_docs, clippy::missing_docs_in_private_items, clippy::all)]

use std::{io::Write, process::exit};

use clap::Parser;
use codespan_reporting::term::{termcolor::StandardStream, ColorArg};
use errors::ProgramDiagnostics;
use log::debug;
use rand::prelude::ThreadRng;

use crate::{program::Program, spans::FileName};

mod annotations;
mod dice;
mod errors;
mod expressions;
#[cfg(test)]
mod markdown_writer;
mod pretty;
mod program;
mod spans;
mod values;

/// Roll dice!
#[derive(Debug, Parser)]
struct Args {
    /// Configure coloring of output.
    #[structopt(
        long = "color",
        default_value = "auto",
        possible_values = ColorArg::VARIANTS,
        case_insensitive = true,
    )]
    color: ColorArg,

    /// Expressions of the form "2d6+3" or "d20".
    dice_exprs: Vec<String>,
}

/// Our main entry point. Wraps [`run`], and handles error reporting.
fn main() {
    env_logger::init();
    let args = Args::parse();
    debug!("Args: {:?}", args);

    if let Err(err) = run(&args) {
        let mut writer = StandardStream::stdout(args.color.into());
        err.write(&mut writer).expect("could not write error");
        exit(1);
    }
}

/// Does the actual work.
fn run(args: &Args) -> Result<(), ProgramDiagnostics> {
    let mut rng = ThreadRng::default();
    let mut writer = StandardStream::stdout(args.color.into());
    for (idx, dice_expr) in args.dice_exprs.iter().enumerate() {
        let file_name = FileName::Arg(idx + 1);
        let program = Program::parse(file_name, dice_expr)?;
        let rolled = program.roll_all(&mut rng);
        rolled.evaluate_and_pretty_format(&mut writer)?;
        writer
            .write(b"\n")
            .map_err(ProgramDiagnostics::from_error)?;
    }
    writer.flush().map_err(ProgramDiagnostics::from_error)?;
    Ok(())
}
