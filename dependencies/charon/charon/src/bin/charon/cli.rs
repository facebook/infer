use charon_lib::options::CliOpts;
use clap::{Args, Parser, Subcommand};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[clap(name = "Charon")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Charon,
}

#[derive(Debug, Subcommand)]
pub enum Charon {
    /// Runs charon on a single rust file (and the modules it references, if any).
    Rustc(RustcArgs),
    /// Runs charon on a cargo project.
    Cargo(CargoArgs),
    /// Print the path to the rustc toolchain used by charon.
    ToolchainPath(ToolchainPathArgs),
    /// Pretty-print the given llbc file.
    PrettyPrint(PrettyPrintArgs),
    /// Print the version.
    Version,
}

/// Read a llbc or ullbc file and pretty print it.
#[derive(Args, Debug)]
pub struct PrettyPrintArgs {
    /// Single file path to llbc or ullbc
    pub file: PathBuf,
}

/// Usage: `charon cargo [charon options] -- [rustc options]`
#[derive(clap::Args, Debug)]
pub struct RustcArgs {
    #[command(flatten)]
    pub opts: CliOpts,

    /// Args that `rustc` accepts.
    #[arg(last = true)]
    pub rustc: Vec<String>,
}

/// Usage: `charon cargo [charon options] -- [cargo build options]`
#[derive(clap::Args, Debug)]
pub struct CargoArgs {
    #[command(flatten)]
    pub opts: CliOpts,

    /// Args that `cargo build` accepts.
    #[arg(last = true)]
    pub cargo: Vec<String>,
}

/// Usage: `charon toolchain-path`
#[derive(clap::Args, Debug)]
pub struct ToolchainPathArgs {}
