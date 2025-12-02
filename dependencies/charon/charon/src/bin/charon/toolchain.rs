use anyhow::{Context, Result};
use serde::Deserialize;
use std::{env, ffi::OsStr, path::PathBuf, process::Command};

// Store the toolchain details directly in the binary.
static PINNED_TOOLCHAIN: &str = include_str!("../../../rust-toolchain");

/// This struct is used to deserialize the "rust-toolchain" file.
#[derive(Deserialize)]
struct ToolchainFile {
    toolchain: Toolchain,
}

#[derive(Deserialize)]
pub struct Toolchain {
    channel: String,
    components: Vec<String>,
}

impl Toolchain {
    fn is_installed(&self) -> Result<bool> {
        // FIXME: check if the right components are installed.
        let output = self.run("rustc").arg("--version").output()?;
        Ok(output.status.success())
    }

    fn install(&self) -> Result<()> {
        Command::new("rustup")
            .arg("install")
            .arg(&self.channel)
            .status()?;
        for component in &self.components {
            Command::new("rustup")
                .arg("component")
                .arg("add")
                .arg("--toolchain")
                .arg(&self.channel)
                .arg(component)
                .status()?;
        }
        Ok(())
    }

    fn run(&self, program: impl AsRef<OsStr>) -> Command {
        let mut cmd = Command::new("rustup");
        cmd.arg("run");
        cmd.arg(&self.channel);
        cmd.arg(program);

        // Add rust driver dll to `PATH` in rustup.
        // cc
        // * https://github.com/AeneasVerif/charon/issues/588
        // * https://github.com/rust-lang/rustup/issues/3825
        if cfg!(windows) {
            cmd.env("RUSTUP_WINDOWS_PATH_ADD_BIN", "1");
        }

        cmd
    }
}

fn get_pinned_toolchain() -> Toolchain {
    let file_contents: ToolchainFile = toml::from_str(PINNED_TOOLCHAIN).unwrap();
    file_contents.toolchain
}

pub fn driver_path() -> PathBuf {
    let mut path = env::current_exe()
        .expect("current executable path invalid")
        .with_file_name("charon-driver");

    if cfg!(windows) {
        path.set_extension("exe");
    }

    path
}

/// Build a command that calls the given binary in the correct toolchain environment. This uses
/// rustup to provide the correct toolchain, unless we're in a nix context where the toolchain is
/// already in PATH.
pub fn in_toolchain(program: impl AsRef<OsStr>) -> Result<Command> {
    let toolchain = get_pinned_toolchain();
    // This is set by the nix develop environment and the nix builder; in both cases the toolchain
    // is set up in `$PATH` and the driver should be correctly dynamically linked.
    let correct_toolchain_is_in_path = env::var("CHARON_TOOLCHAIN_IS_IN_PATH").is_ok();

    let cmd = if correct_toolchain_is_in_path {
        trace!("We appear to have been built with nix; using the rust toolchain in PATH.");
        Command::new(program)
    } else {
        trace!("Using rustup-provided toolchain.");
        if !toolchain.is_installed()? {
            println!("The required toolchain is not installed. Installing...");
            toolchain.install()?;
        }
        toolchain.run(program)
    };
    Ok(cmd)
}

pub fn driver_cmd() -> Result<Command> {
    // We need `in_toolchain` to get the right library paths.
    let mut cmd = in_toolchain(driver_path())?;
    // The driver expects the first arg to be "rustc" because that's how cargo calls it.
    cmd.arg("rustc");
    Ok(cmd)
}

/// Get the path to the sysroot, where the `cargo` and `rustc` binaries can be found. Prefer using
/// [`in_toolchain`] instead of this as much as possible.
pub fn toolchain_path() -> Result<PathBuf> {
    let output = in_toolchain("rustc")?.arg("--print=sysroot").output()?;
    let stdout = String::from_utf8(output.stdout)
        .with_context(|| format!("the output of `rustc --print=sysroot` is not UTF8 encoded"))?;
    Ok(PathBuf::from(stdout.trim_end()))
}
