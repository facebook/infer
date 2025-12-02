use anyhow::{Context, Result, ensure};
use assert_cmd::prelude::CommandCargoExt;
use itertools::Itertools;
use std::{path::PathBuf, process::Command};

fn charon<T>(args: &[&str], dir: &str, f: impl FnOnce(String, String) -> Result<T>) -> Result<T> {
    let cmd_str = std::iter::once("charon")
        .chain(args.iter().copied())
        .join(" ");

    let mut cmd = Command::cargo_bin("charon")?;
    cmd.current_dir(dir);
    let output = cmd.args(args).output()?;

    let stdout = String::from_utf8(output.stdout)
        .with_context(|| format!("`{cmd_str}`:\nthe content of stdout is not UTF8 encoded."))?;
    let stderr = String::from_utf8(output.stderr)
        .with_context(|| format!("`{cmd_str}`:\nthe content of stderr is not UTF8 encoded."))?;

    let status = output.status;
    ensure!(
        status.success(),
        "Error when executing `{cmd_str}`:\nstderr={stderr:?}\nstdout={stdout:?}",
    );

    f(stdout, cmd_str)
}

#[test]
fn charon_pretty_print() -> Result<()> {
    charon(
        &["rustc", "--", "--crate-type=rlib", "tests/ui/arrays.rs"],
        ".",
        |_, _| {
            // arrays.llbc is generated
            let llbc = "arrays.llbc";
            ensure!(std::fs::exists(llbc)?, "{llbc} doesn't exist!");

            charon(&["pretty-print", llbc], ".", |stdout, _| {
                let search = "pub fn index_array_shared";
                ensure!(
                    stdout.contains(search),
                    "Output of pretty-printing {llbc} is:\n{stdout:?}\nIt doesn't contain {search:?}."
                );
                Ok(())
            })
        },
    )
}

#[test]
fn charon_version() -> Result<()> {
    charon(&["version"], ".", |stdout, cmd| {
        let version = charon_lib::VERSION;
        ensure!(
            stdout.trim() == version,
            "Output of `{cmd}` is:\n{stdout:?}\nIt should be {version}."
        );
        Ok(())
    })
}

#[test]
fn charon_cargo_p_crate2() -> Result<()> {
    charon(
        &["cargo", "--print-llbc", "--", "-p", "crate2", "--quiet"],
        "tests/cargo/workspace",
        |stdout, cmd| {
            let search = "pub fn extra_random_number";
            ensure!(
                stdout.contains(search),
                "Output of `{cmd}` is:\n{stdout:?}\nIt doesn't contain {search:?}."
            );
            Ok(())
        },
    )
}

#[test]
fn charon_cargo_features() -> Result<()> {
    let dir = "tests/cargo/dependencies";
    let main = "fn main";
    let take_mut = "pub fn take";

    charon(
        &["cargo", "--print-llbc", "--", "-F", "test_feature"],
        dir,
        |stdout, cmd| {
            ensure!(
                stdout.contains(main),
                "Output of `{cmd}` is:\n{stdout:?}\nIt doesn't contain {main:?}."
            );
            ensure!(
                stdout.contains(take_mut),
                "Output of `{cmd}` is:\n{stdout:?}\nIt doesn't contain {take_mut:?}."
            );
            Ok(())
        },
    )?;

    charon(&["cargo", "--print-llbc"], dir, |stdout, cmd| {
        ensure!(
            stdout.contains(main),
            "Output of `{cmd}` is:\n{stdout:?}\nIt doesn't contain {main:?}."
        );

        let count_fn = stdout.matches("fn").count();
        ensure!(
            count_fn == 1,
            "Output of `{cmd}` is:\n{stdout:?}\nThe count of `fn` should only be one."
        );

        ensure!(
            !stdout.contains(take_mut),
            "Output of `{cmd}` is:\n{stdout:?}\nIt shouldn't contain {take_mut:?}."
        );
        Ok(())
    })
}

#[test]
fn charon_cargo_target() -> Result<()> {
    let target = "riscv64gc-unknown-none-elf";

    let dir = "tests/cargo/multi-targets";
    let fn_ = "pub fn";

    #[cfg(target_family = "unix")]
    charon(&["cargo", "--print-llbc"], dir, |stdout, cmd| {
        let main = "multi_targets::on_unix";
        ensure!(
            stdout.contains(main),
            "Output of `{cmd}` is:\n{stdout:?}\nIt doesn't contain {main:?}."
        );

        let count_fn = stdout.matches(fn_).count();
        ensure!(
            count_fn == 1,
            "Output of `{cmd}` is:\n{stdout:?}\nThe count of {fn_:?} should only be one."
        );
        Ok(())
    })?;

    #[cfg(target_os = "windows")]
    charon(&["cargo", "--print-llbc"], dir, |stdout, cmd| {
        let main = "multi_targets::on_windows";
        ensure!(
            stdout.contains(main),
            "Output of `{cmd}` is:\n{stdout:?}\nIt doesn't contain {main:?}."
        );

        let count_fn = stdout.matches(fn_).count();
        ensure!(
            count_fn == 1,
            "Output of `{cmd}` is:\n{stdout:?}\nThe count of {fn_:?} should only be one."
        );
        Ok(())
    })?;

    let args = &["cargo", "--print-llbc", "--", "--target", target];
    charon(args, dir, |stdout, cmd| {
        let main = "multi_targets::no_os";
        ensure!(
            stdout.contains(main),
            "Output of `{cmd}` is:\n{stdout:?}\nIt doesn't contain {main:?}."
        );

        let count_fn = stdout.matches(fn_).count();
        ensure!(
            count_fn == 1,
            "Output of `{cmd}` is:\n{stdout:?}\nThe count of {fn_:?} should only be one."
        );
        Ok(())
    })
}

#[test]
fn charon_rustc() -> Result<()> {
    let path = "tests/cargo/workspace/crate1/src/lib.rs";
    let args = &["rustc", "--print-llbc", "--", "--crate-type=lib", path];

    // Call rustc without specifying --crate-name, so default to lib as the name.
    let fn_ = "pub fn random_number";

    charon(args, ".", |stdout, cmd| {
        ensure!(
            stdout.contains(fn_),
            "Output of `{cmd}` is:\n{stdout:?}\nIt doesn't contain {fn_:?}."
        );

        let count_fn = stdout.matches("fn").count();
        ensure!(
            count_fn == 1,
            "Output of `{cmd}` is:\n{stdout:?}\nThe count of `fn` should only be one."
        );
        Ok(())
    })
}

#[test]
fn put_options_after_subcommand() -> Result<()> {
    let path = "tests/cargo/workspace/crate1/src/lib.rs";
    let args = &["--print-llbc", "rustc", "--", "--crate-type=lib", path];
    let output = Command::cargo_bin("charon")?.args(args).output()?;
    assert!(!output.status.success());
    Ok(())
}

#[test]
fn charon_rust_target() -> Result<()> {
    let target = "riscv64gc-unknown-none-elf";

    let path = "tests/cargo/multi-targets/src/lib.rs";
    let args = &[
        "rustc",
        "--print-llbc",
        "--",
        "--crate-type=lib",
        path,
        "--crate-name",
        "multi_targets",
        "--target",
        target,
    ];
    let [stdout, rustc_cmd] = charon(args, ".", |stdout, cmd| Ok([stdout, cmd]))?;

    let dir = "tests/cargo/multi-targets";
    let args = &["cargo", "--print-llbc", "--", "--target", target];
    // Suppose outputs from cargo and rustc queries are the same...
    charon(args, dir, |desired, cargo_cmd| {
        ensure!(
            desired == stdout,
            "LLBC output differs between `charon cargo` and `charon rustc`\n\
            `{cargo_cmd}` emits:\n{desired:?}\n\
            `{rustc_cmd}` emits:\n{stdout:?}"
        );
        Ok(())
    })
}

#[test]
fn handle_multi_trailing_rs_args() {
    let file = "arrays.rs";
    let args = &[
        "rustc",
        "--print-llbc",
        "--",
        "--crate-name=arrays.rs",
        file,
    ];
    let err = charon(args, "tests/ui", |_, _| Ok(())).unwrap_err();
    let err = format!("{err:?}");
    assert!(err.contains("invalid character '.' in crate name"), "{err}");
}

#[test]
fn rustc_input_duplicated() {
    let input = "arrays.rs";
    let args = &["rustc", "--print-llbc", "--input", input, "--", input];
    let err = charon(args, "tests/ui", |_, _| Ok(())).unwrap_err();
    let pat = "error: multiple input filenames provided (first two filenames are `arrays.rs` and `arrays.rs`)";
    let err = format!("{err:?}");
    assert!(err.contains(pat), "{err}");
}

#[test]
fn charon_input() -> Result<()> {
    let input = "arrays.rs";
    let args = &[
        "rustc",
        "--print-llbc",
        "--input",
        input,
        "--",
        "--crate-type=lib",
    ];
    charon(args, "tests/ui", |_, _| Ok(()))
}

#[test]
/// Ensure we don't error if the path where the binary file would go in a normal rustc invocation
/// is taken. Charon doesn't emit a binary so doesn't care.
fn filename_conflict() -> Result<()> {
    let input = "./ui/simple/match-on-float.rs";
    let args = &["rustc", "--no-serialize", "--", input, "--crate-name=ui"];
    charon(args, "tests", |_, _| Ok(()))
}

#[test]
fn charon_toolchain_path() -> Result<()> {
    let args = &["toolchain-path"];
    charon(args, ".", |stdout, cmd| {
        let path = PathBuf::from(stdout.trim_end());
        ensure!(
            path.exists(),
            "`{cmd}`: toolchain path {} doesn't exist",
            path.display()
        );
        let rustc_path = path.join("bin").join("rustc");
        ensure!(
            rustc_path.exists(),
            "`{cmd}`: rustc path {} doesn't exist",
            rustc_path.display()
        );
        Ok(())
    })
}
