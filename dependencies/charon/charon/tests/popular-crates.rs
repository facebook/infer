//! This test downlads the `NUMBER_OF_CRATES` most downloaded crates from crates.io and runs charon
//! on each of them.
//!
//! This test requires a feature flag. To run, call `make test-popular-crates`.
#![feature(path_add_extension)]
#![cfg(feature = "popular-crates-test")]
use anyhow::{Context, Result, bail};
use assert_cmd::prelude::CommandCargoExt;
use crates_io_api::Version;
use flate2::read::GzDecoder;
use itertools::Itertools;
use std::{
    fs::File,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::Arc,
    time::Duration,
};
use tar::Archive;
use wait_timeout::ChildExt;

static TESTS_DIR: &str = "tests/popular-crates";

const NUMBER_OF_CRATES: u64 = 50;

/// List of crates that we know we can't extract.
static BLACKLIST: &[&str] = &[
    "aho-corasick",
    "base64",
    "bitflags",
    "cc",
    "clap",
    "crossbeam-utils",
    "getrandom",
    "idna",
    "itertools",
    "num-traits",
    "parking_lot",
    "parking_lot_core",
    "proc-macro2",
    "rand",
    "regex-automata",
    "regex-syntax",
    "semver",
    "serde",
    "serde_json",
    "syn",
    "time",
];

/// Downloads and extracts the crate into a subdirectory of `TESTS_DIR` and returns the path to
/// that directory.
fn extract_crate(version: &Version) -> Result<PathBuf> {
    let full_name = &format!("{}-{}", version.crate_name, version.num);
    let download_url = format!("https://crates.io{}", version.dl_path);
    let directory = PathBuf::from(format!("{}/{}", TESTS_DIR, full_name));
    if directory.exists() {
        // Assuùe the directory already contains the extracted crate.
        return Ok(directory);
    }

    let archive_path = {
        let mut path = directory.clone();
        path.add_extension("tar.gz");
        path
    };
    {
        // Download the crate archive
        let mut temp_file = File::create(&archive_path)
            .with_context(|| format!("while creating `{}`", archive_path.display()))?;
        reqwest::blocking::get(download_url)?.copy_to(&mut temp_file)?;
    }
    {
        // Extract the crate archive
        let temp_file = File::open(&archive_path)?;
        let mut archive = Archive::new(GzDecoder::new(temp_file));
        // This assumes that the archive always contains exactly one folder named
        // `{crate_name}-{version}`, which seems to be the case. Worst case we get unexpected files
        // inside the `popular-crates` subfolder.
        archive
            .unpack(TESTS_DIR)
            .with_context(|| "while extracting archive")?;
    }
    std::fs::remove_file(archive_path)?;

    Ok(directory)
}

fn process_crate(version: &Version) -> Result<()> {
    let crate_dir = extract_crate(version)?;
    let llbc_path = {
        // Relative to the crate directory
        let mut path = Path::new("..").to_path_buf();
        path.push(crate_dir.file_name().unwrap());
        path.add_extension("llbc");
        path
    };

    // Call charon
    let mut child = Command::cargo_bin("charon")?
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .current_dir(&crate_dir)
        .arg("--hide-marker-traits")
        .arg("--dest-file")
        .arg(&llbc_path)
        .spawn()?;
    let timeout = Duration::from_secs(30);
    let status_code = match child.wait_timeout(timeout)? {
        Some(status) => status,
        None => {
            child.kill()?;
            eprintln!("Compilation timed out");
            child.wait()?
        }
    };
    // let output = cmd.output()?;

    if !status_code.success() {
        let stderr = std::io::read_to_string(child.stderr.unwrap())?;
        bail!("Compilation failed: {stderr}")
    }

    Ok(())
}

#[test] // Only include in release mode
pub fn test_popular_crates() -> Result<()> {
    use crates_io_api::*;
    let client = Arc::new(
        SyncClient::new(
            "charon-test-runner (Nadrieril@users.noreply.github.com)",
            std::time::Duration::from_millis(1000),
        )
        .unwrap(),
    );

    let q = CratesQuery::builder()
        .sort(Sort::Downloads)
        .page_size(NUMBER_OF_CRATES)
        .build();
    let crates = client.crates(q)?;

    let tests: Vec<_> = crates
        .crates
        .into_iter()
        .map(|krate| {
            let known_failure = BLACKLIST.contains(&krate.name.as_str());
            let name = format!("{}-{}", krate.name, krate.max_version);
            let client = Arc::clone(&client);
            let test = libtest_mimic::Trial::test(name, move || {
                let krate = client.get_crate(&krate.name)?;
                let version = krate.versions.into_iter().next().unwrap();
                process_crate(&version).map_err(|err| err.into())
            })
            .with_ignored_flag(known_failure);
            Ok::<_, Error>(test)
        })
        .try_collect()?;

    let args = libtest_mimic::Arguments::from_args();
    libtest_mimic::run(&args, tests).exit()
}
