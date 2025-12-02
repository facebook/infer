//! Shared utility functions for use in tests.
//!
//! This is in `util/mod.rs` instead of `util.rs` to avoid cargo treating it like a test file.

// Needed because this is imported from various tests that each use different items from this
// module.
#![allow(dead_code)]
use assert_cmd::prelude::{CommandCargoExt, OutputAssertExt};
use itertools::Itertools;
use snapbox;
use snapbox::filter::Filter;
use std::fmt::Display;
use std::path::Path;
use std::{fs::File, io::BufReader, process::Command};

use charon_lib::ast::*;
use charon_lib::{export::CrateData, logger};

#[derive(Clone, Copy)]
pub enum Action {
    Verify,
    Overwrite,
}

/// Depending on `action`, either check that the contents of `path` matches `output`, or overwrite
/// the file with the given output.
pub fn compare_or_overwrite(
    action: Action,
    output: String,
    path: &Path,
) -> snapbox::assert::Result<()> {
    let output = strip_ansi_escapes::strip_str(output);
    let actual = snapbox::Data::text(output);
    let actual = snapbox::filter::FilterNewlines.filter(actual);
    match action {
        Action::Verify => expect_file_contents(path, actual)?,
        Action::Overwrite => actual.write_to_path(path)?,
    }
    Ok(())
}

/// Compare the file contents with the provided string and error with a diff if they differ.
fn expect_file_contents(path: &Path, actual: snapbox::Data) -> snapbox::assert::Result<()> {
    let expected = snapbox::Data::read_from(path, Some(snapbox::data::DataFormat::Text));
    let expected = snapbox::filter::FilterNewlines.filter(expected);

    if expected != actual {
        let mut buf = String::new();
        snapbox::report::write_diff(
            &mut buf,
            &expected,
            &actual,
            Some(&path.display()),
            Some(&"charon output"),
            Default::default(),
        )
        .map_err(|e| e.to_string())?;
        Err(buf.into())
    } else {
        Ok(())
    }
}

/// Given a string that contains rust code, this calls charon on it and returns the result.
pub fn translate_rust_text(
    code: impl Display,
    options: &[&str],
) -> anyhow::Result<TranslatedCrate> {
    // Initialize the logger
    logger::initialize_logger();

    // Write the code to a temporary file.
    use std::io::Write;
    let tmp_dir = tempfile::TempDir::new()?;
    let input_path = tmp_dir.path().join("test_crate.rs");
    {
        let mut tmp_file = File::create(&input_path)?;
        write!(tmp_file, "{}", code)?;
        drop(tmp_file);
    }

    // Call charon
    let output_path = tmp_dir.path().join("test_crate.llbc");
    Command::cargo_bin("charon")?
        .arg("rustc")
        .arg("--dest-file")
        .arg(&output_path)
        .args(options)
        .arg("--")
        .arg("--edition=2021")
        .arg("--crate-type=rlib")
        .arg(input_path)
        .assert()
        .try_success()?;

    // Extract the computed crate data.
    let crate_data: CrateData = {
        let file = File::open(output_path)?;
        let reader = BufReader::new(file);
        serde_json::from_reader(reader)?
    };

    Ok(crate_data.translated)
}

/// `Name` is a complex datastructure; to inspect it we serialize it a little bit.
pub fn repr_name(crate_data: &TranslatedCrate, n: &Name) -> String {
    n.name
        .iter()
        .map(|path_elem| match path_elem {
            PathElem::Ident(i, _) => i.clone(),
            PathElem::Impl(elem) => match elem {
                ImplElem::Trait(impl_id) => match crate_data.trait_impls.get(*impl_id) {
                    None => format!("<trait impl#{impl_id}>"),
                    Some(timpl) => {
                        let trait_name = trait_name(crate_data, timpl.impl_trait.id);
                        format!("<impl {trait_name} for ??>")
                    }
                },
                ImplElem::Ty(..) => "<inherent impl>".to_string(),
            },
            PathElem::Monomorphized(..) => "<mono>".to_string(),
        })
        .join("::")
}

pub fn repr_span(span: Span) -> String {
    let raw_span = span.span;
    format!("{}-{}", raw_span.beg, raw_span.end)
}

pub fn trait_name(crate_data: &TranslatedCrate, trait_id: TraitDeclId) -> &str {
    let tr = &crate_data.trait_decls[trait_id];
    let PathElem::Ident(trait_name, _) = tr.item_meta.name.name.last().unwrap() else {
        panic!()
    };
    trait_name
}
