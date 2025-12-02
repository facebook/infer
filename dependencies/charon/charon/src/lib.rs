//! This library contains utilities to extract the MIR from a Rust project,
//! by compiling it to an easy-to-use AST called LLBC (Low-Level Borrow Calculus).
//! This AST is serialized into JSON files.
//!
//! A good entry point to explore the project is [`driver`](../charon_driver/index.html),
//! and in particular [`driver::CharonCallbacks`](../charon_driver/driver/struct.CharonCallbacks.html),
//! which implements the callback which we provide to Rustc.
//!
//! The ASTs are in [`ullbc_ast`] (Unstructured LLBC - basically
//! a cleaned-up version of MIR) and [`llbc_ast`] (same as ULLBC, but
//! we reconstructed the control-flow to have `if ... then ... else ...`,
//! loops, etc. instead of `GOTO`s).

// For rustdoc: prevents overflows
#![recursion_limit = "256"]
#![expect(incomplete_features)]
#![feature(assert_matches)]
#![feature(box_patterns)]
#![feature(deref_patterns)]
#![feature(deref_pure_trait)]
#![feature(if_let_guard)]
#![feature(impl_trait_in_assoc_type)]
#![feature(iterator_try_collect)]
#![feature(trait_alias)]
#![feature(register_tool)]
// For when we use charon on itself :3
#![register_tool(charon)]

#[macro_use]
pub mod ids;
#[macro_use]
pub mod logger;
pub mod ast;
pub mod common;
pub mod errors;
pub mod export;
pub mod name_matcher;
pub mod options;
pub mod pretty;
pub mod transform;

// Re-export all the ast modules so we can keep the old import structure.
pub use ast::{builtins, expressions, gast, llbc_ast, meta, names, types, ullbc_ast, values};
pub use pretty::formatter;
pub use transform::{graphs, reorder_decls, ullbc_to_llbc};

/// The version of the crate, as defined in `Cargo.toml`.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Read a `.llbc` file.
pub fn deserialize_llbc(path: &std::path::Path) -> anyhow::Result<ast::TranslatedCrate> {
    use crate::export::CrateData;
    use anyhow::Context;
    use serde::Deserialize;
    use std::fs::File;
    use std::io::BufReader;
    let file = File::open(&path)
        .with_context(|| format!("Failed to read llbc file {}", path.display()))?;
    let reader = BufReader::new(file);
    let mut deserializer = serde_json::Deserializer::from_reader(reader);
    // Deserialize without recursion limit.
    deserializer.disable_recursion_limit();
    // Grow stack space as needed.
    let deserializer = serde_stacker::Deserializer::new(&mut deserializer);
    Ok(CrateData::deserialize(deserializer)?.translated)
}
