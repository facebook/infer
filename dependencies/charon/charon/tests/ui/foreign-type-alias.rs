//@ aux-crate=type_alias.rs
//@ charon-args=--start-from=type_alias
//@ charon-args=--include=type_alias
//! Tests that we can extract type aliases from foreign crates.
extern crate type_alias; // Otherwise it's not accessible.
