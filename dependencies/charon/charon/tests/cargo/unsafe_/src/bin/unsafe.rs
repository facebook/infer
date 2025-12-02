//! Regression test: the crate name being a keyword created a mismatch between the crate namem we
//! use and the one hax uses, which meant crate items were treated as foreign.
fn main() {
    println!("Hello, world!");
}
