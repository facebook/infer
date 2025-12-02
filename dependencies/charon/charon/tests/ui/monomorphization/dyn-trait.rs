//@ known-failure
//@ charon-args=--monomorphize
//@ charon-args=--start-from=crate::main
use std::fmt::Display;

fn dyn_to_string(x: &dyn Display) -> String {
    x.to_string()
}

fn main() {
    let str: String = "hello".to_string();
    let _ = dyn_to_string(&str);
}
