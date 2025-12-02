//@ known-failure
//! This file tests the error messages that indicates why we attempt to translate a given
//! definition.
#![feature(pattern)]
#![feature(register_tool)]
#![register_tool(charon)]

pub fn main() {
    let _ = "".contains("");
    let _ = opaque::other_error();
}

#[charon::opaque]
mod opaque {
    pub fn other_error() {
        let _ = takes_pattern::<&str>();
    }
    fn takes_pattern<T: std::str::pattern::Pattern>() {}
}
