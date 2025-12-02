//! This crate tests the error messages that indicates why we attempt to translate a given
//! definition. We try some fun mutual dependencies to test the output, in particular the
//! multi-file output.
#![feature(pattern)]
#![feature(register_tool)]
#![register_tool(charon)]

#[charon::opaque]
mod module;

#[charon::opaque]
mod opaque {
    pub fn fun2() {
        crate::module::fun3()
    }
    pub fn takes_pattern<T: std::str::pattern::Pattern>() {}
}

fn main() {
    crate::module::fun1()
}
