//! This tests that we order decl methods _after_ the trait.
#![feature(register_tool)]
#![register_tool(charon)]
use foo::Trait;
fn main() {
    let _ = ().defaulted();
}

// Make the module opaque so the trait is discovered like a foreign trait. This affects order.
#[charon::opaque]
mod foo {
    pub trait Trait {
        fn defaulted(&self) {}
    }

    impl Trait for () {}
}
