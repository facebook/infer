//@ aux-crate=foreign-constant-aux.rs
fn foo() -> u8 {
    foreign_constant_aux::CONSTANT
}
