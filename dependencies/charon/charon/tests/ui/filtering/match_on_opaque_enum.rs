//@ known-failure
//@ charon-args=--opaque crate::Enum
pub enum Enum {
    A,
    B,
}

pub fn is_a(x: &Enum) -> bool {
    match x {
        Enum::A => true,
        Enum::B => false,
    }
}
