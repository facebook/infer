//@ charon-args=--hide-marker-traits
pub struct UInt<U, B>(U, B);
pub struct UTerm;
pub struct B0;

pub type LongType = UInt<UInt<UInt<UInt<UInt<UInt<UTerm, B0>, B0>, B0>, B0>, B0>, B0>;

pub fn foo<T>() {}
pub fn main() {
    foo::<LongType>();
}
