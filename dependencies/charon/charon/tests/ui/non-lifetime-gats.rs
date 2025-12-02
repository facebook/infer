//@ known-failure
use std::ops::Deref;

pub trait PointerFamily {
    type Pointer<T>: Deref<Target = T>;

    fn new<T>(value: T) -> Self::Pointer<T>;
}

pub struct BoxFamily;

impl PointerFamily for BoxFamily {
    type Pointer<U> = Box<U>;

    fn new<T>(value: T) -> Self::Pointer<T> {
        Box::new(value)
    }
}

pub fn make_pointer<F: PointerFamily, T>(x: T) -> F::Pointer<T> {
    F::new(x)
}

pub fn main() {
    let _: Box<_> = make_pointer::<BoxFamily, _>(42);
}

pub mod moar_variables {
    // Dummy trait to check we handle variables in clauses correctly.
    pub trait Link<T> {}
    impl<T, U> Link<T> for U {}

    pub trait Trait<T> {
        type Type<U>: Link<T>;
    }

    pub struct Foo;
    impl<T> Trait<Option<T>> for Foo {
        type Type<U> = (T, U);
    }
}
