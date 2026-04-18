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
