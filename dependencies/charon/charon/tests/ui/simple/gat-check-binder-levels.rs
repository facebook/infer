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
