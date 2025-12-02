trait Trait {
    const FOO: usize = 42;
}

impl<T> crate::Trait for T {}

pub fn foo<T>() -> usize {
    T::FOO
}
