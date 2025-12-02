//@ charon-args=--opaque test_crate::opaque
//! We're only translating `Product` because it appears in the clauses of the `Binder` that we
//! construct unconditionally even for methods we won't end up translating.
//! Ideally we wouldn't translate `Product` either.
mod opaque {
    pub trait Product {}
    pub trait Iterator {
        fn product<P: Product>() {}
    }
}
fn foo<T: opaque::Iterator>() {}
