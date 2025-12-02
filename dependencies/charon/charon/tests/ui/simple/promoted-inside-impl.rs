//@ charon-args=--mir_optimized
pub struct Foo<F>(F);

impl<F> Foo<F> {
    pub fn method<T>() {
        let _promoted = &0;
    }
}
