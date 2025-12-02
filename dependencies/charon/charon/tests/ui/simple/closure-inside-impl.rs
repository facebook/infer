pub struct Foo<F>(F);

impl<F> Foo<F> {
    pub fn method<T>() {
        let _closure = |_x: ()| ();
    }
}
