struct Foo<T> {
    vec: Vec<T>,
}

impl<T> Drop for Foo<T> {
    fn drop(&mut self) {
        let _ = ();
    }
}

fn drop_foo(_: Foo<u32>) {}
