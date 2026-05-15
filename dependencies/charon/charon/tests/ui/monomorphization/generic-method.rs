//@ charon-arg=--monomorphize
trait Foo {
    fn bar<T>() -> Self;
}

impl Foo for u64 {
    fn bar<T>() -> Self {
        0
    }
}

fn main() {
    <u64 as Foo>::bar::<bool>();
}
