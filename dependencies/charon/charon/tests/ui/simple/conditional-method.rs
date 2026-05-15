//@ known-failure
struct NeedsClone<T: Clone>(T);

trait Trait {
    fn method(&self) -> NeedsClone<Self>
    where
        Self: Clone,
    {
        loop {}
    }
}

struct Foo<T>(T);

impl<T> Trait for Foo<T> {}

#[charon::error] // trigger an error so we get the stderr output
const _: () = ();
