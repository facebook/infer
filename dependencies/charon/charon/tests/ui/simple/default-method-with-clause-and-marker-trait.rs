//@ charon-args=--hide-marker-traits
// https://github.com/AeneasVerif/charon/issues/561
use std::marker::PhantomData;

trait HasAssoc {
    type Assoc;
}
trait Trait {
    fn default_method<T: HasAssoc>() -> T::Assoc {
        todo!()
    }
}

// The `Sized` trait is the culprit.
impl<T: Sized> Trait for T {}

fn main() {}
