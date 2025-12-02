//@ known-failure
//@ charon-args=--remove-associated-types=*
trait Trait {
    fn method()
    where
        Self: Sized;
}

impl Trait for () {
    fn method() {}
}

pub fn main() {
    <() as Trait>::method()
}
