//@ charon-args=--monomorphize
//@ charon-args=--start-from=crate::main
// Ensures monomorphization happens when trait implementations are involved.
pub trait Trait {
    fn method(&self);
}

impl Trait for bool {
    fn method(&self) {}
}

fn do_test<T: Trait>(x: T) {
    x.method()
}

fn main() {
    do_test(true);
}
