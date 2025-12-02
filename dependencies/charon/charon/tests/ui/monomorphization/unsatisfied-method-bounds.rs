//@ known-failure
//@ charon-args=--mono --start-from=crate::main
trait MyIterator {
    type Item;

    fn method(&self) {}

    // This does not apply to all impls, we must monomorphize it only when applicable.
    fn flatten(self) -> Flatten<Self>
    where
        Self: Sized,
        Self::Item: Copy,
    {
        Flatten(self)
    }
}

struct Flatten<I: MyIterator<Item: Copy>>(I);

struct A;
impl MyIterator for A {
    type Item = String;
}

struct B;
impl MyIterator for B {
    type Item = u8;
}

fn main() {
    A.method();
    B.method();
}
