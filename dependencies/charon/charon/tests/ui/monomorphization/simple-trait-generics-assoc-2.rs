//@ charon-args=--monomorphize
trait Trait<T> {
    type Item;
    fn comsume(&self);
}

impl Trait<usize> for i32 {
    type Item = bool;
    fn comsume(&self) {}
}

impl Trait<bool> for bool {
    type Item = usize;
    fn comsume(&self) {}
}

fn main() {
    let a: &dyn Trait<usize, Item=bool> = &42;
    a.comsume();

    let b: &dyn Trait<bool, Item=usize> = &true;
    b.comsume();
}

