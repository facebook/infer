//@ charon-args=--monomorphize
trait Trait<T> {
    type Item;
    fn comsume(&self);
}

impl Trait<usize> for i32 {
    type Item = bool;
    fn comsume(&self) {}
}


fn main() {
    let a: &dyn Trait<usize, Item=bool> = &42;
    a.comsume();
}