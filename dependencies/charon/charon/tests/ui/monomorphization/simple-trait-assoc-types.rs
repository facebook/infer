//@ charon-args=--monomorphize
trait Trait {
    type Item;
    fn comsume(&self);
}

impl Trait for i32 {
    type Item = bool;
    fn comsume(&self) {}
}


fn main() {
    let a: &dyn Trait<Item=bool> = &42;
    a.comsume();
}