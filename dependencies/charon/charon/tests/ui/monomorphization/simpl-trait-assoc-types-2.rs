//@ charon-args=--monomorphize
trait Trait {
    type Item;
    fn comsume(&self);
}

impl Trait for i32 {
    type Item = bool;
    fn comsume(&self) {}
}

impl Trait for usize {
    type Item = u32;
    fn comsume(&self) {}
}


fn main() {
    let a: &dyn Trait<Item=bool> = &42;
    a.comsume();
    let b: &dyn Trait<Item=u32> = &(42 as usize);
    b.comsume();
}