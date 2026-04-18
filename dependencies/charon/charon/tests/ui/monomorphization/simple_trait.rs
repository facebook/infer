//@ charon-args=--monomorphize
trait Trait {
    fn comsume(&self);
}

impl Trait for i32 {
    fn comsume(&self) {}
}

fn main() {
    let b: &dyn Trait = &42;
    b.comsume();
}
