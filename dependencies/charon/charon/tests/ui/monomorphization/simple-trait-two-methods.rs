//@ charon-args=--monomorphize
trait Trait {
    fn consume(&self);
    fn another(&self);
}

impl Trait for i32 {
    fn consume(&self) {}
    fn another(&self) {}
}

fn main() {
    let b: &dyn Trait = &42;
    b.consume();
}
