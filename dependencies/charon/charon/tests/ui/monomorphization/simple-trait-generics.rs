//@ charon-args=--monomorphize
trait Trait<T> {
    fn comsume(&self) -> T;
}

impl Trait<bool> for i32 {
    fn comsume(&self) -> bool { true }
}


fn main() {
    let a: &dyn Trait<bool> = &42;
    a.comsume();
}