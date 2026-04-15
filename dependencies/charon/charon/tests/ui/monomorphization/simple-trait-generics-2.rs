//@ charon-args=--monomorphize
trait Trait<T> {
    fn comsume(&self) -> T;
}

impl Trait<bool> for i32 {
    fn comsume(&self) -> bool { true }
}

impl Trait<usize> for u32 {
    fn comsume(&self) -> usize { 1 }
}


fn main() {
    let a: &dyn Trait<bool> = &42;
    a.comsume();
    let b: &dyn Trait<usize> = &100;
    b.comsume();
}