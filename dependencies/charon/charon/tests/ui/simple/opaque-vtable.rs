//@ charon-args=--opaque=crate::x
mod x {
    pub trait Trait {
        fn method(&self);
    }
    impl Trait for () {
        fn method(&self) {}
    }
}

fn use_debug(_d: &dyn x::Trait) {}
fn main() {
    use_debug(&());
}
