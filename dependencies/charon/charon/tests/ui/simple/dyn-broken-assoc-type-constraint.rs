//! Taken from https://github.com/rust-lang/rust/issues/69398
pub trait Foo {}
impl Foo for () {}

pub trait Broken {
    type Assoc;
    fn broken(&self);
}

impl<T> Broken for T {
    type Assoc = ();
    fn broken(&self)
    where
        Self::Assoc: Foo,
    {
    }
}

fn main() {
    let _m: &dyn Broken<Assoc = ()> = &();
}
