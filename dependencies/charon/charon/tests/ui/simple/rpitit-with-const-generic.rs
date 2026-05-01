pub trait Foo {
    fn bar<const N: usize>() -> impl Sized;
}

pub struct S;

impl Foo for S {
    fn bar<const N: usize>() -> impl Sized {
        [0u8; N]
    }
}
