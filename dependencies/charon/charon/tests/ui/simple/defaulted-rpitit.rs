pub trait Foo {
    fn bar<const N: usize>() -> impl Sized {
        [0u8; N]
    }
}

pub struct S;
impl Foo for S {}
