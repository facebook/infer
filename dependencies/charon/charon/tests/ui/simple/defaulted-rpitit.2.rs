pub trait MyTrait {
    fn foo() -> impl Sized
    where
        Self: Sized,
    {
        ()
    }
}

pub struct S;
impl MyTrait for S {}
