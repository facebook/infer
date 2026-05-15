pub trait MyTrait {
    fn foo<'a>() -> impl Sized
    where
        Self: 'a;
}
