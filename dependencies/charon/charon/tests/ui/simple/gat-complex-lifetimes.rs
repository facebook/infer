pub trait Foo<T> {
    fn foo(self) -> T;
}

pub trait Bar<'a> {
    type Type<'b>: for<'c> Foo<&'a &'b &'c ()>;
}

pub fn bar<'x, 'y, 'z, T>(x: <T as Bar<'x>>::Type<'y>) -> &'x &'y &'z ()
where
    T: Bar<'x>,
{
    x.foo()
}
