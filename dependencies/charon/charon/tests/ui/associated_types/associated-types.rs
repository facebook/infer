//@ charon-args=--remove-associated-types=*
#![feature(associated_type_defaults)]
trait Foo<'a>: Copy {
    type Item: Clone + 'a;

    fn use_item_required(x: Self::Item) -> Self::Item;
    fn use_item_provided(x: Self::Item) -> Self::Item
    where
        Self::Item: Foo<'a>,
    {
        x
    }
}

impl<'a, T> Foo<'a> for &'a T {
    type Item = Option<&'a T>;

    fn use_item_required(x: Self::Item) -> Self::Item {
        x
    }
}

impl<'a, T: Copy + 'a> Foo<'a> for Option<T> {
    type Item = T;
    fn use_item_required(x: Self::Item) -> Self::Item {
        x
    }
}

fn external_use_item<'a, T: Foo<'a>>(x: T::Item) -> T::Item
where
    T::Item: Foo<'a>,
{
    T::use_item_provided(x).clone()
}

fn call_fn() {
    let _ = external_use_item::<&bool>(None);
}

fn type_equality<'a, I, J>(x: I::Item) -> J::Item
where
    I: Foo<'a>,
    J: Foo<'a, Item = I::Item>,
{
    x
}

mod loopy {
    trait Bar {
        type BarTy;
    }
    impl Bar for () {
        type BarTy = bool;
    }

    // One way of being recursive.
    trait Foo {
        type FooTy: Foo + Bar;
    }
    impl Foo for () {
        type FooTy = ();
    }

    // Another way of being recursive.
    trait Baz<T: Baz<T> + Bar> {
        type BazTy;
    }
    impl Baz<()> for () {
        type BazTy = usize;
    }
}

mod loopy_with_generics {
    trait Bar<'a, T, U> {
        type BarTy;
    }
    impl<'a, T: 'a> Bar<'a, u8, T> for () {
        type BarTy = &'a T;
    }

    trait Foo<'b, T> {
        type FooTy: Foo<'b, T> + Bar<'b, u8, Option<T>>;
    }
    impl Foo<'static, u16> for () {
        type FooTy = ();
    }
}

mod cow {
    enum Cow<'a, B>
    where
        B: 'a + ToOwned + ?Sized,
    {
        Borrowed(&'a B),
        Owned(<B as ToOwned>::Owned),
    }
}

mod params {
    trait Foo<'a, T: 'a> {
        type X: 'a;
        type Item = &'a (T, Self::X);
    }
    impl<'a, T: 'a> Foo<'a, Option<T>> for () {
        type X = &'a ();
    }
}
