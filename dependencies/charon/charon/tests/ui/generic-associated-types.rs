//@ known-failure
// See also `non-lifetime-gats.rs`

pub trait LendingIterator {
    type Item<'a>
    where
        Self: 'a;

    fn next<'a>(&'a mut self) -> Option<Self::Item<'a>>;
}

impl<'a, T> LendingIterator for Option<&'a T> {
    type Item<'b>
        = &'b T
    where
        Self: 'b;

    fn next<'b>(&'b mut self) -> Option<Self::Item<'b>> {
        if let Some(item) = self {
            let item = &**item;
            Some(item)
        } else {
            None
        }
    }
}

pub fn for_each<I: LendingIterator>(mut iter: I, mut f: impl for<'a> FnMut(I::Item<'a>)) {
    while let Some(item) = iter.next() {
        f(item)
    }
}

pub fn main() {
    let x = 42;
    let iter = Some(&42);
    let mut sum = 0;
    for_each(iter, |item| sum += *item);
    assert_eq!(sum, 42);
}

pub mod lifetimes {
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
}
