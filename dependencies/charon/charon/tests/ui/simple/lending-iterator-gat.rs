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
            *self = None;
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
