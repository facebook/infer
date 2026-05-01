//@ charon-args=--lift-associated-types=_
pub trait Iterator {
    type Item;
}

pub trait IsIterator {}

impl<I: Iterator> IsIterator for I {}

fn assert_is_iterator<I>(it: I)
where
    I: IsIterator,
{
}

pub trait IntoIterator {
    type Item;
    type IntoIter: Iterator<Item = Self::Item>;
}

fn check<I: IntoIterator>(it: I::IntoIter) {
    assert_is_iterator(it);
}
