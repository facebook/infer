//@ charon-args=--remove-associated-types=*
pub trait IntoIterator {
    type Item;
}

pub trait FromIterator<A>: Sized {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = A>;
}

impl FromIterator<()> for () {
    fn from_iter<I: IntoIterator<Item = ()>>(iter: I) -> () {}
}
