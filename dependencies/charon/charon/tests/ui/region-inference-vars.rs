//! Regression test for an old behavior of region variables.

pub trait MyTryFrom<T> {
    type Error;
    fn from(v: T) -> Result<Self, Self::Error>
    where
        Self: Sized;
}

impl MyTryFrom<&bool> for bool {
    type Error = ();
    fn from(v: &bool) -> Result<Self, Self::Error> {
        Ok(*v)
    }
}
