//! See https://github.com/rust-lang/rust/issues/41756.
use std::fmt::Debug;

trait Left<T> {}
trait Right<T> {}

trait Join<U> {
    fn test();
}

// If we swap the two clauses, a different type is selected in `try_it`.
impl<T, U> Join<U> for T
where
    T: Left<U>,
    T: Right<U>,
    U: Default + Debug,
{
    fn test() {
        println!("{:?}", U::default())
    }
}

impl<T, U: Default + Debug> Left<U> for T {}
impl<T, U: Default + Debug> Right<U> for T {}

fn try_it<T: Default + Debug>()
where
    T: Left<bool>,
    T: Right<()>,
{
    <T as Join<_>>::test()
}
