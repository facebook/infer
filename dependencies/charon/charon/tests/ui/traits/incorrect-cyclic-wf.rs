//@ known-failure
pub struct Wrap<T: Copy>(T);

pub trait Unwrap {
    type Unwrapped;
}
impl<T: Copy> Unwrap for Wrap<T> {
    type Unwrapped = T;
}

pub trait Trait<T: Unwrap>
where
    T::Unwrapped: Copy,
{
}

pub trait HasVal {
    type Val: Copy;
}

// When trying to prove the required `T::Val: Copy` bound, we use the `T::Unwrapped: Copy` clause
// of `Trait` (because we consider all trait clauses to be implied), which leads to a cycle.
pub fn f<T: HasVal, A: Trait<Wrap<T::Val>>>() {}
