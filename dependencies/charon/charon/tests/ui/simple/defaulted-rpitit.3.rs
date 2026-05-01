pub trait MyTrait<T> {
    fn rows(&self) -> impl Iterator<Item = T> {
        core::iter::empty::<T>()
    }
}

impl<T> MyTrait<T> for () {}
