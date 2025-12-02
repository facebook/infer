pub trait Trait {
    type AssocType;
}
pub type Alias<T> = Option<<T as Trait>::AssocType>;

pub trait C<T> {}

pub struct S<I, F>(I, F)
where
    I: Iterator,
    F: C<I::Item>;

pub type S2<I, F> = S<I, F>;
