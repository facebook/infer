//@ charon-args=--lift-associated-types=*
pub trait HasOutput {
    type Output;
}

pub trait Bound {}

pub trait HasBlockSize {
    type BlockSize: HasOutput;
}

pub trait HasReaderCore {
    type ReaderCore: HasBlockSize;
}

pub struct Wrapper<T: HasBlockSize>(T)
where
    <T::BlockSize as HasOutput>::Output: Bound;

pub fn foo<T>() -> Wrapper<T::ReaderCore>
where
    T: HasReaderCore,
    <T::ReaderCore as HasBlockSize>::BlockSize: HasOutput,
    <<T::ReaderCore as HasBlockSize>::BlockSize as HasOutput>::Output: Bound,
{
    panic!()
}
