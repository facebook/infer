use crate::prelude::*;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct IndexVec<I: 'static, T: 'static> {
    pub raw: Vec<T>,
    _marker: std::marker::PhantomData<fn(_: &I)>,
}

impl<I: rustc_index::Idx, T: Sized> IndexVec<I, T> {
    pub fn into_iter_enumerated(
        self,
    ) -> impl DoubleEndedIterator<Item = (I, T)> + ExactSizeIterator {
        rustc_index::IndexVec::from_raw(self.raw).into_iter_enumerated()
    }
    pub fn into_iter(self) -> impl DoubleEndedIterator<Item = T> + ExactSizeIterator {
        self.raw.into_iter()
    }
}

impl<I: rustc_index::Idx, T: Sized> std::ops::Deref for IndexVec<I, T> {
    type Target = rustc_index::IndexSlice<I, T>;
    fn deref(&self) -> &Self::Target {
        Self::Target::from_raw(&self.raw)
    }
}

impl<I: rustc_index::Idx, T: Sized> std::ops::DerefMut for IndexVec<I, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Self::Target::from_raw_mut(&mut self.raw)
    }
}

impl<I: rustc_index::Idx, T> From<rustc_index::IndexVec<I, T>> for IndexVec<I, T> {
    fn from(val: rustc_index::IndexVec<I, T>) -> Self {
        IndexVec {
            raw: val.raw,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<S, J: rustc_index::Idx, I: rustc_index::Idx + SInto<S, J>, U: Clone, T: SInto<S, U>>
    SInto<S, IndexVec<J, U>> for rustc_index::IndexSlice<I, T>
{
    fn sinto(&self, s: &S) -> IndexVec<J, U> {
        IndexVec {
            raw: self.raw.sinto(s),
            _marker: std::marker::PhantomData,
        }
    }
}

impl<I, T> FromIterator<T> for IndexVec<I, T>
where
    I: rustc_index::Idx,
{
    #[inline]
    fn from_iter<It: IntoIterator<Item = T>>(iter: It) -> Self {
        Self {
            raw: Vec::from_iter(iter),
            _marker: std::marker::PhantomData,
        }
    }
}
