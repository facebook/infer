//! A vector with custom index types.
//!
//! This data-structure is mostly meant to be used with the index types defined
//! with [`crate::generate_index_type!`]: by using custom index types, we
//! leverage the type checker to prevent us from mixing them.

pub use index_vec::Idx;
use index_vec::IdxSliceIndex;
use serde::{Deserialize, Serialize, Serializer};
use serde_state::{DeserializeState, SerializeState};
use std::{
    iter::{FromIterator, IntoIterator},
    ops::{ControlFlow, Deref, DerefMut, Index, IndexMut},
};

use derive_generic_visitor::*;

/// Contiguous indexed vector.
///
// A thin wrapper around `IndexVec` that adds some methods and impos.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndexVec<I, T>
where
    I: Idx,
{
    vector: index_vec::IndexVec<I, T>,
}

impl<I: std::fmt::Debug, T: std::fmt::Debug> std::fmt::Debug for IndexVec<I, T>
where
    I: Idx,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <index_vec::IndexVec<_, _> as std::fmt::Debug>::fmt(&self.vector, f)
    }
}

impl<I: Idx, T> Deref for IndexVec<I, T> {
    type Target = index_vec::IndexVec<I, T>;
    fn deref(&self) -> &Self::Target {
        &self.vector
    }
}
impl<I: Idx, T> DerefMut for IndexVec<I, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vector
    }
}

impl<I, T> IndexVec<I, T>
where
    I: Idx,
{
    pub fn new() -> Self {
        IndexVec {
            vector: index_vec::IndexVec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        IndexVec {
            vector: index_vec::IndexVec::with_capacity(capacity),
        }
    }

    pub fn from_vec(v: Vec<T>) -> Self {
        Self {
            vector: index_vec::IndexVec::from_vec(v),
        }
    }

    pub fn from_array<const N: usize>(v: [T; N]) -> Self {
        v.into_iter().collect()
    }

    /// Shadow the `index_vec::IndexVec` method because it silently shifts ids.
    pub fn remove(&mut self, _: I) -> Option<T>
    where
        // Make it not callable.
        String: Copy,
    {
        panic!("remove")
    }

    pub fn push_with(&mut self, f: impl FnOnce(I) -> T) -> I {
        let id = self.next_idx();
        let x = f(id);
        self.push(x);
        id
    }

    pub fn extend_from_slice(&mut self, other: &Self)
    where
        T: Clone,
    {
        self.vector.extend_from_slice(&other.vector);
    }

    /// Insert a value at that index, shifting all the values with equal or larger indices.
    pub fn insert_and_shift_ids(&mut self, id: I, x: T) {
        self.vector.insert(id, x)
    }

    /// Map each entry to a new one, keeping the same ids.
    pub fn map<U>(self, f: impl FnMut(T) -> U) -> IndexVec<I, U> {
        IndexVec {
            vector: self.vector.into_iter().map(f).collect(),
        }
    }

    /// Map each entry to a new one, keeping the same ids.
    pub fn map_ref<'a, U>(&'a self, f: impl FnMut(&'a T) -> U) -> IndexVec<I, U> {
        IndexVec {
            vector: self.vector.iter().map(f).collect(),
        }
    }

    /// Map each entry to a new one, keeping the same ids.
    pub fn map_ref_mut<'a, U>(&'a mut self, f: impl FnMut(&'a mut T) -> U) -> IndexVec<I, U> {
        IndexVec {
            vector: self.vector.iter_mut().map(f).collect(),
        }
    }

    /// Map each entry to a new one, keeping the same ids.
    pub fn map_ref_indexed<'a, U>(&'a self, mut f: impl FnMut(I, &'a T) -> U) -> IndexVec<I, U> {
        IndexVec {
            vector: self
                .vector
                .iter_enumerated()
                .map(|(i, x)| f(i, x))
                .collect(),
        }
    }

    // TODO: rename once we've migrated from `IndexMap` completely.
    pub fn iter_indexed(&self) -> impl Iterator<Item = (I, &T)> {
        self.vector.iter_enumerated()
    }

    pub fn iter_mut_indexed(&mut self) -> impl Iterator<Item = (I, &mut T)> {
        self.vector.iter_mut_enumerated()
    }

    pub fn into_iter_indexed(self) -> impl Iterator<Item = (I, T)> {
        self.vector.into_iter_enumerated()
    }

    pub fn iter_indexed_values(&self) -> impl Iterator<Item = (I, &T)> {
        self.iter_indexed()
    }

    pub fn into_iter_indexed_values(self) -> impl Iterator<Item = (I, T)> {
        self.into_iter_indexed()
    }

    pub fn iter_indices(&self) -> impl Iterator<Item = I> + '_ {
        self.vector.indices()
    }

    pub fn all_indices(&self) -> impl Iterator<Item = I> + use<I, T> {
        self.vector.indices()
    }

    /// Like `Vec::split_off`.
    pub fn split_off(&mut self, at: usize) -> Self {
        Self {
            vector: self.vector.split_off(I::from_usize(at)),
        }
    }
}

impl<I: Idx, T> Default for IndexVec<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I, R, T> Index<R> for IndexVec<I, T>
where
    I: Idx,
    R: IdxSliceIndex<I, T, Output = T>,
{
    type Output = T;
    fn index(&self, index: R) -> &Self::Output {
        &self.vector[index]
    }
}

impl<I, R, T> IndexMut<R> for IndexVec<I, T>
where
    I: Idx,
    R: IdxSliceIndex<I, T, Output = T>,
{
    fn index_mut(&mut self, index: R) -> &mut Self::Output {
        &mut self.vector[index]
    }
}

impl<'a, I, T> IntoIterator for &'a IndexVec<I, T>
where
    I: Idx,
{
    type Item = &'a T;
    type IntoIter = impl Iterator<Item = &'a T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vector.iter()
    }
}

impl<'a, I, T> IntoIterator for &'a mut IndexVec<I, T>
where
    I: Idx,
{
    type Item = &'a mut T;
    type IntoIter = impl Iterator<Item = &'a mut T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vector.iter_mut()
    }
}

impl<I, T> IntoIterator for IndexVec<I, T>
where
    I: Idx,
{
    type Item = T;
    type IntoIter = impl Iterator<Item = T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vector.into_iter()
    }
}

// FIXME: this impl is a footgun
impl<I, T> FromIterator<T> for IndexVec<I, T>
where
    I: Idx,
{
    #[inline]
    fn from_iter<It: IntoIterator<Item = T>>(iter: It) -> IndexVec<I, T> {
        IndexVec {
            vector: index_vec::IndexVec::from_iter(iter),
        }
    }
}

impl<I: Idx, T: Serialize> Serialize for IndexVec<I, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.vector.serialize(serializer)
    }
}

impl<I: Idx, State, T: SerializeState<State>> SerializeState<State> for IndexVec<I, T> {
    fn serialize_state<S>(&self, state: &State, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.vector.as_vec().serialize_state(state, serializer)
    }
}

impl<'de, I: Idx, T: Deserialize<'de>> Deserialize<'de> for IndexVec<I, T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self {
            vector: Deserialize::deserialize(deserializer)?,
        })
    }
}

impl<'de, I: Idx, State, T: DeserializeState<'de, State>> DeserializeState<'de, State>
    for IndexVec<I, T>
{
    fn deserialize_state<D>(state: &State, deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let vec: Vec<_> = DeserializeState::deserialize_state(state, deserializer)?;
        Ok(Self {
            vector: index_vec::IndexVec::from(vec),
        })
    }
}

impl<'s, I: Idx, T, V: Visit<'s, T>> Drive<'s, V> for IndexVec<I, T> {
    fn drive_inner(&'s self, v: &mut V) -> ControlFlow<V::Break> {
        for x in self {
            v.visit(x)?;
        }
        Continue(())
    }
}
impl<'s, I: Idx, T, V: VisitMut<'s, T>> DriveMut<'s, V> for IndexVec<I, T> {
    fn drive_inner_mut(&'s mut self, v: &mut V) -> ControlFlow<V::Break> {
        for x in self {
            v.visit(x)?;
        }
        Continue(())
    }
}
