//! A vector with custom index types.
//!
//! This data-structure is mostly meant to be used with the index types defined
//! with [`crate::generate_index_type!`]: by using custom index types, we
//! leverage the type checker to prevent us from mixing them.
//!
//! Note that this data structure is implemented by using persistent vectors.
//! This makes the clone operation almost a no-op.

use index_vec::{Idx, IdxSliceIndex, IndexVec};
use serde::{Deserialize, Serialize, Serializer};
use std::{
    iter::{FromIterator, IntoIterator},
    mem,
    ops::{ControlFlow, Deref, Index, IndexMut},
};

use derive_generic_visitor::*;

/// Indexed vector.
/// To prevent accidental id reuse, the vector supports reserving a slot to be filled later.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Vector<I, T>
where
    I: Idx,
{
    vector: IndexVec<I, Option<T>>,
    /// The number of non-`None` elements.
    elem_count: usize,
}

impl<I: std::fmt::Debug, T: std::fmt::Debug> std::fmt::Debug for Vector<I, T>
where
    I: Idx,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <IndexVec<_, _> as std::fmt::Debug>::fmt(&self.vector, f)
    }
}

pub struct ReservedSlot<I: Idx>(I);

impl<I, T> Vector<I, T>
where
    I: Idx,
{
    pub fn new() -> Self {
        Vector {
            vector: IndexVec::new(),
            elem_count: 0,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Vector {
            vector: IndexVec::with_capacity(capacity),
            elem_count: 0,
        }
    }

    pub fn get(&self, i: I) -> Option<&T> {
        self.vector.get(i).map(Option::as_ref).flatten()
    }

    pub fn get_mut(&mut self, i: I) -> Option<&mut T> {
        self.vector.get_mut(i).map(Option::as_mut).flatten()
    }

    pub fn is_empty(&self) -> bool {
        self.elem_count == 0
    }

    /// The number of elements stored in the vector.
    pub fn elem_count(&self) -> usize {
        self.elem_count
    }

    /// The number of slots allocated in the vector (empty or not).
    pub fn slot_count(&self) -> usize {
        self.vector.len()
    }

    /// Gets the value of the next available id. Avoid if possible; use `reserve_slot` instead.
    pub fn next_id(&self) -> I {
        self.vector.next_idx()
    }

    /// Reserve a spot in the vector.
    pub fn reserve_slot(&mut self) -> I {
        // Push a `None` to ensure we don't reuse the id.
        self.vector.push(None)
    }

    /// Fill the reserved slot.
    pub fn set_slot(&mut self, id: I, x: T) {
        assert!(self.vector[id].is_none());
        self.vector[id] = Some(x);
        self.elem_count += 1;
    }

    /// Remove the value from this slot, leaving other ids unchanged.
    pub fn remove(&mut self, id: I) -> Option<T> {
        if self.vector[id].is_some() {
            self.elem_count -= 1;
        }
        self.vector[id].take()
    }

    /// Remove the value from this slot, shifting other ids as needed.
    pub fn remove_and_shift_ids(&mut self, id: I) -> Option<T> {
        if id.index() >= self.slot_count() {
            return None;
        }
        if self.vector[id].is_some() {
            self.elem_count -= 1;
        }
        self.vector.remove(id)
    }

    /// Remove the last slot.
    pub fn pop(&mut self) -> Option<T> {
        if self.vector.last().is_some() {
            self.elem_count -= 1;
        }
        self.vector.pop().flatten()
    }

    pub fn push(&mut self, x: T) -> I {
        self.elem_count += 1;
        self.vector.push(Some(x))
    }

    pub fn push_with(&mut self, f: impl FnOnce(I) -> T) -> I {
        let id = self.reserve_slot();
        let x = f(id);
        self.set_slot(id, x);
        id
    }

    pub fn push_all<It>(&mut self, it: It) -> impl Iterator<Item = I> + use<'_, I, T, It>
    where
        It: IntoIterator<Item = T>,
    {
        it.into_iter().map(move |x| self.push(x))
    }

    pub fn extend<It>(&mut self, it: It)
    where
        It: IntoIterator<Item = T>,
    {
        self.push_all(it).for_each(|_| ())
    }

    pub fn extend_from_slice(&mut self, other: &Self)
    where
        T: Clone,
    {
        self.vector.extend_from_slice(&other.vector);
        self.elem_count += other.elem_count;
    }

    /// Insert a value at that index, shifting all the values with equal or larger indices.
    pub fn insert_and_shift_ids(&mut self, id: I, x: T) {
        self.elem_count += 1;
        self.vector.insert(id, Some(x))
    }

    /// Get a mutable reference into the ith element. If the vector is too short, extend it until
    /// it has enough elements. If the element doesn't exist, use the provided function to
    /// initialize it.
    pub fn get_or_extend_and_insert(&mut self, id: I, f: impl FnOnce() -> T) -> &mut T {
        if id.index() >= self.vector.len() {
            self.vector.resize_with(id.index() + 1, || None);
        }
        self.vector[id].get_or_insert_with(f)
    }

    /// Map each entry to a new one, keeping the same ids.
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> Vector<I, U> {
        Vector {
            vector: self
                .vector
                .into_iter()
                .map(|x_opt| x_opt.map(&mut f))
                .collect(),
            elem_count: self.elem_count,
        }
    }

    /// Map each entry to a new one, keeping the same ids.
    pub fn map_ref<'a, U>(&'a self, mut f: impl FnMut(&'a T) -> U) -> Vector<I, U> {
        Vector {
            vector: self
                .vector
                .iter()
                .map(|x_opt| x_opt.as_ref().map(&mut f))
                .collect(),
            elem_count: self.elem_count,
        }
    }

    /// Map each entry to a new one, keeping the same ids.
    pub fn map_ref_mut<'a, U>(&'a mut self, mut f: impl FnMut(&'a mut T) -> U) -> Vector<I, U> {
        Vector {
            vector: self
                .vector
                .iter_mut()
                .map(|x_opt| x_opt.as_mut().map(&mut f))
                .collect(),
            elem_count: self.elem_count,
        }
    }

    /// Map each entry to a new one, keeping the same ids.
    pub fn map_ref_indexed<'a, U>(&'a self, mut f: impl FnMut(I, &'a T) -> U) -> Vector<I, U> {
        Vector {
            vector: self
                .vector
                .iter_enumerated()
                .map(|(i, x_opt)| x_opt.as_ref().map(|x| f(i, x)))
                .collect(),
            elem_count: self.elem_count,
        }
    }

    /// Map each entry to a new one, keeping the same ids. Includes empty slots.
    pub fn map_opt<U>(self, f: impl FnMut(Option<T>) -> Option<U>) -> Vector<I, U> {
        Vector {
            vector: self.vector.into_iter().map(f).collect(),
            elem_count: self.elem_count,
        }
    }

    /// Map each entry to a new one, keeping the same ids. Includes empty slots.
    pub fn map_ref_opt<'a, U>(
        &'a self,
        mut f: impl FnMut(Option<&'a T>) -> Option<U>,
    ) -> Vector<I, U> {
        let mut ret = Vector {
            vector: self.vector.iter().map(|x_opt| f(x_opt.as_ref())).collect(),
            elem_count: self.elem_count,
        };
        ret.elem_count = ret.iter().count();
        ret
    }

    /// Iter over the nonempty slots.
    pub fn iter(&self) -> impl Iterator<Item = &T> + DoubleEndedIterator + Clone {
        self.vector.iter().filter_map(|opt| opt.as_ref())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> + DoubleEndedIterator {
        self.vector.iter_mut().filter_map(|opt| opt.as_mut())
    }

    pub fn iter_indexed(&self) -> impl Iterator<Item = (I, &T)> {
        self.vector
            .iter_enumerated()
            .flat_map(|(i, opt)| Some((i, opt.as_ref()?)))
    }

    pub fn iter_mut_indexed(&mut self) -> impl Iterator<Item = (I, &mut T)> {
        self.vector
            .iter_mut_enumerated()
            .flat_map(|(i, opt)| Some((i, opt.as_mut()?)))
    }

    pub fn into_iter_indexed(self) -> impl Iterator<Item = (I, T)> {
        self.vector
            .into_iter_enumerated()
            .flat_map(|(i, opt)| Some((i, opt?)))
    }

    pub fn iter_indexed_values(&self) -> impl Iterator<Item = (I, &T)> {
        self.iter_indexed()
    }

    pub fn into_iter_indexed_values(self) -> impl Iterator<Item = (I, T)> {
        self.into_iter_indexed()
    }

    /// Iterate over all slots, even empty ones.
    pub fn iter_all_slots(&self) -> impl Iterator<Item = &Option<T>> {
        self.vector.iter()
    }

    pub fn iter_indexed_all_slots(&self) -> impl Iterator<Item = (I, &Option<T>)> {
        self.vector.iter_enumerated()
    }

    pub fn iter_indices(&self) -> impl Iterator<Item = I> + '_ {
        // Reuse `iter_indexed` to filter only the filled indices.
        self.iter_indexed().map(|(id, _)| id)
    }

    pub fn all_indices(&self) -> impl Iterator<Item = I> + use<I, T> {
        self.vector.indices()
    }

    /// Remove matching items and return and iterator over the removed items. This is lazy: items
    /// are only removed as the iterator is consumed.
    pub fn extract<'a, F: FnMut(&mut T) -> bool>(
        &'a mut self,
        mut f: F,
    ) -> impl Iterator<Item = (I, T)> + use<'a, I, T, F> {
        let elem_count = &mut self.elem_count;
        self.vector
            .iter_mut_enumerated()
            .filter_map(move |(i, opt)| {
                if f(opt.as_mut()?) {
                    *elem_count -= 1;
                    let elem = mem::replace(opt, None)?;
                    Some((i, elem))
                } else {
                    None
                }
            })
    }

    /// Remove the elements that don't match the predicate.
    pub fn retain(&mut self, mut f: impl FnMut(&mut T) -> bool) {
        self.extract(|x| !f(x)).for_each(drop);
    }

    /// Like `Vec::split_off`.
    pub fn split_off(&mut self, at: usize) -> Self {
        let mut ret = Self {
            vector: self.vector.split_off(I::from_usize(at)),
            elem_count: 0,
        };
        self.elem_count = self.iter().count();
        ret.elem_count = ret.iter().count();
        ret
    }
}

impl<I: Idx, T> Default for Vector<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Idx> Deref for ReservedSlot<I> {
    type Target = I;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<I, R, T> Index<R> for Vector<I, T>
where
    I: Idx,
    R: IdxSliceIndex<I, Option<T>, Output = Option<T>>,
{
    type Output = T;
    fn index(&self, index: R) -> &Self::Output {
        self.vector[index].as_ref().unwrap()
    }
}

impl<I, R, T> IndexMut<R> for Vector<I, T>
where
    I: Idx,
    R: IdxSliceIndex<I, Option<T>, Output = Option<T>>,
{
    fn index_mut(&mut self, index: R) -> &mut Self::Output {
        self.vector[index].as_mut().unwrap()
    }
}

impl<'a, I, T> IntoIterator for &'a Vector<I, T>
where
    I: Idx,
{
    type Item = &'a T;
    type IntoIter = impl Iterator<Item = &'a T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vector.iter().flat_map(|opt| opt.as_ref())
    }
}

impl<'a, I, T> IntoIterator for &'a mut Vector<I, T>
where
    I: Idx,
{
    type Item = &'a mut T;
    type IntoIter = impl Iterator<Item = &'a mut T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vector.iter_mut().flat_map(|opt| opt.as_mut())
    }
}

impl<I, T> IntoIterator for Vector<I, T>
where
    I: Idx,
{
    type Item = T;
    type IntoIter = impl Iterator<Item = T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vector.into_iter().flat_map(|opt| opt)
    }
}

// FIXME: this impl is a footgun
impl<I, T> FromIterator<T> for Vector<I, T>
where
    I: Idx,
{
    #[inline]
    fn from_iter<It: IntoIterator<Item = T>>(iter: It) -> Vector<I, T> {
        let mut elem_count = 0;
        let vector = IndexVec::from_iter(iter.into_iter().inspect(|_| elem_count += 1).map(Some));
        Vector { vector, elem_count }
    }
}

// FIXME: this impl is a footgun
impl<I, T> From<Vec<T>> for Vector<I, T>
where
    I: Idx,
{
    fn from(v: Vec<T>) -> Self {
        v.into_iter().collect()
    }
}

// FIXME: this impl is a footgun
impl<I, T, const N: usize> From<[T; N]> for Vector<I, T>
where
    I: Idx,
{
    fn from(v: [T; N]) -> Self {
        v.into_iter().collect()
    }
}

impl<I: Idx, T: Serialize> Serialize for Vector<I, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.vector.serialize(serializer)
    }
}

impl<'de, I: Idx, T: Deserialize<'de>> Deserialize<'de> for Vector<I, T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let mut ret = Self {
            vector: Deserialize::deserialize(deserializer)?,
            elem_count: 0,
        };
        ret.elem_count = ret.iter().count();
        Ok(ret)
    }
}

impl<'s, I: Idx, T, V: Visit<'s, T>> Drive<'s, V> for Vector<I, T> {
    fn drive_inner(&'s self, v: &mut V) -> ControlFlow<V::Break> {
        for x in self {
            v.visit(x)?;
        }
        Continue(())
    }
}
impl<'s, I: Idx, T, V: VisitMut<'s, T>> DriveMut<'s, V> for Vector<I, T> {
    fn drive_inner_mut(&'s mut self, v: &mut V) -> ControlFlow<V::Break> {
        for x in self {
            v.visit(x)?;
        }
        Continue(())
    }
}
