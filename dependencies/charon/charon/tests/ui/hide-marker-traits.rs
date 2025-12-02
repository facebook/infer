//@ charon-args=--hide-marker-traits
//! Reproduces a crash when substituting variables with the `--hide-marker-traits` option.
trait Idx {}

pub struct IndexVec<I>
where
    I: Idx,
{
    i: I,
}

pub struct Vector<I>
where
    I: Idx,
{
    vector: IndexVec<I>,
}

// Makes a `TraitRef::Builtin` for `Drop`. Regression test: we used to not remove marker traits
// from that.
fn foo<T>(_x: T) {}
