//! Type-level variables. There are 4 kinds of variables at the type-level: regions, types, const
//! generics and trait clauses. The relevant definitions are in this module.
use std::{
    borrow::Borrow,
    ops::{Index, IndexMut},
};

use derive_generic_visitor::{Drive, DriveMut};
use index_vec::Idx;
use serde::{Deserialize, Serialize};

use crate::{ast::*, impl_from_enum};

/// The index of a binder, counting from the innermost. See [`DeBruijnVar`] for details.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    Hash,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[serde(transparent)]
#[drive(skip)]
pub struct DeBruijnId {
    pub index: usize,
}

impl DeBruijnId {
    pub const ZERO: DeBruijnId = DeBruijnId { index: 0 };
}

/// Type-level variable.
///
/// Variables are bound in groups. Each item has a top-level binding group in its `generic_params`
/// field, and then inner binders are possible using the `RegionBinder<T>` and `Binder<T>` types.
/// Each variable is linked to exactly one binder. The `Id` then identifies the specific variable
/// among all those bound in that group.
///
/// For instance, we have the following:
/// ```text
/// fn f<'a, 'b>(x: for<'c> fn(&'b u8, &'c u16, for<'d> fn(&'b u32, &'c u64, &'d u128)) -> u64) {}
///      ^^^^^^         ^^       ^       ^          ^^       ^        ^        ^
///        |       inner binder  |       |     inner binder  |        |        |
///  top-level binder            |       |                   |        |        |
///                        Bound(1, b)   |              Bound(2, b)   |     Bound(0, d)
///                                      |                            |
///                                  Bound(0, c)                 Bound(1, c)
/// ```
///
/// To make consumption easier for projects that don't do heavy substitution, a micro-pass at the
/// end changes the variables bound at the top-level (i.e. in the `GenericParams` of items) to be
/// `Free`. This is an optional pass, we may add a flag to deactivate it. The example above
/// becomes:
/// ```text
/// fn f<'a, 'b>(x: for<'c> fn(&'b u8, &'c u16, for<'d> fn(&'b u32, &'c u64, &'d u128)) -> u64) {}
///      ^^^^^^         ^^       ^       ^          ^^       ^        ^        ^
///        |       inner binder  |       |     inner binder  |        |        |
///  top-level binder            |       |                   |        |        |
///                           Free(b)    |                Free(b)     |     Bound(0, d)
///                                      |                            |
///                                  Bound(0, c)                 Bound(1, c)
/// ```
///
/// At the moment only region variables can be bound in a non-top-level binder.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    Hash,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
pub enum DeBruijnVar<Id> {
    /// A variable attached to the nth binder, counting from the innermost.
    Bound(DeBruijnId, Id),
    /// A variable attached to the outermost binder (the one on the item). As explained above, This
    /// is not used in charon internals, only as a micro-pass before exporting the crate data.
    Free(Id),
}

// We need to manipulate a lot of indices for the types, variables, definitions, etc. In order not
// to confuse them, we define an index type for every one of them (which is just a struct with a
// unique usize field), together with some utilities like a fresh index generator, using the
// `generate_index_type` macro.
generate_index_type!(RegionId, "Region");
generate_index_type!(TypeVarId, "T");
generate_index_type!(ConstGenericVarId, "Const");
generate_index_type!(TraitClauseId, "TraitClause");
generate_index_type!(TraitTypeConstraintId, "TraitTypeConstraint");

/// A type variable in a signature or binder.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct TypeVar {
    /// Index identifying the variable among other variables bound at the same level.
    pub index: TypeVarId,
    /// Variable name
    #[drive(skip)]
    pub name: String,
}

/// A region variable in a signature or binder.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Drive, DriveMut,
)]
pub struct RegionVar {
    /// Index identifying the variable among other variables bound at the same level.
    pub index: RegionId,
    /// Region name
    #[drive(skip)]
    pub name: Option<String>,
}

/// A const generic variable in a signature or binder.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct ConstGenericVar {
    /// Index identifying the variable among other variables bound at the same level.
    pub index: ConstGenericVarId,
    /// Const generic name
    #[drive(skip)]
    pub name: String,
    /// Type of the const generic
    pub ty: LiteralTy,
}

/// A trait predicate in a signature, of the form `Type: Trait<Args>`. This functions like a
/// variable binder, to which variables of the form `TraitRefKind::Clause` can refer to.
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct TraitClause {
    /// Index identifying the clause among other clauses bound at the same level.
    pub clause_id: TraitClauseId,
    // TODO: does not need to be an option.
    pub span: Option<Span>,
    /// Where the predicate was written, relative to the item that requires it.
    #[charon::opaque]
    #[drive(skip)]
    pub origin: PredicateOrigin,
    /// The trait that is implemented.
    #[charon::rename("trait")]
    pub trait_: PolyTraitDeclRef,
}

impl PartialEq for TraitClause {
    fn eq(&self, other: &Self) -> bool {
        // Skip `span` and `origin`
        self.clause_id == other.clause_id && self.trait_ == other.trait_
    }
}

impl std::hash::Hash for TraitClause {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.clause_id.hash(state);
        self.trait_.hash(state);
    }
}

pub type RegionDbVar = DeBruijnVar<RegionId>;
pub type TypeDbVar = DeBruijnVar<TypeVarId>;
pub type ConstGenericDbVar = DeBruijnVar<ConstGenericVarId>;
pub type ClauseDbVar = DeBruijnVar<TraitClauseId>;

impl_from_enum!(Region::Var(RegionDbVar));
impl_from_enum!(TyKind::TypeVar(TypeDbVar));
impl_from_enum!(ConstGeneric::Var(ConstGenericDbVar));
impl_from_enum!(TraitRefKind::Clause(ClauseDbVar));
impl From<TypeDbVar> for Ty {
    fn from(x: TypeDbVar) -> Self {
        TyKind::TypeVar(x).into_ty()
    }
}

impl DeBruijnId {
    pub fn zero() -> Self {
        DeBruijnId { index: 0 }
    }

    pub fn one() -> Self {
        DeBruijnId { index: 1 }
    }

    pub fn new(index: usize) -> Self {
        DeBruijnId { index }
    }

    pub fn is_zero(&self) -> bool {
        self.index == 0
    }

    pub fn incr(&self) -> Self {
        DeBruijnId {
            index: self.index + 1,
        }
    }

    pub fn decr(&self) -> Self {
        DeBruijnId {
            index: self.index - 1,
        }
    }

    pub fn plus(&self, delta: Self) -> Self {
        DeBruijnId {
            index: self.index + delta.index,
        }
    }

    pub fn sub(&self, delta: Self) -> Option<Self> {
        Some(DeBruijnId {
            index: self.index.checked_sub(delta.index)?,
        })
    }
}

impl<Id> DeBruijnVar<Id>
where
    Id: Copy,
{
    pub fn new_at_zero(id: Id) -> Self {
        DeBruijnVar::Bound(DeBruijnId::new(0), id)
    }

    pub fn free(id: Id) -> Self {
        DeBruijnVar::Free(id)
    }

    pub fn bound(index: DeBruijnId, id: Id) -> Self {
        DeBruijnVar::Bound(index, id)
    }

    pub fn incr(&self) -> Self {
        match *self {
            DeBruijnVar::Bound(dbid, varid) => DeBruijnVar::Bound(dbid.incr(), varid),
            DeBruijnVar::Free(varid) => DeBruijnVar::Free(varid),
        }
    }

    pub fn decr(&self) -> Self {
        match *self {
            DeBruijnVar::Bound(dbid, varid) => DeBruijnVar::Bound(dbid.decr(), varid),
            DeBruijnVar::Free(varid) => DeBruijnVar::Free(varid),
        }
    }

    /// Returns the variable id if it is bound as the given depth.
    pub fn bound_at_depth(&self, depth: DeBruijnId) -> Option<Id> {
        match *self {
            DeBruijnVar::Bound(dbid, varid) if dbid == depth => Some(varid),
            _ => None,
        }
    }
    /// Returns the variable id if it is bound as the given depth.
    pub fn bound_at_depth_mut(&mut self, depth: DeBruijnId) -> Option<&mut Id> {
        match self {
            DeBruijnVar::Bound(dbid, varid) if *dbid == depth => Some(varid),
            _ => None,
        }
    }

    /// Move the variable out of `depth` binders. Returns `None` if the variable is bound in one of
    /// these `depth` binders.
    pub fn move_out_from_depth(&self, depth: DeBruijnId) -> Option<Self> {
        Some(match *self {
            DeBruijnVar::Bound(dbid, varid) => DeBruijnVar::Bound(dbid.sub(depth)?, varid),
            DeBruijnVar::Free(_) => *self,
        })
    }

    /// Move under `depth` binders.
    pub fn move_under_binders(&self, depth: DeBruijnId) -> Self {
        match *self {
            DeBruijnVar::Bound(dbid, varid) => DeBruijnVar::Bound(dbid.plus(depth), varid),
            DeBruijnVar::Free(_) => *self,
        }
    }
}

impl TypeVar {
    pub fn new(index: TypeVarId, name: String) -> TypeVar {
        TypeVar { index, name }
    }
}

impl Default for DeBruijnId {
    fn default() -> Self {
        Self::zero()
    }
}

/// A stack of values corresponding to nested binders. Each binder introduces an entry in this
/// stack, with the entry as index `0` being the innermost binder. This is indexed by
/// `DeBruijnId`s.
/// Most methods assume that the stack is non-empty and panic if not.
#[derive(Clone, Hash)]
pub struct BindingStack<T> {
    /// The stack, stored in reverse. We push/pop to the end of the `Vec`, and the last pushed
    /// value (i.e. the end of the vec) is considered index 0.
    stack: Vec<T>,
}

impl<T> BindingStack<T> {
    pub fn new(x: T) -> Self {
        Self { stack: vec![x] }
    }
    /// Creates an empty stack. Beware, a number of method calls will panic on an empty stack.
    pub fn empty() -> Self {
        Self { stack: vec![] }
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
    pub fn len(&self) -> usize {
        self.stack.len()
    }
    pub fn depth(&self) -> DeBruijnId {
        DeBruijnId::new(self.stack.len() - 1)
    }
    /// Map a bound variable to ids binding depth.
    pub fn as_bound_var<Id>(&self, var: DeBruijnVar<Id>) -> (DeBruijnId, Id) {
        match var {
            DeBruijnVar::Bound(dbid, varid) => (dbid, varid),
            DeBruijnVar::Free(varid) => (self.depth(), varid),
        }
    }
    pub fn push(&mut self, x: T) {
        self.stack.push(x);
    }
    pub fn pop(&mut self) -> Option<T> {
        self.stack.pop()
    }
    /// Helper that computes the real index into `self.stack`.
    fn real_index(&self, id: DeBruijnId) -> Option<usize> {
        self.stack.len().checked_sub(id.index + 1)
    }
    pub fn get(&self, id: DeBruijnId) -> Option<&T> {
        self.stack.get(self.real_index(id)?)
    }
    pub fn get_var<'a, Id: Idx, Inner>(&'a self, var: DeBruijnVar<Id>) -> Option<&'a Inner::Output>
    where
        T: Borrow<Inner>,
        Inner: HasVectorOf<Id> + 'a,
    {
        let (dbid, varid) = self.as_bound_var(var);
        self.get(dbid)
            .and_then(|x| x.borrow().get_vector().get(varid))
    }
    pub fn get_mut(&mut self, id: DeBruijnId) -> Option<&mut T> {
        let index = self.real_index(id)?;
        self.stack.get_mut(index)
    }
    /// Iterate over the binding levels, from the innermost (0) out.
    pub fn iter(&self) -> impl Iterator<Item = &T> + DoubleEndedIterator + ExactSizeIterator {
        self.stack.iter().rev()
    }
    /// Iterate over the binding levels, from the innermost (0) out.
    pub fn iter_enumerated(
        &self,
    ) -> impl Iterator<Item = (DeBruijnId, &T)> + DoubleEndedIterator + ExactSizeIterator {
        self.iter()
            .enumerate()
            .map(|(i, x)| (DeBruijnId::new(i), x))
    }
    pub fn map_ref<'a, U>(&'a self, f: impl FnMut(&'a T) -> U) -> BindingStack<U> {
        BindingStack {
            stack: self.stack.iter().map(f).collect(),
        }
    }

    pub fn innermost(&self) -> &T {
        self.stack.last().unwrap()
    }
    pub fn innermost_mut(&mut self) -> &mut T {
        self.stack.last_mut().unwrap()
    }
    pub fn outermost(&self) -> &T {
        self.stack.first().unwrap()
    }
    pub fn outermost_mut(&mut self) -> &mut T {
        self.stack.first_mut().unwrap()
    }
}

impl<T> Default for BindingStack<T> {
    fn default() -> Self {
        Self {
            stack: Default::default(),
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for BindingStack<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.stack)
    }
}

impl<T> Index<DeBruijnId> for BindingStack<T> {
    type Output = T;
    fn index(&self, id: DeBruijnId) -> &Self::Output {
        self.get(id).unwrap()
    }
}
impl<T> IndexMut<DeBruijnId> for BindingStack<T> {
    fn index_mut(&mut self, id: DeBruijnId) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}
