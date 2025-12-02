//! Defines some utilities for the variables
use crate::ast::*;
use derive_generic_visitor::{Drive, DriveMut};
use macros::{EnumAsGetters, EnumIsA};
use serde::{Deserialize, Serialize};

generate_index_type!(Disambiguator);

/// See the comments for [Name]
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut, EnumIsA, EnumAsGetters,
)]
#[charon::variants_prefix("Pe")]
pub enum PathElem {
    Ident(#[drive(skip)] String, Disambiguator),
    Impl(ImplElem),
    /// This item was obtained by monomorphizing its parent with the given args.
    Monomorphized(BoxedArgs),
}

/// There are two kinds of `impl` blocks:
/// - impl blocks linked to a type ("inherent" impl blocks following Rust terminology):
///   ```text
///   impl<T> List<T> { ...}
///   ```
/// - trait impl blocks:
///   ```text
///   impl<T> PartialEq for List<T> { ...}
///   ```
/// We distinguish the two.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
#[charon::variants_prefix("ImplElem")]
pub enum ImplElem {
    Ty(Binder<Ty>),
    Trait(TraitImplId),
}

/// An item name/path
///
/// A name really is a list of strings. However, we sometimes need to
/// introduce unique indices to disambiguate. This mostly happens because
/// of "impl" blocks:
///   ```text
///   impl<T> List<T> {
///     ...
///   }
///   ```
///
/// A type in Rust can have several "impl" blocks, and  those blocks can
/// contain items with similar names. For this reason, we need to disambiguate
/// them with unique indices. Rustc calls those "disambiguators". In rustc, this
/// gives names like this:
/// - `betree_main::betree::NodeIdCounter{impl#0}::new`
/// - note that impl blocks can be nested, and macros sometimes generate
///   weird names (which require disambiguation):
///   `betree_main::betree_utils::_#1::{impl#0}::deserialize::{impl#0}`
///
/// Finally, the paths used by rustc are a lot more precise and explicit than
/// those we expose in LLBC: for instance, every identifier belongs to a specific
/// namespace (value namespace, type namespace, etc.), and is coupled with a
/// disambiguator.
///
/// On our side, we want to stay high-level and simple: we use string identifiers
/// as much as possible, insert disambiguators only when necessary (whenever
/// we find an "impl" block, typically) and check that the disambiguator is useless
/// in the other situations (i.e., the disambiguator is always equal to 0).
///
/// Moreover, the items are uniquely disambiguated by their (integer) ids
/// (`TypeDeclId`, etc.), and when extracting the code we have to deal with
/// name clashes anyway. Still, we might want to be more precise in the future.
///
/// Also note that the first path element in the name is always the crate name.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
#[serde(transparent)]
pub struct Name {
    pub name: Vec<PathElem>,
}
