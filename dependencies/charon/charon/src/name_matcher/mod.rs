use std::cmp::Ordering;

use itertools::{EitherOrBoth, Itertools};
use serde::{Deserialize, Serialize};

use crate::ast::*;

mod parser;

pub use Pattern as NamePattern;

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pattern {
    elems: Vec<PatElem>,
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
enum PatElem {
    /// An identifier, optionally with generic arguments. E.g. `std` or `Box<_>`.
    Ident {
        name: String,
        generics: Vec<PatTy>,
        /// For pretty-printing only: whether this is the name of a trait.
        is_trait: bool,
    },
    /// An inherent or trait implementation block. For traits, the implemented type is the first
    /// element of the pattern generics.
    Impl(Box<Pattern>),
    /// A `*` or `_`.
    Glob,
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
enum PatTy {
    /// A path, like `my_crate::foo::Type<_, usize>`
    Pat(Pattern),
    /// `&T`, `&mut T`
    Ref(RefKind, Box<Self>),
}

impl Pattern {
    pub fn parse(i: &str) -> Result<Self, nom_supreme::error::ErrorTree<String>> {
        use std::str::FromStr;
        Self::from_str(i)
    }

    fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn matches(&self, ctx: &TranslatedCrate, name: &Name) -> bool {
        self.matches_with_generics(ctx, name, None)
    }

    pub fn matches_item(&self, ctx: &TranslatedCrate, item: AnyTransItem<'_>) -> bool {
        let generics = item.identity_args();
        let name = &item.item_meta().name;
        self.matches_with_generics(ctx, name, Some(&generics))
    }

    pub fn matches_with_generics(
        &self,
        ctx: &TranslatedCrate,
        name: &Name,
        args: Option<&GenericArgs>,
    ) -> bool {
        let mut scrutinee_elems = name.name.as_slice();
        // Patterns that start with an impl block match that impl block anywhere. In such a case we
        // truncate the scrutinee name to start with the rightmost impl in its name. This isn't
        // fully precise in case of impls within impls, but we'll ignore that.
        if let Some(PatElem::Impl(_)) = self.elems.first() {
            if let Some((i, _)) = scrutinee_elems
                .iter()
                .enumerate()
                .rfind(|(_, elem)| elem.is_impl())
            {
                scrutinee_elems = &scrutinee_elems[i..];
            }
        }

        let zipped = self.elems.iter().zip_longest(scrutinee_elems).collect_vec();
        let zipped_len = zipped.len();
        for (i, x) in zipped.into_iter().enumerate() {
            let is_last = i + 1 == zipped_len;
            match x {
                EitherOrBoth::Both(pat, elem) => {
                    let args = if is_last { args } else { None };
                    if !pat.matches_with_generics(ctx, elem, args) {
                        return false;
                    }
                }
                // The pattern is shorter than the scrutinee and the previous elements match: we
                // count that as matching.
                EitherOrBoth::Right(_) => return true,
                // The pattern is longer than the scrutinee; they don't match.
                EitherOrBoth::Left(_) => return false,
            }
        }
        // Both had the same length and all the elements matched.
        true
    }

    pub fn matches_ty(&self, ctx: &TranslatedCrate, ty: &Ty) -> bool {
        if let [PatElem::Glob] = self.elems.as_slice() {
            return true;
        }
        match ty.kind() {
            TyKind::Adt(tref) => {
                let args = &tref.generics;
                match tref.id {
                    TypeId::Adt(type_id) => {
                        let Some(type_name) = ctx.item_name(type_id) else {
                            return false;
                        };
                        self.matches_with_generics(ctx, type_name, Some(args))
                    }
                    TypeId::Builtin(builtin_ty) => {
                        let name = builtin_ty.get_name();
                        self.matches_with_generics(ctx, &name, Some(args))
                    }
                    TypeId::Tuple => false,
                }
            }
            TyKind::TypeVar(..)
            | TyKind::Literal(..)
            | TyKind::Never
            | TyKind::Ref(..)
            | TyKind::RawPtr(..)
            | TyKind::TraitType(..)
            | TyKind::DynTrait(..)
            | TyKind::FnPtr(..)
            | TyKind::FnDef(..)
            | TyKind::Error(..) => false,
        }
    }

    pub fn matches_const(&self, _ctx: &TranslatedCrate, _c: &ConstGeneric) -> bool {
        if let [PatElem::Glob] = self.elems.as_slice() {
            return true;
        }
        todo!("non-trivial const generics patterns aren't implemented")
    }

    /// Compares two patterns that match the same name, in terms of precision. A pattern that is
    /// fully included in another (i.e. matches a subset of values) is considered "less precise".
    /// Returns nonsense if the patterns don't match the same name.
    pub fn compare(&self, other: &Self) -> Ordering {
        use Ordering::*;
        use PatElem::*;
        match self.len().cmp(&other.len()) {
            o @ (Less | Greater) => return o,
            _ if self.len() == 0 => return Equal,
            Equal => {}
        }
        match (self.elems.last().unwrap(), other.elems.last().unwrap()) {
            (Glob, Glob) => Equal,
            (Glob, _) => Less,
            (_, Glob) => Greater,
            // TODO: compare precision of the generics.
            _ => Equal,
        }
    }
}

/// Orders patterns by precision: the maximal pattern is the most precise. COmparing patterns only
/// makes sense if they match the same name.
impl Ord for Pattern {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare(other)
    }
}
impl PartialOrd for Pattern {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.compare(other))
    }
}

impl PatElem {
    fn matches_with_generics(
        &self,
        ctx: &TranslatedCrate,
        elem: &PathElem,
        args: Option<&GenericArgs>,
    ) -> bool {
        match (self, elem) {
            (PatElem::Glob, _) => true,
            (
                PatElem::Ident {
                    name: pat_ident,
                    generics,
                    ..
                },
                PathElem::Ident(ident, _),
            ) => {
                // `crate` is a special keyword that referes to the current crate.
                let same_ident =
                    pat_ident == ident || (pat_ident == "crate" && ident == &ctx.crate_name);
                same_ident && PatTy::matches_generics(ctx, generics, args)
            }
            (PatElem::Impl(_pat), PathElem::Impl(ImplElem::Ty(..))) => {
                // TODO
                false
            }
            (PatElem::Impl(pat), PathElem::Impl(ImplElem::Trait(impl_id))) => {
                let Some(timpl) = ctx.trait_impls.get(*impl_id) else {
                    return false;
                };
                let Some(trait_name) = ctx.item_name(timpl.impl_trait.id) else {
                    return false;
                };
                pat.matches_with_generics(ctx, trait_name, Some(&timpl.impl_trait.generics))
            }
            _ => false,
        }
    }
}

impl PatTy {
    pub fn matches_generics(
        ctx: &TranslatedCrate,
        pats: &[Self],
        generics: Option<&GenericArgs>,
    ) -> bool {
        let Some(generics) = generics else {
            // If we'r ematching on a plain name without generics info, we ignore pattern generics.
            return true;
        };
        if pats.is_empty() {
            // If no generics are provided, this counts as a match.
            return true;
        }
        // We don't include regions in patterns.
        if pats.len() != generics.types.elem_count() + generics.const_generics.elem_count() {
            return false;
        }
        let (type_pats, const_pats) = pats.split_at(generics.types.elem_count());
        let types_match = generics
            .types
            .iter()
            .zip(type_pats)
            .all(|(ty, pat)| pat.matches_ty(ctx, ty));
        let consts_match = generics
            .const_generics
            .iter()
            .zip(const_pats)
            .all(|(c, pat)| pat.matches_const(ctx, c));
        types_match && consts_match
    }

    pub fn matches_ty(&self, ctx: &TranslatedCrate, ty: &Ty) -> bool {
        match (self, ty.kind()) {
            (PatTy::Pat(p), _) => p.matches_ty(ctx, ty),
            (PatTy::Ref(pat_mtbl, p_ty), TyKind::Ref(_, ty, ty_mtbl)) => {
                pat_mtbl == ty_mtbl && p_ty.matches_ty(ctx, ty)
            }
            _ => false,
        }
    }

    pub fn matches_const(&self, ctx: &TranslatedCrate, c: &ConstGeneric) -> bool {
        match self {
            PatTy::Pat(p) => p.matches_const(ctx, c),
            PatTy::Ref(..) => false,
        }
    }
}

#[test]
fn test_compare() {
    use Ordering::*;
    let tests = [
        ("_", Less, "crate"),
        ("crate::_", Less, "crate::foo"),
        ("crate::foo", Less, "crate::foo::_"),
    ];
    for (x, o, y) in tests {
        let x = Pattern::parse(x).unwrap();
        let y = Pattern::parse(y).unwrap();
        assert_eq!(x.compare(&y), o);
    }
}
