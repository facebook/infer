use crate::{id_table::hash_consing::HashConsed, prelude::*};

pub mod resolution;
mod utils;
pub use utils::{
    ItemPredicate, ItemPredicateId, ItemPredicates, ToPolyTraitRef, erase_and_norm,
    erase_free_regions, implied_predicates, normalize, required_predicates, self_predicate,
};

pub use resolution::PredicateSearcher;
use rustc_middle::ty;
use rustc_span::def_id::DefId as RDefId;

pub use utils::is_sized_related_trait;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImplExprPathChunk {
    AssocItem {
        /// Reference to the item, with generics (for GATs), e.g. the `T` and `T: Clone` `ImplExpr`
        /// in the following example:
        /// ```ignore
        /// trait Foo {
        ///     type Type<T: Clone>: Debug;
        /// }
        /// ```
        item: ItemRef,
        assoc_item: AssocItem,
        /// The implemented predicate.
        predicate: Binder<TraitPredicate>,
        /// The index of this predicate in the list returned by `implied_predicates`.
        index: usize,
    },
    Parent {
        /// The implemented predicate.
        predicate: Binder<TraitPredicate>,
        /// The index of this predicate in the list returned by `implied_predicates`.
        index: usize,
    },
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, ImplExprPathChunk> for resolution::PathChunk<'tcx> {
    fn sinto(&self, s: &S) -> ImplExprPathChunk {
        match self {
            resolution::PathChunk::AssocItem {
                item,
                generic_args,
                predicate,
                index,
                ..
            } => ImplExprPathChunk::AssocItem {
                item: translate_item_ref(s, item.def_id, generic_args),
                assoc_item: AssocItem::sfrom(s, item),
                predicate: predicate.sinto(s),
                index: index.sinto(s),
            },
            resolution::PathChunk::Parent {
                predicate, index, ..
            } => ImplExprPathChunk::Parent {
                predicate: predicate.sinto(s),
                index: index.sinto(s),
            },
        }
    }
}

/// The source of a particular trait implementation. Most often this is either `Concrete` for a
/// concrete `impl Trait for Type {}` item, or `LocalBound` for a context-bound `where T: Trait`.
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx> >, from: resolution::ImplExprAtom<'tcx>, state: S as s)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImplExprAtom {
    /// A concrete `impl Trait for Type {}` item.
    #[custom_arm(FROM_TYPE::Concrete { def_id, generics } => TO_TYPE::Concrete(
        translate_item_ref(s, *def_id, generics),
    ),)]
    Concrete(ItemRef),
    /// A context-bound clause like `where T: Trait`.
    LocalBound {
        id: GenericPredicateId,
        r#trait: Binder<TraitRef>,
        path: Vec<ImplExprPathChunk>,
    },
    /// The implicit `Self: Trait` clause present inside a `trait Trait {}` item.
    // TODO: should we also get that clause for trait impls?
    SelfImpl {
        r#trait: Binder<TraitRef>,
        path: Vec<ImplExprPathChunk>,
    },
    /// `dyn Trait` is a wrapped value with a virtual table for trait
    /// `Trait`.  In other words, a value `dyn Trait` is a dependent
    /// triple that gathers a type τ, a value of type τ and an
    /// instance of type `Trait`.
    /// `dyn Trait` implements `Trait` using a built-in implementation; this refers to that
    /// built-in implementation.
    Dyn,
    /// A built-in trait whose implementation is computed by the compiler, such as `FnMut`. This
    /// morally points to an invisible `impl` block; as such it contains the information we may
    /// need from one.
    Builtin {
        /// Extra data for the given trait.
        trait_data: BuiltinTraitData,
        /// The `ImplExpr`s required to satisfy the implied predicates on the trait declaration.
        /// E.g. since `FnMut: FnOnce`, a built-in `T: FnMut` impl would have an `ImplExpr` for `T:
        /// FnOnce`.
        impl_exprs: Vec<ImplExpr>,
        /// The values of the associated types for this trait.
        types: Vec<(DefId, Ty, Vec<ImplExpr>)>,
    },
    /// An error happened while resolving traits.
    Error(String),
}

#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx> >, from: resolution::BuiltinTraitData<'tcx>, state: S as s)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BuiltinTraitData {
    /// A virtual `Destruct` implementation.
    /// `Destruct` is implemented automatically for all types. For our purposes, we chose to attach
    /// the information about `drop_in_place` to that trait. This data tells us what kind of
    /// `drop_in_place` the target type has.
    Destruct(DestructData),
    /// Some other builtin trait.
    Other,
}

#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx> >, from: resolution::DestructData<'tcx>, state: S as s)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum DestructData {
    /// A drop that does nothing, e.g. for scalars and pointers.
    Noop,
    /// An implicit `Destruct` local clause, if the `resolve_destruct_bounds` option is `false`. If
    /// that option is `true`, we'll add `Destruct` bounds to every type param, and use that to
    /// resolve `Destruct` impls of generics. If it's `false`, we use this variant to indicate that
    /// the clause comes from a generic or associated type.
    Implicit,
    /// The `drop_in_place` is known and non-trivial.
    Glue {
        /// The type we're generating glue for.
        ty: Ty,
    },
}

/// An `ImplExpr` describes the full data of a trait implementation. Because of generics, this may
/// need to combine several concrete trait implementation items. For example, `((1u8, 2u8),
/// "hello").clone()` combines the generic implementation of `Clone` for `(A, B)` with the
/// concrete implementations for `u8` and `&str`, represented as a tree.
pub type ImplExpr = HashConsed<ImplExprContents>;

#[derive(Clone, Debug, Hash, PartialEq, Eq, AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx> >, from: resolution::ImplExpr<'tcx>, state: S as s)]
pub struct ImplExprContents {
    /// The trait this is an impl for.
    pub r#trait: Binder<TraitRef>,
    /// The kind of implemention of the root of the tree.
    pub r#impl: ImplExprAtom,
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, ImplExpr> for resolution::ImplExpr<'tcx> {
    fn sinto(&self, s: &S) -> ImplExpr {
        HashConsed::new(self.sinto(s))
    }
}

/// Given a clause `clause` in the context of some impl block `impl_did`, susbts correctly `Self`
/// from `clause` and (1) derive a `Clause` and (2) resolve an `ImplExpr`.
pub fn super_clause_to_clause_and_impl_expr<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    impl_did: rustc_span::def_id::DefId,
    clause: rustc_middle::ty::Clause<'tcx>,
    span: rustc_span::Span,
) -> Option<(Clause, ImplExpr, Span)> {
    let tcx = s.base().tcx;
    if !matches!(
        tcx.def_kind(impl_did),
        rustc_hir::def::DefKind::Impl { of_trait: true }
    ) {
        return None;
    }
    let impl_trait_ref =
        rustc_middle::ty::Binder::dummy(tcx.impl_trait_ref(impl_did).instantiate_identity());
    let new_clause = clause.instantiate_supertrait(tcx, impl_trait_ref);
    let impl_expr = solve_trait(
        s,
        new_clause
            .as_predicate()
            .as_trait_clause()?
            .to_poly_trait_ref(),
    );
    let new_clause = new_clause.sinto(s);
    Some((new_clause, impl_expr, span.sinto(s)))
}

/// This is the entrypoint of the solving.
#[tracing::instrument(level = "trace", skip(s))]
pub fn solve_trait<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    trait_ref: rustc_middle::ty::PolyTraitRef<'tcx>,
) -> ImplExpr {
    let warn = |_msg: &str| {};
    if let Some(impl_expr) = s.with_cache(|cache| cache.impl_exprs.get(&trait_ref).cloned()) {
        return impl_expr;
    }
    let resolved =
        s.with_predicate_searcher(|pred_searcher| pred_searcher.resolve(&trait_ref, &warn));
    let impl_expr: ImplExpr = match resolved {
        Ok(x) => x.sinto(s),
        Err(e) => crate::fatal!(s, "{}", e),
    };
    s.with_cache(|cache| cache.impl_exprs.insert(trait_ref, impl_expr.clone()));
    impl_expr
}

/// Translate a reference to an item, resolving the appropriate trait clauses as needed.
#[tracing::instrument(level = "trace", skip(s), ret)]
pub fn translate_item_ref<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    def_id: RDefId,
    generics: ty::GenericArgsRef<'tcx>,
) -> ItemRef {
    ItemRef::translate(s, def_id, generics)
}

pub fn inherits_parent_clauses<'tcx>(tcx: ty::TyCtxt<'tcx>, def_id: RDefId) -> bool {
    use rustc_hir::def::DefKind::*;
    match tcx.def_kind(def_id) {
        AssocTy | AssocFn | AssocConst | Closure | Ctor(..) | Variant | AnonConst | InlineConst => {
            true
        }
        _ => false,
    }
}

/// Solve the trait obligations for a specific item use (for example, a method call, an ADT, etc.)
/// in the current context. Just like generic args include generics of parent items, this includes
/// impl exprs for parent items.
#[tracing::instrument(level = "trace", skip(s), ret)]
pub fn solve_item_required_traits<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    def_id: RDefId,
    generics: ty::GenericArgsRef<'tcx>,
) -> Vec<ImplExpr> {
    fn accumulate<'tcx, S: UnderOwnerState<'tcx>>(
        s: &S,
        def_id: RDefId,
        generics: ty::GenericArgsRef<'tcx>,
        impl_exprs: &mut Vec<ImplExpr>,
    ) {
        let tcx = s.base().tcx;
        if inherits_parent_clauses(tcx, def_id) {
            let parent = tcx.parent(def_id);
            accumulate(s, parent, generics, impl_exprs);
        }
        let predicates = required_predicates(tcx, def_id, s.base().options.bounds_options);
        impl_exprs.extend(solve_item_traits_inner(s, generics, predicates));
    }
    let mut impl_exprs = vec![];
    accumulate(s, def_id, generics, &mut impl_exprs);
    impl_exprs
}

/// Solve the trait obligations for implementing a trait (or for trait associated type bounds) in
/// the current context.
#[tracing::instrument(level = "trace", skip(s), ret)]
pub fn solve_item_implied_traits<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    def_id: RDefId,
    generics: ty::GenericArgsRef<'tcx>,
) -> Vec<ImplExpr> {
    let predicates = implied_predicates(s.base().tcx, def_id, s.base().options.bounds_options);
    solve_item_traits_inner(s, generics, predicates)
}

/// Apply the given generics to the provided clauses and resolve the trait references in the
/// current context.
fn solve_item_traits_inner<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    generics: ty::GenericArgsRef<'tcx>,
    predicates: utils::ItemPredicates<'tcx>,
) -> Vec<ImplExpr> {
    let tcx = s.base().tcx;
    let typing_env = s.typing_env();
    predicates
        .iter()
        .map(|pred| pred.clause)
        .filter_map(|clause| clause.as_trait_clause())
        .map(|clause| clause.to_poly_trait_ref())
        // Substitute the item generics
        .map(|trait_ref| ty::EarlyBinder::bind(trait_ref).instantiate(tcx, generics))
        // We unfortunately don't have a way to normalize without erasing regions.
        .map(|trait_ref| {
            tcx.try_normalize_erasing_regions(typing_env, trait_ref)
                .unwrap_or(trait_ref)
        })
        // Resolve
        .map(|trait_ref| solve_trait(s, trait_ref))
        .collect()
}

/// Retrieve the `Self: Trait` clause for a trait associated item.
pub fn self_clause_for_item<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    def_id: RDefId,
    generics: rustc_middle::ty::GenericArgsRef<'tcx>,
) -> Option<ImplExpr> {
    let tcx = s.base().tcx;

    let tr_def_id = tcx.trait_of_assoc(def_id)?;
    // The "self" predicate in the context of the trait.
    let self_pred = self_predicate(tcx, tr_def_id);
    // Substitute to be in the context of the current item.
    let generics = generics.truncate_to(tcx, tcx.generics_of(tr_def_id));
    let self_pred = ty::EarlyBinder::bind(self_pred).instantiate(tcx, generics);

    // Resolve
    Some(solve_trait(s, self_pred))
}

/// Solve the `T: Sized` predicate.
pub fn solve_sized<'tcx, S: UnderOwnerState<'tcx>>(s: &S, ty: ty::Ty<'tcx>) -> ImplExpr {
    let tcx = s.base().tcx;
    let sized_trait = tcx.lang_items().sized_trait().unwrap();
    let ty = erase_free_regions(tcx, ty);
    let tref = ty::Binder::dummy(ty::TraitRef::new(tcx, sized_trait, [ty]));
    solve_trait(s, tref)
}

/// Solve the `T: Destruct` predicate.
pub fn solve_destruct<'tcx, S: UnderOwnerState<'tcx>>(s: &S, ty: ty::Ty<'tcx>) -> ImplExpr {
    let tcx = s.base().tcx;
    let destruct_trait = tcx.lang_items().destruct_trait().unwrap();
    let tref = ty::Binder::dummy(ty::TraitRef::new(tcx, destruct_trait, [ty]));
    solve_trait(s, tref)
}
