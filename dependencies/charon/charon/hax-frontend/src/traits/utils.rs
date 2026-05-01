//! Each item can involve three kinds of predicates:
//! - input aka required predicates: the predicates required to mention the item. These are usually `where`
//!   clauses (or equivalent) on the item:
//! ```ignore
//! struct Foo<T: Clone> { ... }
//! trait Foo<T> where T: Clone { ... }
//! fn function<I>() where I: Iterator, I::Item: Clone { ... }
//! ```
//! - output aka implied predicates: the predicates that are implied by the presence of this item in a
//!   signature. This is mostly trait parent predicates:
//! ```ignore
//! trait Foo: Clone { ... }
//! fn bar<T: Foo>() {
//!   // from `T: Foo` we can deduce `T: Clone`
//! }
//! ```
//!   This could also include implied predicates such as `&'a T` implying `T: 'a` but we don't
//!   consider these.
//! - "self" predicate: that's the special `Self: Trait` predicate in scope within a trait
//!   declaration or implementation for trait `Trait`.
//!
//! Note that within a given item the polarity is reversed: input predicates are the ones that can
//! be assumed to hold and output predicates must be proven to hold. The "self" predicate is both
//! assumed and proven within an impl block, and just assumed within a trait declaration block.
//!
//! The current implementation considers all predicates on traits to be outputs, which has the
//! benefit of reducing the size of signatures. Moreover, the rules on which bounds are required vs
//! implied are subtle. We may change this if this proves to be a problem.
use crate::options::BoundsOptions;
use itertools::Itertools;
use rustc_hir::LangItem;
use rustc_hir::def::DefKind;
use rustc_middle::ty::*;
use rustc_span::def_id::DefId;
use rustc_span::{DUMMY_SP, Span};

/// Uniquely identifies a predicate.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum ItemPredicateId {
    /// A predicate that counts as "input" for an item, e.g. `where` clauses on a function or impl.
    /// Numbered in some arbitrary but consistent order.
    Required(DefId, u32),
    /// A predicate that counts as "output" of an item, e.g. supertrait clauses in a trait. Note
    /// that we count `where` clauses on a trait as implied.
    /// Numbered in some arbitrary but consistent order.
    Implied(DefId, u32),
    /// Predicate inside a non-item binder, e.g. within a `dyn Trait`.
    /// Numbered in some arbitrary but consistent order.
    Unmapped(u32),
    /// The special `Self: Trait` clause available within trait `Trait`.
    TraitSelf,
}

impl ItemPredicateId {
    fn new(def_id: DefId, required: bool, next_id: &mut usize) -> Self {
        let i = *next_id as u32;
        *next_id += 1;
        if required {
            ItemPredicateId::Required(def_id, i)
        } else {
            ItemPredicateId::Implied(def_id, i)
        }
    }
    fn new_unmapped(next_id: &mut usize) -> Self {
        let i = *next_id as u32;
        *next_id += 1;
        ItemPredicateId::Unmapped(i)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ItemPredicate<'tcx> {
    pub id: ItemPredicateId,
    pub clause: Clause<'tcx>,
    pub span: Span,
}

#[derive(Debug, Default, Clone)]
pub struct ItemPredicates<'tcx> {
    required: bool,
    next_id: usize,
    pub predicates: Vec<ItemPredicate<'tcx>>,
}

impl<'tcx> ItemPredicates<'tcx> {
    pub fn new(
        def_id: DefId,
        required: bool,
        predicates: impl IntoIterator<Item = (Clause<'tcx>, Span)>,
    ) -> Self {
        let mut next_id = 0;
        let predicates = predicates
            .into_iter()
            .map(|(clause, span)| {
                let id = ItemPredicateId::new(def_id, required, &mut next_id);
                ItemPredicate { id, clause, span }
            })
            .collect_vec();
        Self {
            next_id,
            required,
            predicates,
        }
    }
    pub fn new_unmapped(span: Span, predicates: impl IntoIterator<Item = Clause<'tcx>>) -> Self {
        let mut next_id = 0;
        let predicates = predicates
            .into_iter()
            .map(|clause| {
                let id = ItemPredicateId::new_unmapped(&mut next_id);
                ItemPredicate { id, clause, span }
            })
            .collect_vec();
        Self {
            next_id,
            required: true,
            predicates,
        }
    }

    pub fn add_synthetic_predicates(
        &mut self,
        def_id: DefId,
        predicates: impl IntoIterator<Item = Clause<'tcx>>,
    ) {
        self.predicates.extend(predicates.into_iter().map(|clause| {
            let id = ItemPredicateId::new(def_id, self.required, &mut self.next_id);
            ItemPredicate {
                id,
                clause,
                span: DUMMY_SP,
            }
        }));
    }

    pub fn iter(&self) -> impl Iterator<Item = ItemPredicate<'tcx>> {
        self.predicates.iter().copied()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut ItemPredicate<'tcx>> {
        self.predicates.iter_mut()
    }
}

/// Returns a list of type predicates for the definition with ID `def_id`, including inferred
/// lifetime constraints. This is the basic list of predicates we use for essentially all items.
fn predicates_defined_on(tcx: TyCtxt<'_>, def_id: DefId, required: bool) -> ItemPredicates<'_> {
    let predicates = tcx
        .explicit_predicates_of(def_id)
        .predicates
        .iter()
        .copied()
        .chain(
            tcx.inferred_outlives_of(def_id)
                .iter()
                .copied()
                .map(|(clause, span)| (clause.upcast(tcx), span)),
        );
    ItemPredicates::new(def_id, required, predicates)
}

/// Add `T: Destruct` bounds for every generic parameter of the given item.
fn add_destruct_bounds<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    predicates: &mut ItemPredicates<'tcx>,
) {
    let def_kind = tcx.def_kind(def_id);
    if matches!(def_kind, DefKind::Closure) {
        // Closures have fictitious weird type parameters in their `own_args` that we don't want to
        // add `Destruct` bounds for.
        return;
    }
    // Add a `T: Destruct` bound for every generic.
    let destruct_trait = tcx.lang_items().destruct_trait().unwrap();
    let extra_bounds = tcx
        .generics_of(def_id)
        .own_params
        .iter()
        .filter(|param| matches!(param.kind, GenericParamDefKind::Type { .. }))
        .map(|param| tcx.mk_param_from_def(param))
        .map(|ty| Binder::dummy(TraitRef::new(tcx, destruct_trait, [ty])))
        .map(|tref| tref.upcast(tcx));
    predicates.add_synthetic_predicates(def_id, extra_bounds);
}

/// The predicates that must hold to mention this item. E.g.
///
/// ```ignore
/// // `U: OtherTrait` is required, `Self: Sized` is implied.
/// trait Trait<U: OtherTrait>: Sized {
///     // `T: Clone` is required, `Self::Type<T>: Debug` is implied.
///     type Type<T: Clone>: Debug;
/// }
/// ```
///
/// If `add_drop` is true, we add a `T: Drop` bound for every type generic.
pub fn required_predicates<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    options: BoundsOptions,
) -> ItemPredicates<'tcx> {
    use DefKind::*;
    let def_kind = tcx.def_kind(def_id);
    let mut predicates = match def_kind {
        AssocConst
        | AssocFn
        | AssocTy
        | Const
        | Enum
        | Fn
        | ForeignTy
        | Impl { .. }
        | OpaqueTy
        | Static { .. }
        | Struct
        | TyAlias
        | Union => predicates_defined_on(tcx, def_id, true),
        // We consider all predicates on traits to be outputs
        Trait | TraitAlias => Default::default(),
        // `predicates_defined_on` ICEs on other def kinds.
        _ => Default::default(),
    };
    // For methods and assoc consts in trait definitions, we add an explicit `Self: Trait` clause.
    // Associated types get to use the implicit `Self: Trait` clause instead.
    if !matches!(def_kind, AssocTy)
        && let Some(trait_def_id) = tcx.trait_of_assoc(def_id)
    {
        let self_clause = self_predicate(tcx, trait_def_id).upcast(tcx);
        predicates.predicates.insert(
            0,
            ItemPredicate {
                id: ItemPredicateId::TraitSelf,
                clause: self_clause,
                span: DUMMY_SP,
            },
        );
    }
    if options.resolve_destruct && !matches!(def_kind, Trait | TraitAlias) {
        // Add a `T: Destruct` bound for every generic. For traits we consider these predicates
        // implied instead of required.
        add_destruct_bounds(tcx, def_id, &mut predicates);
    }
    if options.prune_sized {
        prune_sized_predicates(tcx, &mut predicates);
    }
    predicates
}

/// The special "self" predicate on a trait.
pub fn self_predicate<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> PolyTraitRef<'tcx> {
    // Copied from the code of `tcx.predicates_of()`.
    Binder::dummy(TraitRef::identity(tcx, def_id))
}

/// The predicates that can be deduced from the presence of this item in a signature. We only
/// consider predicates implied by traits here, not implied bounds such as `&'a T` implying `T:
/// 'a`. E.g.
///
/// ```ignore
/// // `U: OtherTrait` is required, `Self: Sized` is implied.
/// trait Trait<U: OtherTrait>: Sized {
///     // `T: Clone` is required, `Self::Type<T>: Debug` is implied.
///     type Type<T: Clone>: Debug;
/// }
/// ```
///
/// If `add_drop` is true, we add a `T: Drop` bound for every type generic and associated type.
pub fn implied_predicates<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    options: BoundsOptions,
) -> ItemPredicates<'tcx> {
    use DefKind::*;
    let parent = tcx.opt_parent(def_id);
    let mut predicates = match tcx.def_kind(def_id) {
        // We consider all predicates on traits to be outputs
        Trait | TraitAlias => {
            let mut predicates = predicates_defined_on(tcx, def_id, false);
            if options.resolve_destruct {
                // Add a `T: Drop` bound for every generic, unless the current trait is `Drop` itself, or a
                // built-in marker trait that we know doesn't need the bound.
                if !matches!(
                    tcx.as_lang_item(def_id),
                    Some(
                        LangItem::Destruct
                            | LangItem::Sized
                            | LangItem::MetaSized
                            | LangItem::PointeeSized
                            | LangItem::DiscriminantKind
                            | LangItem::PointeeTrait
                            | LangItem::Tuple
                    )
                ) {
                    add_destruct_bounds(tcx, def_id, &mut predicates);
                }
            }
            predicates
        }
        AssocTy if matches!(tcx.def_kind(parent.unwrap()), Trait) => {
            // `skip_binder` is for the GAT `EarlyBinder`
            let mut predicates = ItemPredicates::new(
                def_id,
                false,
                tcx.explicit_item_bounds(def_id)
                    .skip_binder()
                    .iter()
                    .copied(),
            );
            if options.resolve_destruct {
                // Add a `Drop` bound to the assoc item.
                let destruct_trait = tcx.lang_items().destruct_trait().unwrap();
                let ty =
                    Ty::new_projection(tcx, def_id, GenericArgs::identity_for_item(tcx, def_id));
                let tref = Binder::dummy(TraitRef::new(tcx, destruct_trait, [ty]));
                predicates.add_synthetic_predicates(def_id, [tref.upcast(tcx)]);
            }
            predicates
        }
        _ => ItemPredicates::default(),
    };
    if options.prune_sized {
        prune_sized_predicates(tcx, &mut predicates);
    }
    predicates
}

/// Normalize a value.
pub fn normalize<'tcx, T>(tcx: TyCtxt<'tcx>, typing_env: TypingEnv<'tcx>, value: T) -> T
where
    T: TypeFoldable<TyCtxt<'tcx>> + Clone,
{
    use rustc_infer::infer::TyCtxtInferExt;
    use rustc_middle::traits::ObligationCause;
    use rustc_trait_selection::traits::query::normalize::QueryNormalizeExt;
    let (infcx, param_env) = tcx.infer_ctxt().build_with_typing_env(typing_env);
    infcx
        .at(&ObligationCause::dummy(), param_env)
        .query_normalize(value.clone())
        // We ignore the generated outlives relations. Unsure what we should do with them.
        .map(|x| x.value)
        .unwrap_or(value)
}

/// Erase free regions from the given value. Largely copied from `tcx.erase_and_anonymize_regions`, but also
/// erases bound regions that are bound outside `value`, so we can call this function inside a
/// `Binder`.
pub fn erase_free_regions<'tcx, T>(tcx: TyCtxt<'tcx>, value: T) -> T
where
    T: TypeFoldable<TyCtxt<'tcx>>,
{
    use rustc_middle::ty;
    struct RegionEraserVisitor<'tcx> {
        tcx: TyCtxt<'tcx>,
        depth: u32,
    }

    impl<'tcx> TypeFolder<TyCtxt<'tcx>> for RegionEraserVisitor<'tcx> {
        fn cx(&self) -> TyCtxt<'tcx> {
            self.tcx
        }

        fn fold_ty(&mut self, ty: Ty<'tcx>) -> Ty<'tcx> {
            ty.super_fold_with(self)
        }

        fn fold_binder<T>(&mut self, t: ty::Binder<'tcx, T>) -> ty::Binder<'tcx, T>
        where
            T: TypeFoldable<TyCtxt<'tcx>>,
        {
            let t = self.tcx.anonymize_bound_vars(t);
            self.depth += 1;
            let t = t.super_fold_with(self);
            self.depth -= 1;
            t
        }

        fn fold_region(&mut self, r: ty::Region<'tcx>) -> ty::Region<'tcx> {
            // We don't erase bound regions that are bound inside the expression we started with,
            // but we do erase those that point "outside of it".
            match r.kind() {
                ty::ReBound(BoundVarIndexKind::Bound(dbid), _) if dbid.as_u32() < self.depth => r,
                _ => self.tcx.lifetimes.re_erased,
            }
        }
    }
    value.fold_with(&mut RegionEraserVisitor { tcx, depth: 0 })
}

// Normalize and erase lifetimes, erasing more lifetimes than normal because we might be already
// inside a binder and rustc doesn't like that.
pub fn erase_and_norm<'tcx, T>(tcx: TyCtxt<'tcx>, typing_env: TypingEnv<'tcx>, x: T) -> T
where
    T: TypeFoldable<TyCtxt<'tcx>> + Copy,
{
    erase_free_regions(
        tcx,
        tcx.try_normalize_erasing_regions(typing_env, x)
            .unwrap_or(x),
    )
}

/// Given our currently hacky handling of binders, in order for trait resolution to work we must
/// empty out the binders of trait refs. Specifically it's so that we can reconnect associated type
/// constraints with the trait ref they come from, given that the projection in question doesn't
/// track the right binder currently.
pub fn normalize_bound_val<'tcx, T>(
    tcx: TyCtxt<'tcx>,
    typing_env: TypingEnv<'tcx>,
    x: Binder<'tcx, T>,
) -> Binder<'tcx, T>
where
    T: TypeFoldable<TyCtxt<'tcx>> + Copy,
{
    Binder::dummy(erase_and_norm(tcx, typing_env, x.skip_binder()))
}

/// Returns true whenever `def_id` is `MetaSized`, `Sized` or `PointeeSized`.
pub fn is_sized_related_trait<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    use rustc_hir::lang_items::LangItem;
    let lang_item = tcx.as_lang_item(def_id);
    matches!(
        lang_item,
        Some(LangItem::PointeeSized | LangItem::MetaSized | LangItem::Sized)
    )
}

/// Given a `GenericPredicates`, prune every occurence of a sized-related clause.
/// Prunes bounds of the shape `T: MetaSized`, `T: Sized` or `T: PointeeSized`.
fn prune_sized_predicates<'tcx>(tcx: TyCtxt<'tcx>, generic_predicates: &mut ItemPredicates<'tcx>) {
    generic_predicates.predicates.retain(|pred| {
        pred.clause.as_trait_clause().is_none_or(|trait_predicate| {
            !is_sized_related_trait(tcx, trait_predicate.skip_binder().def_id())
        })
    });
}

pub trait ToPolyTraitRef<'tcx> {
    fn to_poly_trait_ref(&self) -> PolyTraitRef<'tcx>;
}

impl<'tcx> ToPolyTraitRef<'tcx> for PolyTraitPredicate<'tcx> {
    fn to_poly_trait_ref(&self) -> PolyTraitRef<'tcx> {
        self.map_bound_ref(|trait_pred| trait_pred.trait_ref)
    }
}
