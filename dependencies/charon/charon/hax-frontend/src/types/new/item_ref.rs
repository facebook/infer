use crate::prelude::*;

use rustc_middle::ty;
use rustc_span::def_id::DefId as RDefId;

/// Reference to an item, with generics. Basically any mention of an item (function, type, etc)
/// uses this.
///
/// This can refer to a top-level item or to a trait associated item. Example:
/// ```ignore
/// trait MyTrait<TraitType, const TraitConst: usize> {
///   fn meth<MethType>(...) {...}
/// }
/// fn example_call<TraitType, SelfType: MyTrait<TraitType, 12>>(x: SelfType) {
///   x.meth::<String>(...)
/// }
/// ```
/// Here, in the call `x.meth::<String>(...)` we will build an `ItemRef` that looks like:
/// ```ignore
/// ItemRef {
///     def_id = MyTrait::meth,
///     generic_args = [String],
///     impl_exprs = [<proof of `String: Sized`>],
///     in_trait = Some(<proof of `SelfType: MyTrait<TraitType, 12>`>,
/// }
/// ```
/// The `in_trait` `ImplExpr` will have in its `trait` field a representation of the `SelfType:
/// MyTrait<TraitType, 12>` predicate, which looks like:
/// ```ignore
/// ItemRef {
///     def_id = MyTrait,
///     generic_args = [SelfType, TraitType, 12],
///     impl_exprs = [],
///     in_trait = None,
/// }
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ItemRef {
    pub(crate) contents: id_table::hash_consing::HashConsed<ItemRefContents>,
}

impl ItemRefContents {
    fn intern<'tcx, S: BaseState<'tcx>>(self, _s: &S) -> ItemRef {
        let contents = id_table::hash_consing::HashConsed::new(self);
        ItemRef { contents }
    }
}

impl ItemRef {
    /// The main way to obtain an `ItemRef`: from a `def_id` and generics.
    pub fn translate<'tcx, S: UnderOwnerState<'tcx>>(
        s: &S,
        def_id: RDefId,
        generics: ty::GenericArgsRef<'tcx>,
    ) -> ItemRef {
        let hax_def_id = def_id.sinto(s);
        Self::translate_from_hax_def_id(s, hax_def_id, generics)
    }

    pub fn translate_from_hax_def_id<'tcx, S: UnderOwnerState<'tcx>>(
        s: &S,
        def_id: DefId,
        generics: ty::GenericArgsRef<'tcx>,
    ) -> ItemRef {
        Self::translate_maybe_resolve_impl(
            s,
            s.base().options.item_ref_use_concrete_impl,
            def_id,
            generics,
        )
    }

    /// Makes a `ItemRef` from a `def_id` and generics.
    ///
    /// If `resolve_trait_ref == true` and `(def_id, generics)` points to a trait item that
    /// can be resolved to a specific `impl`, `translate` rewrites `def_id` to the
    /// concrete associated item from that `impl` and re-bases the generics.
    ///
    /// For instance, [`<u32 as From<u8>>::from`] produces a [`ItemRef`] with a
    /// [`DefId`] looking like `core::convert::num::Impl#42::from` when
    /// `resolve_impl` is `true`, `core::convert::From::from` otherwise.
    pub fn translate_maybe_resolve_impl<'tcx, S: UnderOwnerState<'tcx>>(
        s: &S,
        // Whether to resolve trait references.
        resolve_trait_ref: bool,
        mut hax_def_id: DefId,
        generics: ty::GenericArgsRef<'tcx>,
    ) -> ItemRef {
        use rustc_infer::infer::canonical::ir::TypeVisitableExt;
        let tcx = s.base().tcx;
        let typing_env = s.typing_env();
        let mut def_id = hax_def_id.as_def_id_even_synthetic();
        let key = (hax_def_id.clone(), generics, resolve_trait_ref);
        // Normalize the generics. This is crucial for `rustc_args()` because if we don't we might
        // get a `Option<T>` ItemRef with `rustc_args() = Option<<Self as Iterator>::Item>`, but
        // because the mapping is global we'd return these args in item contexts where they aren't
        // valid.
        let mut generics = normalize(tcx, typing_env, generics);
        if let Some(item) = s.with_cache(|cache| cache.item_refs.get(&key).cloned()) {
            return item;
        }

        if tcx.is_typeck_child(def_id) {
            // Rustc gives closures/inline consts extra generic for inference that we don't care about.
            generics = generics.truncate_to(tcx, tcx.generics_of(tcx.typeck_root_def_id(def_id)));
        }

        // If this is an associated item, resolve the trait reference.
        let mut trait_info = if resolve_trait_ref {
            self_clause_for_item(s, def_id, generics)
        } else {
            None
        };

        // If the reference is a known trait impl and the impl implements the target item, we can
        // point directly to the implemented item.
        if let Some(tinfo) = &trait_info
            && let ImplExprAtom::Concrete(impl_ref) = &tinfo.r#impl
            && let impl_def_id = impl_ref.def_id.real_rust_def_id()
            && let Some(implemented_item) = tcx
                .associated_items(impl_def_id)
                .in_definition_order()
                .find(|item| item.trait_item_def_id() == Some(def_id))
        {
            let trait_def_id = tcx.parent(def_id);
            def_id = implemented_item.def_id;
            hax_def_id = def_id.sinto(s);
            generics = generics.rebase_onto(tcx, trait_def_id, impl_ref.rustc_args(s));
            trait_info = None;
        }

        let mut hax_generics = generics.sinto(s);
        let mut impl_exprs = solve_item_required_traits(s, def_id, generics);

        // Fixup the generics.
        if let Some(tinfo) = &trait_info {
            // The generics are split in two: the arguments of the trait and the arguments of the
            // method/associated item.
            //
            // For instance, if we have:
            // ```
            // trait Foo<T> {
            //     fn baz<U>(...) { ... }
            // }
            //
            // fn test<T : Foo<u32>(x: T) {
            //     x.baz(...);
            //     ...
            // }
            // ```
            // The generics for the call to `baz` will be the concatenation: `<T, u32, U>`, which we
            // split into `<T, u32>` and `<U>`.
            let trait_ref = tinfo.r#trait.hax_skip_binder_ref();
            let num_trait_generics = trait_ref.generic_args.len();
            hax_generics.drain(0..num_trait_generics);
            let mut num_trait_trait_clauses = trait_ref.impl_exprs.len();
            // Items other than associated types get an extra `Self: Trait` clause as the first
            // clause, we skip that one too. Note: that clause is the same as `tinfo`.
            if !matches!(hax_def_id.kind, DefKind::AssocTy) {
                num_trait_trait_clauses += 1;
            };
            impl_exprs.drain(0..num_trait_trait_clauses);
        }

        let content = ItemRefContents {
            def_id: hax_def_id,
            generic_args: hax_generics,
            impl_exprs,
            in_trait: trait_info,
            has_param: generics.has_param()
                || generics.has_escaping_bound_vars()
                || generics.has_free_regions(),
            has_non_lt_param: generics.has_param(),
        };
        let item = content.intern(s);
        s.with_cache(|cache| {
            cache.item_refs.insert(key, item.clone());
        });
        s.with_global_cache(|cache| {
            cache.reverse_item_refs_map.insert(item.clone(), generics);
        });
        item
    }

    /// Construct an `ItemRef` for items that can't have generics (e.g. modules).
    pub fn dummy_without_generics<'tcx, S: BaseState<'tcx>>(s: &S, def_id: DefId) -> ItemRef {
        let content = ItemRefContents {
            def_id,
            generic_args: Default::default(),
            impl_exprs: Default::default(),
            in_trait: Default::default(),
            has_param: false,
            has_non_lt_param: false,
        };
        let item = content.intern(s);
        s.with_global_cache(|cache| {
            cache
                .reverse_item_refs_map
                .insert(item.clone(), ty::GenericArgsRef::default());
        });
        item
    }

    /// For an `ItemRef` that refers to a trait, this returns values for each of the non-gat
    /// associated types of this trait and its parents, in a fixed order.
    pub fn trait_associated_types<'tcx, S: UnderOwnerState<'tcx>>(&self, s: &S) -> Vec<Ty> {
        if !matches!(self.def_id.kind, DefKind::Trait | DefKind::TraitAlias) {
            panic!("`ItemRef::trait_associated_types` expected a trait")
        }
        let tcx = s.base().tcx;
        let typing_env = s.typing_env();
        let def_id = self.def_id.real_rust_def_id();
        let generics = self.rustc_args(s);
        let tref = ty::TraitRef::new(tcx, def_id, generics);
        rustc_utils::assoc_tys_for_trait(tcx, typing_env, tref)
            .into_iter()
            .map(|alias_ty| ty::Ty::new_alias(tcx, ty::Projection, alias_ty))
            .map(|ty| normalize(tcx, typing_env, ty))
            .map(|ty| ty.sinto(s))
            .collect()
    }

    /// Erase lifetimes from the generic arguments of this item.
    pub fn erase<'tcx, S: UnderOwnerState<'tcx>>(&self, s: &S) -> Self {
        let args = self.rustc_args(s);
        let args = erase_and_norm(s.base().tcx, s.typing_env(), args);
        Self::translate_from_hax_def_id(s, self.def_id.clone(), args)
    }

    pub fn contents(&self) -> &ItemRefContents {
        &self.contents
    }

    /// Recover the original rustc args that generated this `ItemRef`. Will panic if the `ItemRef`
    /// was built by hand instead of using `translate_item_ref`.
    pub fn rustc_args<'tcx, S: BaseState<'tcx>>(&self, s: &S) -> ty::GenericArgsRef<'tcx> {
        s.with_global_cache(|cache| *cache.reverse_item_refs_map.get(self).unwrap())
    }

    /// Mutate the `DefId`, keeping the same generic args.
    pub fn mutate_def_id<'tcx, S: BaseState<'tcx>>(
        &self,
        s: &S,
        f: impl FnOnce(&mut DefId),
    ) -> Self {
        let args = self.rustc_args(s);
        let mut contents = self.contents().clone();
        f(&mut contents.def_id);
        let new = contents.intern(s);
        s.with_global_cache(|cache| {
            cache.reverse_item_refs_map.insert(new.clone(), args);
        });
        new
    }

    /// Set the `DefId`, keeping the same generic args.
    pub fn with_def_id<'tcx, S: BaseState<'tcx>>(&self, s: &S, def_id: &DefId) -> Self {
        self.mutate_def_id(s, |d| *d = def_id.clone())
    }
}

impl std::ops::Deref for ItemRef {
    type Target = ItemRefContents;
    fn deref(&self) -> &Self::Target {
        self.contents()
    }
}
