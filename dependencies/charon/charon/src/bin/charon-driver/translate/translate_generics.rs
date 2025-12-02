use crate::translate::translate_predicates::PredicateLocation;

use super::translate_ctx::ItemTransCtx;
use charon_lib::ast::*;
use charon_lib::common::hash_by_addr::HashByAddr;
use hax_frontend_exporter as hax;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;

/// A level of binding for type-level variables. Each item has a top-level binding level
/// corresponding to the parameters and clauses to the items. We may then encounter inner binding
/// levels in the following cases:
/// - `for<..>` binders in predicates;
/// - `fn<..>` function pointer types;
/// - `dyn Trait` types, represented as `dyn<T: Trait>` (TODO);
/// - types in a trait declaration or implementation block (TODO);
/// - methods in a trait declaration or implementation block (TODO).
///
/// At each level, we store two things: a `GenericParams` that contains the parameters bound at
/// this level, and various maps from the rustc-internal indices to our indices.
#[derive(Debug, Default)]
pub(crate) struct BindingLevel {
    /// The parameters and predicates bound at this level.
    pub params: GenericParams,
    /// Whether this binder corresponds to an item (method, type) or not (`for<..>` predicate, `fn`
    /// pointer, etc). This indicates whether it corresponds to a rustc `ParamEnv` and therefore
    /// whether we should resolve rustc variables there.
    pub is_item_binder: bool,
    /// Rust makes the distinction between early and late-bound region parameters. We do not make
    /// this distinction, and merge early and late bound regions. For details, see:
    /// <https://smallcultfollowing.com/babysteps/blog/2013/10/29/intermingled-parameter-lists/>
    /// <https://smallcultfollowing.com/babysteps/blog/2013/11/04/intermingled-parameter-lists/>
    ///
    /// The map from rust early regions to translated region indices.
    pub early_region_vars: std::collections::BTreeMap<hax::EarlyParamRegion, RegionId>,
    /// The map from rust late/bound regions to translated region indices.
    pub bound_region_vars: Vec<RegionId>,
    /// The regions added for by-ref upvars, in order of upvars.
    pub by_ref_upvar_regions: Vec<RegionId>,
    /// The map from rust type variable indices to translated type variable indices.
    pub type_vars_map: HashMap<u32, TypeVarId>,
    /// The map from rust const generic variables to translate const generic variable indices.
    pub const_generic_vars_map: HashMap<u32, ConstGenericVarId>,
    /// Cache the translation of types. This harnesses the deduplication of `TyKind` that hax does.
    // Important: we can't reuse type caches from earlier binders as the new binder may change what
    // a given variable resolves to.
    pub type_trans_cache: HashMap<HashByAddr<Arc<hax::TyKind>>, Ty>,
}

/// Small helper: we ignore some region names (when they are equal to "'_")
fn translate_region_name(s: String) -> Option<String> {
    if s == "'_" { None } else { Some(s) }
}

impl BindingLevel {
    pub(crate) fn new(is_item_binder: bool) -> Self {
        Self {
            is_item_binder,
            ..Default::default()
        }
    }

    /// Important: we must push all the early-bound regions before pushing any other region.
    pub(crate) fn push_early_region(&mut self, region: hax::EarlyParamRegion) -> RegionId {
        let name = translate_region_name(region.name.clone());
        // Check that there are no late-bound regions
        assert!(
            self.bound_region_vars.is_empty(),
            "Early regions must be translated before late ones"
        );
        let rid = self
            .params
            .regions
            .push_with(|index| RegionVar { index, name });
        self.early_region_vars.insert(region, rid);
        rid
    }

    /// Important: we must push all the early-bound regions before pushing any other region.
    pub(crate) fn push_bound_region(&mut self, region: hax::BoundRegionKind) -> RegionId {
        use hax::BoundRegionKind::*;
        let name = match region {
            Anon => None,
            NamedAnon(symbol) | Named(_, symbol) => translate_region_name(symbol.clone()),
            ClosureEnv => Some("@env".to_owned()),
        };
        let rid = self
            .params
            .regions
            .push_with(|index| RegionVar { index, name });
        self.bound_region_vars.push(rid);
        rid
    }

    /// Add a region for a by_ref upvar in a closure.
    pub fn push_upvar_region(&mut self) -> RegionId {
        // We musn't push to `bound_region_vars` because that will contain the higher-kinded
        // signature lifetimes if any and they must be lookup-able.
        let region_id = self
            .params
            .regions
            .push_with(|index| RegionVar { index, name: None });
        self.by_ref_upvar_regions.push(region_id);
        region_id
    }

    pub(crate) fn push_type_var(&mut self, rid: u32, name: String) -> TypeVarId {
        let var_id = self.params.types.push_with(|index| TypeVar { index, name });
        self.type_vars_map.insert(rid, var_id);
        var_id
    }

    pub(crate) fn push_const_generic_var(&mut self, rid: u32, ty: LiteralTy, name: String) {
        let var_id = self
            .params
            .const_generics
            .push_with(|index| ConstGenericVar { index, name, ty });
        self.const_generic_vars_map.insert(rid, var_id);
    }

    /// Translate a binder of regions by appending the stored reguions to the given vector.
    pub(crate) fn push_params_from_binder(&mut self, binder: hax::Binder<()>) -> Result<(), Error> {
        assert!(
            self.bound_region_vars.is_empty(),
            "Trying to use two binders at the same binding level"
        );
        use hax::BoundVariableKind::*;
        for p in binder.bound_vars {
            match p {
                Region(region) => {
                    self.push_bound_region(region);
                }
                Ty(_) => {
                    panic!("Unexpected locally bound type variable");
                }
                Const => {
                    panic!("Unexpected locally bound const generic variable");
                }
            }
        }
        Ok(())
    }
}

impl<'tcx, 'ctx> ItemTransCtx<'tcx, 'ctx> {
    /// Get the only binding level. Panics if there are other binding levels.
    pub(crate) fn the_only_binder(&self) -> &BindingLevel {
        assert_eq!(self.binding_levels.len(), 1);
        self.innermost_binder()
    }

    /// Get the only binding level. Panics if there are other binding levels.
    pub(crate) fn the_only_binder_mut(&mut self) -> &mut BindingLevel {
        assert_eq!(self.binding_levels.len(), 1);
        self.innermost_binder_mut()
    }

    pub(crate) fn outermost_binder(&self) -> &BindingLevel {
        self.binding_levels.outermost()
    }

    pub(crate) fn innermost_binder(&self) -> &BindingLevel {
        self.binding_levels.innermost()
    }

    pub(crate) fn innermost_binder_mut(&mut self) -> &mut BindingLevel {
        self.binding_levels.innermost_mut()
    }

    pub(crate) fn innermost_generics_mut(&mut self) -> &mut GenericParams {
        &mut self.innermost_binder_mut().params
    }

    pub(crate) fn lookup_bound_region(
        &mut self,
        span: Span,
        dbid: hax::DebruijnIndex,
        var: hax::BoundVar,
    ) -> Result<RegionDbVar, Error> {
        let dbid = DeBruijnId::new(dbid);
        if let Some(rid) = self
            .binding_levels
            .get(dbid)
            .and_then(|bl| bl.bound_region_vars.get(var))
        {
            Ok(DeBruijnVar::bound(dbid, *rid))
        } else {
            raise_error!(
                self,
                span,
                "Unexpected error: could not find region '{dbid}_{var}"
            )
        }
    }

    pub(crate) fn lookup_param<Id: Copy>(
        &mut self,
        span: Span,
        f: impl for<'a> Fn(&'a BindingLevel) -> Option<Id>,
        mk_err: impl FnOnce() -> String,
    ) -> Result<DeBruijnVar<Id>, Error> {
        for (dbid, bl) in self.binding_levels.iter_enumerated() {
            if let Some(id) = f(bl) {
                return Ok(DeBruijnVar::bound(dbid, id));
            }
        }
        let err = mk_err();
        raise_error!(self, span, "Unexpected error: could not find {}", err)
    }

    pub(crate) fn lookup_early_region(
        &mut self,
        span: Span,
        region: &hax::EarlyParamRegion,
    ) -> Result<RegionDbVar, Error> {
        self.lookup_param(
            span,
            |bl| bl.early_region_vars.get(region).copied(),
            || format!("the region variable {region:?}"),
        )
    }

    pub(crate) fn lookup_type_var(
        &mut self,
        span: Span,
        param: &hax::ParamTy,
    ) -> Result<TypeDbVar, Error> {
        self.lookup_param(
            span,
            |bl| bl.type_vars_map.get(&param.index).copied(),
            || format!("the type variable {}", param.name),
        )
    }

    pub(crate) fn lookup_const_generic_var(
        &mut self,
        span: Span,
        param: &hax::ParamConst,
    ) -> Result<ConstGenericDbVar, Error> {
        self.lookup_param(
            span,
            |bl| bl.const_generic_vars_map.get(&param.index).copied(),
            || format!("the const generic variable {}", param.name),
        )
    }

    pub(crate) fn lookup_clause_var(
        &mut self,
        span: Span,
        mut id: usize,
    ) -> Result<ClauseDbVar, Error> {
        // The clause indices returned by hax count clauses in order, starting from the parentmost.
        // While adding clauses to a binding level we already need to translate types and clauses,
        // so the innermost item binder may not have all the clauses yet. Hence for that binder we
        // ignore the clause count.
        let innermost_item_binder_id = self
            .binding_levels
            .iter_enumerated()
            .find(|(_, bl)| bl.is_item_binder)
            .unwrap()
            .0;
        // Iterate over the binders, starting from the outermost.
        for (dbid, bl) in self.binding_levels.iter_enumerated().rev() {
            let num_clauses_bound_at_this_level = bl.params.trait_clauses.elem_count();
            if id < num_clauses_bound_at_this_level || dbid == innermost_item_binder_id {
                let id = TraitClauseId::from_usize(id);
                return Ok(DeBruijnVar::bound(dbid, id));
            } else {
                id -= num_clauses_bound_at_this_level
            }
        }
        // Actually unreachable
        raise_error!(
            self,
            span,
            "Unexpected error: could not find clause variable {}",
            id
        )
    }

    pub(crate) fn push_generic_params(&mut self, generics: &hax::TyGenerics) -> Result<(), Error> {
        for param in &generics.params {
            self.push_generic_param(param)?;
        }
        Ok(())
    }

    pub(crate) fn push_generic_param(&mut self, param: &hax::GenericParamDef) -> Result<(), Error> {
        match &param.kind {
            hax::GenericParamDefKind::Lifetime => {
                let region = hax::EarlyParamRegion {
                    index: param.index,
                    name: param.name.clone(),
                };
                let _ = self.innermost_binder_mut().push_early_region(region);
            }
            hax::GenericParamDefKind::Type { .. } => {
                let _ = self
                    .innermost_binder_mut()
                    .push_type_var(param.index, param.name.clone());
            }
            hax::GenericParamDefKind::Const { ty, .. } => {
                let span = self.def_span(&param.def_id);
                // The type should be primitive, meaning it shouldn't contain variables,
                // non-primitive adts, etc. As a result, we can use an empty context.
                let ty = self.translate_ty(span, ty)?;
                match ty.kind().as_literal() {
                    Some(ty) => self.innermost_binder_mut().push_const_generic_var(
                        param.index,
                        *ty,
                        param.name.clone(),
                    ),
                    None => raise_error!(
                        self,
                        span,
                        "Constant parameters of non-literal type are not supported"
                    ),
                }
            }
        }

        Ok(())
    }

    /// Add the generics and predicates of this item and its parents to the current context.
    #[tracing::instrument(skip(self, span))]
    fn push_generics_for_def(
        &mut self,
        span: Span,
        def: &hax::FullDef,
        is_parent: bool,
    ) -> Result<(), Error> {
        // Add generics from the parent item, recursively (recursivity is important for closures,
        // as they can be nested).
        if let Some(parent_item) = def.typing_parent(self.hax_state()) {
            let parent_def = self.hax_def(&parent_item)?;
            self.push_generics_for_def(span, &parent_def, true)?;
        }
        self.push_generics_for_def_without_parents(span, def, !is_parent, !is_parent)?;
        Ok(())
    }

    /// Add the generics and predicates of this item. This does not include the parent generics;
    /// use `push_generics_for_def` to get the full list.
    fn push_generics_for_def_without_parents(
        &mut self,
        _span: Span,
        def: &hax::FullDef,
        include_late_bound: bool,
        include_assoc_ty_clauses: bool,
    ) -> Result<(), Error> {
        use hax::FullDefKind;
        if let Some(param_env) = def.param_env() {
            // Add the generic params.
            self.push_generic_params(&param_env.generics)?;
            // Add the predicates.
            let origin = match &def.kind {
                FullDefKind::Adt { .. }
                | FullDefKind::TyAlias { .. }
                | FullDefKind::AssocTy { .. } => PredicateOrigin::WhereClauseOnType,
                FullDefKind::Fn { .. }
                | FullDefKind::AssocFn { .. }
                | FullDefKind::Const { .. }
                | FullDefKind::AssocConst { .. }
                | FullDefKind::Static { .. } => PredicateOrigin::WhereClauseOnFn,
                FullDefKind::TraitImpl { .. } | FullDefKind::InherentImpl { .. } => {
                    PredicateOrigin::WhereClauseOnImpl
                }
                FullDefKind::Trait { .. } | FullDefKind::TraitAlias { .. } => {
                    PredicateOrigin::WhereClauseOnTrait
                }
                _ => panic!("Unexpected def: {def:?}"),
            };
            self.register_predicates(
                &param_env.predicates,
                origin.clone(),
                &PredicateLocation::Base,
            )?;
            // Also register implied predicates.
            if let FullDefKind::Trait {
                implied_predicates, ..
            }
            | FullDefKind::TraitAlias {
                implied_predicates, ..
            }
            | FullDefKind::AssocTy {
                implied_predicates, ..
            } = &def.kind
            {
                self.register_predicates(implied_predicates, origin, &PredicateLocation::Parent)?;
            }

            if let hax::FullDefKind::Trait { items, .. } = &def.kind
                && include_assoc_ty_clauses
                && !self.monomorphize()
            {
                // Also add the predicates on associated types.
                // FIXME(gat): don't skip GATs.
                // FIXME: don't mix up implied and required predicates.
                for assoc in items {
                    let item_def = self.poly_hax_def(&assoc.def_id)?;
                    if let hax::FullDefKind::AssocTy {
                        param_env,
                        implied_predicates,
                        ..
                    } = item_def.kind()
                        && param_env.generics.params.is_empty()
                    {
                        let name = self.t_ctx.translate_trait_item_name(item_def.def_id())?;
                        self.register_predicates(
                            &implied_predicates,
                            PredicateOrigin::TraitItem(name.clone()),
                            &PredicateLocation::Item(name),
                        )?;
                    }
                }
            }
        }

        if let hax::FullDefKind::Closure { args, .. } = def.kind()
            && include_late_bound
        {
            // Add the lifetime generics coming from the by-ref upvars.
            args.upvar_tys.iter().for_each(|ty| {
                if matches!(
                    ty.kind(),
                    hax::TyKind::Ref(
                        hax::Region {
                            kind: hax::RegionKind::ReErased
                        },
                        ..
                    )
                ) {
                    self.the_only_binder_mut().push_upvar_region();
                }
            });
        }

        // The parameters (and in particular the lifetimes) are split between
        // early bound and late bound parameters. See those blog posts for explanations:
        // https://smallcultfollowing.com/babysteps/blog/2013/10/29/intermingled-parameter-lists/
        // https://smallcultfollowing.com/babysteps/blog/2013/11/04/intermingled-parameter-lists/
        // Note that only lifetimes can be late bound.
        //
        // [TyCtxt.generics_of] gives us the early-bound parameters. We add the late-bound
        // parameters here.
        let signature = match &def.kind {
            hax::FullDefKind::Fn { sig, .. } => Some(sig),
            hax::FullDefKind::AssocFn { sig, .. } => Some(sig),
            _ => None,
        };
        if let Some(signature) = signature
            && include_late_bound
        {
            let innermost_binder = self.innermost_binder_mut();
            assert!(innermost_binder.bound_region_vars.is_empty());
            innermost_binder.push_params_from_binder(signature.rebind(()))?;
        }

        Ok(())
    }

    /// Translate the generics and predicates of this item and its parents.
    /// This adds generic parameters and predicates to the current environment (as a binder in `self.binding_levels`).
    /// This is necessary to translate types that depend on these generics (such as `Ty` and `TraitRef`).
    /// The constructed `GenericParams` can be recovered at the end using `self.into_generics()` and stored in the translated item.
    pub(crate) fn translate_def_generics(
        &mut self,
        span: Span,
        def: &hax::FullDef,
    ) -> Result<(), Error> {
        assert!(self.binding_levels.len() == 0);
        self.binding_levels.push(BindingLevel::new(true));
        self.push_generics_for_def(span, def, false)?;
        self.innermost_binder_mut().params.check_consistency();
        Ok(())
    }

    /// Translate the generics and predicates of this item without its parents.
    pub(crate) fn translate_def_generics_without_parents(
        &mut self,
        span: Span,
        def: &hax::FullDef,
    ) -> Result<(), Error> {
        self.binding_levels.push(BindingLevel::new(true));
        self.push_generics_for_def_without_parents(span, def, true, true)?;
        self.innermost_binder().params.check_consistency();
        Ok(())
    }

    /// Push a new binding level corresponding to the provided `def` for the duration of the inner
    /// function call.
    pub(crate) fn translate_binder_for_def<F, U>(
        &mut self,
        span: Span,
        kind: BinderKind,
        def: &hax::FullDef,
        f: F,
    ) -> Result<Binder<U>, Error>
    where
        F: FnOnce(&mut Self) -> Result<U, Error>,
    {
        assert!(!self.binding_levels.is_empty());

        // Register the type-level parameters. This pushes a new binding level.
        self.translate_def_generics_without_parents(span, def)?;

        // Call the continuation. Important: do not short-circuit on error here.
        let res = f(self);

        // Reset
        let params = self.binding_levels.pop().unwrap().params;

        // Return
        res.map(|skip_binder| Binder {
            kind,
            params,
            skip_binder,
        })
    }

    /// Push a group of bound regions and call the continuation.
    /// We use this when diving into a `for<'a>`, or inside an arrow type (because
    /// it contains universally quantified regions).
    pub(crate) fn translate_region_binder<F, T, U>(
        &mut self,
        _span: Span,
        binder: &hax::Binder<T>,
        f: F,
    ) -> Result<RegionBinder<U>, Error>
    where
        F: FnOnce(&mut Self, &T) -> Result<U, Error>,
    {
        assert!(!self.binding_levels.is_empty());

        // Register the variables
        let mut binding_level = BindingLevel::new(false);
        binding_level.push_params_from_binder(binder.rebind(()))?;
        self.binding_levels.push(binding_level);

        // Call the continuation. Important: do not short-circuit on error here.
        let res = f(self, binder.hax_skip_binder_ref());

        // Reset
        let regions = self.binding_levels.pop().unwrap().params.regions;

        // Return
        res.map(|skip_binder| RegionBinder {
            regions,
            skip_binder,
        })
    }

    pub(crate) fn into_generics(mut self) -> GenericParams {
        assert!(self.binding_levels.len() == 1);
        self.binding_levels.pop().unwrap().params
    }
}
