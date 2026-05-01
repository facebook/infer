use super::translate_ctx::*;
use charon_lib::ast::*;
use charon_lib::ids::IndexMap;
use rustc_span::{kw, sym};

impl<'tcx> TranslateCtx<'tcx> {
    pub fn recognize_builtin_impl(
        &self,
        trait_data: &hax::BuiltinTraitData,
        trait_def: &hax::FullDef,
    ) -> Option<BuiltinImplData> {
        Some(match trait_data {
            hax::BuiltinTraitData::Destruct(x) => {
                match x {
                    hax::DestructData::Noop => BuiltinImplData::NoopDestruct,
                    hax::DestructData::Implicit => BuiltinImplData::UntrackedDestruct,
                    // This is unconditionally replaced by a `TraitImpl`.
                    hax::DestructData::Glue { .. } => return None,
                }
            }
            hax::BuiltinTraitData::Other => match &trait_def.lang_item {
                _ if self
                    .tcx
                    .trait_is_auto(trait_def.def_id().real_rust_def_id()) =>
                {
                    BuiltinImplData::Auto
                }
                None => return None,
                Some(litem) => match *litem {
                    sym::sized => BuiltinImplData::Sized,
                    sym::meta_sized => BuiltinImplData::MetaSized,
                    sym::tuple_trait => BuiltinImplData::Tuple,
                    kw::Fn => BuiltinImplData::Fn,
                    sym::fn_mut => BuiltinImplData::FnMut,
                    sym::fn_once => BuiltinImplData::FnOnce,
                    sym::pointee_trait => BuiltinImplData::Pointee,
                    sym::clone => BuiltinImplData::Clone,
                    sym::copy => BuiltinImplData::Copy,
                    sym::discriminant_kind => BuiltinImplData::DiscriminantKind,
                    _ => return None,
                },
            },
        })
    }
}

impl<'tcx, 'ctx> ItemTransCtx<'tcx, 'ctx> {
    /// Translates the given predicates and stores them as resuired preciates of the innermost
    /// binder.
    ///
    /// This function should be called **after** we translated the generics (type parameters,
    /// regions...).
    pub(crate) fn register_predicates(
        &mut self,
        preds: &hax::GenericPredicates,
        origin: PredicateOrigin,
    ) -> Result<(), Error> {
        self.translate_predicates(preds, origin, None)?;
        Ok(())
    }

    /// Translates the given predicates. This function should be called **after** we translated the
    /// generics (type parameters, regions...).
    pub(crate) fn translate_predicates(
        &mut self,
        preds: &hax::GenericPredicates,
        origin: PredicateOrigin,
        // Either put clauses there or in the innermost binder.
        mut trait_clauses: Option<&mut IndexMap<TraitClauseId, TraitParam>>,
    ) -> Result<(), Error> {
        if trait_clauses.is_none() {
            // Register the mapping from trait preds to their id early on, as these can be mentioned
            // while translating any other predicate including themselves. Each trait pred gives rise
            // to exactly one trait clause inserted into `trait_clauses`, which we use to compute
            // clause ids.
            let next_clause_id = self.innermost_generics_mut().trait_clauses.next_id();
            for (i, pred) in preds
                .predicates
                .iter()
                .filter(|pred| {
                    matches!(
                        pred.clause.kind.hax_skip_binder_ref(),
                        hax::ClauseKind::Trait(_)
                    )
                })
                .enumerate()
            {
                self.innermost_binder_mut()
                    .trait_preds
                    .insert(pred.id.clone(), next_clause_id + i);
            }
        }

        for pred in &preds.predicates {
            self.translate_predicate(pred, origin.clone(), trait_clauses.as_deref_mut())?;
        }
        Ok(())
    }

    pub(crate) fn translate_poly_trait_ref(
        &mut self,
        span: Span,
        bound_trait_ref: &hax::Binder<hax::TraitRef>,
    ) -> Result<PolyTraitDeclRef, Error> {
        self.translate_region_binder(span, bound_trait_ref, move |ctx, trait_ref| {
            ctx.translate_trait_ref(span, trait_ref)
        })
    }

    pub(crate) fn translate_poly_trait_predicate(
        &mut self,
        span: Span,
        bound_trait_ref: &hax::Binder<hax::TraitPredicate>,
    ) -> Result<PolyTraitDeclRef, Error> {
        self.translate_region_binder(span, bound_trait_ref, move |ctx, trait_ref| {
            ctx.translate_trait_predicate(span, trait_ref)
        })
    }

    pub(crate) fn translate_trait_predicate(
        &mut self,
        span: Span,
        trait_pred: &hax::TraitPredicate,
    ) -> Result<TraitDeclRef, Error> {
        // we don't handle negative trait predicates.
        assert!(trait_pred.is_positive);
        self.translate_trait_ref(span, &trait_pred.trait_ref)
    }

    pub(crate) fn translate_trait_ref(
        &mut self,
        span: Span,
        trait_ref: &hax::TraitRef,
    ) -> Result<TraitDeclRef, Error> {
        self.translate_trait_decl_ref(span, trait_ref)
    }

    pub(crate) fn translate_predicate(
        &mut self,
        pred: &hax::GenericPredicate,
        origin: PredicateOrigin,
        // Either put clauses there or in the innermost binder.
        mut trait_clauses: Option<&mut IndexMap<TraitClauseId, TraitParam>>,
    ) -> Result<(), Error> {
        use hax::ClauseKind;
        let clause = &pred.clause;
        trace!("{:?}", clause);
        let span = self.translate_span(&pred.span);
        match clause.kind.hax_skip_binder_ref() {
            ClauseKind::Trait(trait_pred) => {
                let trait_pred = self.translate_region_binder(span, &clause.kind, |ctx, _| {
                    ctx.translate_trait_predicate(span, trait_pred)
                })?;
                let clause_id = trait_clauses
                    .as_deref_mut()
                    .unwrap_or(&mut self.innermost_generics_mut().trait_clauses)
                    .push_with(|clause_id| TraitParam {
                        clause_id,
                        origin,
                        span: Some(span),
                        trait_: trait_pred,
                    });

                if trait_clauses.is_none() {
                    // Sanity check.
                    let expected_clause_id = self
                        .innermost_binder_mut()
                        .trait_preds
                        .get(&pred.id)
                        .unwrap();
                    debug_assert_eq!(clause_id, *expected_clause_id);
                }
            }
            ClauseKind::RegionOutlives(p) => {
                let pred = self.translate_region_binder(span, &clause.kind, |ctx, _| {
                    let r0 = ctx.translate_region(span, &p.lhs)?;
                    let r1 = ctx.translate_region(span, &p.rhs)?;
                    Ok(OutlivesPred(r0, r1))
                })?;
                self.innermost_generics_mut().regions_outlive.push(pred);
            }
            ClauseKind::TypeOutlives(p) => {
                let pred = self.translate_region_binder(span, &clause.kind, |ctx, _| {
                    let ty = ctx.translate_ty(span, &p.lhs)?;
                    let r = ctx.translate_region(span, &p.rhs)?;
                    Ok(OutlivesPred(ty, r))
                })?;
                self.innermost_generics_mut().types_outlive.push(pred);
            }
            ClauseKind::Projection(p) => {
                // This is used to express constraints over associated types.
                // For instance:
                // ```
                // T : Foo<S = String>
                //         ^^^^^^^^^^
                // ```
                let pred = self.translate_region_binder(span, &clause.kind, |ctx, _| {
                    let trait_ref = ctx.translate_trait_impl_expr(span, &p.impl_expr)?;
                    let ty = ctx.translate_ty(span, &p.ty)?;
                    let type_name = ctx.t_ctx.translate_trait_item_name(&p.assoc_item.def_id)?;
                    Ok(TraitTypeConstraint {
                        trait_ref,
                        type_name,
                        ty,
                    })
                })?;
                self.innermost_generics_mut()
                    .trait_type_constraints
                    .push(pred);
            }
            ClauseKind::ConstArgHasType(..) => {
                // These are used for trait resolution to get access to the type of const generics.
                // We don't need them.
            }
            ClauseKind::HostEffect(..) => {
                // These are used for `const Trait` clauses. Part of the `const_traits` unstable
                // features. We ignore them for now.
            }
            ClauseKind::WellFormed(..) | ClauseKind::ConstEvaluatable(..) => {
                // This is e.g. a clause `[(); N+1]:` (without anything after the `:`). This is
                // used to require that the fallible `N+1` expression succeeds, so that it can be
                // used at the type level. Part of the `generic_const_exprs` unstable feature.
            }
            ClauseKind::UnstableFeature(..) => {
                // Unclear what this means, related to stability markers which we don't care about.
            }
            #[expect(unreachable_patterns)]
            kind => raise_error!(self, span, "Unsupported clause: {:?}", kind),
        }
        Ok(())
    }

    pub(crate) fn translate_trait_impl_exprs(
        &mut self,
        span: Span,
        impl_sources: &[hax::ImplExpr],
    ) -> Result<IndexMap<TraitClauseId, TraitRef>, Error> {
        impl_sources
            .iter()
            .map(|x| self.translate_trait_impl_expr(span, x))
            .try_collect()
    }

    #[tracing::instrument(skip(self, span, impl_expr))]
    pub(crate) fn translate_trait_impl_expr(
        &mut self,
        span: Span,
        impl_expr: &hax::ImplExpr,
    ) -> Result<TraitRef, Error> {
        let trait_decl_ref = self.translate_poly_trait_ref(span, &impl_expr.r#trait)?;

        match self.translate_trait_impl_expr_aux(span, impl_expr, trait_decl_ref.clone()) {
            Ok(res) => Ok(res),
            Err(err) => {
                register_error!(self, span, "Error during trait resolution: {}", &err.msg);
                Ok(TraitRef::new(
                    TraitRefKind::Unknown(err.msg),
                    trait_decl_ref,
                ))
            }
        }
    }

    pub(crate) fn translate_trait_impl_expr_aux(
        &mut self,
        span: Span,
        impl_source: &hax::ImplExpr,
        trait_decl_ref: PolyTraitDeclRef,
    ) -> Result<TraitRef, Error> {
        trace!("impl_expr: {:#?}", impl_source);
        use hax::DestructData;
        use hax::ImplExprAtom;

        let trait_ref = match &impl_source.r#impl {
            ImplExprAtom::Concrete(item) => {
                let impl_ref =
                    self.translate_trait_impl_ref(span, item, TraitImplSource::Normal)?;
                TraitRef::new(TraitRefKind::TraitImpl(impl_ref), trait_decl_ref)
            }
            ImplExprAtom::SelfImpl {
                r#trait: trait_ref,
                path,
            }
            | ImplExprAtom::LocalBound {
                r#trait: trait_ref,
                path,
                ..
            } => {
                trace!(
                    "impl source (self or clause): param:\n- trait_ref: {:?}\n- path: {:?}",
                    trait_ref, path,
                );
                // If we are referring to a trait clause, we need to find the relevant one.
                let mut tref_kind = match &impl_source.r#impl {
                    ImplExprAtom::SelfImpl { .. } => TraitRefKind::SelfId,
                    ImplExprAtom::LocalBound { id, .. } => match self.lookup_clause_var(span, id) {
                        Ok(var) => TraitRefKind::Clause(var),
                        Err(err) => TraitRefKind::Unknown(err.msg),
                    },
                    _ => unreachable!(),
                };

                let mut current_pred = self.translate_poly_trait_ref(span, trait_ref)?;

                // Apply the path
                for path_elem in path {
                    use hax::ImplExprPathChunk::*;
                    let trait_ref = Box::new(TraitRef::new(tref_kind, current_pred));
                    match path_elem {
                        AssocItem {
                            item,
                            predicate,
                            index,
                            ..
                        } => {
                            let name = self.t_ctx.translate_trait_item_name(&item.def_id)?;
                            tref_kind = TraitRefKind::ItemClause(
                                trait_ref,
                                name,
                                TraitClauseId::new(*index),
                            );
                            current_pred = self.translate_poly_trait_predicate(span, predicate)?;
                        }
                        Parent {
                            predicate, index, ..
                        } => {
                            tref_kind =
                                TraitRefKind::ParentClause(trait_ref, TraitClauseId::new(*index));
                            current_pred = self.translate_poly_trait_predicate(span, predicate)?;
                        }
                    }
                }

                TraitRef::new(tref_kind, trait_decl_ref)
            }
            ImplExprAtom::Dyn => TraitRef::new(TraitRefKind::Dyn, trait_decl_ref),
            ImplExprAtom::Builtin {
                trait_data,
                impl_exprs,
                types,
                ..
            } => {
                let tref = &impl_source.r#trait;
                let trait_def =
                    self.hax_def(&tref.hax_skip_binder_ref().erase(self.hax_state_with_id()))?;
                let kind = if let hax::FullDefKind::TraitAlias { .. } = trait_def.kind() {
                    // We reuse the same `def_id` to generate a blanket impl for the trait.
                    let impl_id = self.register_item(
                        span,
                        tref.hax_skip_binder_ref(),
                        TransItemSourceKind::TraitImpl(TraitImplSource::TraitAlias),
                    );
                    let mut generics = self.erase_region_binder(trait_decl_ref.clone()).generics;
                    assert!(
                        generics.trait_refs.is_empty(),
                        "found trait alias with non-empty required predicates"
                    );
                    generics.trait_refs = self.translate_trait_impl_exprs(span, &impl_exprs)?;
                    TraitRefKind::TraitImpl(TraitImplRef {
                        id: impl_id,
                        generics,
                    })
                } else if let hax::BuiltinTraitData::Destruct(DestructData::Glue { ty, .. }) =
                    trait_data
                {
                    let (hax::TyKind::Adt(item)
                    | hax::TyKind::Closure(hax::ClosureArgs { item, .. })
                    | hax::TyKind::Array(item)
                    | hax::TyKind::Slice(item)
                    | hax::TyKind::Tuple(item)) = ty.kind()
                    else {
                        raise_error!(self, span, "failed to translate drop glue for type {ty:?}")
                    };
                    TraitRefKind::TraitImpl(self.translate_trait_impl_ref(
                        span,
                        item,
                        TraitImplSource::ImplicitDestruct,
                    )?)
                } else {
                    let Some(builtin_data) = self.recognize_builtin_impl(trait_data, &trait_def)
                    else {
                        raise_error!(
                            self,
                            span,
                            "found a built-in trait impl we did not recognize: \
                            {:?} (lang_item={:?})",
                            trait_def.def_id(),
                            trait_def.lang_item,
                        )
                    };
                    // TODO: here, if closure_ty is a FnDef, we need to generate the matching trait
                    // impls, with an empty state as the first argument.
                    if let Some(closure_kind) = builtin_data.as_closure_kind()
                        && let Some(hax::GenericArg::Type(closure_ty)) = impl_source
                            .r#trait
                            .hax_skip_binder_ref()
                            .generic_args
                            .first()
                        && let hax::TyKind::Closure(closure_args) = closure_ty.kind()
                    {
                        let binder = self.translate_region_binder(
                            span,
                            &impl_source.r#trait,
                            |ctx, _tref| {
                                ctx.translate_closure_impl_ref(span, closure_args, closure_kind)
                            },
                        )?;
                        TraitRefKind::TraitImpl(self.erase_region_binder(binder))
                    } else {
                        let parent_trait_refs =
                            self.translate_trait_impl_exprs(span, &impl_exprs)?;
                        let types = if self.monomorphize() {
                            vec![]
                        } else {
                            types
                                .iter()
                                .map(|(def_id, ty, impl_exprs)| -> Result<_, Error> {
                                    let name = self.t_ctx.translate_trait_item_name(def_id)?;
                                    let assoc_ty = TraitAssocTyImpl {
                                        value: self.translate_ty(span, ty)?,
                                        implied_trait_refs: self
                                            .translate_trait_impl_exprs(span, impl_exprs)?,
                                    };
                                    Ok((name, assoc_ty))
                                })
                                .try_collect()?
                        };
                        TraitRefKind::BuiltinOrAuto {
                            builtin_data,
                            parent_trait_refs,
                            types,
                        }
                    }
                };
                TraitRef::new(kind, trait_decl_ref)
            }
            ImplExprAtom::Error(msg) => {
                let trait_ref = TraitRef::new(TraitRefKind::Unknown(msg.clone()), trait_decl_ref);
                if self.error_on_impl_expr_error {
                    register_error!(self, span, "Error during trait resolution: {}", msg);
                }
                trait_ref
            }
        };
        Ok(trait_ref)
    }
}
