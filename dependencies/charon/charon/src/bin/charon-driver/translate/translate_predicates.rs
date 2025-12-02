use super::translate_ctx::*;
use charon_lib::ast::*;
use charon_lib::formatter::IntoFormatter;
use charon_lib::ids::Vector;
use charon_lib::pretty::FmtWithCtx;
use hax_frontend_exporter as hax;

/// The context in which we are translating a clause, used to generate the appropriate ids and
/// trait references.
pub(crate) enum PredicateLocation {
    /// We're translating the parent clauses of this trait.
    Parent,
    /// We're translating the item clauses of this trait.
    Item(TraitItemName),
    /// We're translating anything else.
    Base,
}

impl<'tcx, 'ctx> ItemTransCtx<'tcx, 'ctx> {
    /// This function should be called **after** we translated the generics (type parameters,
    /// regions...).
    pub(crate) fn register_predicates(
        &mut self,
        preds: &hax::GenericPredicates,
        origin: PredicateOrigin,
        location: &PredicateLocation,
    ) -> Result<(), Error> {
        // Translate the trait predicates first, because associated type constraints may refer to
        // them. E.g. in `fn foo<I: Iterator<Item=usize>>()`, the `I: Iterator` clause must be
        // translated before the `<I as Iterator>::Item = usize` predicate.
        for (clause, span) in &preds.predicates {
            if matches!(clause.kind.value, hax::ClauseKind::Trait(_)) {
                self.register_predicate(clause, span, origin.clone(), location)?;
            }
        }
        for (clause, span) in &preds.predicates {
            if !matches!(clause.kind.value, hax::ClauseKind::Trait(_)) {
                self.register_predicate(clause, span, origin.clone(), location)?;
            }
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

    pub(crate) fn register_predicate(
        &mut self,
        clause: &hax::Clause,
        hspan: &hax::Span,
        origin: PredicateOrigin,
        location: &PredicateLocation,
    ) -> Result<(), Error> {
        use hax::ClauseKind;
        trace!("{:?}", clause);
        let span = self.translate_span_from_hax(hspan);
        match clause.kind.hax_skip_binder_ref() {
            ClauseKind::Trait(trait_pred) => {
                let pred = self.translate_region_binder(span, &clause.kind, |ctx, _| {
                    ctx.translate_trait_predicate(span, trait_pred)
                })?;
                let location = match location {
                    PredicateLocation::Base => &mut self.innermost_generics_mut().trait_clauses,
                    PredicateLocation::Parent => &mut self.parent_trait_clauses,
                    PredicateLocation::Item(item_name) => self
                        .item_trait_clauses
                        .entry(item_name.clone())
                        .or_default(),
                };
                location.push_with(|clause_id| TraitClause {
                    clause_id,
                    origin,
                    span: Some(span),
                    trait_: pred,
                });
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
                // I don't really understand that one. Why don't they put
                // the type information in the const generic parameters
                // directly? For now we just ignore it.
            }
            ClauseKind::WellFormed(_) => {
                raise_error!(self, span, "Well-formedness clauses are unsupported")
            }
            kind => {
                raise_error!(self, span, "Unsupported clause: {:?}", kind)
            }
        }
        Ok(())
    }

    pub(crate) fn translate_trait_impl_exprs(
        &mut self,
        span: Span,
        impl_sources: &[hax::ImplExpr],
    ) -> Result<Vector<TraitClauseId, TraitRef>, Error> {
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
                Ok(TraitRef {
                    kind: TraitRefKind::Unknown(err.msg),
                    trait_decl_ref,
                })
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
        use hax::DropData;
        use hax::ImplExprAtom;

        let trait_ref = match &impl_source.r#impl {
            ImplExprAtom::Concrete(item) => {
                let impl_ref =
                    self.translate_trait_impl_ref(span, item, TraitImplSource::Normal)?;
                TraitRef {
                    kind: TraitRefKind::TraitImpl(impl_ref),
                    trait_decl_ref,
                }
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
                // If we are refering to a trait clause, we need to find the relevant one.
                let mut tref_kind = match &impl_source.r#impl {
                    ImplExprAtom::SelfImpl { .. } => TraitRefKind::SelfId,
                    ImplExprAtom::LocalBound { index, .. } => {
                        let var = self.lookup_clause_var(span, *index)?;
                        TraitRefKind::Clause(var)
                    }
                    _ => unreachable!(),
                };

                let mut current_pred = self.translate_poly_trait_ref(span, trait_ref)?;

                // Apply the path
                for path_elem in path {
                    use hax::ImplExprPathChunk::*;
                    let trait_ref = Box::new(TraitRef {
                        kind: tref_kind,
                        trait_decl_ref: current_pred,
                    });
                    match path_elem {
                        AssocItem {
                            item,
                            predicate,
                            index,
                            ..
                        } => {
                            let name = self.t_ctx.translate_trait_item_name(&item.def_id)?;
                            if !item.generic_args.is_empty() {
                                raise_error!(
                                    self,
                                    span,
                                    "Found unsupported GAT `{}` when resolving trait `{}`",
                                    name,
                                    trait_decl_ref.with_ctx(&self.into_fmt())
                                )
                            }
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

                TraitRef {
                    kind: tref_kind,
                    trait_decl_ref,
                }
            }
            ImplExprAtom::Dyn => TraitRef {
                kind: TraitRefKind::Dyn(trait_decl_ref.clone()),
                trait_decl_ref,
            },
            ImplExprAtom::Builtin {
                trait_data,
                impl_exprs,
                types,
                ..
            } => {
                let tref = &impl_source.r#trait;
                let trait_def =
                    self.hax_def(&tref.hax_skip_binder_ref().erase(&self.hax_state_with_id()))?;
                let closure_kind = trait_def.lang_item.as_deref().and_then(|lang| match lang {
                    "fn_once" => Some(ClosureKind::FnOnce),
                    "fn_mut" => Some(ClosureKind::FnMut),
                    "r#fn" => Some(ClosureKind::Fn),
                    _ => None,
                });

                // TODO: here, if closure_ty is a FnDef, we need to generate the matching
                // trait impls, with an empty state as the first argument.
                let kind = if let Some(closure_kind) = closure_kind
                    && let Some(hax::GenericArg::Type(closure_ty)) = impl_source
                        .r#trait
                        .hax_skip_binder_ref()
                        .generic_args
                        .first()
                    && let hax::TyKind::Closure(closure_args) = closure_ty.kind()
                {
                    let binder =
                        self.translate_region_binder(span, &impl_source.r#trait, |ctx, _tref| {
                            ctx.translate_closure_impl_ref(span, closure_args, closure_kind)
                        })?;
                    TraitRefKind::TraitImpl(binder.erase())
                } else if let hax::FullDefKind::TraitAlias { .. } = trait_def.kind() {
                    // We reuse the same `def_id` to generate a blanket impl for the trait.
                    let impl_id = self.register_item(
                        span,
                        tref.hax_skip_binder_ref(),
                        TransItemSourceKind::TraitImpl(TraitImplSource::TraitAlias),
                    );
                    let mut generics = trait_decl_ref.clone().erase().generics;
                    assert!(
                        generics.trait_refs.is_empty(),
                        "found trait alias with non-empty required predicates"
                    );
                    generics.trait_refs = self.translate_trait_impl_exprs(span, &impl_exprs)?;
                    TraitRefKind::TraitImpl(TraitImplRef {
                        id: impl_id,
                        generics,
                    })
                } else if let hax::BuiltinTraitData::Drop(DropData::Glue { ty, .. }) = trait_data
                    && let hax::TyKind::Adt(item) = ty.kind()
                {
                    let impl_ref =
                        self.translate_trait_impl_ref(span, item, TraitImplSource::DropGlue)?;
                    TraitRefKind::TraitImpl(impl_ref)
                } else {
                    let parent_trait_refs = self.translate_trait_impl_exprs(span, &impl_exprs)?;
                    let types = types
                        .iter()
                        .map(|(def_id, ty, impl_exprs)| {
                            let name = self.t_ctx.translate_trait_item_name(def_id)?;
                            let ty = self.translate_ty(span, ty)?;
                            let trait_refs = self.translate_trait_impl_exprs(span, impl_exprs)?;
                            Ok((name, ty, trait_refs))
                        })
                        .try_collect()?;
                    TraitRefKind::BuiltinOrAuto {
                        trait_decl_ref: trait_decl_ref.clone(),
                        parent_trait_refs,
                        types,
                    }
                };
                TraitRef {
                    kind,
                    trait_decl_ref,
                }
            }
            ImplExprAtom::Error(msg) => {
                let trait_ref = TraitRef {
                    kind: TraitRefKind::Unknown(msg.clone()),
                    trait_decl_ref,
                };
                if self.error_on_impl_expr_error {
                    register_error!(self, span, "Error during trait resolution: {}", msg);
                }
                trait_ref
            }
        };
        Ok(trait_ref)
    }
}
