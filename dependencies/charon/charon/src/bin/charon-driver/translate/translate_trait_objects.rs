use super::{
    translate_crate::TransItemSourceKind, translate_ctx::*, translate_generics::BindingLevel,
    translate_predicates::PredicateLocation,
};

use charon_lib::ids::Vector;
use charon_lib::ullbc_ast::*;
use hax_frontend_exporter as hax;
use itertools::Itertools;

fn dummy_public_attr_info() -> AttrInfo {
    AttrInfo {
        public: true,
        ..Default::default()
    }
}

fn usize_ty() -> Ty {
    Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize)))
}

/// Takes a `T` valid in the context of a trait ref and transforms it into a `T` valid in the
/// context of its vtable definition, i.e. no longer mentions `Self`. If `new_self` is `Some`, we
/// replace any mention of `Self` with it; otherwise we panic if `Self` is mentioned.
fn dynify<T: TyVisitable>(mut x: T, new_self: Option<Ty>) -> T {
    struct ReplaceSelfVisitor(Option<Ty>);
    impl VarsVisitor for ReplaceSelfVisitor {
        fn visit_type_var(&mut self, v: TypeDbVar) -> Option<Ty> {
            if let DeBruijnVar::Bound(DeBruijnId::ZERO, type_id) = v {
                // Replace type 0 and decrement the others.
                Some(if let Some(new_id) = type_id.index().checked_sub(1) {
                    TyKind::TypeVar(DeBruijnVar::Bound(DeBruijnId::ZERO, TypeVarId::new(new_id)))
                        .into_ty()
                } else {
                    self.0.clone().expect(
                        "Found unexpected `Self` 
                        type when constructing vtable",
                    )
                })
            } else {
                None
            }
        }
    }
    x.visit_vars(&mut ReplaceSelfVisitor(new_self));
    x
}

//// Translate the `dyn Trait` type.
impl ItemTransCtx<'_, '_> {
    pub fn check_at_most_one_pred_has_methods(
        &mut self,
        span: Span,
        preds: &hax::GenericPredicates,
    ) -> Result<(), Error> {
        // Only the first clause is allowed to have methods.
        for (clause, _) in preds.predicates.iter().skip(1) {
            if let hax::ClauseKind::Trait(trait_predicate) = clause.kind.hax_skip_binder_ref() {
                let trait_def_id = &trait_predicate.trait_ref.def_id;
                let trait_def = self.poly_hax_def(trait_def_id)?;
                let has_methods = match trait_def.kind() {
                    hax::FullDefKind::Trait { items, .. } => items
                        .iter()
                        .any(|assoc| matches!(assoc.kind, hax::AssocKind::Fn { .. })),
                    hax::FullDefKind::TraitAlias { .. } => false,
                    _ => unreachable!(),
                };
                if has_methods {
                    raise_error!(
                        self,
                        span,
                        "`dyn Trait` with multiple method-bearing predicates is not supported"
                    );
                }
            }
        }
        Ok(())
    }

    pub fn translate_existential_predicates(
        &mut self,
        span: Span,
        self_ty: &hax::ParamTy,
        preds: &hax::GenericPredicates,
        region: &hax::Region,
    ) -> Result<DynPredicate, Error> {
        // This is a robustness check: the current version of Rustc
        // accepts at most one method-bearing predicate in a trait object.
        // But things may change in the future.
        self.check_at_most_one_pred_has_methods(span, preds)?;

        // Translate the region outside the binder.
        let region = self.translate_region(span, region)?;
        let region = region.move_under_binder();

        // Add a binder that contains the existentially quantified type.
        self.binding_levels.push(BindingLevel::new(true));

        // Add the existentially quantified type.
        let ty_id = self
            .innermost_binder_mut()
            .push_type_var(self_ty.index, self_ty.name.clone());
        let ty = TyKind::TypeVar(DeBruijnVar::new_at_zero(ty_id)).into_ty();

        self.innermost_binder_mut()
            .params
            .types_outlive
            .push(RegionBinder::empty(OutlivesPred(ty.clone(), region)));
        self.register_predicates(preds, PredicateOrigin::Dyn, &PredicateLocation::Base)?;

        let params = self.binding_levels.pop().unwrap().params;
        let binder = Binder {
            params: params,
            skip_binder: ty,
            kind: BinderKind::Dyn,
        };
        Ok(DynPredicate { binder })
    }
}

//// Generate the vtable struct.
impl ItemTransCtx<'_, '_> {
    /// Query whether a trait is dyn compatible.
    /// TODO(dyn): for now we return `false` if the trait has any associated types, as we don't
    /// handle associated types in vtables.
    pub fn trait_is_dyn_compatible(&mut self, def_id: &hax::DefId) -> Result<bool, Error> {
        let def = self.poly_hax_def(def_id)?;
        Ok(match def.kind() {
            hax::FullDefKind::Trait {
                dyn_self: Some(dyn_self),
                ..
            }
            | hax::FullDefKind::TraitAlias {
                dyn_self: Some(dyn_self),
                ..
            } => {
                match dyn_self.kind() {
                    // `dyn_self` looks like `dyn Trait<Args.., Ty0 = .., Ty1 = ..>`. The first
                    // predicate is `_: Trait<Args..>`, the rest are type constraints. Hence the
                    // trait recursively has no assoc types iff `preds.len() == 1`.
                    hax::TyKind::Dynamic(_, preds, _) => preds.predicates.len() == 1,
                    _ => panic!("unexpected `dyn_self`: {dyn_self:?}"),
                }
            }
            _ => false,
        })
    }

    /// Check whether this trait ref is of the form `Self: Trait<...>`.
    fn pred_is_for_self(&self, tref: &hax::TraitRef) -> bool {
        let first_ty = tref
            .generic_args
            .iter()
            .filter_map(|arg| match arg {
                hax::GenericArg::Type(ty) => Some(ty),
                _ => None,
            })
            .next();
        match first_ty {
            None => false,
            Some(first_ty) => match first_ty.kind() {
                hax::TyKind::Param(param_ty) if param_ty.index == 0 => {
                    assert_eq!(param_ty.name, "Self");
                    true
                }
                _ => false,
            },
        }
    }

    /// Given a trait ref, return a reference to its vtable struct, if it is dyn compatible.
    pub fn translate_vtable_struct_ref(
        &mut self,
        span: Span,
        tref: &hax::TraitRef,
    ) -> Result<Option<TypeDeclRef>, Error> {
        if !self.trait_is_dyn_compatible(&tref.def_id)? {
            return Ok(None);
        }
        // Don't enqueue the vtable for translation by default. It will be enqueued if used in a
        // `dyn Trait`.
        let mut vtable_ref: TypeDeclRef =
            self.translate_item_no_enqueue(span, tref, TransItemSourceKind::VTable)?;
        // Remove the `Self` type variable from the generic parameters.
        vtable_ref
            .generics
            .types
            .remove_and_shift_ids(TypeVarId::ZERO);
        Ok(Some(vtable_ref))
    }

    /// Add a `method_name: fn(...) -> ...` field for the method.
    fn add_method_to_vtable_def(
        &mut self,
        span: Span,
        trait_def: &hax::FullDef,
        mut mk_field: impl FnMut(String, Ty),
        item: &hax::AssocItem,
    ) -> Result<(), Error> {
        let item_def_id = &item.def_id;
        let item_def = self.hax_def(
            &trait_def
                .this()
                .with_def_id(&self.t_ctx.hax_state, item_def_id),
        )?;
        let hax::FullDefKind::AssocFn {
            sig,
            vtable_safe: true,
            ..
        } = item_def.kind()
        else {
            return Ok(());
        };

        let item_name = self.t_ctx.translate_trait_item_name(item_def_id)?;
        // It's ok to translate the method signature in the context of the trait because
        // `vtable_safe: true` ensures the method has no generics of its own.
        let sig = self.translate_fun_sig(span, sig)?;
        let ty = TyKind::FnPtr(sig).into_ty();

        mk_field(format!("method_{}", item_name.0), ty);
        Ok(())
    }

    /// Add `super_trait_n: &'static SuperTraitNVTable` fields.
    fn add_supertraits_to_vtable_def(
        &mut self,
        span: Span,
        mut mk_field: impl FnMut(String, Ty),
        implied_predicates: &hax::GenericPredicates,
    ) -> Result<(), Error> {
        let mut counter = (0..).into_iter();
        for (clause, _span) in &implied_predicates.predicates {
            if let hax::ClauseKind::Trait(pred) = clause.kind.hax_skip_binder_ref() {
                // If a clause looks like `Self: OtherTrait<...>`, we consider it a supertrait.
                if !self.pred_is_for_self(&pred.trait_ref) {
                    continue;
                }
                let vtbl_struct = self
                    .translate_region_binder(span, &clause.kind, |ctx, _| {
                        ctx.translate_vtable_struct_ref(span, &pred.trait_ref)
                    })?
                    .erase()
                    .expect("parent trait should be dyn compatible");
                let ty = Ty::new(TyKind::Ref(
                    Region::Static,
                    Ty::new(TyKind::Adt(vtbl_struct)),
                    RefKind::Shared,
                ));
                mk_field(format!("super_trait_{}", counter.next().unwrap()), ty);
            }
        }
        Ok(())
    }

    fn gen_vtable_struct_fields(
        &mut self,
        span: Span,
        trait_def: &hax::FullDef,
        implied_predicates: &hax::GenericPredicates,
    ) -> Result<Vector<FieldId, Field>, Error> {
        let mut fields = Vector::new();
        let mut mk_field = |name, ty| {
            fields.push(Field {
                span,
                attr_info: dummy_public_attr_info(),
                name: Some(name),
                ty,
            });
        };

        // Add the basic fields.
        // Field: `size: usize`
        mk_field("size".into(), usize_ty());
        // Field: `align: usize`
        mk_field("align".into(), usize_ty());
        // Field: `drop: fn(*mut Self)`
        mk_field("drop".into(), {
            let self_ty = TyKind::TypeVar(DeBruijnVar::new_at_zero(TypeVarId::ZERO)).into_ty();
            let self_ptr = TyKind::RawPtr(self_ty, RefKind::Mut).into_ty();
            Ty::new(TyKind::FnPtr(RegionBinder::empty((
                [self_ptr].into(),
                Ty::mk_unit(),
            ))))
        });

        // Add the method pointers (trait aliases don't have methods).
        if let hax::FullDefKind::Trait { items, .. } = trait_def.kind() {
            for item in items {
                self.add_method_to_vtable_def(span, trait_def, &mut mk_field, item)?;
            }
        }

        // Add the supertrait vtables.
        self.add_supertraits_to_vtable_def(span, &mut mk_field, implied_predicates)?;

        Ok(fields)
    }

    /// Construct the type of the vtable for this trait.
    ///
    /// It's a struct that has for generics the generics of the trait + one parameter for each
    /// associated type of the trait and its parents.
    /// TODO(dyn): add the associated types.
    ///
    /// struct TraitVTable<TraitArgs.., AssocTys..> {
    ///   size: usize,
    ///   align: usize,
    ///   drop: fn(*mut dyn Trait<...>),
    ///   method_name: fn(&dyn Trait<...>, Args..) -> Output,
    ///   ... other methods
    ///   super_trait_0: &'static SuperTrait0VTable
    ///   ... other supertraits
    /// }
    pub(crate) fn translate_vtable_struct(
        mut self,
        type_id: TypeDeclId,
        item_meta: ItemMeta,
        trait_def: &hax::FullDef,
    ) -> Result<TypeDecl, Error> {
        let span = item_meta.span;
        if !self.trait_is_dyn_compatible(trait_def.def_id())? {
            raise_error!(
                self,
                span,
                "Trying to compute the vtable type \
                for a non-dyn-compatible trait"
            );
        }

        self.translate_def_generics(span, trait_def)?;
        // TODO(dyn): add the associated types.

        let (hax::FullDefKind::Trait {
            dyn_self,
            implied_predicates,
            ..
        }
        | hax::FullDefKind::TraitAlias {
            dyn_self,
            implied_predicates,
            ..
        }) = trait_def.kind()
        else {
            panic!()
        };
        let Some(dyn_self) = dyn_self else {
            panic!("Trying to generate a vtable for a non-dyn-compatible trait")
        };

        // The `dyn Trait<Args..>` type for this trait.
        let mut dyn_self = self.translate_ty(span, dyn_self)?;
        // First construct fields that use the real method signatures (which may use the `Self`
        // type). We fixup the types and generics below.
        let fields = self.gen_vtable_struct_fields(span, trait_def, implied_predicates)?;
        let mut kind = TypeDeclKind::Struct(fields);
        let layout = self.generate_naive_layout(span, &kind)?;

        // Replace any use of `Self` with `dyn Trait<...>`, and remove the `Self` type variable
        // from the generic parameters.
        let mut generics = self.into_generics();
        {
            dyn_self = dynify(dyn_self, None);
            generics = dynify(generics, Some(dyn_self.clone()));
            kind = dynify(kind, Some(dyn_self.clone()));
            generics.types.remove_and_shift_ids(TypeVarId::ZERO);
            generics.types.iter_mut().for_each(|ty| {
                ty.index -= 1;
            });
        }

        let dyn_predicate = dyn_self
            .kind()
            .as_dyn_trait()
            .expect("incorrect `dyn_self`");
        Ok(TypeDecl {
            def_id: type_id,
            item_meta: item_meta,
            generics: generics,
            src: ItemKind::VTableTy {
                dyn_predicate: dyn_predicate.clone(),
            },
            kind,
            layout: Some(layout),
            ptr_metadata: None,
        })
    }
}

//// Generate a vtable value.
impl ItemTransCtx<'_, '_> {
    pub fn translate_vtable_instance_ref(
        &mut self,
        span: Span,
        trait_ref: &hax::TraitRef,
        impl_ref: &hax::ItemRef,
    ) -> Result<Option<GlobalDeclRef>, Error> {
        if !self.trait_is_dyn_compatible(&trait_ref.def_id)? {
            return Ok(None);
        }
        // Don't enqueue the vtable for translation by default. It will be enqueued if used in a
        // `dyn Trait` coercion.
        // TODO(dyn): To do this properly we'd need to know for each clause whether it ultimately
        // ends up used in a vtable cast.
        let vtable_ref: GlobalDeclRef = self.translate_item_no_enqueue(
            span,
            impl_ref,
            TransItemSourceKind::VTableInstance(TraitImplSource::Normal),
        )?;
        Ok(Some(vtable_ref))
    }

    /// Local helper function to get the vtable struct reference and trait declaration reference
    fn get_vtable_instance_info<'a>(
        &mut self,
        span: Span,
        impl_def: &'a hax::FullDef,
        impl_kind: &TraitImplSource,
    ) -> Result<(TraitImplRef, TraitDeclRef, TypeDeclRef), Error> {
        let implemented_trait = match impl_def.kind() {
            hax::FullDefKind::TraitImpl { trait_pred, .. } => &trait_pred.trait_ref,
            _ => unreachable!(),
        };
        let vtable_struct_ref = self
            .translate_vtable_struct_ref(span, implemented_trait)?
            .expect("trait should be dyn-compatible");
        let implemented_trait = self.translate_trait_decl_ref(span, implemented_trait)?;
        let impl_ref = self.translate_item(
            span,
            impl_def.this(),
            TransItemSourceKind::TraitImpl(*impl_kind),
        )?;
        Ok((impl_ref, implemented_trait, vtable_struct_ref))
    }

    /// E.g.,
    /// global <T..., VT...>
    ///     trait::{vtable_instance}::<ImplTy<T...>> :
    ///         trait::{vtable}<VT...> = trait::{vtable}<VT...> {
    ///     drop: &ignore / &<ImplTy<T...> as Drop>::drop,
    ///     size: size_of(<ImplTy<T...>>),
    ///     align: align_of(<ImplTy<T...>>),
    ///     method_0: &<ImplTy<T...> as Trait>::method_0::{shim},
    ///     method_1: &<ImplTy<T...> as Trait>::method_1::{shim},
    ///     ...
    ///     super_trait_0: &SuperTrait0<VT...>::{vtable_instance}::<ImplTy<T...>>,
    ///     super_trait_1: &SuperTrait1<VT...>::{vtable_instance}::<ImplTy<T...>>,
    ///     ...
    /// }
    pub(crate) fn translate_vtable_instance(
        mut self,
        global_id: GlobalDeclId,
        item_meta: ItemMeta,
        impl_def: &hax::FullDef,
        impl_kind: &TraitImplSource,
    ) -> Result<GlobalDecl, Error> {
        let span = item_meta.span;
        self.translate_def_generics(span, impl_def)?;

        let (impl_ref, _, vtable_struct_ref) =
            self.get_vtable_instance_info(span, impl_def, impl_kind)?;
        // Initializer function for this global.
        let init = self.register_item(
            span,
            impl_def.this(),
            TransItemSourceKind::VTableInstanceInitializer(*impl_kind),
        );

        Ok(GlobalDecl {
            def_id: global_id,
            item_meta,
            generics: self.into_generics(),
            kind: ItemKind::VTableInstance { impl_ref },
            // it should be static to have its own address
            global_kind: GlobalKind::Static,
            ty: Ty::new(TyKind::Adt(vtable_struct_ref)),
            init,
        })
    }

    fn add_method_to_vtable_value(
        &mut self,
        span: Span,
        impl_def: &hax::FullDef,
        item: &hax::ImplAssocItem,
        mut mk_field: impl FnMut(RawConstantExpr),
    ) -> Result<(), Error> {
        // Exit if the item isn't a vtable safe method.
        match self.poly_hax_def(&item.decl_def_id)?.kind() {
            hax::FullDefKind::AssocFn {
                vtable_safe: true, ..
            } => {}
            _ => return Ok(()),
        }

        let const_kind = match &item.value {
            hax::ImplAssocItemValue::Provided {
                def_id: item_def_id,
                ..
            } => {
                // The method is vtable safe so it has no generics, hence we can reuse the impl
                // generics.
                let item_ref = impl_def.this().with_def_id(self.hax_state(), item_def_id);
                let shim_ref =
                    self.translate_item(span, &item_ref, TransItemSourceKind::VTableMethod)?;
                RawConstantExpr::FnPtr(shim_ref)
            }
            hax::ImplAssocItemValue::DefaultedFn { .. } => RawConstantExpr::Opaque(
                "shim for provided methods \
                    aren't yet supported"
                    .to_string(),
            ),
            _ => return Ok(()),
        };

        mk_field(const_kind);

        Ok(())
    }

    fn add_supertraits_to_vtable_value(
        &mut self,
        span: Span,
        trait_def: &hax::FullDef,
        impl_def: &hax::FullDef,
        mut mk_field: impl FnMut(RawConstantExpr),
    ) -> Result<(), Error> {
        let hax::FullDefKind::TraitImpl {
            implied_impl_exprs, ..
        } = impl_def.kind()
        else {
            unreachable!()
        };
        let hax::FullDefKind::Trait {
            implied_predicates, ..
        } = trait_def.kind()
        else {
            unreachable!()
        };
        for ((clause, _), impl_expr) in implied_predicates.predicates.iter().zip(implied_impl_exprs)
        {
            if let hax::ClauseKind::Trait(pred) = clause.kind.hax_skip_binder_ref() {
                // If a clause looks like `Self: OtherTrait<...>`, we consider it a supertrait.
                if !self.pred_is_for_self(&pred.trait_ref) {
                    continue;
                }
            }

            let vtable_def_ref = self
                .translate_region_binder(span, &impl_expr.r#trait, |ctx, tref| {
                    ctx.translate_vtable_struct_ref(span, tref)
                })?
                .erase()
                .expect("parent trait should be dyn compatible");
            let fn_ptr_ty = TyKind::Adt(vtable_def_ref).into_ty();
            let kind = match &impl_expr.r#impl {
                hax::ImplExprAtom::Concrete(impl_item) => {
                    let vtable_instance_ref = self
                        .translate_region_binder(span, &impl_expr.r#trait, |ctx, tref| {
                            ctx.translate_vtable_instance_ref(span, tref, impl_item)
                        })?
                        .erase()
                        .expect("parent trait should be dyn compatible");
                    let global = Box::new(ConstantExpr {
                        value: RawConstantExpr::Global(vtable_instance_ref),
                        ty: fn_ptr_ty,
                    });
                    RawConstantExpr::Ref(global)
                }
                // TODO(dyn): builtin impls
                _ => RawConstantExpr::Opaque("missing supertrait vtable".into()),
            };
            mk_field(kind);
        }
        Ok(())
    }

    /// Generate the body of the vtable instance function.
    /// This is for `impl Trait for T` implementation, it does NOT handle builtin impls.
    /// ```ignore
    /// let ret@0 : VTable;
    /// ret@0 = VTable { ... };
    /// return;
    /// ```
    fn gen_vtable_instance_init_body(
        &mut self,
        span: Span,
        impl_def: &hax::FullDef,
        vtable_struct_ref: TypeDeclRef,
    ) -> Result<Body, Error> {
        let mut locals = Locals {
            arg_count: 0,
            locals: Vector::new(),
        };
        let ret_ty = Ty::new(TyKind::Adt(vtable_struct_ref.clone()));
        let ret_place = locals.new_var(Some("ret".into()), ret_ty.clone());

        let hax::FullDefKind::TraitImpl {
            trait_pred, items, ..
        } = impl_def.kind()
        else {
            unreachable!()
        };
        let trait_def = self.hax_def(&trait_pred.trait_ref)?;

        // Retreive the expected field types from the struct definition. This avoids complicated
        // substitutions.
        let field_tys = {
            let vtable_decl_id = vtable_struct_ref.id.as_adt().unwrap().clone();
            let AnyTransItem::Type(vtable_def) =
                self.t_ctx.get_or_translate(vtable_decl_id.into())?
            else {
                unreachable!()
            };
            let TypeDeclKind::Struct(fields) = &vtable_def.kind else {
                unreachable!()
            };
            fields
                .iter()
                .map(|f| &f.ty)
                .cloned()
                .map(|ty| ty.substitute(&vtable_struct_ref.generics))
                .collect_vec()
        };

        let mut statements = vec![];
        let mut aggregate_fields = vec![];
        // For each vtable field, assign the desired value to a new local.
        let mut field_ty_iter = field_tys.into_iter();
        let mut mk_field = |kind| {
            let ty = field_ty_iter.next().unwrap();
            aggregate_fields.push(Operand::Const(Box::new(ConstantExpr { value: kind, ty })));
        };

        // TODO(dyn): provide values
        mk_field(RawConstantExpr::Opaque("unknown size".to_string()));
        mk_field(RawConstantExpr::Opaque("unknown align".to_string()));
        mk_field(RawConstantExpr::Opaque("unknown drop".to_string()));

        for item in items {
            self.add_method_to_vtable_value(span, impl_def, item, &mut mk_field)?;
        }

        self.add_supertraits_to_vtable_value(span, &trait_def, impl_def, &mut mk_field)?;

        if field_ty_iter.next().is_some() {
            raise_error!(
                self,
                span,
                "Missed some fields in vtable value construction"
            )
        }

        // Construct the final struct.
        statements.push(Statement::new(
            span,
            RawStatement::Assign(
                ret_place,
                Rvalue::Aggregate(
                    AggregateKind::Adt(vtable_struct_ref.clone(), None, None),
                    aggregate_fields,
                ),
            ),
        ));

        let block = BlockData {
            statements,
            terminator: Terminator::new(span, RawTerminator::Return),
        };

        Ok(Body::Unstructured(GExprBody {
            span,
            locals,
            comments: Vec::new(),
            body: [block].into(),
        }))
    }

    pub(crate) fn translate_vtable_instance_init(
        mut self,
        init_func_id: FunDeclId,
        item_meta: ItemMeta,
        impl_def: &hax::FullDef,
        impl_kind: &TraitImplSource,
    ) -> Result<FunDecl, Error> {
        let span = item_meta.span;
        self.translate_def_generics(span, impl_def)?;

        let (impl_ref, _, vtable_struct_ref) =
            self.get_vtable_instance_info(span, impl_def, impl_kind)?;
        let init_for = self.register_item(
            span,
            impl_def.this(),
            TransItemSourceKind::VTableInstance(*impl_kind),
        );

        // Signature: `() -> VTable`.
        let sig = FunSig {
            is_unsafe: false,
            generics: self.the_only_binder().params.clone(),
            inputs: vec![],
            output: Ty::new(TyKind::Adt(vtable_struct_ref.clone())),
        };

        let body = match impl_kind {
            TraitImplSource::Normal => {
                let body = self.gen_vtable_instance_init_body(span, impl_def, vtable_struct_ref)?;
                Ok(body)
            }
            _ => {
                raise_error!(
                    self,
                    span,
                    "Don't know how to generate a vtable for a virtual impl {impl_kind:?}"
                );
            }
        };

        Ok(FunDecl {
            def_id: init_func_id,
            item_meta: item_meta,
            signature: sig,
            kind: ItemKind::VTableInstance { impl_ref },
            is_global_initializer: Some(init_for),
            body,
        })
    }

    // pub(crate) fn translate_vtable_shim(
    //     self,
    //     _fun_id: FunDeclId,
    //     item_meta: ItemMeta,
    //     _impl_func_def: &hax::FullDef,
    // ) -> Result<FunDecl, Error> {
    //     let span = item_meta.span;
    //     raise_error!(self, span, "unimplemented")
    // }
}
