use itertools::Itertools;
use rustc_middle::ty;
use rustc_span::sym;

use super::translate_ctx::*;
use charon_lib::ast::*;
use charon_lib::ids::{IndexMap, IndexVec};
use hax::{HasOwner, HasParamEnv, Visibility};

impl<'tcx, 'ctx> ItemTransCtx<'tcx, 'ctx> {
    /// Translate an erased region. If we're inside a body, this will return a fresh body region
    /// instead.
    pub(crate) fn translate_erased_region(&mut self) -> Region {
        if let Some(v) = &mut self.lifetime_freshener {
            Region::Body(v.push(()))
        } else {
            Region::Erased
        }
    }

    /// Erase a region binder by supplying erased lifetimes (or fresh body lifetimes) for all its
    /// arguments.
    pub(crate) fn erase_region_binder<T: TyVisitable>(&mut self, b: RegionBinder<T>) -> T {
        let regions = b
            .regions
            .map_ref_indexed(|_, _| self.translate_erased_region());
        b.apply(regions)
    }

    // Translate a region
    pub(crate) fn translate_region(
        &mut self,
        span: Span,
        region: &hax::Region,
    ) -> Result<Region, Error> {
        use hax::RegionKind::*;
        match &region.kind {
            ReErased => Ok(self.translate_erased_region()),
            ReStatic => Ok(Region::Static),
            ReBound(hax::BoundVarIndexKind::Bound(id), br) => {
                Ok(match self.lookup_bound_region(span, *id, br.var) {
                    Ok(var) => Region::Var(var),
                    Err(_) => Region::Erased,
                })
            }
            ReEarlyParam(region) => Ok(match self.lookup_early_region(span, region) {
                Ok(var) => Region::Var(var),
                Err(_) => Region::Erased,
            }),
            ReVar(..) | RePlaceholder(..) => {
                // Shouldn't exist outside of type inference.
                raise_error!(
                    self,
                    span,
                    "Should not exist outside of type inference: {region:?}"
                )
            }
            ReBound(..) | ReLateParam(..) | ReError(..) => {
                raise_error!(self, span, "Unexpected region kind: {region:?}")
            }
        }
    }

    pub(crate) fn translate_hax_int_ty(int_ty: &hax::IntTy) -> IntTy {
        match int_ty {
            hax::IntTy::Isize => IntTy::Isize,
            hax::IntTy::I8 => IntTy::I8,
            hax::IntTy::I16 => IntTy::I16,
            hax::IntTy::I32 => IntTy::I32,
            hax::IntTy::I64 => IntTy::I64,
            hax::IntTy::I128 => IntTy::I128,
        }
    }

    pub(crate) fn translate_hax_uint_ty(uint_ty: &hax::UintTy) -> UIntTy {
        use hax::UintTy;
        match uint_ty {
            UintTy::Usize => UIntTy::Usize,
            UintTy::U8 => UIntTy::U8,
            UintTy::U16 => UIntTy::U16,
            UintTy::U32 => UIntTy::U32,
            UintTy::U64 => UIntTy::U64,
            UintTy::U128 => UIntTy::U128,
        }
    }

    /// Translate a Ty.
    ///
    /// Typically used in this module to translate the fields of a structure/
    /// enumeration definition, or later to translate the type of a variable.
    ///
    /// Note that we take as parameter a function to translate regions, because
    /// regions can be translated in several manners (non-erased region or erased
    /// regions), in which case the return type is different.
    #[tracing::instrument(skip(self, span))]
    pub(crate) fn translate_ty(&mut self, span: Span, hax_ty: &hax::Ty) -> Result<Ty, Error> {
        let mut ty = if let Some(ty) = self
            .innermost_binder()
            .type_trans_cache
            .get(&hax_ty)
            .cloned()
        {
            ty
        } else {
            let ty = self
                .translate_ty_inner(span, hax_ty)
                .unwrap_or_else(|e| TyKind::Error(e.msg).into_ty());
            self.innermost_binder_mut()
                .type_trans_cache
                .insert(hax_ty.clone(), ty.clone());
            ty
        };
        if let Some(v) = &mut self.lifetime_freshener {
            // We might be reusing a value from cache: we must refresh the erased & body regions.
            ty = ty.replace_erased_regions(|| Region::Body(v.push(())));
        }
        Ok(ty)
    }

    fn translate_ty_inner(&mut self, span: Span, ty: &hax::Ty) -> Result<Ty, Error> {
        trace!("{:?}", ty);
        let kind = match ty.kind() {
            hax::TyKind::Bool => TyKind::Literal(LiteralTy::Bool),
            hax::TyKind::Char => TyKind::Literal(LiteralTy::Char),
            hax::TyKind::Int(int_ty) => {
                TyKind::Literal(LiteralTy::Int(Self::translate_hax_int_ty(int_ty)))
            }
            hax::TyKind::Uint(uint_ty) => {
                TyKind::Literal(LiteralTy::UInt(Self::translate_hax_uint_ty(uint_ty)))
            }
            hax::TyKind::Float(float_ty) => {
                use hax::FloatTy;
                TyKind::Literal(LiteralTy::Float(match float_ty {
                    FloatTy::F16 => types::FloatTy::F16,
                    FloatTy::F32 => types::FloatTy::F32,
                    FloatTy::F64 => types::FloatTy::F64,
                    FloatTy::F128 => types::FloatTy::F128,
                }))
            }
            hax::TyKind::Never => TyKind::Never,

            hax::TyKind::Alias(alias) => match &alias.kind {
                hax::AliasKind::Projection {
                    impl_expr,
                    assoc_item,
                } => {
                    let trait_ref = self.translate_trait_impl_expr(span, impl_expr)?;
                    let name = self.t_ctx.translate_trait_item_name(&assoc_item.def_id)?;
                    TyKind::TraitType(trait_ref, name)
                }
                hax::AliasKind::Opaque { hidden_ty, .. } => {
                    return self.translate_ty(span, hidden_ty);
                }
                _ => {
                    raise_error!(self, span, "Unsupported alias type: {:?}", alias.kind)
                }
            },

            hax::TyKind::Adt(item) => {
                let tref = self.translate_type_decl_ref(span, item)?;
                TyKind::Adt(tref)
            }
            hax::TyKind::Str => {
                let tref = TypeDeclRef::new(TypeId::Builtin(BuiltinTy::Str), GenericArgs::empty());
                TyKind::Adt(tref)
            }
            hax::TyKind::Array(item_ref) => {
                let mut args = self.translate_generic_args(span, &item_ref.generic_args, &[])?;
                assert!(args.types.elem_count() == 1 && args.const_generics.elem_count() == 1);
                TyKind::Array(
                    args.types.pop().unwrap(),
                    Box::new(args.const_generics.pop().unwrap()),
                )
            }
            hax::TyKind::Slice(item_ref) => {
                let mut args = self.translate_generic_args(span, &item_ref.generic_args, &[])?;
                assert!(args.types.elem_count() == 1);
                TyKind::Slice(args.types.pop().unwrap())
            }
            hax::TyKind::Tuple(item_ref) => {
                let args = self.translate_generic_args(span, &item_ref.generic_args, &[])?;
                let tref = TypeDeclRef::new(TypeId::Tuple, args);
                TyKind::Adt(tref)
            }
            hax::TyKind::Ref(region, ty, mutability) => {
                trace!("Ref");

                let region = self.translate_region(span, region)?;
                let ty = self.translate_ty(span, ty)?;
                let kind = if mutability.is_mut() {
                    RefKind::Mut
                } else {
                    RefKind::Shared
                };
                TyKind::Ref(region, ty, kind)
            }
            hax::TyKind::RawPtr(ty, mutbl) => {
                trace!("RawPtr: {:?}", (ty, mutbl));
                let ty = self.translate_ty(span, ty)?;
                let kind = if mutbl.is_mut() {
                    RefKind::Mut
                } else {
                    RefKind::Shared
                };
                TyKind::RawPtr(ty, kind)
            }

            hax::TyKind::Param(param) => {
                // A type parameter, for example `T` in `fn f<T>(x : T) {}`.
                // Note that this type parameter may actually have been
                // instantiated (in our environment, we may map it to another
                // type): we just have to look it up.
                // Note that if we are using this function to translate a field
                // type in a type definition, it should actually map to a type
                // parameter.
                match self.lookup_type_var(span, param) {
                    Ok(var) => TyKind::TypeVar(var),
                    Err(err) => TyKind::Error(err.msg),
                }
            }

            hax::TyKind::Foreign(item) => {
                let tref = self.translate_type_decl_ref(span, item)?;
                TyKind::Adt(tref)
            }

            hax::TyKind::Arrow(sig) => {
                trace!("Arrow");
                trace!("bound vars: {:?}", sig.bound_vars);
                let sig = self.translate_poly_fun_sig(span, sig)?;
                TyKind::FnPtr(sig)
            }
            hax::TyKind::FnDef { item, .. } => {
                let fnref = self.translate_bound_fn_ptr(span, item, TransItemSourceKind::Fun)?;
                TyKind::FnDef(fnref)
            }
            hax::TyKind::Closure(args) => {
                let tref = self.translate_closure_type_ref(span, args)?;
                TyKind::Adt(tref)
            }

            hax::TyKind::Dynamic(dyn_binder, region) => {
                // self.check_no_monomorphize(span)?;
                // Translate the region outside the binder.
                let region = self.translate_region(span, region)?;

                let binder = self.translate_dyn_binder(span, dyn_binder, |ctx, ty, ()| {
                    let region = region.move_under_binder();
                    ctx.innermost_binder_mut()
                        .params
                        .types_outlive
                        .push(RegionBinder::empty(OutlivesPred(ty.clone(), region)));
                    Ok(ty)
                })?;

                if let hax::ClauseKind::Trait(trait_predicate) = dyn_binder.predicates.predicates[0]
                    .clause
                    .kind
                    .hax_skip_binder_ref()
                {
                    // TODO(dyn): for now, we consider traits with associated types to not be dyn
                    // compatible because we don't know how to handle them; for these we skip
                    // translating the vtable.
                    if self.trait_is_dyn_compatible(&trait_predicate.trait_ref.def_id)? {
                        // Ensure the vtable type is translated. The first predicate is the one that
                        // can have methods, i.e. a vtable.
                        if self.monomorphize() {
                            let item_src = TransItemSource::monomorphic_trait(
                                &trait_predicate.trait_ref.def_id,
                                TransItemSourceKind::VTable,
                            );
                            let _: TypeDeclId = self.register_and_enqueue(span, item_src);
                        } else {
                            let _: TypeDeclId = self.register_item(
                                span,
                                &trait_predicate.trait_ref,
                                TransItemSourceKind::VTable,
                            );
                        }
                    }
                }
                TyKind::DynTrait(DynPredicate { binder })
            }

            hax::TyKind::Infer(_) => {
                raise_error!(self, span, "Unsupported type: infer type")
            }
            hax::TyKind::Coroutine(..) => {
                raise_error!(self, span, "Coroutine types are not supported yet")
            }
            hax::TyKind::Bound(_, _) => {
                raise_error!(self, span, "Unexpected type kind: bound")
            }
            hax::TyKind::Placeholder(_) => {
                raise_error!(self, span, "Unsupported type: placeholder")
            }

            hax::TyKind::Error => {
                raise_error!(self, span, "Type checking error")
            }
            hax::TyKind::Todo(s) => {
                raise_error!(self, span, "Unsupported type: {:?}", s)
            }
        };
        Ok(kind.into_ty())
    }

    pub(crate) fn translate_rustc_ty(
        &mut self,
        span: Span,
        ty: &ty::Ty<'tcx>,
    ) -> Result<Ty, Error> {
        let ty = self.t_ctx.catch_sinto(&self.hax_state, span, ty)?;
        self.translate_ty(span, &ty)
    }

    pub fn translate_poly_fun_sig(
        &mut self,
        span: Span,
        sig: &hax::Binder<hax::TyFnSig>,
    ) -> Result<RegionBinder<FunSig>, Error> {
        self.translate_region_binder(span, sig, |ctx, sig| ctx.translate_fun_sig(span, sig))
    }
    pub fn translate_fun_sig(&mut self, span: Span, sig: &hax::TyFnSig) -> Result<FunSig, Error> {
        let inputs = sig
            .inputs
            .iter()
            .map(|x| self.translate_ty(span, x))
            .try_collect()?;
        let output = self.translate_ty(span, &sig.output)?;
        Ok(FunSig {
            is_unsafe: sig.safety == hax::Safety::Unsafe,
            inputs,
            output,
        })
    }

    /// Translate generic args. Don't call directly; use `translate_xxx_ref` as much as possible.
    pub fn translate_generic_args(
        &mut self,
        span: Span,
        substs: &[hax::GenericArg],
        trait_refs: &[hax::ImplExpr],
    ) -> Result<GenericArgs, Error> {
        use hax::GenericArg::*;
        trace!("{:?}", substs);

        let mut regions = IndexMap::new();
        let mut types = IndexMap::new();
        let mut const_generics = IndexMap::new();
        for param in substs {
            match param {
                Type(param_ty) => {
                    types.push(self.translate_ty(span, param_ty)?);
                }
                Lifetime(region) => {
                    regions.push(self.translate_region(span, region)?);
                }
                Const(c) => {
                    const_generics.push(self.translate_constant_expr(span, c)?);
                }
            }
        }
        let trait_refs = self.translate_trait_impl_exprs(span, trait_refs)?;

        Ok(GenericArgs {
            regions,
            types,
            const_generics,
            trait_refs,
        })
    }

    /// Checks whether the given id corresponds to a built-in type.
    pub(crate) fn recognize_builtin_type(
        &mut self,
        item: &hax::ItemRef,
    ) -> Result<Option<BuiltinTy>, Error> {
        let def = self.hax_def(item)?;
        let ty = if def.lang_item == Some(sym::owned_box) && self.t_ctx.options.treat_box_as_builtin
        {
            Some(BuiltinTy::Box)
        } else {
            None
        };
        Ok(ty)
    }

    /// Translate a Dynamically Sized Type metadata kind.
    ///
    /// Returns `None` if the type is generic, or if it is not a DST.
    pub fn translate_ptr_metadata(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
    ) -> Result<PtrMetadata, Error> {
        // prepare the call to the method
        use rustc_middle::ty;
        let tcx = self.t_ctx.tcx;
        let rdefid = item.def_id.real_rust_def_id();
        let hax_state = &self.hax_state;
        let ty_env = hax_state.typing_env();
        let ty = tcx
            .type_of(rdefid)
            .instantiate(tcx, item.rustc_args(hax_state));

        // Get the tail type, which determines the metadata of `ty`.
        let tail_ty = tcx.struct_tail_raw(
            ty,
            &rustc_middle::traits::ObligationCause::dummy(),
            |ty| tcx.try_normalize_erasing_regions(ty_env, ty).unwrap_or(ty),
            || {},
        );
        let hax_ty: hax::Ty = self.t_ctx.catch_sinto(hax_state, span, &tail_ty)?;

        // If we're hiding `Sized`, let's consider everything to be sized.
        let everything_is_sized = self.t_ctx.options.hide_marker_traits;
        let ret = match tail_ty.kind() {
            _ if everything_is_sized || tail_ty.is_sized(tcx, ty_env) => PtrMetadata::None,
            ty::Str | ty::Slice(..) => PtrMetadata::Length,
            ty::Dynamic(..) => match hax_ty.kind() {
                hax::TyKind::Dynamic(dyn_binder, _) => {
                    let vtable = self.translate_region_binder(
                        span,
                        &dyn_binder.predicates.predicates[0].clause.kind,
                        |ctx, kind: &hax::ClauseKind| {
                            let hax::ClauseKind::Trait(trait_predicate) = kind else {
                                unreachable!()
                            };
                            ctx.translate_vtable_struct_ref(span, &trait_predicate.trait_ref)
                        },
                    )?;
                    let vtable = self.erase_region_binder(vtable);
                    PtrMetadata::VTable(vtable)
                }
                _ => unreachable!("Unexpected hax type {hax_ty:?} for dynamic type: {ty:?}"),
            },
            ty::Param(..) => PtrMetadata::InheritFrom(self.translate_ty(span, &hax_ty)?),
            ty::Placeholder(..) | ty::Infer(..) | ty::Bound(..) => {
                panic!(
                    "We should never encounter a placeholder, infer, or bound type from ptr_metadata translation. Got: {tail_ty:?}"
                )
            }
            _ => PtrMetadata::None,
        };

        Ok(ret)
    }

    /// Translate a type layout.
    ///
    /// Translates the layout as queried from rustc into
    /// the more restricted [`Layout`].
    #[tracing::instrument(skip(self))]
    pub fn translate_layout(&self, item: &hax::ItemRef) -> Option<Layout> {
        use rustc_abi as r_abi;

        fn translate_variant_layout(
            variant_layout: &r_abi::LayoutData<r_abi::FieldIdx, r_abi::VariantIdx>,
            tag: Option<ScalarValue>,
        ) -> VariantLayout {
            let field_offsets = match &variant_layout.fields {
                r_abi::FieldsShape::Arbitrary { offsets, .. } => {
                    offsets.iter().map(|o| o.bytes()).collect()
                }
                r_abi::FieldsShape::Primitive | r_abi::FieldsShape::Union(_) => IndexVec::default(),
                r_abi::FieldsShape::Array { .. } => panic!("Unexpected layout shape"),
            };
            VariantLayout {
                field_offsets,
                uninhabited: variant_layout.is_uninhabited(),
                tag,
            }
        }

        fn translate_primitive_int(int_ty: r_abi::Integer, signed: bool) -> IntegerTy {
            if signed {
                IntegerTy::Signed(match int_ty {
                    r_abi::Integer::I8 => IntTy::I8,
                    r_abi::Integer::I16 => IntTy::I16,
                    r_abi::Integer::I32 => IntTy::I32,
                    r_abi::Integer::I64 => IntTy::I64,
                    r_abi::Integer::I128 => IntTy::I128,
                })
            } else {
                IntegerTy::Unsigned(match int_ty {
                    r_abi::Integer::I8 => UIntTy::U8,
                    r_abi::Integer::I16 => UIntTy::U16,
                    r_abi::Integer::I32 => UIntTy::U32,
                    r_abi::Integer::I64 => UIntTy::U64,
                    r_abi::Integer::I128 => UIntTy::U128,
                })
            }
        }

        let tcx = self.t_ctx.tcx;
        let rdefid = item.def_id.real_rust_def_id();
        let hax_state = self.hax_state_with_id();
        assert_eq!(hax_state.owner(), item.def_id);
        let ty_env = hax_state.typing_env();
        let ty = tcx
            .type_of(rdefid)
            .instantiate(tcx, item.rustc_args(hax_state));
        let pseudo_input = ty_env.as_query_input(ty);

        // If layout computation returns an error, we return `None`.
        let layout = tcx.layout_of(pseudo_input).ok()?.layout;
        let (size, align) = if layout.is_sized() {
            (
                Some(layout.size().bytes()),
                Some(layout.align().abi.bytes()),
            )
        } else {
            (None, None)
        };

        // Get the layout of the discriminant when there is one (even if it is encoded in a niche).
        let discriminant_layout = match layout.variants() {
            r_abi::Variants::Multiple {
                tag,
                tag_encoding,
                tag_field,
                ..
            } => {
                // The tag_field is the index into the `offsets` vector.
                let r_abi::FieldsShape::Arbitrary { offsets, .. } = layout.fields() else {
                    unreachable!()
                };

                let tag_ty = match tag.primitive() {
                    r_abi::Primitive::Int(int_ty, signed) => {
                        translate_primitive_int(int_ty, signed)
                    }
                    // Try to handle pointer as integers of the same size.
                    r_abi::Primitive::Pointer(_) => IntegerTy::Signed(IntTy::Isize),
                    r_abi::Primitive::Float(_) => {
                        unreachable!()
                    }
                };

                let encoding = match tag_encoding {
                    r_abi::TagEncoding::Direct => TagEncoding::Direct,
                    r_abi::TagEncoding::Niche {
                        untagged_variant, ..
                    } => TagEncoding::Niche {
                        untagged_variant: VariantId::from_usize(r_abi::VariantIdx::as_usize(
                            *untagged_variant,
                        )),
                    },
                };
                offsets.get(*tag_field).map(|s| DiscriminantLayout {
                    offset: r_abi::Size::bytes(*s),
                    tag_ty,
                    encoding,
                })
            }
            r_abi::Variants::Single { .. } | r_abi::Variants::Empty => None,
        };

        let mut variant_layouts: IndexVec<VariantId, VariantLayout> = IndexVec::new();

        match layout.variants() {
            r_abi::Variants::Multiple { variants, .. } => {
                let tag_ty = discriminant_layout
                    .as_ref()
                    .expect("No discriminant layout for enum?")
                    .tag_ty;
                let ptr_size = self.t_ctx.translated.target_information.target_pointer_size;
                let tag_size = r_abi::Size::from_bytes(tag_ty.target_size(ptr_size));

                for (id, variant_layout) in variants.iter_enumerated() {
                    let tag = if variant_layout.is_uninhabited() {
                        None
                    } else {
                        tcx.tag_for_variant(ty_env.as_query_input((ty, id)))
                            .map(|s| match tag_ty {
                                IntegerTy::Signed(int_ty) => {
                                    ScalarValue::from_int(ptr_size, int_ty, s.to_int(tag_size))
                                        .unwrap()
                                }
                                IntegerTy::Unsigned(uint_ty) => {
                                    ScalarValue::from_uint(ptr_size, uint_ty, s.to_uint(tag_size))
                                        .unwrap()
                                }
                            })
                    };
                    variant_layouts.push(translate_variant_layout(variant_layout, tag));
                }
            }
            r_abi::Variants::Single { index } => {
                if let r_abi::FieldsShape::Arbitrary { .. } = layout.fields() {
                    let n_variants = match ty.kind() {
                        _ if let Some(range) = ty.variant_range(tcx) => range.end.index(),
                        _ => 1,
                    };
                    // All the variants not initialized below are uninhabited.
                    variant_layouts = (0..n_variants)
                        .map(|_| VariantLayout {
                            field_offsets: IndexVec::default(),
                            uninhabited: true,
                            tag: None,
                        })
                        .collect();
                    variant_layouts[index.index()] = translate_variant_layout(&layout, None);
                }
            }
            r_abi::Variants::Empty => {}
        }

        Some(Layout {
            size,
            align,
            discriminant_layout,
            uninhabited: layout.is_uninhabited(),
            variant_layouts,
        })
    }

    /// Generate a naive layout for this type.
    pub fn generate_naive_layout(&self, span: Span, ty: &TypeDeclKind) -> Result<Layout, Error> {
        match ty {
            TypeDeclKind::Struct(fields) => {
                let mut size = 0;
                let mut align = 0;
                let ptr_size = self.t_ctx.translated.target_information.target_pointer_size;
                let field_offsets = fields.map_ref(|field| {
                    let offset = size;
                    let size_of_ty = match field.ty.kind() {
                        TyKind::Literal(literal_ty) => literal_ty.target_size(ptr_size) as u64,
                        // This is a lie, the pointers could be fat...
                        TyKind::Ref(..) | TyKind::RawPtr(..) | TyKind::FnPtr(..) => ptr_size,
                        _ => panic!("Unsupported type for `generate_naive_layout`: {ty:?}"),
                    };
                    size += size_of_ty;
                    // For these types, align == size is good enough.
                    align = std::cmp::max(align, size);
                    offset
                });

                Ok(Layout {
                    size: Some(size),
                    align: Some(align),
                    discriminant_layout: None,
                    uninhabited: false,
                    variant_layouts: IndexVec::from_array([VariantLayout {
                        field_offsets,
                        tag: None,
                        uninhabited: false,
                    }]),
                })
            }
            _ => raise_error!(
                self,
                span,
                "`generate_naive_layout` only supports structs at the moment"
            ),
        }
    }

    /// Translate the body of a type declaration.
    ///
    /// Note that the type may be external, in which case we translate the body
    /// only if it is public (i.e., it is a public enumeration, or it is a
    /// struct with only public fields).
    pub(crate) fn translate_adt_def(
        &mut self,
        trans_id: TypeDeclId,
        def_span: Span,
        item_meta: &ItemMeta,
        def: &hax::FullDef,
    ) -> Result<TypeDeclKind, Error> {
        use hax::AdtKind;
        let hax::FullDefKind::Adt {
            adt_kind, variants, ..
        } = def.kind()
        else {
            unreachable!()
        };

        if item_meta.opacity.is_opaque() {
            return Ok(TypeDeclKind::Opaque);
        }

        trace!("{}", trans_id);

        // In case the type is external, check if we should consider the type as
        // transparent (i.e., extract its body). If it is an enumeration, then yes
        // (because the variants of public enumerations are public, together with their
        // fields). If it is a structure, we check if all the fields are public.
        let contents_are_public = match adt_kind {
            AdtKind::Enum => true,
            AdtKind::Struct | AdtKind::Union => {
                // Check the unique variant
                error_assert!(self, def_span, variants.len() == 1);
                variants[hax::VariantIdx::from(0usize)]
                    .fields
                    .iter()
                    .all(|f| matches!(f.vis, Visibility::Public))
            }
            // The rest are fake adt kinds that won't reach here.
            _ => unreachable!(),
        };

        if item_meta
            .opacity
            .with_content_visibility(contents_are_public)
            .is_opaque()
        {
            return Ok(TypeDeclKind::Opaque);
        }

        // The type is transparent: explore the variants
        let mut translated_variants: IndexVec<VariantId, Variant> = Default::default();
        for (i, var_def) in variants.iter().enumerate() {
            trace!("variant {i}: {var_def:?}");

            let mut fields: IndexVec<FieldId, Field> = Default::default();
            /* This is for sanity: check that either all the fields have names, or
             * none of them has */
            let mut have_names: Option<bool> = None;
            for (j, field_def) in var_def.fields.iter().enumerate() {
                trace!("variant {i}: field {j}: {field_def:?}");
                let field_span = self.t_ctx.translate_span(&field_def.span);
                // Translate the field type
                let ty = self.translate_ty(field_span, &field_def.ty)?;
                let field_full_def =
                    self.hax_def(&def.this().with_def_id(self.hax_state(), &field_def.did))?;
                let field_attrs = self.t_ctx.translate_attr_info(&field_full_def);

                // Retrieve the field name.
                let field_name = field_def.name.map(|s| s.to_string());
                // Sanity check
                match &have_names {
                    None => {
                        have_names = match &field_name {
                            None => Some(false),
                            Some(_) => Some(true),
                        }
                    }
                    Some(b) => {
                        error_assert!(self, field_span, *b == field_name.is_some());
                    }
                };

                // Store the field
                let field = Field {
                    span: field_span,
                    attr_info: field_attrs,
                    name: field_name,
                    ty,
                };
                fields.push(field);
            }

            let discriminant = self.translate_discriminant(def_span, &var_def.discr_val)?;
            let variant_span = self.t_ctx.translate_span(&var_def.span);
            let variant_name = var_def.name.to_string();
            let variant_full_def =
                self.hax_def(&def.this().with_def_id(self.hax_state(), &var_def.def_id))?;
            let variant_attrs = self.t_ctx.translate_attr_info(&variant_full_def);

            let mut variant = Variant {
                span: variant_span,
                attr_info: variant_attrs,
                name: variant_name,
                fields,
                discriminant,
            };
            // Propagate a `#[charon::variants_prefix(..)]` or `#[charon::variants_suffix(..)]` attribute to the variants.
            if variant.attr_info.rename.is_none() {
                let prefix = item_meta
                    .attr_info
                    .attributes
                    .iter()
                    .filter_map(|a| a.as_variants_prefix())
                    .next()
                    .map(|attr| attr.as_str());
                let suffix = item_meta
                    .attr_info
                    .attributes
                    .iter()
                    .filter_map(|a| a.as_variants_suffix())
                    .next()
                    .map(|attr| attr.as_str());
                if prefix.is_some() || suffix.is_some() {
                    let prefix = prefix.unwrap_or_default();
                    let suffix = suffix.unwrap_or_default();
                    let name = &variant.name;
                    variant.attr_info.rename = Some(format!("{prefix}{name}{suffix}"));
                }
            }
            translated_variants.push(variant);
        }

        // Register the type
        let type_def_kind: TypeDeclKind = match adt_kind {
            AdtKind::Struct => TypeDeclKind::Struct(translated_variants[0].fields.clone()),
            AdtKind::Enum => TypeDeclKind::Enum(translated_variants),
            AdtKind::Union => TypeDeclKind::Union(translated_variants[0].fields.clone()),
            // The rest are fake adt kinds that won't reach here.
            _ => unreachable!(),
        };

        Ok(type_def_kind)
    }

    fn translate_discriminant(
        &mut self,
        def_span: Span,
        discr: &hax::DiscriminantValue,
    ) -> Result<Literal, Error> {
        let ty = self.translate_ty(def_span, &discr.ty)?;
        let lit_ty = ty.kind().as_literal().unwrap();
        match Literal::from_bits(lit_ty, discr.val) {
            Some(lit) => Ok(lit),
            None => raise_error!(self, def_span, "unexpected discriminant type: {ty:?}",),
        }
    }

    pub fn translate_repr_options(&mut self, hax_repr_options: &hax::ReprOptions) -> ReprOptions {
        let repr_algo = if hax_repr_options.flags.is_c {
            ReprAlgorithm::C
        } else {
            ReprAlgorithm::Rust
        };

        let align_mod = if let Some(align) = &hax_repr_options.align {
            Some(AlignmentModifier::Align(align.bytes))
        } else if let Some(pack) = &hax_repr_options.pack {
            Some(AlignmentModifier::Pack(pack.bytes))
        } else {
            None
        };

        ReprOptions {
            transparent: hax_repr_options.flags.is_transparent,
            explicit_discr_type: hax_repr_options.int_specified,
            repr_algo,
            align_modif: align_mod,
        }
    }
}
