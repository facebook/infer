use super::translate_crate::*;
use super::translate_ctx::*;
use charon_lib::ast::ullbc_ast_utils::BodyBuilder;
use charon_lib::ast::*;
use charon_lib::formatter::IntoFormatter;
use charon_lib::pretty::FmtWithCtx;
use derive_generic_visitor::Visitor;
use hax::SInto;
use itertools::Itertools;
use rustc_span::sym;
use std::mem;
use std::ops::ControlFlow;

impl<'tcx, 'ctx> TranslateCtx<'tcx> {
    pub(crate) fn translate_item(&mut self, item_src: &TransItemSource) {
        let trans_id = self.register_no_enqueue(&None, item_src);
        let def_id = item_src.def_id();
        if let Some(trans_id) = trans_id {
            if self.translate_stack.contains(&trans_id) {
                register_error!(
                    self,
                    Span::dummy(),
                    "Cycle detected while translating {def_id:?}! Stack: {:?}",
                    &self.translate_stack
                );
                return;
            } else {
                self.translate_stack.push(trans_id);
            }
        }
        self.with_def_id(def_id, trans_id, |mut ctx| {
            let span = ctx.def_span(def_id);
            // Catch cycles
            let res = {
                // Stopgap measure because there are still many panics in charon and hax.
                let mut ctx = std::panic::AssertUnwindSafe(&mut ctx);
                std::panic::catch_unwind(move || ctx.translate_item_aux(item_src, trans_id))
            };
            match res {
                Ok(Ok(())) => return,
                // Translation error
                Ok(Err(_)) => {
                    register_error!(ctx, span, "Item `{def_id:?}` caused errors; ignoring.")
                }
                // Panic
                Err(_) => register_error!(
                    ctx,
                    span,
                    "Thread panicked when extracting item `{def_id:?}`."
                ),
            };
        });
        // We must be careful not to early-return from this function to not unbalance the stack.
        self.translate_stack.pop();
    }

    pub(crate) fn translate_item_aux(
        &mut self,
        item_src: &TransItemSource,
        trans_id: Option<ItemId>,
    ) -> Result<(), Error> {
        // Translate the meta information
        let name = self.translate_name(item_src)?;
        if let Some(trans_id) = trans_id {
            self.translated.item_names.insert(trans_id, name.clone());
        }
        let opacity = self.opacity_for_name(&name);
        if opacity.is_invisible() {
            // Don't even start translating the item. In particular don't call `hax_def` on it.
            return Ok(());
        }
        let def = self.hax_def_for_item(&item_src.item)?;
        let item_meta = self.translate_item_meta(&def, item_src, name, opacity);
        if item_meta.opacity.is_invisible() {
            return Ok(());
        }

        // For items in the current crate that have bodies, also enqueue items defined in that
        // body.
        if !item_meta.opacity.is_opaque()
            && let Some(def_id) = def.def_id().as_rust_def_id()
            && let Some(ldid) = def_id.as_local()
            && let node = self.tcx.hir_node_by_def_id(ldid)
            && let Some(body_id) = node.body_id()
        {
            use rustc_hir::intravisit;
            #[allow(non_local_definitions)]
            impl<'tcx> intravisit::Visitor<'tcx> for TranslateCtx<'tcx> {
                fn visit_nested_item(&mut self, id: rustc_hir::ItemId) {
                    let def_id = id.owner_id.def_id.to_def_id();
                    let def_id = def_id.sinto(&self.hax_state);
                    self.enqueue_module_item(&def_id);
                }
            }
            let body = self.tcx.hir_body(body_id);
            intravisit::walk_body(self, body);
        }

        // Initialize the item translation context
        let mut bt_ctx = ItemTransCtx::new(item_src.clone(), trans_id, self);
        trace!(
            "About to translate item `{:?}` as a {:?}; \
            target_id={trans_id:?}, mono={}",
            def.def_id(),
            item_src.kind,
            bt_ctx.monomorphize(),
        );
        if !matches!(
            &item_src.kind,
            TransItemSourceKind::InherentImpl | TransItemSourceKind::Module,
        ) {
            bt_ctx.translate_item_generics(item_meta.span, &def, &item_src.kind)?;
        }
        match &item_src.kind {
            TransItemSourceKind::InherentImpl | TransItemSourceKind::Module => {
                bt_ctx.register_module(item_meta, &def);
            }
            TransItemSourceKind::Type => {
                let Some(ItemId::Type(id)) = trans_id else {
                    unreachable!()
                };
                let ty = bt_ctx.translate_type_decl(id, item_meta, &def)?;
                self.translated.type_decls.set_slot(id, ty);
            }
            TransItemSourceKind::Fun => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_fun_decl(id, item_meta, &def)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::Global => {
                let Some(ItemId::Global(id)) = trans_id else {
                    unreachable!()
                };
                let global_decl = bt_ctx.translate_global(id, item_meta, &def)?;
                self.translated.global_decls.set_slot(id, global_decl);
            }
            TransItemSourceKind::TraitDecl => {
                let Some(ItemId::TraitDecl(id)) = trans_id else {
                    unreachable!()
                };
                let trait_decl = bt_ctx.translate_trait_decl(id, item_meta, &def)?;
                self.translated.trait_decls.set_slot(id, trait_decl);
            }
            TransItemSourceKind::TraitImpl(kind) => {
                let Some(ItemId::TraitImpl(id)) = trans_id else {
                    unreachable!()
                };
                // In Mono mode, only user-defined trait is supported for now.
                let trait_impl = match kind {
                    TraitImplSource::Normal => bt_ctx.translate_trait_impl(id, item_meta, &def)?,
                    TraitImplSource::TraitAlias => {
                        bt_ctx.translate_trait_alias_blanket_impl(id, item_meta, &def)?
                    }
                    &TraitImplSource::Closure(kind) => {
                        bt_ctx.translate_closure_trait_impl(id, item_meta, &def, kind)?
                    }
                    TraitImplSource::ImplicitDestruct => {
                        bt_ctx.translate_implicit_destruct_impl(id, item_meta, &def)?
                    }
                };
                self.translated.trait_impls.set_slot(id, trait_impl);
            }
            &TransItemSourceKind::DefaultedMethod(impl_kind, name) => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl =
                    bt_ctx.translate_defaulted_method(id, item_meta, &def, impl_kind, name)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            &TransItemSourceKind::ClosureMethod(kind) => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_closure_method(id, item_meta, &def, kind)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::ClosureAsFnCast => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_stateless_closure_as_fn(id, item_meta, &def)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            &TransItemSourceKind::DropInPlaceMethod(impl_kind) => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl =
                    bt_ctx.translate_drop_in_place_method(id, item_meta, &def, impl_kind)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::VTable => {
                let Some(ItemId::Type(id)) = trans_id else {
                    unreachable!()
                };
                let ty_decl = bt_ctx.translate_vtable_struct(id, item_meta, &def)?;
                self.translated.type_decls.set_slot(id, ty_decl);
            }
            TransItemSourceKind::VTableInstance(impl_kind) => {
                let Some(ItemId::Global(id)) = trans_id else {
                    unreachable!()
                };
                let global_decl =
                    bt_ctx.translate_vtable_instance(id, item_meta, &def, impl_kind)?;
                self.translated.global_decls.set_slot(id, global_decl);
            }
            TransItemSourceKind::VTableInstanceInitializer(impl_kind) => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl =
                    bt_ctx.translate_vtable_instance_init(id, item_meta, &def, impl_kind)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::VTableMethod => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_vtable_shim(id, item_meta, &def)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::VTableDropShim => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_vtable_drop_shim(id, item_meta, &def)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::VTableDropPreShim => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_vtable_drop_preshim(id, item_meta, &def)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::VTableMethodPreShim(trait_id, name) => {
                let Some(ItemId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl =
                    bt_ctx.translate_vtable_method_preshim(id, item_meta, &def, name, trait_id)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
        }
        Ok(())
    }

    /// While translating an item you may need the contents of another. Use this to retreive the
    /// translated version of this item. Use with care as this could create cycles.
    pub(crate) fn get_or_translate(&mut self, id: ItemId) -> Result<krate::ItemRef<'_>, Error> {
        // We have to call `get_item` a few times because we're running into the classic `Polonius`
        // problem case.
        if self.translated.get_item(id).is_none() {
            let item_source = self.reverse_id_map.get(&id).unwrap().clone();
            self.translate_item(&item_source);
            if self.translated.get_item(id).is_none() {
                let span = self.def_span(item_source.def_id());
                let name = self
                    .translated
                    .item_name(id)
                    .map(|n| n.to_string_with_ctx(&self.into_fmt()))
                    .unwrap_or_else(|| id.to_string());
                // Not a real error, its message won't be displayed.
                return Err(Error {
                    span,
                    msg: format!("Failed to translate item {name}."),
                });
                // raise_error!(self, span, "Failed to translate item {name}.")
            }
            // Add to avoid the double translation of the same item
            self.processed.insert(item_source.clone());
        }
        let item = self.translated.get_item(id);
        Ok(item.unwrap())
    }

    /// Add a `const UNIT: () = ();` const, used as metadata for thin pointers/references.
    pub fn translate_unit_metadata_const(&mut self) {
        use charon_lib::ullbc_ast::*;
        let name = Name::from_path(&["UNIT_METADATA"]);
        let item_meta = ItemMeta {
            name,
            span: Span::dummy(),
            source_text: None,
            attr_info: AttrInfo::default(),
            is_local: false,
            opacity: ItemOpacity::Foreign,
            lang_item: None,
        };

        let body = {
            let mut builder = BodyBuilder::new(Span::dummy(), 0);
            let _ = builder.new_var(None, Ty::mk_unit());
            builder.build()
        };

        let global_id = self.translated.global_decls.reserve_slot();
        let initializer = self.translated.fun_decls.push_with(|def_id| FunDecl {
            def_id,
            item_meta: item_meta.clone(),
            src: ItemSource::TopLevel,
            is_global_initializer: Some(global_id),
            generics: Default::default(),
            signature: FunSig {
                is_unsafe: false,
                inputs: vec![],
                output: Ty::mk_unit(),
            },
            body: Body::Unstructured(body),
        });
        self.translated.global_decls.set_slot(
            global_id,
            GlobalDecl {
                def_id: global_id,
                item_meta,
                generics: Default::default(),
                ty: Ty::mk_unit(),
                src: ItemSource::TopLevel,
                global_kind: GlobalKind::NamedConst,
                init: initializer,
            },
        );
        self.translated.unit_metadata = Some(GlobalDeclRef {
            id: global_id,
            generics: Box::new(GenericArgs::empty()),
        });
    }

    /// Keep only the methods we marked as "used".
    pub fn remove_unused_methods(&mut self) {
        let method_is_used = |trait_id, name| {
            self.method_status.get(trait_id).is_some_and(|map| {
                map.get(&name)
                    .is_some_and(|status| matches!(status, MethodStatus::Used))
            })
        };
        for tdecl in self.translated.trait_decls.iter_mut() {
            tdecl
                .methods
                .retain(|m| method_is_used(tdecl.def_id, m.name()));
        }
        for timpl in self.translated.trait_impls.iter_mut() {
            let trait_id = timpl.impl_trait.id;
            timpl
                .methods
                .retain(|(name, _)| method_is_used(trait_id, *name));
        }
    }
}

impl ItemTransCtx<'_, '_> {
    /// Register the items inside this module or inherent impl.
    // TODO: we may want to accumulate the set of modules we found, to check that all
    // the opaque modules given as arguments actually exist
    #[tracing::instrument(skip(self, item_meta, def))]
    pub(crate) fn register_module(&mut self, item_meta: ItemMeta, def: &hax::FullDef) {
        if !item_meta.opacity.is_transparent() {
            return;
        }
        match def.kind() {
            hax::FullDefKind::InherentImpl { items, .. } => {
                for assoc in items {
                    self.t_ctx.enqueue_module_item(&assoc.def_id);
                }
            }
            hax::FullDefKind::Mod { items, .. } => {
                for (_, def_id) in items {
                    self.t_ctx.enqueue_module_item(def_id);
                }
            }
            hax::FullDefKind::ForeignMod { items, .. } => {
                for def_id in items {
                    self.t_ctx.enqueue_module_item(def_id);
                }
            }
            _ => panic!("Item should be a module but isn't: {def:?}"),
        }
    }

    pub(crate) fn get_item_source(
        &mut self,
        span: Span,
        def: &hax::FullDef,
    ) -> Result<ItemSource, Error> {
        let assoc = match def.kind() {
            hax::FullDefKind::AssocTy {
                associated_item, ..
            }
            | hax::FullDefKind::AssocConst {
                associated_item, ..
            }
            | hax::FullDefKind::AssocFn {
                associated_item, ..
            } => associated_item,
            hax::FullDefKind::Closure { args, .. } => {
                let info = self.translate_closure_info(span, args)?;
                return Ok(ItemSource::Closure { info });
            }
            _ => return Ok(ItemSource::TopLevel),
        };
        Ok(match &assoc.container {
            // E.g.:
            // ```
            // impl<T> List<T> {
            //   fn new() -> Self { ... } <- inherent method
            // }
            // ```
            hax::AssocItemContainer::InherentImplContainer { .. } => ItemSource::TopLevel,
            // E.g.:
            // ```
            // impl Foo for Bar {
            //   fn baz(...) { ... } // <- implementation of a trait method
            // }
            // ```
            hax::AssocItemContainer::TraitImplContainer {
                impl_,
                implemented_trait_ref,
                overrides_default,
                ..
            } => {
                let impl_ref =
                    self.translate_trait_impl_ref(span, impl_, TraitImplSource::Normal)?;
                let trait_ref = self.translate_trait_ref(span, implemented_trait_ref)?;
                let item_name = self.t_ctx.translate_trait_item_name(def.def_id())?;
                if matches!(def.kind(), hax::FullDefKind::AssocFn { .. }) {
                    // If the implementation is getting translated, that means the method is
                    // getting used.
                    self.mark_method_as_used(trait_ref.id, item_name);
                }
                ItemSource::TraitImpl {
                    impl_ref,
                    trait_ref,
                    item_name,
                    reuses_default: !overrides_default,
                }
            }
            // This method is the *declaration* of a trait item
            // E.g.:
            // ```
            // trait Foo {
            //   fn baz(...); // <- declaration of a trait method
            // }
            // ```
            hax::AssocItemContainer::TraitContainer { trait_ref, .. } => {
                // The trait id should be Some(...): trait markers (that we may eliminate)
                // don't have associated items.
                let trait_ref = self.translate_trait_ref(span, trait_ref)?;
                let item_name = self.t_ctx.translate_trait_item_name(def.def_id())?;
                ItemSource::TraitDecl {
                    trait_ref,
                    item_name,
                    has_default: assoc.has_value,
                }
            }
        })
    }

    /// Translate a type definition.
    ///
    /// Note that we translate the types one by one: we don't need to take into
    /// account the fact that some types are mutually recursive at this point
    /// (we will need to take that into account when generating the code in a file).
    #[tracing::instrument(skip(self, item_meta, def))]
    pub fn translate_type_decl(
        mut self,
        trans_id: TypeDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<TypeDecl, Error> {
        let span = item_meta.span;

        // Get the kind of the type decl -- is it a closure?
        let src = self.get_item_source(span, def)?;

        let mut repr: Option<ReprOptions> = None;

        // Translate type body
        let kind = match &def.kind {
            _ if item_meta.opacity.is_opaque() => Ok(TypeDeclKind::Opaque),
            hax::FullDefKind::OpaqueTy | hax::FullDefKind::ForeignTy => Ok(TypeDeclKind::Opaque),
            hax::FullDefKind::TyAlias { ty, .. } => {
                // Don't error on missing trait refs.
                self.error_on_impl_expr_error = false;
                self.translate_ty(span, ty).map(TypeDeclKind::Alias)
            }
            hax::FullDefKind::Adt { repr: hax_repr, .. } => {
                repr = Some(self.translate_repr_options(hax_repr));
                self.translate_adt_def(trans_id, span, &item_meta, def)
            }
            hax::FullDefKind::Closure { args, .. } => self.translate_closure_adt(span, &args),
            _ => panic!("Unexpected item when translating types: {def:?}"),
        };

        let kind = match kind {
            Ok(kind) => kind,
            Err(err) => TypeDeclKind::Error(err.msg),
        };
        let layout = self.translate_layout(def.this());
        let ptr_metadata = self.translate_ptr_metadata(span, def.this())?;
        let type_def = TypeDecl {
            def_id: trans_id,
            item_meta,
            generics: self.into_generics(),
            kind,
            src,
            layout,
            ptr_metadata,
            repr,
        };

        Ok(type_def)
    }

    /// Translate one function.
    #[tracing::instrument(skip(self, item_meta, def))]
    pub fn translate_fun_decl(
        mut self,
        def_id: FunDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<FunDecl, Error> {
        let span = item_meta.span;

        let src = self.get_item_source(span, def)?;

        if let hax::FullDefKind::Ctor {
            fields, output_ty, ..
        } = def.kind()
        {
            let signature = FunSig {
                inputs: fields
                    .iter()
                    .map(|field| self.translate_ty(span, &field.ty))
                    .try_collect()?,
                output: self.translate_ty(span, output_ty)?,
                is_unsafe: false,
            };

            let body = if item_meta.opacity.with_private_contents().is_opaque() {
                Body::Opaque
            } else {
                self.build_ctor_body(span, def)?
            };
            return Ok(FunDecl {
                def_id,
                item_meta,
                generics: self.into_generics(),
                signature,
                src,
                is_global_initializer: None,
                body,
            });
        }

        // Translate the function signature
        trace!("Translating function signature");
        let signature = match &def.kind {
            hax::FullDefKind::Fn { sig, .. } | hax::FullDefKind::AssocFn { sig, .. } => {
                self.translate_fun_sig(span, &sig.value)?
            }
            hax::FullDefKind::Const { ty, .. }
            | hax::FullDefKind::AssocConst { ty, .. }
            | hax::FullDefKind::Static { ty, .. } => FunSig {
                inputs: vec![],
                output: self.translate_ty(span, ty)?,
                is_unsafe: false,
            },
            _ => panic!("Unexpected definition for function: {def:?}"),
        };

        // Check whether this function is a method declaration for a trait definition.
        // If this is the case, it shouldn't contain a body.
        let is_trait_method_decl_without_default = match &src {
            ItemSource::TraitDecl { has_default, .. } => !has_default,
            _ => false,
        };

        let is_global_initializer = matches!(
            def.kind(),
            hax::FullDefKind::Const { .. }
                | hax::FullDefKind::AssocConst { .. }
                | hax::FullDefKind::Static { .. }
        );
        let is_global_initializer = is_global_initializer
            .then(|| self.register_item(span, def.this(), TransItemSourceKind::Global));

        let body = if item_meta.opacity.with_private_contents().is_opaque() {
            Body::Opaque
        } else if is_trait_method_decl_without_default {
            Body::TraitMethodWithoutDefault
        } else {
            // Translate the MIR body for this definition.
            self.translate_def_body(item_meta.span, def)
        };
        Ok(FunDecl {
            def_id,
            item_meta,
            generics: self.into_generics(),
            signature,
            src,
            is_global_initializer,
            body,
        })
    }

    /// Translate one global.
    #[tracing::instrument(skip(self, item_meta, def))]
    pub fn translate_global(
        mut self,
        def_id: GlobalDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<GlobalDecl, Error> {
        let span = item_meta.span;

        // Retrieve the kind
        let item_source = self.get_item_source(span, def)?;

        trace!("Translating global type");
        let ty = match &def.kind {
            hax::FullDefKind::Const { ty, .. }
            | hax::FullDefKind::AssocConst { ty, .. }
            | hax::FullDefKind::Static { ty, .. } => ty,
            _ => panic!("Unexpected def for constant: {def:?}"),
        };
        let ty = self.translate_ty(span, ty)?;

        let global_kind = match &def.kind {
            hax::FullDefKind::Static { .. } => GlobalKind::Static,
            hax::FullDefKind::Const {
                kind: hax::ConstKind::TopLevel,
                ..
            }
            | hax::FullDefKind::AssocConst { .. } => GlobalKind::NamedConst,
            hax::FullDefKind::Const { .. } => GlobalKind::AnonConst,
            _ => panic!("Unexpected def for constant: {def:?}"),
        };

        let initializer = self.register_item(span, def.this(), TransItemSourceKind::Fun);

        Ok(GlobalDecl {
            def_id,
            item_meta,
            generics: self.into_generics(),
            ty,
            src: item_source,
            global_kind,
            init: initializer,
        })
    }

    // either Poly or MonoTrait
    #[tracing::instrument(skip(self, item_meta, def))]
    pub fn translate_trait_decl(
        mut self,
        def_id: TraitDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<TraitDecl, Error> {
        let span = item_meta.span;

        let (hax::FullDefKind::Trait {
            implied_predicates, ..
        }
        | hax::FullDefKind::TraitAlias {
            implied_predicates, ..
        }) = def.kind()
        else {
            raise_error!(self, span, "Unexpected definition: {def:?}");
        };

        // Register implied predicates. We gather the clauses and consider the other predicates as
        // required since the distinction doesn't matter for non-trait-clauses.
        let mut implied_clauses = Default::default();
        self.translate_predicates(
            implied_predicates,
            PredicateOrigin::WhereClauseOnTrait,
            Some(&mut implied_clauses),
        )?;

        let vtable = self.translate_vtable_struct_ref_no_enqueue(span, def.this())?;

        if let hax::FullDefKind::TraitAlias { .. } = def.kind() {
            // Trait aliases don't have any items. Everything interesting is in the parent clauses.
            return Ok(TraitDecl {
                def_id,
                item_meta,
                implied_clauses,
                generics: self.into_generics(),
                consts: Default::default(),
                types: Default::default(),
                methods: Default::default(),
                vtable,
            });
        }

        let hax::FullDefKind::Trait {
            items,
            self_predicate,
            ..
        } = &def.kind
        else {
            unreachable!()
        };
        let self_trait_ref = TraitRef::new(
            TraitRefKind::SelfId,
            RegionBinder::empty(self.translate_trait_predicate(span, self_predicate)?),
        );
        let items: Vec<(TraitItemName, &hax::AssocItem)> = items
            .iter()
            .map(|item| -> Result<_, Error> {
                let name = self.t_ctx.translate_trait_item_name(&item.def_id)?;
                Ok((name, item))
            })
            .try_collect()?;

        // Translate the associated items
        let mut consts = Vec::new();
        let mut types = Vec::new();
        let mut methods = Vec::new();

        // skip all associated items of trait decl in mono mode
        // question: what if the associated methods (or consts) has default implmentation?
        // TODO: support default methods and default consts
        if self.monomorphize() {
            return Ok(TraitDecl {
                def_id,
                item_meta,
                implied_clauses,
                generics: self.into_generics(),
                consts,
                types,
                methods,
                vtable,
            });
        }

        for &(item_name, ref hax_item) in &items {
            let item_def_id = &hax_item.def_id;
            let item_span = self.def_span(item_def_id);

            // In --mono mode, we keep only non-polymorphic items; in not-mono mode, we use the
            // polymorphic item as usual.
            let trans_kind = match hax_item.kind {
                hax::AssocKind::Fn { .. } => TransItemSourceKind::Fun,
                hax::AssocKind::Const { .. } => TransItemSourceKind::Global,
                hax::AssocKind::Type { .. } => TransItemSourceKind::Type,
            };

            let item_def = self.poly_hax_def(item_def_id)?;
            let item_src = TransItemSource::polymorphic(item_def_id, trans_kind);
            let attr_info = self.translate_attr_info(&item_def);

            match item_def.kind() {
                hax::FullDefKind::AssocFn { .. } => {
                    let method_id = self.register_no_enqueue(item_span, &item_src);
                    // Register this method.
                    self.register_method_impl(def_id, item_name, method_id);
                    // By default we only enqueue required methods (those that don't have a default
                    // impl). If the trait is transparent, we enqueue all its methods.
                    if self.options.translate_all_methods
                        || item_meta.opacity.is_transparent()
                        || !hax_item.has_value
                    {
                        self.mark_method_as_used(def_id, item_name);
                    }

                    let binder_kind = BinderKind::TraitMethod(def_id, item_name);
                    let mut method = self.translate_binder_for_def(
                        item_span,
                        binder_kind,
                        &item_def,
                        |bt_ctx| {
                            assert_eq!(bt_ctx.binding_levels.len(), 2);
                            let fun_generics = bt_ctx
                                .outermost_binder()
                                .params
                                .identity_args_at_depth(DeBruijnId::one())
                                .concat(
                                    &bt_ctx
                                        .innermost_binder()
                                        .params
                                        .identity_args_at_depth(DeBruijnId::zero()),
                                );
                            let fn_ref = FunDeclRef {
                                id: method_id,
                                generics: Box::new(fun_generics),
                            };
                            Ok(TraitMethod {
                                name: item_name.clone(),
                                attr_info,
                                item: fn_ref,
                            })
                        },
                    )?;
                    // In hax, associated items take an extra explicit `Self: Trait` clause, but we
                    // don't want that to be part of the method clauses. Hence we remove the first
                    // bound clause and replace its uses with references to the ambient `Self`
                    // clause available in trait declarations.
                    struct ReplaceSelfVisitor;
                    impl VarsVisitor for ReplaceSelfVisitor {
                        fn visit_clause_var(&mut self, v: ClauseDbVar) -> Option<TraitRefKind> {
                            if let DeBruijnVar::Bound(DeBruijnId::ZERO, clause_id) = v {
                                // Replace clause 0 and decrement the others.
                                Some(if let Some(new_id) = clause_id.index().checked_sub(1) {
                                    TraitRefKind::Clause(DeBruijnVar::Bound(
                                        DeBruijnId::ZERO,
                                        TraitClauseId::new(new_id),
                                    ))
                                } else {
                                    TraitRefKind::SelfId
                                })
                            } else {
                                None
                            }
                        }
                    }
                    method.params.visit_vars(&mut ReplaceSelfVisitor);
                    method.skip_binder.visit_vars(&mut ReplaceSelfVisitor);
                    method
                        .params
                        .trait_clauses
                        .remove_and_shift_ids(TraitClauseId::ZERO);
                    method.params.trait_clauses.iter_mut().for_each(|clause| {
                        clause.clause_id -= 1;
                    });

                    // We insert the `Binder<TraitMethod>` unconditionally here; we'll remove the
                    // ones that correspond to unused methods at the end of translation.
                    methods.push(method);
                }
                hax::FullDefKind::AssocConst { ty, .. } => {
                    // The const is defined in a context that has an extra `Self: Trait` clause, so
                    // we translate it bound first.
                    let bound_assoc_const = self.translate_binder_for_def(
                        item_span,
                        BinderKind::Other,
                        &item_def,
                        |ctx| {
                            // Check if the constant has a value (i.e., a body).
                            let default = hax_item.has_value.then(|| {
                                // The parameters of the constant are the same as those of the item that
                                // declares them.
                                let id = ctx.register_and_enqueue(item_span, item_src);
                                let generics = ctx
                                    .outermost_binder()
                                    .params
                                    .identity_args_at_depth(DeBruijnId::one())
                                    .concat(
                                        &ctx.innermost_binder()
                                            .params
                                            .identity_args_at_depth(DeBruijnId::zero()),
                                    );
                                GlobalDeclRef {
                                    id,
                                    generics: Box::new(generics),
                                }
                            });
                            let ty = ctx.translate_ty(item_span, ty)?;
                            Ok(TraitAssocConst {
                                name: item_name.clone(),
                                attr_info,
                                ty,
                                default,
                            })
                        },
                    )?;
                    let assoc_const = bound_assoc_const.apply(&{
                        let mut generics = GenericArgs::empty();
                        // Provide the `Self` clause.
                        generics.trait_refs.push(self_trait_ref.clone());
                        generics
                    });
                    consts.push(assoc_const);
                }
                hax::FullDefKind::AssocTy {
                    implied_predicates,
                    value: default,
                    ..
                } => {
                    let binder_kind = BinderKind::TraitType(def_id, item_name.clone());
                    let assoc_ty =
                        self.translate_binder_for_def(item_span, binder_kind, &item_def, |ctx| {
                            // Also add the implied predicates.
                            let mut implied_clauses = Default::default();
                            ctx.translate_predicates(
                                &implied_predicates,
                                PredicateOrigin::TraitItem(item_name.clone()),
                                Some(&mut implied_clauses),
                            )?;

                            let default = default
                                .as_ref()
                                .map(|(ty, impl_exprs)| -> Result<_, Error> {
                                    let ty = ctx.translate_ty(item_span, ty)?;
                                    let trefs = ctx.translate_trait_impl_exprs(span, impl_exprs)?;
                                    Ok(TraitAssocTyImpl {
                                        value: ty,
                                        implied_trait_refs: trefs,
                                    })
                                })
                                .transpose()?;
                            Ok(TraitAssocTy {
                                name: item_name.clone(),
                                attr_info,
                                default,
                                implied_clauses,
                            })
                        })?;
                    types.push(assoc_ty);
                }
                _ => panic!("Unexpected definition for trait item: {item_def:?}"),
            }
        }

        if def.lang_item == Some(sym::destruct) {
            // Add a `drop_in_place(*mut self)` method that contains the drop glue for this type.
            let (method_name, method_binder) =
                self.prepare_drop_in_place_method(def, span, def_id, None);
            self.mark_method_as_used(def_id, method_name);
            methods.push(method_binder.map(|fn_ref| TraitMethod {
                name: method_name,
                attr_info: AttrInfo::dummy_public(),
                item: fn_ref,
            }));
        }

        // In case of a trait implementation, some values may not have been
        // provided, in case the declaration provided default values. We
        // check those, and lookup the relevant values.
        Ok(TraitDecl {
            def_id,
            item_meta,
            implied_clauses,
            generics: self.into_generics(),
            consts,
            types,
            methods,
            vtable,
        })
    }

    #[tracing::instrument(skip(self, item_meta, def))]
    pub fn translate_trait_impl(
        mut self,
        def_id: TraitImplId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<TraitImpl, Error> {
        let span = item_meta.span;

        let hax::FullDefKind::TraitImpl {
            trait_pred,
            implied_impl_exprs,
            items: impl_items,
            ..
        } = &def.kind
        else {
            unreachable!()
        };

        // Retrieve the information about the implemented trait.
        let implemented_trait = self.translate_trait_ref(span, &trait_pred.trait_ref)?;
        let trait_id = implemented_trait.id;
        // A `TraitRef` that points to this impl with the correct generics.
        let self_predicate = TraitRef::new(
            TraitRefKind::TraitImpl(TraitImplRef {
                id: def_id,
                generics: Box::new(self.the_only_binder().params.identity_args()),
            }),
            RegionBinder::empty(implemented_trait.clone()),
        );

        let vtable =
            self.translate_vtable_instance_ref_no_enqueue(span, &trait_pred.trait_ref, def.this())?;

        // The trait refs which implement the parent clauses of the implemented trait decl.
        let implied_trait_refs = self.translate_trait_impl_exprs(span, &implied_impl_exprs)?;

        {
            // Debugging
            let ctx = self.into_fmt();
            let refs = implied_trait_refs
                .iter()
                .map(|c| c.with_ctx(&ctx))
                .format("\n");
            trace!(
                "Trait impl: {:?}\n- implied_trait_refs:\n{}",
                def.def_id(),
                refs
            );
        }

        let implemented_trait_def = self.poly_hax_def(&trait_pred.trait_ref.def_id)?;
        if implemented_trait_def.lang_item == Some(sym::destruct) {
            raise_error!(
                self,
                span,
                "found an explicit impl of `core::marker::Destruct`, this should not happen"
            );
        }

        // Explore the associated items
        let mut consts = Vec::new();
        let mut types = Vec::new();
        let mut methods = Vec::new();

        // In mono mode, we do not translate any associated items in trait impl.
        if self.monomorphize() {
            return Ok(TraitImpl {
                def_id,
                item_meta,
                impl_trait: implemented_trait,
                generics: self.into_generics(),
                implied_trait_refs,
                consts,
                types,
                methods,
                vtable,
            });
        }

        for impl_item in impl_items {
            use hax::ImplAssocItemValue::*;
            let name = self
                .t_ctx
                .translate_trait_item_name(&impl_item.decl_def_id)?;
            let item_def_id = impl_item.def_id();
            let item_span = self.def_span(item_def_id);
            //
            // In not-mono mode, we use the polymorphic item as usual.
            let item_def = self.poly_hax_def(item_def_id)?;
            let trans_kind = match item_def.kind() {
                hax::FullDefKind::AssocFn { .. } => TransItemSourceKind::Fun,
                hax::FullDefKind::AssocConst { .. } => TransItemSourceKind::Global,
                hax::FullDefKind::AssocTy { .. } => TransItemSourceKind::Type,
                _ => unreachable!(),
            };
            let item_src = TransItemSource::polymorphic(item_def_id, trans_kind);

            match item_def.kind() {
                hax::FullDefKind::AssocFn { .. } => {
                    let method_id: FunDeclId = {
                        let method_src = match &impl_item.value {
                            Provided { .. } => item_src,
                            // This will generate a copy of the default method. Note that the base
                            // item for `DefaultedMethod` is the trait impl.
                            DefaultedFn { .. } => TransItemSource::from_item(
                                def.this(),
                                TransItemSourceKind::DefaultedMethod(TraitImplSource::Normal, name),
                                self.monomorphize(),
                            ),
                            _ => unreachable!(),
                        };
                        self.register_no_enqueue(item_span, &method_src)
                    };

                    // Register this method.
                    self.register_method_impl(trait_id, name, method_id);
                    // By default we only enqueue required methods (those that don't have a default
                    // impl). If the impl is transparent, we enqueue all the implemented methods.
                    if matches!(impl_item.value, Provided { .. })
                        && item_meta.opacity.is_transparent()
                    {
                        self.mark_method_as_used(trait_id, name);
                    }

                    let binder_kind = BinderKind::TraitMethod(trait_id, name);
                    let bound_fn_ref = match &impl_item.value {
                        Provided { .. } => self.translate_binder_for_def(
                            item_span,
                            binder_kind,
                            &item_def,
                            |ctx| {
                                let generics = ctx
                                    .outermost_generics()
                                    .identity_args_at_depth(DeBruijnId::one())
                                    .concat(
                                        &ctx.innermost_generics()
                                            .identity_args_at_depth(DeBruijnId::zero()),
                                    );
                                Ok(FunDeclRef {
                                    id: method_id,
                                    generics: Box::new(generics),
                                })
                            },
                        )?,
                        DefaultedFn { .. } => {
                            // Retrieve the method generics from the trait decl.
                            let decl_methods =
                                match self.get_or_translate(implemented_trait.id.into()) {
                                    Ok(ItemRef::TraitDecl(tdecl)) => tdecl.methods.as_slice(),
                                    _ => &[],
                                };
                            let Some(bound_method) = decl_methods.iter().find(|m| m.name() == name)
                            else {
                                continue;
                            };
                            let method_params = bound_method
                                .clone()
                                .substitute_with_tref(&self_predicate)
                                .params;

                            let generics = self
                                .outermost_generics()
                                .identity_args_at_depth(DeBruijnId::one())
                                .concat(&method_params.identity_args_at_depth(DeBruijnId::zero()));
                            let fn_ref = FunDeclRef {
                                id: method_id,
                                generics: Box::new(generics),
                            };
                            Binder::new(binder_kind, method_params, fn_ref)
                        }
                        _ => unreachable!(),
                    };

                    // We insert the `Binder<FunDeclRef>` unconditionally here; we'll remove the
                    // ones that correspond to unused methods at the end of translation.
                    methods.push((name, bound_fn_ref));
                }
                hax::FullDefKind::AssocConst { .. } => {
                    let id = self.register_and_enqueue(item_span, item_src);
                    // The parameters of the constant are the same as those of the item that
                    // declares them.
                    let generics = match &impl_item.value {
                        Provided { .. } => self.the_only_binder().params.identity_args(),
                        _ => {
                            let mut generics = implemented_trait.generics.as_ref().clone();
                            // For default consts, we add an extra `Self` predicate.
                            generics.trait_refs.push(self_predicate.clone());
                            generics
                        }
                    };
                    let gref = GlobalDeclRef {
                        id,
                        generics: Box::new(generics),
                    };
                    consts.push((name, gref));
                }
                hax::FullDefKind::AssocTy { value, .. } => {
                    let binder_kind = BinderKind::TraitType(trait_id, name.clone());
                    let assoc_ty = match &impl_item.value {
                        Provided { .. } => self.translate_binder_for_def(
                            item_span,
                            binder_kind,
                            &item_def,
                            |ctx| {
                                let (ty, _impl_exprs) = value.as_ref().unwrap();
                                let ty = ctx.translate_ty(item_span, ty)?;
                                let implied_trait_refs = ctx.translate_trait_impl_exprs(
                                    item_span,
                                    &impl_item.required_impl_exprs,
                                )?;
                                Ok(TraitAssocTyImpl {
                                    value: ty,
                                    implied_trait_refs,
                                })
                            },
                        )?,
                        DefaultedTy { .. } => {
                            // Retrieve the type from the trait decl.
                            let decl_types =
                                match self.get_or_translate(implemented_trait.id.into()) {
                                    Ok(ItemRef::TraitDecl(tdecl)) => tdecl.types.as_slice(),
                                    _ => &[],
                                };
                            let Some(bound_ty) =
                                decl_types.iter().find(|m| *m.name() == name).cloned()
                            else {
                                register_error!(
                                    self,
                                    item_span,
                                    "couldn't translate defaulted associated type; \
                                    either the corresponding trait decl caused errors \
                                    or it was declared opaque."
                                );
                                continue;
                            };
                            bound_ty
                                .substitute_with_tref(&self_predicate)
                                .map(|ty_decl: TraitAssocTy| ty_decl.default.unwrap())
                        }
                        _ => unreachable!(),
                    };

                    types.push((name.clone(), assoc_ty));
                }
                _ => panic!("Unexpected definition for trait item: {item_def:?}"),
            }
        }

        Ok(TraitImpl {
            def_id,
            item_meta,
            impl_trait: implemented_trait,
            generics: self.into_generics(),
            implied_trait_refs,
            consts,
            types,
            methods,
            vtable,
        })
    }

    /// Generate a blanket impl for this trait, as in:
    /// ```
    ///     trait Alias<U> = Trait<Option<U>, Item = u32> + Clone;
    /// ```
    /// becomes:
    /// ```
    ///     trait Alias<U>: Trait<Option<U>, Item = u32> + Clone {}
    ///     impl<U, Self: Trait<Option<U>, Item = u32> + Clone> Alias<U> for Self {}
    /// ```
    #[tracing::instrument(skip(self, item_meta, def))]
    pub fn translate_trait_alias_blanket_impl(
        mut self,
        def_id: TraitImplId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<TraitImpl, Error> {
        let span = item_meta.span;

        let hax::FullDefKind::TraitAlias {
            implied_predicates, ..
        } = &def.kind
        else {
            raise_error!(self, span, "Unexpected definition: {def:?}");
        };

        let trait_id = self.register_item(span, def.this(), TransItemSourceKind::TraitDecl);

        // Register the trait implied clauses as required clauses for the impl.
        assert!(self.innermost_generics_mut().trait_clauses.is_empty());
        self.register_predicates(implied_predicates, PredicateOrigin::WhereClauseOnTrait)?;

        let mut generics = self.the_only_binder().params.identity_args();
        // Do the inverse operation: the trait considers the clauses as implied.
        let implied_trait_refs = mem::take(&mut generics.trait_refs);
        let implemented_trait = TraitDeclRef {
            id: trait_id,
            generics: Box::new(generics),
        };

        let mut timpl = TraitImpl {
            def_id,
            item_meta,
            impl_trait: implemented_trait,
            generics: self.the_only_binder().params.clone(),
            implied_trait_refs,
            consts: Default::default(),
            types: Default::default(),
            methods: Default::default(),
            // TODO(dyn)
            vtable: None,
        };
        // We got the predicates from a trait decl, so they may refer to the virtual `Self`
        // clause, which doesn't exist for impls. We fix that up here.
        {
            struct FixSelfVisitor {
                binder_depth: DeBruijnId,
            }
            struct UnhandledSelf;
            impl Visitor for FixSelfVisitor {
                type Break = UnhandledSelf;
            }
            impl VisitorWithBinderDepth for FixSelfVisitor {
                fn binder_depth_mut(&mut self) -> &mut DeBruijnId {
                    &mut self.binder_depth
                }
            }
            impl VisitAstMut for FixSelfVisitor {
                fn visit<'a, T: AstVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
                    VisitWithBinderDepth::new(self).visit(x)
                }
                fn visit_trait_ref_kind(
                    &mut self,
                    kind: &mut TraitRefKind,
                ) -> ControlFlow<Self::Break> {
                    match kind {
                        TraitRefKind::SelfId => return ControlFlow::Break(UnhandledSelf),
                        TraitRefKind::ParentClause(sub, clause_id)
                            if matches!(sub.kind, TraitRefKind::SelfId) =>
                        {
                            *kind = TraitRefKind::Clause(DeBruijnVar::bound(
                                self.binder_depth,
                                *clause_id,
                            ))
                        }
                        _ => (),
                    }
                    self.visit_inner(kind)
                }
            }
            match timpl.drive_mut(&mut FixSelfVisitor {
                binder_depth: DeBruijnId::zero(),
            }) {
                ControlFlow::Continue(()) => {}
                ControlFlow::Break(UnhandledSelf) => {
                    register_error!(
                        self,
                        span,
                        "Found `Self` clause we can't handle \
                         in a trait alias blanket impl."
                    );
                }
            }
        };

        Ok(timpl)
    }

    /// Make a trait impl from a hax `VirtualTraitImpl`. Used for constructing fake trait impls for
    /// builtin types like `FnOnce`.
    #[tracing::instrument(skip(self, item_meta))]
    pub fn translate_virtual_trait_impl(
        &mut self,
        def_id: TraitImplId,
        item_meta: ItemMeta,
        vimpl: &hax::VirtualTraitImpl,
    ) -> Result<TraitImpl, Error> {
        let span = item_meta.span;
        let trait_def = self.hax_def(&vimpl.trait_pred.trait_ref)?;
        let hax::FullDefKind::Trait {
            items: trait_items, ..
        } = trait_def.kind()
        else {
            panic!()
        };

        let implemented_trait = self.translate_trait_predicate(span, &vimpl.trait_pred)?;
        let implied_trait_refs =
            self.translate_trait_impl_exprs(span, &vimpl.implied_impl_exprs)?;

        let mut types = vec![];
        // Monomorphic traits have no associated types.
        if !self.monomorphize() {
            let type_items = trait_items.iter().filter(|assoc| match assoc.kind {
                hax::AssocKind::Type { .. } => true,
                _ => false,
            });
            for ((ty, impl_exprs), assoc) in vimpl.types.iter().zip(type_items) {
                let name = self.t_ctx.translate_trait_item_name(&assoc.def_id)?;
                let assoc_ty = TraitAssocTyImpl {
                    value: self.translate_ty(span, ty)?,
                    implied_trait_refs: self.translate_trait_impl_exprs(span, impl_exprs)?,
                };
                let binder_kind = BinderKind::TraitType(implemented_trait.id, name.clone());
                types.push((name, Binder::empty(binder_kind, assoc_ty)));
            }
        }

        let generics = self.the_only_binder().params.clone();
        Ok(TraitImpl {
            def_id,
            item_meta,
            impl_trait: implemented_trait,
            generics,
            implied_trait_refs,
            consts: vec![],
            types,
            methods: vec![],
            // TODO(dyn): generate vtable instances for builtin traits
            vtable: None,
        })
    }

    /// Record that `method_id` is an implementation of the given method of the trait. If the
    /// method is not used anywhere yet we simply record the implementation. If the method is used
    /// then we enqueue it for translation.
    pub fn register_method_impl(
        &mut self,
        trait_id: TraitDeclId,
        method_name: TraitItemName,
        method_id: FunDeclId,
    ) {
        match self
            .method_status
            .get_or_extend_and_insert_default(trait_id)
            .entry(method_name)
            .or_default()
        {
            MethodStatus::Unused { implementors } => {
                implementors.insert(method_id);
            }
            MethodStatus::Used => {
                self.enqueue_id(method_id);
            }
        }
    }

    /// Mark the method as "used", which will enqueue for translation all the implementations of
    /// that method.
    pub fn mark_method_as_used(&mut self, trait_id: TraitDeclId, method_name: TraitItemName) {
        let method_status = self
            .method_status
            .get_or_extend_and_insert_default(trait_id)
            .entry(method_name)
            .or_default();
        match method_status {
            MethodStatus::Unused { implementors } => {
                let implementors = mem::take(implementors);
                *method_status = MethodStatus::Used;
                for fun_id in implementors {
                    self.enqueue_id(fun_id);
                }
            }
            MethodStatus::Used => {}
        }
    }

    /// In case an impl does not override a trait method, this duplicates the original trait method
    /// and adjusts its generics to make the corresponding impl method.
    #[tracing::instrument(skip(self, item_meta, def))]
    pub fn translate_defaulted_method(
        &mut self,
        def_id: FunDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
        impl_kind: TraitImplSource,
        method_name: TraitItemName,
    ) -> Result<FunDecl, Error> {
        let span = item_meta.span;

        let hax::FullDefKind::TraitImpl { trait_pred, .. } = &def.kind else {
            unreachable!()
        };

        // Retrieve the information about the implemented trait.
        let implemented_trait = self.translate_trait_ref(span, &trait_pred.trait_ref)?;
        // A `TraitRef` that points to this impl with the correct generics.
        let self_impl_ref = self.translate_trait_impl_ref(span, def.this(), impl_kind)?;
        let self_predicate_kind = TraitRefKind::TraitImpl(self_impl_ref.clone());

        // Build a reference to the original declared method.
        let ItemRef::TraitDecl(tdecl) = self.get_or_translate(implemented_trait.id.into())? else {
            panic!()
        };
        let Some(bound_method) = tdecl.methods.iter().find(|m| m.name() == method_name) else {
            raise_error!(
                self,
                span,
                "Could not find a method with name \
                `{method_name}` in trait `{:?}`",
                trait_pred.trait_ref.def_id,
            )
        };
        let bound_fn_ref: Binder<FunDeclRef> = bound_method
            .clone()
            .substitute_with_self(&implemented_trait.generics, &self_predicate_kind)
            .map(|m| m.item);

        // Now we need to create the generic params for the new method. These are the concatenation
        // of the impl params and the method params. We obtain that by making the two existing
        // binders explicit and using `binder.flatten()`.
        let bound_fn_ref: Binder<Binder<FunDeclRef>> = Binder {
            params: self.outermost_generics().clone(),
            skip_binder: bound_fn_ref,
            kind: BinderKind::Other,
        };
        let bound_fn_ref: Binder<FunDeclRef> = bound_fn_ref.flatten();

        // Create a copy of the provided method with the new generics.
        let original_method_id = bound_fn_ref.skip_binder.id;
        let ItemRef::Fun(fun_decl) = self.get_or_translate(original_method_id.into())? else {
            panic!()
        };
        let mut fun_decl = fun_decl
            .clone()
            .substitute_params(bound_fn_ref.map(|x| *x.generics));

        // Update the rest of the data.
        fun_decl.def_id = def_id;
        // We use the span of the *impl*, not the span of the default method in the
        // original trait declaration.
        fun_decl.item_meta = ItemMeta {
            name: item_meta.name,
            opacity: item_meta.opacity,
            is_local: item_meta.is_local,
            span: item_meta.span,
            source_text: fun_decl.item_meta.source_text,
            attr_info: fun_decl.item_meta.attr_info,
            lang_item: fun_decl.item_meta.lang_item,
        };
        fun_decl.src = if let ItemSource::TraitDecl {
            trait_ref,
            item_name,
            ..
        } = fun_decl.src
        {
            ItemSource::TraitImpl {
                impl_ref: self_impl_ref.clone(),
                trait_ref,
                item_name,
                reuses_default: true,
            }
        } else {
            unreachable!()
        };
        if !item_meta.opacity.is_transparent() {
            fun_decl.body = Body::Opaque;
        }

        Ok(fun_decl)
    }
}
