use crate::translate::{translate_bodies::BodyTransCtx, translate_crate::TransItemSourceKind};

use super::translate_ctx::*;
use charon_lib::ast::*;
use hax_frontend_exporter::{self as hax, FullDefKind};

impl ItemTransCtx<'_, '_> {
    fn translate_drop_method_body(
        &mut self,
        span: Span,
        def: &hax::FullDef,
    ) -> Result<Result<Body, Opaque>, Error> {
        let (hax::FullDefKind::Adt { drop_glue, .. } | hax::FullDefKind::Closure { drop_glue, .. }) =
            def.kind()
        else {
            panic!("Unexpected def adt: {def:?}")
        };
        let Some(body) = drop_glue else {
            return Ok(Err(Opaque));
        };

        let mut bt_ctx = BodyTransCtx::new(self);
        Ok(match bt_ctx.translate_body(span, body, &def.source_text) {
            Ok(Ok(body)) => Ok(body),
            Ok(Err(Opaque)) => Err(Opaque),
            Err(_) => Err(Opaque),
        })
    }

    /// Given an item that is a closure, generate the `call_once`/`call_mut`/`call` method
    /// (depending on `target_kind`).
    #[tracing::instrument(skip(self, item_meta))]
    pub fn translate_drop_method(
        mut self,
        def_id: FunDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<FunDecl, Error> {
        let span = item_meta.span;

        self.translate_def_generics(span, def)?;

        let (FullDefKind::Adt { drop_impl, .. } | FullDefKind::Closure { drop_impl, .. }) =
            def.kind()
        else {
            unreachable!()
        };
        let implemented_trait = self.translate_trait_predicate(span, &drop_impl.trait_pred)?;
        let self_ty = implemented_trait
            .self_ty(&self.t_ctx.translated)
            .unwrap()
            .clone();
        let drop_impl_id = self.register_item(
            span,
            def.this(),
            TransItemSourceKind::TraitImpl(TraitImplSource::DropGlue),
        );
        let impl_ref = TraitImplRef {
            id: drop_impl_id,
            generics: Box::new(self.the_only_binder().params.identity_args()),
        };

        let kind = ItemKind::TraitImpl {
            impl_ref,
            trait_ref: implemented_trait,
            item_name: TraitItemName("drop".to_owned()),
            reuses_default: false,
        };

        // Add the method lifetime generic.
        assert!(self.innermost_binder_mut().bound_region_vars.is_empty());
        let region_id = self
            .innermost_binder_mut()
            .push_bound_region(hax::BoundRegionKind::Anon);

        let body = if item_meta.opacity.with_private_contents().is_opaque() {
            Err(Opaque)
        } else {
            self.translate_drop_method_body(span, def)?
        };

        let input = TyKind::Ref(
            Region::Var(DeBruijnVar::new_at_zero(region_id)),
            self_ty,
            RefKind::Mut,
        )
        .into_ty();
        let signature = FunSig {
            generics: self.into_generics(),
            is_unsafe: false,
            inputs: vec![input],
            output: Ty::mk_unit(),
        };

        Ok(FunDecl {
            def_id,
            item_meta,
            signature,
            kind,
            is_global_initializer: None,
            body,
        })
    }

    #[tracing::instrument(skip(self, item_meta))]
    pub fn translate_drop_impl(
        mut self,
        impl_id: TraitImplId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<TraitImpl, Error> {
        let span = item_meta.span;

        self.translate_def_generics(span, def)?;

        let (FullDefKind::Adt { drop_impl, .. } | FullDefKind::Closure { drop_impl, .. }) =
            def.kind()
        else {
            unreachable!()
        };
        let mut timpl = self.translate_virtual_trait_impl(impl_id, item_meta, drop_impl)?;

        // Construct the method reference.
        let method_id = self.register_item(span, def.this(), TransItemSourceKind::DropGlueMethod);
        let method_name = TraitItemName("drop".to_owned());
        let method_binder = {
            let mut method_params = GenericParams::empty();
            method_params
                .regions
                .push_with(|index| RegionVar { index, name: None });

            let generics = self
                .outermost_binder()
                .params
                .identity_args_at_depth(DeBruijnId::one())
                .concat(&method_params.identity_args_at_depth(DeBruijnId::zero()));
            Binder::new(
                BinderKind::TraitMethod(timpl.impl_trait.id, method_name.clone()),
                method_params,
                FunDeclRef {
                    id: method_id,
                    generics: Box::new(generics),
                },
            )
        };
        timpl.methods.push((method_name, method_binder));

        Ok(timpl)
    }
}
