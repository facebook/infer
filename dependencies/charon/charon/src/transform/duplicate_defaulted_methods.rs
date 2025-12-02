//! Add missing methods to trait impls by duplicating the default method.
use std::{collections::HashMap, mem};

use crate::ast::*;

use super::{TransformCtx, ctx::TransformPass};

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        for impl_id in ctx.translated.trait_impls.all_indices() {
            let Some(timpl) = ctx.translated.trait_impls.get_mut(impl_id) else {
                continue;
            };
            let Some(tdecl) = ctx.translated.trait_decls.get(timpl.impl_trait.id) else {
                continue;
            };
            if tdecl.methods.len() == timpl.methods.len() {
                continue;
            }

            // A `TraitRef` that points to this impl with the correct generics.
            let self_impl_ref = TraitImplRef {
                id: timpl.def_id,
                generics: Box::new(timpl.generics.identity_args()),
            };
            let self_predicate = TraitRef {
                kind: TraitRefKind::TraitImpl(self_impl_ref.clone()),
                trait_decl_ref: RegionBinder::empty(timpl.impl_trait.clone()),
            };
            // Map of methods we already have in the impl.
            let mut methods_map: HashMap<TraitItemName, _> =
                mem::take(&mut timpl.methods).into_iter().collect();
            // Borrow shared to get access to the rest of the crate.
            let timpl = ctx.translated.trait_impls.get(impl_id).unwrap();
            let mut methods = vec![];
            for (name, decl_fn_ref) in &tdecl.methods {
                if let Some(kv) = methods_map.remove_entry(name) {
                    methods.push(kv);
                    continue;
                }
                let declared_fun_id = decl_fn_ref.skip_binder.id;
                let declared_fun_name = ctx.translated.item_name(declared_fun_id).unwrap();
                let new_fun_name = {
                    let mut item_name = timpl.item_meta.name.clone();
                    item_name
                        .name
                        .push(declared_fun_name.name.last().unwrap().clone());
                    item_name
                };
                let opacity = ctx.opacity_for_name(&new_fun_name);
                let new_fun_id = ctx.translated.fun_decls.reserve_slot();
                ctx.translated
                    .item_names
                    .insert(new_fun_id.into(), new_fun_name.clone());

                // Substitute the method reference to be valid in the context of the impl.
                let bound_fn = decl_fn_ref
                    .clone()
                    .substitute_with_self(&timpl.impl_trait.generics, &self_predicate.kind);
                // The new function item has for params the concatenation of impl params and method
                // params. We build a FunDeclRef to this even if we don't end up adding the new
                // function item below.
                let new_fn_ref = Binder {
                    skip_binder: FunDeclRef {
                        id: new_fun_id,
                        generics: Box::new(
                            timpl
                                .generics
                                .identity_args_at_depth(DeBruijnId::one())
                                .concat(
                                    &bound_fn.params.identity_args_at_depth(DeBruijnId::zero()),
                                ),
                        ),
                    },
                    params: bound_fn.params.clone(),
                    kind: bound_fn.kind.clone(),
                };
                methods.push((name.clone(), new_fn_ref));

                if let Some(fun_decl) = ctx.translated.fun_decls.get(declared_fun_id)
                    && !opacity.is_invisible()
                {
                    let bound_fn = Binder {
                        params: timpl.generics.clone(),
                        skip_binder: bound_fn,
                        kind: BinderKind::Other,
                    };
                    // Flatten into a single binder level. This gives us the concatenated
                    // parameters that we'll use for the new function item, and the arguments to
                    // pass to the old function item.
                    let bound_fn = bound_fn.flatten();
                    // Create a copy of the provided method and update all the relevant data.
                    let FunDecl {
                        def_id: _,
                        item_meta,
                        signature,
                        kind,
                        is_global_initializer,
                        body,
                    } = fun_decl.clone();
                    // We use the span of the *impl*, not the span of the
                    // default method in the original trait declaration.
                    let span = timpl.item_meta.span;
                    let item_meta = ItemMeta {
                        name: new_fun_name,
                        is_local: timpl.item_meta.is_local,
                        opacity,
                        span,
                        ..item_meta
                    };
                    let signature = FunSig {
                        generics: bound_fn.params,
                        inputs: signature.inputs.substitute_with_self(
                            &bound_fn.skip_binder.generics,
                            &self_predicate.kind,
                        ),
                        output: signature.output.substitute_with_self(
                            &bound_fn.skip_binder.generics,
                            &self_predicate.kind,
                        ),
                        ..signature
                    };
                    let kind = if let ItemKind::TraitDecl {
                        trait_ref,
                        item_name,
                        ..
                    } = kind
                    {
                        ItemKind::TraitImpl {
                            impl_ref: self_impl_ref.clone(),
                            trait_ref: trait_ref.substitute_with_self(
                                &bound_fn.skip_binder.generics,
                                &self_predicate.kind,
                            ),
                            item_name,
                            reuses_default: true,
                        }
                    } else {
                        unreachable!()
                    };
                    let body = if opacity.is_transparent() {
                        body.substitute_with_self(
                            &bound_fn.skip_binder.generics,
                            &self_predicate.kind,
                        )
                    } else {
                        Err(Opaque)
                    };
                    ctx.translated.fun_decls.set_slot(
                        new_fun_id,
                        FunDecl {
                            def_id: new_fun_id,
                            item_meta,
                            signature,
                            kind,
                            is_global_initializer,
                            body,
                        },
                    );
                }
            }
            let timpl = ctx.translated.trait_impls.get_mut(impl_id).unwrap();
            timpl.methods = methods;
        }
    }
}
