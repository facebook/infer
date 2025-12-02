use crate::{register_error, transform::TransformCtx, ullbc_ast::*};

use super::ctx::UllbcPass;

fn transform_call(ctx: &mut TransformCtx, span: Span, call: &mut Call) {
    // We find calls to a trait method where the impl is known; otherwise we return.
    let FnOperand::Regular(fn_ptr) = &mut call.func else {
        return;
    };
    let FunIdOrTraitMethodRef::Trait(trait_ref, name, _) = fn_ptr.func.as_ref() else {
        return;
    };
    let TraitRefKind::TraitImpl(impl_ref) = &trait_ref.kind else {
        return;
    };
    let Some(trait_impl) = &ctx.translated.trait_impls.get(impl_ref.id) else {
        return;
    };
    // Find the function declaration corresponding to this impl.
    let Some((_, bound_fn)) = trait_impl.methods().find(|(n, _)| n == name) else {
        return;
    };
    let method_generics = &fn_ptr.generics;

    if !method_generics.matches(&bound_fn.params) {
        register_error!(
            ctx,
            span,
            "Mismatched method generics:\nparams:   {:?}\nsupplied: {:?}",
            bound_fn.params,
            method_generics
        );
    }

    // Make the two levels of binding explicit: outer binder for the impl block, inner binder for
    // the method.
    let fn_ref: Binder<Binder<FunDeclRef>> = Binder::new(
        BinderKind::Other,
        trait_impl.generics.clone(),
        bound_fn.clone(),
    );
    // Substitute the appropriate generics into the function call.
    let fn_ref = fn_ref.apply(&impl_ref.generics).apply(method_generics);
    fn_ptr.generics = fn_ref.generics;
    fn_ptr.func = Box::new(FunIdOrTraitMethodRef::Fun(FunId::Regular(fn_ref.id)));
}

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, b: &mut ExprBody) {
        for block in b.body.iter_mut() {
            if let RawTerminator::Call { call, .. } = &mut block.terminator.content {
                transform_call(ctx, block.terminator.span, call)
            }
        }
    }
}
