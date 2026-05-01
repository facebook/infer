use derive_generic_visitor::Visitor;

use crate::transform::ctx::TransformPass;
use crate::{register_error, transform::TransformCtx, ullbc_ast::*};

#[derive(Visitor)]
struct NormalizeFnPtr<'a> {
    ctx: &'a TransformCtx,
    span: Span,
}

impl VisitorWithSpan for NormalizeFnPtr<'_> {
    fn current_span(&mut self) -> &mut Span {
        &mut self.span
    }
}

impl VisitAstMut for NormalizeFnPtr<'_> {
    fn visit<'a, T: AstVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
        // Track a useful enclosing span, for error messages.
        VisitWithSpan::new(self).visit(x)
    }

    fn enter_fn_ptr(&mut self, fn_ptr: &mut FnPtr) {
        transform_fn_ptr(self.ctx, self.span, fn_ptr);
    }
}

fn transform_fn_ptr(ctx: &TransformCtx, span: Span, fn_ptr: &mut FnPtr) {
    // We find references to a trait method where the impl is known; otherwise we return.
    let FnPtrKind::Trait(trait_ref, name, _) = fn_ptr.kind.as_ref() else {
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
    fn_ptr.kind = Box::new(FnPtrKind::Fun(FunId::Regular(fn_ref.id)));
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_item_mut(|ctx, mut item| {
            let _ = item.drive_mut(&mut NormalizeFnPtr {
                ctx,
                span: Span::dummy(),
            });
        })
    }
}
