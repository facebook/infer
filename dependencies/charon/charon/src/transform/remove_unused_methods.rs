//! Remove the trait/impl methods that were not translated.
use crate::ast::*;

use super::{TransformCtx, ctx::TransformPass};

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        let method_is_translated = |(_, method): &(TraitItemName, Binder<FunDeclRef>)| {
            ctx.translated
                .fun_decls
                .get(method.skip_binder.id)
                .is_some()
        };
        // Keep only the methods for which we translated the corresponding `FunDecl`. We ensured
        // that this would be translated if the method is used or transparently implemented.
        for tdecl in ctx.translated.trait_decls.iter_mut() {
            tdecl.methods.retain(method_is_translated);
        }
        for timpl in ctx.translated.trait_impls.iter_mut() {
            timpl.methods.retain(method_is_translated);
        }
    }
}
