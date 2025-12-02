//! We cannot filter trait impls by name before translating them, because we need to know the
//! trait/type pair that is being implemented. We therefore filter them in a post-processing pass.
use std::collections::HashSet;

use crate::ast::*;

use super::{TransformCtx, ctx::TransformPass};

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        let exclude: HashSet<TraitImplId> = ctx
            .translated
            .trait_impls
            .iter()
            .filter(|timpl| ctx.opacity_for_name(&timpl.item_meta.name).is_invisible())
            .map(|timpl| timpl.def_id)
            .collect();

        for id in exclude {
            ctx.translated.trait_impls.remove(id);
        }
    }
}
