//! Remove the useless no-ops.
use crate::ast::*;
use crate::transform::TransformCtx;

use super::ctx::TransformPass;

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|_ctx, fun| {
            if let Ok(body) = &mut fun.body {
                match body {
                    Body::Unstructured(body) => {
                        for blk in &mut body.body {
                            if blk.statements.iter().any(|st| st.content.is_nop()) {
                                blk.statements.retain(|st| !st.content.is_nop())
                            }
                        }
                    }
                    Body::Structured(body) => {
                        body.body.visit_blocks_bwd(|blk: &mut llbc_ast::Block| {
                            // Remove all the `Nop`s from this sequence.
                            if blk.statements.iter().any(|st| st.content.is_nop()) {
                                blk.statements.retain(|st| !st.content.is_nop())
                            }
                        });
                    }
                }
            }
        });
    }
}
