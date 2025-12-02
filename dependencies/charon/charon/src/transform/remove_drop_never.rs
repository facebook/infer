//! The MIR code often contains variables with type `!` that come from `panic!`s and similar
//! `!`-returning` functions.
//!
//! We want to get rid of these variables since they are never initialized. The only instruction
//! that uses them is `StorageLive`/`StorageDead`; we remove these, which may hide some UB but
//! shouldn't have other consequences.
//!
//! The now-unused local will then be removed in `remove_unused_locals`.
use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use super::ctx::UllbcPass;

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        let locals = b.locals.clone();
        b.visit_statements(|st: &mut Statement| {
            // Remove any `Storage{Live,Dead}(x)` where `x` has type `!`. Otherwise leave it unchanged.
            if let RawStatement::StorageLive(var_id) | RawStatement::StorageDead(var_id) =
                &st.content
                && locals[*var_id].ty.is_never()
            {
                st.content = RawStatement::Nop;
            }
        });
    }
}
