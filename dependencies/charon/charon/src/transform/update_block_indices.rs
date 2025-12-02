//! Update the block indices to make sure they are consecutive

use std::mem;

use crate::ids::*;
use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use super::ctx::UllbcPass;

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        // Push each block into a new vector to make it consecutive and return the map from old to
        // new ids.
        let id_map: Vector<BlockId, BlockId> =
            mem::take(&mut b.body).map(|block| b.body.push(block));

        // Update the ids.
        b.body
            .dyn_visit_in_body_mut(|id: &mut BlockId| *id = id_map[*id]);
    }
}
