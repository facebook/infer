//! The MIR uses a unique `return` node, which can be an issue when reconstructing
//! the control-flow.
//!
//! For instance, it often leads to code of the following shape:
//! ```text
//! if b {
//!   ...
//!   x = 0;
//! }
//! else {
//!   ...
//!   x = 1;
//! }
//! return x;
//! ```
//!
//! while a more natural reconstruction would be:
//! ```text
//! if b {
//!   ...
//!   return 0;
//! }
//! else {
//!   ...
//!   return 1;
//! }
//! ```

use crate::ids::Generator;
use crate::transform::TransformCtx;
use crate::ullbc_ast::*;
use std::collections::HashMap;

use super::ctx::UllbcPass;

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        // Find the return block id (there should be one).
        let returns: HashMap<BlockId, Span> = b
            .body
            .iter_indexed()
            .filter_map(|(bid, block)| {
                if block.statements.is_empty() && block.terminator.content.is_return() {
                    Some((bid, block.terminator.span))
                } else {
                    None
                }
            })
            .collect();

        // Whenever we find a goto the return block, introduce an auxiliary block
        // for this (remark: in the end, it makes the return block dangling).
        // We do this in two steps.
        // First, introduce fresh ids.
        let mut generator = Generator::new_with_init_value(b.body.next_id());
        let mut new_spans = Vec::new();
        b.body.dyn_visit_in_body_mut(|bid: &mut BlockId| {
            if let Some(span) = returns.get(bid) {
                *bid = generator.fresh_id();
                new_spans.push(*span);
            }
        });

        // Then introduce the new blocks
        for span in new_spans {
            let _ = b.body.push(BlockData {
                statements: Vec::new(),
                terminator: Terminator::new(span, RawTerminator::Return),
            });
        }
    }
}
