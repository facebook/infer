//! Some passes like [`crate::transform::reconstruct_asserts`] lead to the apparition of "dangling" blocks,
//! which are referenced nowhere and thus become unreachable. This pass filters those out.

use std::collections::{HashMap, HashSet};

use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use super::ctx::UllbcPass;

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        // Perform a depth-first search to identify all the blocks reachable
        // from the first block.
        let mut explored: HashSet<BlockId> = HashSet::new();
        let mut to_explore: Vec<BlockId> = vec![BlockId::from_usize(0)];
        while let Some(bid) = to_explore.pop() {
            if explored.contains(&bid) {
                continue;
            }
            explored.insert(bid);
            to_explore.append(&mut b.body[bid].targets());
        }

        // Renumerotate
        let mut bid_map: HashMap<BlockId, BlockId> = HashMap::new();
        for (bid, block) in std::mem::take(&mut b.body).into_iter_indexed() {
            if explored.contains(&bid) {
                let nbid = b.body.push(block);
                bid_map.insert(bid, nbid);
            }
        }

        // Update all block ids
        b.body.dyn_visit_in_body_mut(|bid: &mut BlockId| {
            *bid = *bid_map.get(bid).unwrap();
        });
    }
}
