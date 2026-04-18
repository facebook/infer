//! # Micro-pass: merge single-origin gotos into their parent and skip over blocks that consist of
//! only a goto.
use std::collections::HashSet;
use std::mem;

use crate::ids::IndexVec;
use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use crate::transform::ctx::UllbcPass;

/// Set of antecedents of a given block. We only care about block ids if there's a single
/// antecedent.
enum Antecedents {
    Zero,
    One {
        id: BlockId,
        /// Whether the antecedent is a goto.
        is_goto: bool,
    },
    Many,
}

impl Antecedents {
    fn add(&mut self, id: BlockId, is_goto: bool) {
        match self {
            Antecedents::Zero => *self = Antecedents::One { id, is_goto },
            Antecedents::One { .. } => *self = Antecedents::Many,
            Antecedents::Many => {}
        }
    }
}

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, body: &mut ExprBody) {
        // Compute for each block the set of blocks that points to it.
        let mut antecedents: IndexVec<BlockId, Antecedents> =
            body.body.map_ref(|_| Antecedents::Zero);
        for (block_id, block) in body.body.iter_indexed() {
            let is_goto = block.terminator.kind.is_goto();
            for target in block.targets() {
                antecedents.get_mut(target).unwrap().add(block_id, is_goto);
            }
        }
        // Merge blocks with a single antecedent into their antecedent.
        for mut id in body.body.all_indices() {
            // Go up the chain to find the first parent into which we can merge.
            while let Antecedents::One {
                id: antecedent_id,
                is_goto: true,
            } = antecedents[id]
            {
                id = antecedent_id;
            }
            // While the current block is a straight goto, merge the target block back into this
            // one.
            while let source = &body.body[id]
                && let TerminatorKind::Goto { target } = source.terminator.kind
                && let Antecedents::One { .. } = antecedents[target]
            {
                antecedents[target] = Antecedents::Zero;
                let mut target = mem::replace(&mut body.body[target], BlockData::new_unreachable());
                let source = &mut body.body[id];
                source.statements.append(&mut target.statements);
                source.terminator = target.terminator;
            }
        }
        // Skip over trivial gotos.
        let mut visited = HashSet::new(); // detect and skip loops
        for id in body.body.all_indices() {
            if body.body[id]
                .targets()
                .into_iter()
                .any(|t| body.body[t].as_trivial_goto().is_some())
            {
                // Merge any forward goto chains that start here.
                let mut source = mem::replace(&mut body.body[id], BlockData::new_unreachable());
                for target_id in source.terminator.targets_mut() {
                    visited.clear();
                    visited.insert(id);
                    while let Some(b) = body.body[*target_id].as_trivial_goto()
                        && visited.insert(*target_id)
                    {
                        *target_id = b
                    }
                }
                body.body[id] = source;
            }
        }
    }
}
