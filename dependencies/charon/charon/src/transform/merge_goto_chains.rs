//! # Micro-pass: merge single-origin gotos into their parent to reduce CFG graph size.
use crate::ids::Vector;
use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use super::ctx::UllbcPass;

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
    fn transform_body(&self, ctx: &mut TransformCtx, body: &mut ExprBody) {
        // Check the option which instructs to ignore this pass
        if ctx.options.no_merge_goto_chains {
            return;
        }

        // Compute for each block the set of blocks that points to it.
        let mut antecedents: Vector<BlockId, Antecedents> =
            body.body.map_ref(|_| Antecedents::Zero);
        for (block_id, block) in body.body.iter_indexed() {
            let is_goto = block.terminator.content.is_goto();
            for target in block.targets() {
                antecedents.get_mut(target).unwrap().add(block_id, is_goto);
            }
        }
        // Merge blocks with a single antecedent into their antecedent.
        for mut id in body.body.all_indices() {
            // Go up the chain to find the first parent with zero (the start block) or multiple
            // antecedents. This avoids quadratic behavior where we repeatedly copy a growing list
            // of statements, since blocks may not be sorted..
            while let Antecedents::One {
                id: antecedent_id,
                is_goto: true,
            } = antecedents[id]
            {
                id = antecedent_id;
            }
            // While the current block is a straight goto, merge the target block back into this
            // one.
            while let Some(source) = body.body.get(id)
                && let RawTerminator::Goto { target } = source.terminator.content
                && let Antecedents::One { .. } = antecedents[target]
            {
                let mut target = body.body.remove(target).unwrap();
                let source = &mut body.body[id];
                source.statements.append(&mut target.statements);
                source.terminator = target.terminator;
            }
        }
    }
}
