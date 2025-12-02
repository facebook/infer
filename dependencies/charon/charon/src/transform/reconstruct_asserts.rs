//! In the MIR AST, it seems `assert` are introduced to check preconditions
//! (for the binops for example). The `assert!` introduced by the user
//! introduce `if ... then { panic!(...) } else { ...}`.
//! This pass introduces `assert` instead in order to make the code shorter.

use std::collections::HashMap;

use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use super::ctx::UllbcPass;

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        // Start by computing the set of blocks which are actually panics.
        // Remark: doing this in two steps because reading the blocks at random
        // while doing in-place updates is not natural to do in Rust.
        let panics: HashMap<BlockId, AbortKind> = b
            .body
            .iter_indexed()
            .filter_map(|(bid, block)| {
                if block
                    .statements
                    .iter()
                    .all(|st| st.content.is_storage_live())
                    && let RawTerminator::Abort(abort) = &block.terminator.content
                {
                    Some((bid, abort.clone()))
                } else {
                    None
                }
            })
            .collect();

        for block in b.body.iter_mut() {
            match &block.terminator.content {
                RawTerminator::Switch {
                    discr: _,
                    targets: SwitchTargets::If(bid0, bid1),
                } => {
                    let (nbid, expected, abort) = if let Some(abort) = panics.get(bid0) {
                        (*bid1, false, abort)
                    } else if let Some(abort) = panics.get(bid1) {
                        (*bid0, true, abort)
                    } else {
                        continue;
                    };

                    let content = std::mem::replace(
                        &mut block.terminator.content,
                        RawTerminator::Goto { target: nbid },
                    );
                    let (discr, _) = content.as_switch().unwrap();
                    block.statements.push(Statement::new(
                        block.terminator.span,
                        RawStatement::Assert(Assert {
                            cond: discr.clone(),
                            expected,
                            on_failure: abort.clone(),
                        }),
                    ));
                }
                _ => (),
            }
        }
    }
}
