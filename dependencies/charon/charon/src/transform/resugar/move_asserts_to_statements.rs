//! # Micro-pass: if we want to reconstruct fallible operations, the first step
//! is to move all assert terminators that correspond do dynamic checks into
//! statements, to make them easier to recognize and transform in the next pass.
//! It's important this pass runs before [merge_goto_chains], to ensure the gotos
//! we substitute the asserts with get merged, but before [reconstruct_fallible_operations],
//! since it will expect the asserts to be in statements.

use crate::ast::*;
use crate::transform::TransformCtx;
use crate::ullbc_ast::{ExprBody, Statement, StatementKind, TerminatorKind};

use crate::transform::ctx::UllbcPass;

pub struct Transform;
impl UllbcPass for Transform {
    fn should_run(&self, options: &crate::options::TranslateOptions) -> bool {
        options.reconstruct_fallible_operations
    }

    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        // Start by computing the set of blocks which are actually panics.
        // Remark: doing this in two steps because reading the blocks at random
        // while doing in-place updates is not natural to do in Rust.
        let panics = b.as_abort_map();

        for block in b.body.iter_mut() {
            let TerminatorKind::Assert {
                assert:
                    Assert {
                        check_kind: Some(_),
                        ..
                    },
                target,
                ..
            } = &block.terminator.kind
            else {
                continue;
            };

            let new_terminator = TerminatorKind::Goto { target: *target };
            let old_terminator = std::mem::replace(&mut block.terminator.kind, new_terminator);
            let TerminatorKind::Assert {
                assert, on_unwind, ..
            } = old_terminator
            else {
                unreachable!();
            };

            let on_failure = panics
                .get(&on_unwind)
                .map(AbortKind::clone)
                .unwrap_or(AbortKind::Panic(None));

            block.statements.push(Statement {
                kind: StatementKind::Assert { assert, on_failure },
                span: block.terminator.span,
                comments_before: block.terminator.comments_before.clone(),
            });
        }
    }
}
