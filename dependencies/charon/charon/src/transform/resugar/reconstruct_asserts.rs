//! In the MIR AST, it seems `assert` are introduced to check preconditions
//! (for the binops for example). The `assert!` introduced by the user
//! introduce `if ... then { panic!(...) } else { ...}`.
//! This pass introduces `assert` instead in order to make the code shorter.

use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use crate::transform::ctx::UllbcPass;

pub struct Transform;
impl UllbcPass for Transform {
    fn should_run(&self, options: &crate::options::TranslateOptions) -> bool {
        options.reconstruct_asserts
    }

    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        // Start by computing the set of blocks which are actually panics.
        // Remark: doing this in two steps because reading the blocks at random
        // while doing in-place updates is not natural to do in Rust.
        let panics = b.as_abort_map();

        for block in b.body.iter_mut() {
            match &block.terminator.kind {
                TerminatorKind::Switch {
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

                    let kind = std::mem::replace(
                        &mut block.terminator.kind,
                        TerminatorKind::Goto { target: nbid },
                    );
                    let (discr, _) = kind.as_switch().unwrap();
                    block.statements.push(Statement::new(
                        block.terminator.span,
                        StatementKind::Assert {
                            assert: Assert {
                                cond: discr.clone(),
                                expected,
                                check_kind: None,
                            },
                            on_failure: abort.clone(),
                        },
                    ));
                }
                _ => (),
            }
        }
    }
}
