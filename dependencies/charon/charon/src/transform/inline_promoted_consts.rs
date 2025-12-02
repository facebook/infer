use std::{collections::HashMap, mem};

use super::{TransformCtx, ctx::UllbcPass};
use crate::{ids::Generator, ullbc_ast::*};

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        // Currently the only anon consts that are not already evaluated are promoted consts. If
        // that changes, we'll have to restrict this pass to the consts that can be inlined into a
        // body.

        // Map each anon const id to its initializer, and remove both from `translated`.
        let anon_consts: HashMap<GlobalDeclId, ExprBody> = ctx
            .translated
            .global_decls
            .extract(|gdecl| matches!(gdecl.global_kind, GlobalKind::AnonConst))
            .filter_map(|(id, gdecl)| {
                let fdecl = ctx.translated.fun_decls.remove(gdecl.init)?;
                let body = fdecl.body.ok()?;
                let body = body.to_unstructured()?;
                Some((id, body))
            })
            .collect();

        ctx.for_each_fun_decl(|_ctx, decl| {
            if let Ok(outer_body) = &mut decl.body {
                let outer_body = outer_body.as_unstructured_mut().unwrap();
                for block_id in outer_body.body.all_indices() {
                    // Subtle: This generator must be managed to correctly track the indices that will
                    // be generated when pushing onto `outer_body.body`.
                    let mut bid_generator =
                        Generator::new_with_init_value(outer_body.body.next_id());
                    let start_new_bodies = bid_generator.next_id();
                    let Some(block) = outer_body.body.get_mut(block_id) else {
                        continue;
                    };
                    let mut new_blocks = vec![];
                    block.dyn_visit_in_body_mut(|op: &mut Operand| {
                        if let Operand::Const(c) = op
                            && let RawConstantExpr::Global(gref) = &mut c.value
                            && let Some(inner_body) = anon_consts.get(&gref.id)
                        {
                            // We inline the required body by shifting its local ids and block ids
                            // and adding its blocks to the outer body. The inner body's return
                            // local becomes a normal local that we can read from. We redirect some
                            // gotos so that the inner body is executed before the current block.
                            let mut inner_body = inner_body.clone().substitute(&gref.generics);

                            let return_local = outer_body.locals.locals.next_id();
                            inner_body.dyn_visit_in_body_mut(|l: &mut LocalId| {
                                *l += return_local;
                            });

                            let start_block = bid_generator.next_id();
                            bid_generator.advance(inner_body.body.elem_count());
                            let end_block = bid_generator.next_id();
                            inner_body.dyn_visit_in_body_mut(|b: &mut BlockId| {
                                *b += start_block;
                            });
                            // Make all returns point to `end_block`. This block doesn't exist yet,
                            // it will either be the start block of another inner body, or the
                            // current outer block that we'll push at the end.
                            inner_body.body.dyn_visit_in_body_mut(|t: &mut Terminator| {
                                if let RawTerminator::Return = t.content {
                                    t.content = RawTerminator::Goto { target: end_block };
                                }
                            });

                            outer_body
                                .locals
                                .locals
                                .extend(inner_body.locals.locals.into_iter());
                            new_blocks.extend(inner_body.body);
                            *op = Operand::Move(outer_body.locals.place_for_var(return_local));
                        }
                    });
                    if !new_blocks.is_empty() {
                        // Instead of the current block, start evaluating the new bodies.
                        let block = mem::replace(
                            block,
                            BlockData::new_goto(Span::dummy(), start_new_bodies),
                        );
                        // Add the new blocks. They've been set up so that each new inner body
                        // returns to what follows it in the sequence. Hence the last added body
                        // points to the not-yet-existing block at `start_new_bodies`
                        outer_body.body.extend(new_blocks.into_iter());
                        // Push the current block to be executed after the newly-added ones.
                        outer_body.body.push(block);
                    }
                }
            }
        });
    }
}
