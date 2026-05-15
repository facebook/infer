//! # Micro-pass: reconstruct piecewise box allocations using `malloc` and `ShallowInitBox`.

use crate::register_error;
use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use crate::transform::ctx::UllbcPass;

pub struct Transform;

/// The special `alloc::boxed::box_new(x)` intrinsic becomes the following:
///
/// ```text
/// @4 := alloc::alloc::exchange_malloc(const .., const ..)
/// storage_live(@5)
/// @5 := shallow_init_box::<i32>(move (@4))
/// // possibly some intermediate statements
/// *(@5) := x
/// ```
///
/// We reconstruct this into a call to `Box::new(x)`.
impl UllbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, b: &mut ExprBody) {
        if !ctx.options.treat_box_as_builtin {
            return;
        }

        // We need to find a block that has exchange_malloc as the terminator:
        // ```text
        // @4 := alloc::alloc::exchange_malloc(..)
        // ```
        // We then check that that the target block starts with:
        // ```text
        // storage_live(@5)
        // @5 := shallow_init_box::<i32>(move (@4))
        // ```
        // We then look for the assignment into the box and take a note of its index.
        // ```text
        // *(@5) := x
        // ```
        // Finally, we replace all these assignments with a call to `@5 = Box::new(x)`
        // We do so by replacing the terminator (exchange_malloc) with the correct call and adding
        // a `StorageLive`. Everything else becomes Nop.

        for candidate_block_idx in b.body.all_indices() {
            let second_block;
            let box_place;
            let box_generics;
            let value_to_write;
            let old_assign_idx;
            let assign_span;
            let unwind_target;

            if let Some(candidate_block) = b.body.get(candidate_block_idx)
                // If the terminator is a call
                && let TerminatorKind::Call {
                    target: target_block_idx,
                    call:
                        Call {
                            args: malloc_args,
                            func: _, // TODO: once we have a system to recognize intrinsics, check the call is to exchange_malloc.
                            dest: malloc_dest,
                        },
                        on_unwind,
                } = &candidate_block.terminator.kind
                // The call has two const arguments
                && let [Operand::Const(..), Operand::Const(..)] = malloc_args.as_slice()
                && let Some(target_block) = b.body.get(*target_block_idx)
                && let [Statement {
                            kind: StatementKind::StorageLive(target_var),
                            ..
                        }, Statement {
                            kind:
                                StatementKind::Assign(box_make, Rvalue::ShallowInitBox(Operand::Move(alloc_use), _)),
                            ..
                        }, rest @ ..] = target_block.statements.as_slice()
                && alloc_use == malloc_dest
                && let Some(local_id) = box_make.as_local()
                && local_id == *target_var
                && let TyKind::Adt(ty_ref) = box_make.ty().kind()
                && let TypeId::Builtin(BuiltinTy::Box) = ty_ref.id
                && let Some((assign_idx_in_rest, val, span)) = rest.iter().enumerate().find_map(|(idx, st)| {
                    if let Statement {
                            kind: StatementKind::Assign(box_deref, val),
                            span,
                            ..
                        } = st
                        && let Some((sub, ProjectionElem::Deref)) = box_deref.as_projection()
                        && sub == box_make
                    {
                        Some((idx, val, span))
                    } else {
                        None
                    }
                })
            {
                box_place = box_make.clone();
                old_assign_idx = assign_idx_in_rest + 2; // +2 because rest skips the first two statements
                value_to_write = val.clone();
                box_generics = ty_ref.generics.clone();
                second_block = *target_block_idx;
                assign_span = *span;
                unwind_target = *on_unwind;
            } else {
                continue;
            }

            let first_block = b.body.get_mut(candidate_block_idx).unwrap();
            let box_place_local = box_place.as_local().unwrap();
            let value_to_write = match value_to_write {
                Rvalue::Use(op) => op,
                _ => {
                    // We need to create a new variable to store the value.
                    let name = b.locals[box_place_local].name.clone();
                    let ty = box_generics.types[0].clone();
                    let var = b.locals.new_var(name, ty);
                    first_block.statements.push(Statement::new(
                        assign_span,
                        StatementKind::StorageLive(var.as_local().unwrap()),
                    ));
                    first_block.statements.push(Statement::new(
                        assign_span,
                        StatementKind::Assign(var.clone(), value_to_write),
                    ));
                    Operand::Move(var)
                }
            };
            first_block.statements.push(Statement::new(
                assign_span,
                StatementKind::StorageLive(box_place_local),
            ));
            first_block.terminator.kind = TerminatorKind::Call {
                call: Call {
                    func: FnOperand::Regular(FnPtr::new(
                        FnPtrKind::Fun(FunId::Builtin(BuiltinFunId::BoxNew)),
                        box_generics,
                    )),
                    args: vec![value_to_write],
                    dest: box_place,
                },
                target: second_block,
                on_unwind: unwind_target,
            };

            // We now update the statements in the second block.
            let second_block = b.body.get_mut(second_block).unwrap();
            second_block.statements.get_mut(0).unwrap().kind = StatementKind::Nop;
            second_block.statements.get_mut(1).unwrap().kind = StatementKind::Nop;
            second_block
                .statements
                .get_mut(old_assign_idx)
                .unwrap()
                .kind = StatementKind::Nop;
        }

        // Make sure we got all the `ShallowInitBox`es.
        b.body.dyn_visit_in_body(|rvalue: &Rvalue| {
            if rvalue.is_shallow_init_box() {
                register_error!(
                    ctx,
                    b.span,
                    "Could not reconstruct `Box` initialization; \
                    branching during `Box` initialization is not supported."
                );
            }
        });
    }
}
