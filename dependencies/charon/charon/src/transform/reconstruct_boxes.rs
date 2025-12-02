//! # Micro-pass: reconstruct piecewise box allocations using `malloc` and `ShallowInitBox`.

use crate::register_error;
use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use super::ctx::UllbcPass;

pub struct Transform;

/// The special `alloc::boxed::box_new(x)` intrinsic becomes the following:
///
/// ```text
/// @2 := size_of<i32>
/// @3 := align_of<i32>
/// @4 := alloc::alloc::exchange_malloc(move (@2), move (@3))
/// storage_live(@5)
/// @5 := shallow_init_box::<i32>(move (@4))
/// // possibly some intermediate statements
/// *(@5) := x
/// ```
///
/// We reconstruct this into a call to `Box::new(x)`.
impl UllbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, b: &mut ExprBody) {
        if ctx.options.raw_boxes {
            return;
        }

        // We need to find a block that has exchange_malloc as the following terminator:
        // ```text
        // @4 := alloc::alloc::exchange_malloc(move (@2), move (@3))
        // ```
        // We then chekc that that this block ends with two assignments:
        // ```text
        // @2 := size_of<i32>
        // @3 := align_of<i32>
        // ```
        // If that is the case, we look at the target block and check that it starts with`
        // ```text
        // storage_live(@5)
        // @5 := shallow_init_box::<i32>(move (@4))
        // ```
        // We then look for the assignment into the box and take a not of its index.
        // ```text
        // *(@5) := x
        // ```
        // Finally, we replace all these assignments with a call to `@5 = Box::new(x)`
        // We do so by replacing the terminator (exchange_malloc) with the correct call
        // and replacing the assignment @3 := align_of<i32> with the storage live.
        // Everything else becomes Nop.

        for candidate_block_idx in b.body.all_indices() {
            let second_block;
            let at_5;
            let at_5_ty;
            let box_generics;
            let value_to_write;
            let old_assign_idx;
            let assign_span;
            let unwind_target;

            if let Some(candidate_block) = b.body.get(candidate_block_idx)
                // If the terminator is a call
                && let RawTerminator::Call {
                    target: target_block_idx,
                    call:
                        Call {
                            args: malloc_args,
                            func: _, // TODO: once we have a system to recognize intrinsics, check the call is to exchange_malloc.
                            dest: malloc_dest,
                        },
                        on_unwind,
                } = &candidate_block.terminator.content
                // The call has two move arguments
                && let [Operand::Move(arg0), Operand::Move(arg1)] = malloc_args.as_slice()
                && let [ .., Statement {
                            content: RawStatement::Assign(size, Rvalue::NullaryOp(NullOp::SizeOf, _)),
                            ..
                        }, Statement {
                            content: RawStatement::Assign(align, Rvalue::NullaryOp(NullOp::AlignOf, _)),
                            ..
                        }] = candidate_block.statements.as_slice()
                && arg0 == size && arg1 == align
                && let Some(target_block) = b.body.get(*target_block_idx)
                && let [Statement {
                            content: RawStatement::StorageLive(target_var),
                            ..
                        }, Statement {
                            content:
                                RawStatement::Assign(box_make, Rvalue::ShallowInitBox(Operand::Move(alloc_use), _)),
                            ..
                        }, rest @ ..] = target_block.statements.as_slice()
                && alloc_use == malloc_dest
                && let Some(local_id) = box_make.as_local()
                && local_id == *target_var
                && let TyKind::Adt(ty_ref) = b.locals[*target_var].ty.kind()
                && let TypeId::Builtin(BuiltinTy::Box) = ty_ref.id
                && let Some((assign_idx_in_rest, val, span)) = rest.iter().enumerate().find_map(|(idx, st)| {
                    if let Statement {
                            content: RawStatement::Assign(box_deref, val),
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
                at_5 = local_id;
                at_5_ty = box_make.ty().clone();
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
            let number_statements = first_block.statements.len();
            let value_to_write = match value_to_write {
                Rvalue::Use(op) => {
                    first_block
                        .statements
                        .get_mut(number_statements - 2)
                        .unwrap()
                        .content = RawStatement::Nop;
                    op
                }
                _ => {
                    // We need to create a new variable to store the value.
                    let name = b.locals[at_5].name.clone();
                    let ty = box_generics.types[0].clone();
                    let var = b.locals.new_var(name, ty);
                    let st = Statement::new(
                        assign_span,
                        RawStatement::Assign(var.clone(), value_to_write),
                    );
                    // We overide the @2 := size_of<i32> statement with the rvalue assignment
                    *first_block
                        .statements
                        .get_mut(number_statements - 2)
                        .unwrap() = st;
                    Operand::Move(var)
                }
            };
            first_block
                .statements
                .get_mut(number_statements - 1)
                .unwrap()
                .content = RawStatement::StorageLive(at_5);
            first_block.terminator.content = RawTerminator::Call {
                call: Call {
                    func: FnOperand::Regular(FnPtr {
                        func: Box::new(FunIdOrTraitMethodRef::Fun(FunId::Builtin(
                            BuiltinFunId::BoxNew,
                        ))),
                        generics: box_generics,
                    }),
                    args: vec![value_to_write],
                    dest: Place::new(at_5, at_5_ty),
                },
                target: second_block,
                on_unwind: unwind_target,
            };

            // We now update the statements in the second block.
            let second_block = b.body.get_mut(second_block).unwrap();
            second_block.statements.get_mut(0).unwrap().content = RawStatement::Nop;
            second_block.statements.get_mut(1).unwrap().content = RawStatement::Nop;
            second_block
                .statements
                .get_mut(old_assign_idx)
                .unwrap()
                .content = RawStatement::Nop;
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
