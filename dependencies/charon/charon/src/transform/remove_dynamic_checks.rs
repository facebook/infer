//! # Micro-pass: remove the dynamic checks for array/slice bounds, overflow, and division by zero.
//! Note that from a semantic point of view, an out-of-bound access or a division by zero
//! must lead to a panic in Rust (which is why those checks are always present, even when
//! compiling for release). In our case, we take this into account in the semantics of our
//! array/slice manipulation and arithmetic functions, on the verification side.

use derive_generic_visitor::*;

use crate::ast::*;
use crate::transform::TransformCtx;
use crate::ullbc_ast::{ExprBody, RawStatement, Statement};

use super::ctx::UllbcPass;

/// Whether the value uses the given local in a place.
fn uses_local<T: BodyVisitable>(x: &T, local: LocalId) -> bool {
    struct FoundIt;
    struct UsesLocalVisitor(LocalId);

    impl Visitor for UsesLocalVisitor {
        type Break = FoundIt;
    }
    impl VisitBody for UsesLocalVisitor {
        fn visit_place(&mut self, x: &Place) -> ::std::ops::ControlFlow<Self::Break> {
            if let Some(local_id) = x.as_local() {
                if local_id == self.0 {
                    return ControlFlow::Break(FoundIt);
                }
            }
            self.visit_inner(x)
        }
    }

    x.drive_body(&mut UsesLocalVisitor(local)).is_break()
}

fn make_binop_overflow_panic<T: BodyVisitable>(
    x: &mut [T],
    matches: impl Fn(&BinOp, &Operand, &Operand) -> bool,
) -> bool {
    let mut found = false;
    for y in x.iter_mut() {
        y.dyn_visit_in_body_mut(|rv: &mut Rvalue| {
            if let Rvalue::BinaryOp(binop, op_l, op_r) = rv
                && matches(binop, op_l, op_r)
            {
                *binop = binop.with_overflow(OverflowMode::Panic);
                found = true;
            }
        });
    }
    found
}

fn make_unop_overflow_panic<T: BodyVisitable>(
    x: &mut [T],
    matches: impl Fn(&UnOp, &Operand) -> bool,
) -> bool {
    let mut found = false;
    for y in x.iter_mut() {
        y.dyn_visit_in_body_mut(|rv: &mut Rvalue| {
            if let Rvalue::UnaryOp(unop, op) = rv
                && matches(unop, op)
            {
                *unop = unop.with_overflow(OverflowMode::Panic);
                found = true;
            }
        });
    }
    found
}

/// Check if the two operands are equivalent: either they're the same constant, or they represent
/// the same place (regardless of whether the operand is a move or a copy)
fn equiv_op(op_l: &Operand, op_r: &Operand) -> bool {
    match (op_l, op_r) {
        (Operand::Copy(l) | Operand::Move(l), Operand::Copy(r) | Operand::Move(r)) => l == r,
        (Operand::Const(l), Operand::Const(r)) => l == r,
        _ => false,
    }
}

/// Rustc inserts dynamic checks during MIR lowering. They all end in an `Assert` statement (and
/// this is the only use of this statement).
fn remove_dynamic_checks(
    _ctx: &mut TransformCtx,
    locals: &mut Locals,
    statements: &mut [Statement],
) {
    // We return the statements we want to keep, which must be a prefix of `block.statements`.
    let statements_to_keep = match statements {
        // Bounds checks for slices. They look like:
        //   l := ptr_metadata(copy a)
        //   b := copy x < copy l
        //   assert(move b == true)
        [
            Statement {
                content:
                    RawStatement::Assign(len, Rvalue::UnaryOp(UnOp::PtrMetadata, Operand::Copy(len_op))),
                ..
            },
            Statement {
                content:
                    RawStatement::Assign(
                        is_in_bounds,
                        Rvalue::BinaryOp(BinOp::Lt, _, Operand::Copy(lt_op2)),
                    ),
                ..
            },
            Statement {
                content:
                    RawStatement::Assert(Assert {
                        cond: Operand::Move(cond),
                        expected: true,
                        ..
                    }),
                ..
            },
            rest @ ..,
        ] if lt_op2 == len && cond == is_in_bounds && len_op.ty().is_ref() => rest,
        // Sometimes that instead looks like:
        //   a := &raw const *z
        //   l := ptr_metadata(move a)
        //   b := copy x < copy l
        //   assert(move b == true)
        [
            Statement {
                content: RawStatement::Assign(reborrow, Rvalue::RawPtr(_, RefKind::Shared)),
                ..
            },
            Statement {
                content:
                    RawStatement::Assign(len, Rvalue::UnaryOp(UnOp::PtrMetadata, Operand::Move(len_op))),
                ..
            },
            Statement {
                content:
                    RawStatement::Assign(
                        is_in_bounds,
                        Rvalue::BinaryOp(BinOp::Lt, _, Operand::Copy(lt_op2)),
                    ),
                ..
            },
            Statement {
                content:
                    RawStatement::Assert(Assert {
                        cond: Operand::Move(cond),
                        expected: true,
                        ..
                    }),
                ..
            },
            rest @ ..,
        ] if reborrow == len_op && lt_op2 == len && cond == is_in_bounds => rest,

        // Zero checks for division and remainder. They look like:
        //   b := copy y == const 0
        //   assert(move b == false)
        //   ...
        //   res := x {/,%} move y;
        //   ... or ...
        //   b := const y == const 0
        //   assert(move b == false)
        //   ...
        //   res := x {/,%} const y;
        //
        // This also overlaps with overflow checks for negation, which looks like:
        //   is_min := x == INT::min
        //   assert(move is_min == false)
        //   ...
        //   res := -x;
        [
            Statement {
                content:
                    RawStatement::Assign(
                        is_zero,
                        Rvalue::BinaryOp(BinOp::Eq, y_op, Operand::Const(_zero)),
                    ),
                ..
            },
            Statement {
                content:
                    RawStatement::Assert(Assert {
                        cond: Operand::Move(cond),
                        expected: false,
                        ..
                    }),
                ..
            },
            rest @ ..,
        ] if cond == is_zero => {
            let found = make_binop_overflow_panic(rest, |bop, _, r| {
                matches!(bop, BinOp::Div(_) | BinOp::Rem(_)) && equiv_op(r, y_op)
            }) || make_unop_overflow_panic(rest, |unop, o| {
                matches!(unop, UnOp::Neg(_)) && equiv_op(o, y_op)
            });
            if found {
                rest
            } else {
                return;
            }
        }

        // Overflow checks for signed division and remainder. They look like:
        //   is_neg_1 := y == (-1)
        //   is_min := x == INT::min
        //   has_overflow := move (is_neg_1) & move (is_min)
        //   assert(move has_overflow == false)
        // Note here we don't need to update the operand to panic, as this was already done
        // by the previous pass for division by zero.
        [
            Statement {
                content:
                    RawStatement::Assign(is_neg_1, Rvalue::BinaryOp(BinOp::Eq, _y_op, _minus_1)),
                ..
            },
            Statement {
                content: RawStatement::Assign(is_min, Rvalue::BinaryOp(BinOp::Eq, _x_op, _int_min)),
                ..
            },
            Statement {
                content:
                    RawStatement::Assign(
                        has_overflow,
                        Rvalue::BinaryOp(
                            BinOp::BitAnd,
                            Operand::Move(and_op1),
                            Operand::Move(and_op2),
                        ),
                    ),
                ..
            },
            Statement {
                content:
                    RawStatement::Assert(Assert {
                        cond: Operand::Move(cond),
                        expected: false,
                        ..
                    }),
                ..
            },
            rest @ ..,
        ] if and_op1 == is_neg_1 && and_op2 == is_min && cond == has_overflow => rest,

        // Overflow checks for right/left shift. They can look like:
        //   a := y as u32; // or another type
        //   b := move a < const 32; // or another constant
        //   assert(move b == true);
        //   ...
        //   res := x {<<,>>} y;
        [
            Statement {
                content: RawStatement::Assign(cast, Rvalue::UnaryOp(UnOp::Cast(_), y_op)),
                ..
            },
            Statement {
                content:
                    RawStatement::Assign(
                        has_overflow,
                        Rvalue::BinaryOp(BinOp::Lt, Operand::Move(lhs), Operand::Const(..)),
                    ),
                ..
            },
            Statement {
                content:
                    RawStatement::Assert(Assert {
                        cond: Operand::Move(cond),
                        expected: true,
                        ..
                    }),
                ..
            },
            rest @ ..,
        ] if cond == has_overflow
            && lhs == cast
            && let Some(cast_local) = cast.as_local()
            && !rest.iter().any(|st| uses_local(st, cast_local)) =>
        {
            let found = make_binop_overflow_panic(rest, |bop, _, r| {
                matches!(bop, BinOp::Shl(_) | BinOp::Shr(_)) && equiv_op(r, y_op)
            });
            if found {
                rest
            } else {
                return;
            }
        }
        // or like:
        //   b := y < const 32; // or another constant
        //   assert(move b == true);
        //   ...
        //   res := x {<<,>>} y;
        //
        // this also overlaps with out of bounds checks for arrays, so we check for either;
        // these look like:
        //   b := copy y < const _
        //   assert(move b == true)
        //   ...
        //   res := a[y];
        [
            Statement {
                content:
                    RawStatement::Assign(
                        has_overflow,
                        Rvalue::BinaryOp(BinOp::Lt, y_op, Operand::Const(..)),
                    ),
                ..
            },
            Statement {
                content:
                    RawStatement::Assert(Assert {
                        cond: Operand::Move(cond),
                        expected: true,
                        ..
                    }),
                ..
            },
            rest @ ..,
        ] if cond == has_overflow => {
            // look for a shift operation
            let mut found = make_binop_overflow_panic(rest, |bop, _, r| {
                matches!(bop, BinOp::Shl(_) | BinOp::Shr(_)) && equiv_op(r, y_op)
            });
            if !found {
                // otherwise, look for an array access
                for stmt in rest.iter_mut() {
                    stmt.dyn_visit_in_body(|p: &Place| {
                        if let Some((_, ProjectionElem::Index { offset, .. })) = p.as_projection()
                            && equiv_op(offset, y_op)
                        {
                            found = true;
                        }
                    });
                }
            }

            if found {
                rest
            } else {
                return;
            }
        }

        // Overflow checks for addition/subtraction/multiplication. They look like:
        // ```text
        //   r := x checked.+ y;
        //   assert(move r.1 == false);
        //   ...
        //   z := move r.0;
        // ```
        // We replace that with:
        // ```text
        // z := x + y;
        // ```
        //
        // But sometimes, because of constant promotion, we end up with a lone checked operation
        // without assert. In that case we replace it with its wrapping equivalent.
        [
            Statement {
                content:
                    RawStatement::Assign(
                        result,
                        Rvalue::BinaryOp(
                            binop @ (BinOp::AddChecked | BinOp::SubChecked | BinOp::MulChecked),
                            _,
                            _,
                        ),
                    ),
                ..
            },
            rest @ ..,
        ] if let Some(result_local_id) = result.as_local() => {
            // Look for uses of the overflow boolean.
            let mut overflow_is_used = false;
            for stmt in rest.iter_mut() {
                stmt.dyn_visit_in_body(|p: &Place| {
                    if let Some((sub, ProjectionElem::Field(FieldProjKind::Tuple(..), fid))) =
                        p.as_projection()
                        && fid.index() == 1
                        && sub == result
                    {
                        overflow_is_used = true;
                    }
                });
            }
            // Check if the operation is followed by an assert.
            let followed_by_assert = if let [
                Statement {
                    content:
                        RawStatement::Assert(Assert {
                            cond: Operand::Move(assert_cond),
                            expected: false,
                            ..
                        }),
                    ..
                },
                ..,
            ] = rest
                && let Some((sub, ProjectionElem::Field(FieldProjKind::Tuple(..), fid))) =
                    assert_cond.as_projection()
                && fid.index() == 1
                && sub == result
            {
                true
            } else {
                false
            };
            if overflow_is_used && !followed_by_assert {
                // The overflow boolean is used in a way that isn't a builtin overflow check; we
                // change nothing.
                return;
            }

            if followed_by_assert {
                // We have a compiler-emitted assert. We replace the operation with one that has
                // panic-on-overflow semantics.
                *binop = binop.with_overflow(OverflowMode::Panic);
                // The failure behavior is part of the binop now, so we remove the assert.
                rest[0].content = RawStatement::Nop;
            } else {
                // The overflow boolean is not used, we replace the operations with wrapping
                // semantics.
                *binop = binop.with_overflow(OverflowMode::Wrap);
            }
            // Fixup the local type.
            let result_local = &mut locals.locals[result_local_id];
            result_local.ty = result_local.ty.as_tuple().unwrap()[0].clone();
            // Fixup the place type.
            let new_result_place = locals.place_for_var(result_local_id);
            // Replace uses of `r.0` with `r`.
            for stmt in rest.iter_mut() {
                stmt.dyn_visit_in_body_mut(|p: &mut Place| {
                    if let Some((sub, ProjectionElem::Field(FieldProjKind::Tuple(..), fid))) =
                        p.as_projection()
                        && sub == result
                    {
                        assert_eq!(fid.index(), 0);
                        *p = new_result_place.clone()
                    }
                });
            }
            *result = new_result_place;
            return;
        }

        _ => return,
    };

    // Remove the statements we're not keeping.
    let keep_len = statements_to_keep.len();
    for i in 0..statements.len() - keep_len {
        statements[i].content = RawStatement::Nop;
    }
}

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, b: &mut ExprBody) {
        b.transform_sequences_fwd(|locals, seq| {
            remove_dynamic_checks(ctx, locals, seq);
            Vec::new()
        });
    }
}
