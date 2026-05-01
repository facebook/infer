//! The MIR constant expressions lead to a lot of duplication: there are
//! for instance constant ADTs which duplicate the "regular" aggregated
//! ADTs in the operands, constant references, etc. This reduces the number
//! of cases to handle and eases the function translation in Aeneas.
//!
//! This pass removes all those occurrences so that only the
//! `ConstantExpression::Literal`. It does so by introducing intermediate statements.
//!
//! A small remark about the intermediate statements we introduce for the globals:
//! we do so because, when evaluating the code in "concrete" mode, it allows to
//! handle the globals like function calls.

use itertools::Itertools;

use crate::transform::TransformCtx;
use crate::transform::ctx::{BodyTransformCtx, UllbcPass, UllbcStatementTransformCtx};
use crate::ullbc_ast::*;

/// If the constant value is a constant ADT, push `Assign::Aggregate` statements
/// to the vector of statements, that bind new variables to the ADT parts and
/// the variable assigned to the complete ADT.
///
/// Goes fom e.g. `f(T::A(x, y))` to `let a = T::A(x, y); f(a)`.
/// The function is recursively called on the aggregate fields (e.g. here x and y).
fn transform_constant_expr(
    ctx: &mut UllbcStatementTransformCtx<'_>,
    mut val: Box<ConstantExpr>,
) -> Operand {
    let rval = match val.kind {
        ConstantExprKind::Literal(_)
        | ConstantExprKind::Var(_)
        | ConstantExprKind::RawMemory(..)
        | ConstantExprKind::TraitConst(..)
        | ConstantExprKind::FnDef(..)
        | ConstantExprKind::Opaque(_) => {
            // Nothing to do
            // TODO: for trait const: might come from a top-level impl, so we might
            // want to introduce an intermediate statement to be able to evaluate
            // it as a function call, like for globals.
            return Operand::Const(val);
        }
        // Here we use a copy, rather than a move -- moving a global would leave it uninitialized,
        // which would e.g. make the following code fail:
        //     const GLOBAL: usize = 0;
        //     let x = GLOBAL;
        //     let y = GLOBAL; // if moving, at this point GLOBAL would be uninitialized
        ConstantExprKind::Global(global_ref) => {
            return Operand::Copy(Place::new_global(global_ref, val.ty));
        }
        ConstantExprKind::PtrNoProvenance(ptr) => {
            let usize_ty = TyKind::Literal(LiteralTy::UInt(UIntTy::Usize)).into_ty();
            let ptr_usize = ConstantExprKind::Literal(Literal::Scalar(ScalarValue::Unsigned(
                UIntTy::Usize,
                ptr,
            )));
            let cast = UnOp::Cast(CastKind::RawPtr(usize_ty.clone(), val.ty.clone()));
            Rvalue::UnaryOp(
                cast,
                Operand::Const(Box::new(ConstantExpr {
                    kind: ptr_usize,
                    ty: usize_ty,
                })),
            )
        }
        cexpr @ (ConstantExprKind::Ref(..) | ConstantExprKind::Ptr(..)) => {
            let (rk, bval, metadata) = match cexpr {
                ConstantExprKind::Ref(bval, metadata) => (None, bval, metadata),
                ConstantExprKind::Ptr(rk, bval, metadata) => (Some(rk), bval, metadata),
                _ => unreachable!("Unexpected constant expr kind in ref/ptr"),
            };

            // As the value is originally an argument, it must be Sized, hence no metadata
            let place = match bval.kind {
                ConstantExprKind::Global(global_ref) => Place::new_global(global_ref, bval.ty),
                _ => {
                    // Recurse on the borrowed value
                    let bval = transform_constant_expr(ctx, bval);

                    // Evaluate the referenced value
                    let bval_ty = bval.ty().clone();
                    ctx.rval_to_place(Rvalue::Use(bval), bval_ty)
                }
            };
            match (rk, metadata) {
                // Borrow the place.
                (None, None) => ctx.borrow(place, BorrowKind::Shared),
                (Some(rk), None) => ctx.raw_borrow(place, rk),
                // Unsizing borrow.
                (None, Some(metadata)) => {
                    let sized_ref = ctx.borrow_to_new_var(place, BorrowKind::Shared, None);
                    Rvalue::UnaryOp(
                        UnOp::Cast(CastKind::Unsize(
                            sized_ref.ty.clone(),
                            val.ty.clone(),
                            metadata,
                        )),
                        Operand::Move(sized_ref),
                    )
                }
                (Some(rk), Some(metadata)) => {
                    let sized_raw_ref = ctx.raw_borrow_to_new_var(place, rk, None);
                    Rvalue::UnaryOp(
                        UnOp::Cast(CastKind::Unsize(
                            sized_raw_ref.ty.clone(),
                            val.ty.clone(),
                            metadata,
                        )),
                        Operand::Move(sized_raw_ref),
                    )
                }
            }
        }
        ConstantExprKind::Adt(variant, fields) => {
            let fields = fields
                .into_iter()
                .map(|x| transform_constant_expr(ctx, Box::new(x)))
                .collect();

            // Build an `Aggregate` rvalue.
            let tref = val.ty.kind().as_adt().unwrap();
            let aggregate_kind = AggregateKind::Adt(tref.clone(), variant, None);
            Rvalue::Aggregate(aggregate_kind, fields)
        }
        ConstantExprKind::Array(fields) => {
            let fields = fields
                .into_iter()
                .map(|x| transform_constant_expr(ctx, Box::new(x)))
                .collect_vec();

            let len =
                ConstantExpr::mk_usize(ScalarValue::Unsigned(UIntTy::Usize, fields.len() as u128));
            let TyKind::Array(ty, _) = val.ty.kind() else {
                unreachable!("Non array type in array constant");
            };
            Rvalue::Aggregate(AggregateKind::Array(ty.clone(), Box::new(len)), fields)
        }
        ConstantExprKind::FnPtr(fptr) => {
            let TyKind::FnPtr(sig) = val.ty.kind() else {
                unreachable!("FnPtr constant must have FnPtr type");
            };
            let from_ty =
                TyKind::FnDef(sig.clone().map(|_| fptr.clone().move_under_binder())).into_ty();
            let to_ty = TyKind::FnPtr(sig.clone()).into_ty();

            Rvalue::UnaryOp(
                UnOp::Cast(CastKind::FnPtr(from_ty.clone(), to_ty)),
                Operand::Const(Box::new(ConstantExpr {
                    kind: ConstantExprKind::FnDef(fptr),
                    ty: from_ty,
                })),
            )
        }
        ConstantExprKind::VTableRef(tref)
            if let TraitRefKind::TraitImpl(impl_ref) = &tref.kind
                && let Some(timpl) = ctx.ctx.translated.trait_impls.get(impl_ref.id)
                && let Some(vtable_ref) = &timpl.vtable
                && let TyKind::Ref(_, vtable_ty, _) = val.ty.kind() =>
        {
            let inner = Box::new(ConstantExpr {
                kind: ConstantExprKind::Global(vtable_ref.clone()),
                ty: vtable_ty.clone(),
            });
            val.kind = ConstantExprKind::Ref(inner, None);
            // Normalize further into a place access.
            return transform_constant_expr(ctx, val);
        }
        ConstantExprKind::VTableRef(..) => return Operand::Const(val),
    };
    Operand::Move(ctx.rval_to_place(rval, val.ty.clone()))
}

fn transform_operand(ctx: &mut UllbcStatementTransformCtx<'_>, op: &mut Operand) {
    // Transform the constant operands (otherwise do nothing)
    take_mut::take(op, |op| {
        if let Operand::Const(val) = op {
            transform_constant_expr(ctx, val)
        } else {
            op
        }
    })
}

pub struct Transform;
impl UllbcPass for Transform {
    fn should_run(&self, options: &crate::options::TranslateOptions) -> bool {
        !options.raw_consts
    }

    fn transform_function(&self, ctx: &mut TransformCtx, fun_decl: &mut FunDecl) {
        fun_decl.transform_ullbc_operands(ctx, transform_operand);
        if let Some(body) = fun_decl.body.as_unstructured_mut() {
            for block in body.body.iter_mut() {
                // Simplify array with repeated constants into array repeats.
                block.dyn_visit_in_body_mut(|rvalue: &mut Rvalue| {
                    take_mut::take(rvalue, |rvalue| match rvalue {
                        Rvalue::Aggregate(AggregateKind::Array(ty, len), ref fields)
                            if fields.len() >= 2
                                && fields.iter().all(|x| x.is_const())
                                && let Ok(op) = fields.iter().dedup().exactly_one() =>
                        {
                            Rvalue::Repeat(op.clone(), ty.clone(), len)
                        }
                        _ => rvalue,
                    });
                });
            }
        }
    }
}
