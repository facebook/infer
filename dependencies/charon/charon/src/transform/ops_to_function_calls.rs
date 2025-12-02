//! Desugar some unary/binary operations and the array repeats to function calls.
//! For instance, we desugar ArrayToSlice from an unop to a function call.
//! This allows a more uniform treatment later on.
//! TODO: actually transform all the unops and binops to function calls?
use crate::llbc_ast::*;
use crate::transform::TransformCtx;

use super::ctx::LlbcPass;

fn transform_st(s: &mut Statement) {
    match &s.content {
        // Transform the ArrayToSlice unop
        RawStatement::Assign(
            p,
            Rvalue::UnaryOp(
                UnOp::Cast(CastKind::Unsize(src_ty, tgt_ty, UnsizingMetadata::Length(_))),
                op,
            ),
        ) => {
            if let (
                TyKind::Ref(_, deref!(TyKind::Adt(tref1)), kind1),
                TyKind::Ref(_, deref!(TyKind::Adt(tref2)), kind2),
            ) = (src_ty.kind(), tgt_ty.kind())
                && matches!(tref1.id, TypeId::Builtin(BuiltinTy::Array))
                && matches!(tref2.id, TypeId::Builtin(BuiltinTy::Slice))
            {
                // In MIR terminology, we go from &[T; l] to &[T] which means we
                // effectively "unsize" the type, as `l` no longer appears in the
                // destination type. At runtime, the converse happens: the length
                // materializes into the fat pointer.
                assert!(
                    tref1.generics.types.elem_count() == 1
                        && tref1.generics.const_generics.elem_count() == 1
                );
                assert!(tref1.generics.types[0] == tref2.generics.types[0]);
                assert!(kind1 == kind2);
                // We could avoid the clone operations below if we take the content of
                // the statement. In practice, this shouldn't have much impact.
                let id = match *kind1 {
                    RefKind::Mut => BuiltinFunId::ArrayToSliceMut,
                    RefKind::Shared => BuiltinFunId::ArrayToSliceShared,
                };
                let func = FunIdOrTraitMethodRef::mk_builtin(id);
                let generics = GenericArgs::new(
                    [Region::Erased].into(),
                    tref1.generics.types.clone(),
                    tref1.generics.const_generics.clone(),
                    [].into(),
                );
                let func = FnOperand::Regular(FnPtr {
                    func: Box::new(func),
                    generics: Box::new(generics),
                });
                s.content = RawStatement::Call(Call {
                    func,
                    args: vec![op.clone()],
                    dest: p.clone(),
                });
            }
        }
        // Transform the array aggregates to function calls
        RawStatement::Assign(p, Rvalue::Repeat(op, ty, cg)) => {
            // We could avoid the clone operations below if we take the content of
            // the statement. In practice, this shouldn't have much impact.
            let id = BuiltinFunId::ArrayRepeat;
            let func = FunIdOrTraitMethodRef::mk_builtin(id);
            let generics = GenericArgs::new(
                [Region::Erased].into(),
                [ty.clone()].into(),
                [cg.clone()].into(),
                [].into(),
            );
            let func = FnOperand::Regular(FnPtr {
                func: Box::new(func),
                generics: Box::new(generics),
            });
            s.content = RawStatement::Call(Call {
                func,
                args: vec![op.clone()],
                dest: p.clone(),
            });
        }
        // Transform the raw pointer aggregate to a function call
        RawStatement::Assign(p, Rvalue::Aggregate(AggregateKind::RawPtr(ty, is_mut), ops)) => {
            let id = BuiltinFunId::PtrFromParts(is_mut.clone());
            let func = FunIdOrTraitMethodRef::mk_builtin(id);
            let generics = GenericArgs::new(
                [Region::Erased].into(),
                [ty.clone()].into(),
                [].into(),
                [].into(),
            );

            let func = FnOperand::Regular(FnPtr {
                func: Box::new(func),
                generics: Box::new(generics),
            });
            s.content = RawStatement::Call(Call {
                func,
                args: ops.clone(),
                dest: p.clone(),
            });
        }
        _ => {}
    }
}

pub struct Transform;
impl LlbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, b: &mut ExprBody) {
        if ctx.options.no_ops_to_function_calls {
            return;
        }
        b.body.visit_statements(&mut transform_st);
    }
}
