//! Desugar some unary/binary operations and the array repeats to function calls.
//! For instance, we desugar ArrayToSlice from an unop to a function call.
//! This allows a more uniform treatment later on.
//! TODO: actually transform all the unops and binops to function calls?
use crate::llbc_ast::*;
use crate::transform::TransformCtx;

use crate::transform::ctx::LlbcPass;

fn transform_st(s: &mut Statement) {
    match &s.kind {
        // Transform the ArrayToSlice unop
        StatementKind::Assign(
            p,
            Rvalue::UnaryOp(
                UnOp::Cast(CastKind::Unsize(src_ty, tgt_ty, UnsizingMetadata::Length(_))),
                op,
            ),
        ) => {
            if let (
                TyKind::Ref(_, deref!(TyKind::Array(arr_ty, len)), kind1),
                TyKind::Ref(_, deref!(TyKind::Slice(_)), kind2),
            ) = (src_ty.kind(), tgt_ty.kind())
            {
                // In MIR terminology, we go from &[T; l] to &[T] which means we
                // effectively "unsize" the type, as `l` no longer appears in the
                // destination type. At runtime, the converse happens: the length
                // materializes into the fat pointer.
                assert!(kind1 == kind2);
                // We could avoid the clone operations below if we take the content of
                // the statement. In practice, this shouldn't have much impact.
                let id = match *kind1 {
                    RefKind::Mut => BuiltinFunId::ArrayToSliceMut,
                    RefKind::Shared => BuiltinFunId::ArrayToSliceShared,
                };
                let func = FnPtrKind::mk_builtin(id);
                let generics = GenericArgs::new(
                    [Region::Erased].into(),
                    [arr_ty.clone()].into(),
                    [*len.clone()].into(),
                    [].into(),
                );
                s.kind = StatementKind::Call(Call {
                    func: FnOperand::Regular(FnPtr::new(func, generics)),
                    args: vec![op.clone()],
                    dest: p.clone(),
                });
            }
        }
        // Transform the array aggregates to function calls
        StatementKind::Assign(p, Rvalue::Repeat(op, ty, cg)) => {
            // We could avoid the clone operations below if we take the content of
            // the statement. In practice, this shouldn't have much impact.
            let id = BuiltinFunId::ArrayRepeat;
            let func = FnPtrKind::mk_builtin(id);
            let generics = GenericArgs::new(
                [].into(),
                [ty.clone()].into(),
                [*cg.clone()].into(),
                [].into(),
            );
            s.kind = StatementKind::Call(Call {
                func: FnOperand::Regular(FnPtr::new(func, generics)),
                args: vec![op.clone()],
                dest: p.clone(),
            });
        }
        // Transform the raw pointer aggregate to a function call
        StatementKind::Assign(p, Rvalue::Aggregate(AggregateKind::RawPtr(ty, is_mut), ops)) => {
            let id = BuiltinFunId::PtrFromParts(is_mut.clone());
            let func = FnPtrKind::mk_builtin(id);
            let generics = GenericArgs::new(
                [Region::Erased].into(),
                [ty.clone()].into(),
                [].into(),
                [].into(),
            );

            s.kind = StatementKind::Call(Call {
                func: FnOperand::Regular(FnPtr::new(func, generics)),
                args: ops.clone(),
                dest: p.clone(),
            });
        }
        _ => {}
    }
}

pub struct Transform;
impl LlbcPass for Transform {
    fn should_run(&self, options: &crate::options::TranslateOptions) -> bool {
        options.ops_to_function_calls
    }

    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        b.body.visit_statements(&mut transform_st);
    }
}
