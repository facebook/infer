use crate::transform::TransformCtx;
use crate::transform::ctx::{BodyTransformCtx, UllbcPass};
use crate::ullbc_ast::*;

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_function(&self, ctx: &mut TransformCtx, decl: &mut FunDecl) {
        decl.transform_ullbc_terminators(ctx, |ctx, term| {
            let TerminatorKind::Call { call, target, .. } = &term.kind else {
                return;
            };
            let FnOperand::Regular(fn_ptr) = &call.func else {
                return;
            };
            let FnPtrKind::Fun(FunId::Regular(fun_id)) = &fn_ptr.kind else {
                return;
            };
            let Some(fun_decl) = ctx.ctx.translated.fun_decls.get(*fun_id) else {
                return;
            };
            if fun_decl.item_meta.lang_item.as_deref() == Some("offset_of")
                && let generics = fn_ptr.pre_mono_generics(&ctx.ctx.translated)
                && let Some(ty) = generics.types.get(TypeVarId::ZERO)
                && let TyKind::Adt(tref) = ty.kind()
                && let TypeId::Adt(type_id) = tref.id
                && let [Operand::Const(arg0), Operand::Const(arg1)] = call.args.as_slice()
                && let ConstantExprKind::Literal(Literal::Scalar(ScalarValue::Unsigned(
                    UIntTy::U32,
                    variant_id,
                ))) = &arg0.kind
                && let ConstantExprKind::Literal(Literal::Scalar(ScalarValue::Unsigned(
                    UIntTy::U32,
                    field_id,
                ))) = &arg1.kind
                && let Some(tdecl) = ctx.ctx.translated.type_decls.get(type_id)
            {
                // TODO: move into a pass, maybe also size_of/align_of? or remove the nullops.
                // maybe this is a constant also.
                let variant_id = if tdecl.kind.is_enum() {
                    Some(VariantId::from_usize(*variant_id as usize))
                } else {
                    None
                };
                let field_id = FieldId::from_usize(*field_id as usize);
                let rval = Rvalue::NullaryOp(
                    NullOp::OffsetOf(tref.clone(), variant_id, field_id),
                    Ty::mk_usize(),
                );
                ctx.insert_assn_stmt(call.dest.clone(), rval);
                term.kind = TerminatorKind::Goto { target: *target };
            }
        });
    }
}
