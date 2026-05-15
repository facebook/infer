use crate::transform::TransformCtx;
use crate::transform::ctx::UllbcPass;
use crate::ullbc_ast::*;

fn is_trivial_drop(stmt: &Terminator) -> bool {
    matches!(
        &stmt.kind,
        TerminatorKind::Drop {  tref, .. }
            if matches!(
                &tref.kind,
                TraitRefKind::BuiltinOrAuto { builtin_data: BuiltinImplData::NoopDestruct, .. }
            )
    )
}

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, body: &mut ullbc_ast::ExprBody) {
        for block in &mut body.body {
            if is_trivial_drop(&block.terminator) {
                match &block.terminator.kind {
                    TerminatorKind::Drop { target, .. } => {
                        block.terminator.kind = TerminatorKind::Goto { target: *target };
                    }
                    _ => {}
                }
            }
        }
    }
}
