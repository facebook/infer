use crate::llbc_ast::*;
use crate::transform::TransformCtx;

use crate::transform::ctx::LlbcPass;

pub struct Transform;

impl Transform {
    fn update_statements(locals: &Locals, seq: &mut [Statement]) -> Vec<Statement> {
        // Remove double aborts. This can happen when a function call is turned into an `Abort` by
        // `inline_local_panic_functions`.
        if let [
            Statement {
                kind: StatementKind::Abort(_),
                ..
            },
            Statement {
                kind: second_abort @ StatementKind::Abort(_),
                ..
            },
            ..,
        ] = seq
        {
            *second_abort = StatementKind::Nop;
            return Vec::new();
        }
        if let [
            Statement {
                kind: StatementKind::Call(call),
                ..
            },
            Statement {
                kind: second_abort @ StatementKind::Abort(_),
                ..
            },
            ..,
        ] = seq
            && let Some(local_id) = call.dest.as_local()
            && locals[local_id].ty.kind().is_never()
        {
            *second_abort = StatementKind::Nop;
            return Vec::new();
        }

        Vec::new()
    }
}

impl LlbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        b.body
            .transform_sequences(|seq| Transform::update_statements(&b.locals, seq))
    }
}
