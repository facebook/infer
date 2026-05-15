//! Add missing StorageLives -- in MIR, some locals are considered "always" initialised, and have
//! no StorageLive and StorageDead instructions associated; this always includes the arguments
//! and the return value, but also sometimes includes other locals. We make sure these additional
//! locals get initialised at the start of the function if they're used anywhere.
use derive_generic_visitor::Visitor;

use crate::ast::*;
use crate::ids::IndexVec;
use crate::transform::TransformCtx;
use crate::transform::ctx::TransformPass;
use crate::ullbc_ast::BlockId;

#[derive(Visitor)]
struct StorageVisitor {
    local_status: IndexVec<LocalId, LocalStatus>,
}

enum LocalStatus {
    Unused,
    UsedAndNoExplicitStorage,
    UsedAndHasExplicitStorage,
}

impl StorageVisitor {
    fn new(locals: &Locals) -> Self {
        let local_status = locals.locals.map_ref(|local| {
            if locals.is_return_or_arg(local.index) {
                // The return and argument places count as having a `StorageLive` already.
                LocalStatus::UsedAndHasExplicitStorage
            } else {
                LocalStatus::Unused
            }
        });
        Self { local_status }
    }
}

impl VisitBody for StorageVisitor {
    fn visit_locals(&mut self, _: &Locals) -> ::std::ops::ControlFlow<Self::Break> {
        // Don't look inside the local declarations otherwise we'll think they're all used.
        ControlFlow::Continue(())
    }
    fn enter_local_id(&mut self, lid: &LocalId) {
        let status = &mut self.local_status[*lid];
        if let LocalStatus::Unused = *status {
            *status = LocalStatus::UsedAndNoExplicitStorage
        }
    }
    fn enter_llbc_statement(&mut self, st: &llbc_ast::Statement) {
        match &st.kind {
            llbc_ast::StatementKind::StorageDead(lid)
            | llbc_ast::StatementKind::StorageLive(lid) => {
                self.local_status[*lid] = LocalStatus::UsedAndHasExplicitStorage
            }
            _ => {}
        }
    }
    fn enter_ullbc_statement(&mut self, st: &ullbc_ast::Statement) {
        match &st.kind {
            ullbc_ast::StatementKind::StorageDead(lid)
            | ullbc_ast::StatementKind::StorageLive(lid) => {
                self.local_status[*lid] = LocalStatus::UsedAndHasExplicitStorage
            }
            _ => {}
        }
    }
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|_ctx, fun| {
            let body = &mut fun.body;
            if !body.has_contents() {
                return;
            }

            let mut storage_visitor = StorageVisitor::new(body.locals());
            let _ = storage_visitor.visit(body);

            // Insert StorageLive instructions for the always initialised locals.
            let locals_with_missing_storage = storage_visitor
                .local_status
                .iter_indexed()
                .filter(|(_, status)| matches!(status, LocalStatus::UsedAndNoExplicitStorage))
                .map(|(local, _)| local);
            match body {
                Body::Structured(body) => {
                    let first_span = body.body.statements.first().unwrap().span;
                    let new_statements = locals_with_missing_storage.map(|local| {
                        llbc_ast::Statement::new(
                            first_span,
                            llbc_ast::StatementKind::StorageLive(local),
                        )
                    });
                    body.body.statements.splice(0..0, new_statements);
                }
                Body::Unstructured(body) => {
                    let first_block = body.body.get_mut(BlockId::ZERO).unwrap();
                    let first_span = if let Some(fst) = first_block.statements.first() {
                        fst.span
                    } else {
                        first_block.terminator.span
                    };
                    let new_statements = locals_with_missing_storage.map(|local| {
                        ullbc_ast::Statement::new(
                            first_span,
                            ullbc_ast::StatementKind::StorageLive(local),
                        )
                    });
                    first_block.statements.splice(0..0, new_statements);
                }
                _ => unreachable!(),
            }
        });
    }
}
