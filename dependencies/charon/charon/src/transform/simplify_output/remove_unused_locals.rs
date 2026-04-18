//! Remove the locals (which are not used for the input arguments) which are
//! never used in the function bodies.  This is useful to remove the locals with
//! type `Never`. We actually check that there are no such local variables
//! remaining afterwards.
use derive_generic_visitor::Visitor;
use std::mem;
use std::ops::ControlFlow::Continue;

use crate::ast::*;
use crate::ids::IndexVec;
use crate::transform::TransformCtx;
use crate::transform::ctx::TransformPass;

#[derive(Visitor)]
struct LocalsUsageVisitor {
    used_locals: IndexVec<LocalId, bool>,
}

impl VisitBody for LocalsUsageVisitor {
    fn enter_local_id(&mut self, lid: &LocalId) {
        self.used_locals[*lid] = true;
    }
    fn visit_llbc_statement(&mut self, st: &llbc_ast::Statement) -> ControlFlow<Self::Break> {
        match &st.kind {
            llbc_ast::StatementKind::StorageDead(_) | llbc_ast::StatementKind::StorageLive(_) => {
                // These statements don't count as a variable use.
                Continue(())
            }
            _ => self.visit_inner(st),
        }
    }
    fn visit_ullbc_statement(&mut self, st: &ullbc_ast::Statement) -> ControlFlow<Self::Break> {
        match &st.kind {
            ullbc_ast::StatementKind::StorageDead(_) | ullbc_ast::StatementKind::StorageLive(_) => {
                // These statements don't count as a variable use.
                Continue(())
            }
            _ => self.visit_inner(st),
        }
    }
}

#[derive(Visitor)]
struct LocalsRenumberVisitor {
    ids_map: IndexVec<LocalId, Option<LocalId>>,
}

impl VisitBodyMut for LocalsRenumberVisitor {
    fn enter_local_id(&mut self, lid: &mut LocalId) {
        *lid = self.ids_map[*lid].unwrap();
    }
    fn enter_llbc_statement(&mut self, st: &mut llbc_ast::Statement) {
        match st.kind {
            llbc_ast::StatementKind::StorageDead(lid)
            | llbc_ast::StatementKind::StorageLive(lid)
                if self.ids_map[lid].is_none() =>
            {
                st.kind = llbc_ast::StatementKind::Nop;
            }
            _ => {}
        }
    }
    fn enter_ullbc_statement(&mut self, st: &mut ullbc_ast::Statement) {
        match st.kind {
            ullbc_ast::StatementKind::StorageDead(lid)
            | ullbc_ast::StatementKind::StorageLive(lid)
                if self.ids_map[lid].is_none() =>
            {
                st.kind = ullbc_ast::StatementKind::Nop;
            }
            _ => {}
        }
    }
}

fn remove_unused_locals<Body: BodyVisitable>(body: &mut GExprBody<Body>) {
    // Compute the set of used locals.
    // We always register the return variable and the input arguments.
    let mut visitor = LocalsUsageVisitor {
        used_locals: body
            .locals
            .locals
            .map_ref(|local| body.locals.is_return_or_arg(local.index)),
    };
    let _ = body.body.drive_body(&mut visitor);
    let used_locals = visitor.used_locals;
    trace!("used_locals: {:?}", used_locals);

    // Keep only the variables that are used (storage statements don't count) and update their
    // indices to be contiguous.
    let mut ids_map: IndexVec<LocalId, Option<LocalId>> = body.locals.locals.map_ref(|_| None);
    for local in mem::take(&mut body.locals.locals) {
        if used_locals[local.index] {
            let old_id = local.index;
            let new_id = body
                .locals
                .locals
                .push_with(|index| Local { index, ..local });
            ids_map[old_id] = Some(new_id);
        }
    }
    trace!("ids_maps: {:?}", ids_map);

    // Update all `LocalId`s.
    let mut visitor = LocalsRenumberVisitor { ids_map };
    let _ = body.body.drive_body_mut(&mut visitor);
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|_ctx, fun| match &mut fun.body {
            Body::Unstructured(body) => remove_unused_locals(body),
            Body::Structured(body) => remove_unused_locals(body),
            _ => {}
        });
    }
}
