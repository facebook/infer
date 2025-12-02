//! Add missing StorageLives -- in MIR, some locals are considered "always" initialised, and have
//! no StorageLive and StorageDead instructions associated; this always includes the arguments
//! and the return value, but also sometimes includes other locals. We make sure these additional
//! locals get initialised at the start of the function.
use std::collections::BTreeSet;
use std::ops::ControlFlow::{self, Continue};

use derive_generic_visitor::Visitor;

use crate::ast::*;
use crate::transform::TransformCtx;
use crate::ullbc_ast::BlockId;

use super::ctx::TransformPass;

#[derive(Visitor)]
struct StorageVisitor {
    unmentioned_locals: BTreeSet<LocalId>,
}

impl StorageVisitor {
    fn new(locals: &Locals) -> Self {
        let mut unmentioned_locals = BTreeSet::new();
        for local in locals.locals.iter() {
            if local.index > locals.arg_count {
                unmentioned_locals.insert(local.index);
            }
        }
        Self { unmentioned_locals }
    }
}

impl VisitAst for StorageVisitor {
    fn visit_llbc_statement(&mut self, st: &llbc_ast::Statement) -> ControlFlow<Self::Break> {
        match st.content {
            llbc_ast::RawStatement::StorageDead(loc) | llbc_ast::RawStatement::StorageLive(loc) => {
                self.unmentioned_locals.remove(&loc);
            }
            _ => {}
        }
        Continue(())
    }

    fn visit_ullbc_statement(&mut self, st: &ullbc_ast::Statement) -> ControlFlow<Self::Break> {
        match st.content {
            ullbc_ast::RawStatement::StorageDead(loc)
            | ullbc_ast::RawStatement::StorageLive(loc) => {
                self.unmentioned_locals.remove(&loc);
            }
            _ => {}
        }
        Continue(())
    }
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|_ctx, fun| {
            let Ok(body) = &mut fun.body else {
                return;
            };

            let mut storage_visitor = match body {
                Body::Structured(body) => StorageVisitor::new(&body.locals),
                Body::Unstructured(body) => StorageVisitor::new(&body.locals),
            };
            let _ = storage_visitor.visit(body);

            // Insert StorageLive instructions for the always initialised locals.
            match body {
                Body::Structured(body) => {
                    let first_span = body.body.statements.first().unwrap().span;
                    let new_statements = storage_visitor.unmentioned_locals.iter().map(|local| {
                        llbc_ast::Statement::new(
                            first_span,
                            llbc_ast::RawStatement::StorageLive(*local),
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
                    let new_statements = storage_visitor.unmentioned_locals.iter().map(|local| {
                        ullbc_ast::Statement::new(
                            first_span,
                            ullbc_ast::RawStatement::StorageLive(*local),
                        )
                    });
                    first_block.statements.splice(0..0, new_statements);
                }
            }
        });
    }
}
