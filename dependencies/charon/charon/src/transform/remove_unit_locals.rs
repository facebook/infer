use derive_generic_visitor::*;
use std::collections::HashSet;

use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use super::ctx::UllbcPass;

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, body: &mut ExprBody) {
        // Replace any copy/move of a unit local to a plain const assignment. Note: we don't touch
        // other `Rvalue`s as they might have side-effects (e.g. reading through a pointer).
        body.visit_statements(|st| {
            if let RawStatement::Assign(_, rvalue) = &mut st.content
                && let Rvalue::Use(Operand::Move(from) | Operand::Copy(from)) = rvalue
                && from.is_local()
                && from.ty().is_unit()
            {
                *rvalue = Rvalue::unit_value()
            }
        });

        // Find the unused locals of unit type.
        #[derive(Visitor)]
        struct UnitLocalsVisitor {
            unused_unit_locals: HashSet<LocalId>,
        }
        impl VisitBody for UnitLocalsVisitor {
            fn enter_place(&mut self, x: &Place) {
                if let Some(var_id) = x.as_local() {
                    self.unused_unit_locals.remove(&var_id);
                }
            }
            fn visit_ullbc_statement(
                &mut self,
                x: &ullbc_ast::Statement,
            ) -> ControlFlow<Self::Break> {
                match &x.content {
                    RawStatement::Assign(place, rvalue) => {
                        if place.is_local() && place.ty().is_unit() {
                            // Don't count the assignment as a use.
                        } else {
                            self.visit(place)?;
                        }
                        self.visit(rvalue)?;
                    }
                    _ => self.visit_inner(x)?,
                }
                Continue(())
            }
        }
        let unused_unit_locals = (UnitLocalsVisitor {
            unused_unit_locals: body
                .locals
                .non_argument_locals()
                .filter(|(_, var)| var.ty.is_unit())
                .map(|(id, _)| id)
                .collect(),
        })
        .visit_by_val_infallible(&*body)
        .unused_unit_locals;

        // Remove side-effect-free assignments into unused places.
        body.visit_statements(|st| {
            if let RawStatement::Assign(place, rvalue) = &st.content
                && let Some(var_id) = place.as_local()
                && unused_unit_locals.contains(&var_id)
                && rvalue.is_aggregate()
            {
                st.content = RawStatement::Nop;
            }
        });
    }
}
