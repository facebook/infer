//! When the function's return type is unit, the generated MIR doesn't
//! set the return value to `()`. This can be a concern: in the case
//! of AENEAS, it means the return variable contains ⊥ upon returning.
//! For this reason, when the function has return type unit, we insert
//! an extra assignment just before returning.
use crate::transform::TransformCtx;
use crate::ullbc_ast::*;

use super::ctx::UllbcPass;

pub struct Transform;
impl UllbcPass for Transform {
    fn transform_function(&self, _ctx: &mut TransformCtx, decl: &mut FunDecl) {
        if decl.signature.output.is_unit() {
            if let Ok(body) = &mut decl.body {
                let body = body.as_unstructured_mut().unwrap();
                for block in &mut body.body {
                    if let RawTerminator::Return = block.terminator.content {
                        let return_place = body.locals.return_place();
                        let assign_st = Statement::new(
                            block.terminator.span,
                            RawStatement::Assign(return_place, Rvalue::unit_value()),
                        );
                        block.statements.push(assign_st)
                    }
                }
            }
        }
    }
}
