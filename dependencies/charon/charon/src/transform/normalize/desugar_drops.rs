use super::super::ctx::UllbcPass;
use crate::{
    transform::{
        TransformCtx,
        ctx::{BodyTransformCtx, UllbcStatementTransformCtx},
    },
    ullbc_ast::*,
};

fn is_noop_destruct(tref: &TraitRef) -> bool {
    matches!(
        &tref.kind,
        TraitRefKind::BuiltinOrAuto {
            builtin_data: BuiltinImplData::NoopDestruct,
            ..
        }
    )
}

impl<'a> UllbcStatementTransformCtx<'a> {
    /// Transform a Drop to a Call that calls the drop_in_place method.
    /// If we cannot desugar this drop, we just leave it unchanged.
    fn transform_drop_to_call(&mut self, term: &mut Terminator) {
        if let TerminatorKind::Drop {
            kind: DropKind::Precise,
            place,
            tref,
            target,
            on_unwind,
        } = &mut term.kind
        {
            // check if this drop is noop
            if is_noop_destruct(tref) {
                term.kind = TerminatorKind::Goto {
                    target: target.clone(),
                };
                return;
            }

            // assign `&raw mut place` to a new variable
            let drop_arg =
                self.raw_borrow_to_new_var(place.clone(), RefKind::Mut, Some("drop_arg".into()));

            // Get the declaration id of drop_in_place from tref
            let trait_id = tref.trait_decl_ref.skip_binder.id;
            let Some(tdecl) = self.ctx.translated.trait_decls.get(trait_id) else {
                return;
            };
            let method_name = TraitItemName("drop_in_place".into());
            let Some(bound_method) = tdecl.methods.iter().find(|m| m.name() == method_name) else {
                // skip this drop if we cannot find its method id
                return;
            };
            let method_decl_id = bound_method.skip_binder.item.id;

            let drop_ret = self.fresh_var(Some("drop_ret".into()), Ty::mk_unit());
            let fn_ptr = FnPtr::new(
                FnPtrKind::Trait(tref.clone(), method_name, method_decl_id),
                GenericArgs::empty(),
            );
            let call = Call {
                func: FnOperand::Regular(fn_ptr),
                args: Vec::from([Operand::Move(drop_arg)]),
                dest: drop_ret,
            };
            term.kind = TerminatorKind::Call {
                call,
                target: target.clone(),
                on_unwind: on_unwind.clone(),
            };
        }
    }
}

pub struct Transform;

impl UllbcPass for Transform {
    fn transform_function(&self, ctx: &mut TransformCtx, decl: &mut FunDecl) {
        if !ctx.options.desugar_drops {
            return;
        }
        decl.transform_ullbc_terminators(ctx, |ctx, term| {
            ctx.transform_drop_to_call(term);
        });
    }
}
