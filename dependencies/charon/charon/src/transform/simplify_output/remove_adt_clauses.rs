use crate::ast::*;
use crate::transform::{TransformCtx, ctx::TransformPass};

#[derive(Visitor)]
struct RemoveAdtClausesVisitor;

impl VisitAstMut for RemoveAdtClausesVisitor {
    fn enter_type_decl(&mut self, decl: &mut TypeDecl) {
        decl.generics.trait_clauses.clear();
        decl.generics.trait_type_constraints.clear();
    }

    fn enter_type_decl_ref(&mut self, tref: &mut TypeDeclRef) {
        if matches!(tref.id, TypeId::Adt(_)) {
            tref.generics.trait_refs.clear();
        }
    }
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        if !ctx.options.remove_adt_clauses {
            return;
        }
        let _ = ctx.translated.drive_mut(&mut RemoveAdtClausesVisitor);
    }
}
