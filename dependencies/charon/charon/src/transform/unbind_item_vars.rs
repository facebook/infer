//! Replace variables bound at the top-level with `Free` vars. This is for convenience for
//! consumers of the charon ast.
use derive_generic_visitor::*;
use index_vec::Idx;

use crate::ast::*;

use super::{TransformCtx, ctx::TransformPass};

/// Replace variables bound at the top-level with `Free` vars.
#[derive(Default, Visitor)]
pub(crate) struct UnbindVarVisitor {
    // Tracks the depth of binders we're inside of.
    binder_depth: DeBruijnId,
}

impl VisitorWithBinderDepth for UnbindVarVisitor {
    fn binder_depth_mut(&mut self) -> &mut DeBruijnId {
        &mut self.binder_depth
    }
}
impl VisitAstMut for UnbindVarVisitor {
    fn visit<'a, T: AstVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
        VisitWithBinderDepth::new(self).visit(x)
    }

    fn exit_de_bruijn_var<T: AstVisitable + Idx>(&mut self, var: &mut DeBruijnVar<T>) {
        match var {
            DeBruijnVar::Bound(dbid, varid) if *dbid == self.binder_depth => {
                *var = DeBruijnVar::Free(*varid)
            }
            DeBruijnVar::Bound(..) => {}
            DeBruijnVar::Free(_) => unreachable!("Found unexpected free variable"),
        }
    }
}

pub struct Check;
impl TransformPass for Check {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        let mut visitor = UnbindVarVisitor::default();
        for mut item in ctx.translated.all_items_mut() {
            let _ = item.drive_mut(&mut visitor);
        }
    }
}
