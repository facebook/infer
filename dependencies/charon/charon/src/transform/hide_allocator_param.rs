use derive_generic_visitor::*;
use itertools::Itertools;
use std::collections::HashSet;

use crate::{ast::*, name_matcher::NamePattern};

use super::{TransformCtx, ctx::TransformPass};

#[derive(Visitor)]
struct RemoveLastParamVisitor {
    types: HashSet<TypeId>,
}

impl VisitAstMut for RemoveLastParamVisitor {
    fn enter_type_decl_ref(&mut self, x: &mut TypeDeclRef) {
        if self.types.contains(&x.id) {
            // Remove the last param.
            x.generics.types.pop();
        }
    }
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        if !ctx.options.hide_allocator {
            return;
        }
        let types = &[
            "alloc::boxed::Box",
            "alloc::vec::Vec",
            "alloc::rc::Rc",
            "alloc::sync::Arc",
        ];

        let types: Vec<NamePattern> = types
            .into_iter()
            .map(|s| NamePattern::parse(s).unwrap())
            .collect_vec();
        let types: HashSet<TypeId> = ctx
            .translated
            .item_names
            .iter()
            .filter(|(_, name)| types.iter().any(|p| p.matches(&ctx.translated, name)))
            .filter_map(|(id, _)| id.as_type())
            .copied()
            .map(TypeId::Adt)
            .chain([TypeId::Builtin(BuiltinTy::Box)])
            .collect();

        for &id in &types {
            if let Some(&id) = id.as_adt()
                && let Some(tdecl) = ctx.translated.type_decls.get_mut(id)
            {
                struct SubstWithErrorVisitor(TypeVarId);
                impl VarsVisitor for SubstWithErrorVisitor {
                    fn visit_type_var(&mut self, v: TypeDbVar) -> Option<Ty> {
                        if let DeBruijnVar::Bound(DeBruijnId::ZERO, var_id) = v
                            && var_id == self.0
                        {
                            Some(TyKind::Error("removed allocator parameter".to_owned()).into_ty())
                        } else {
                            None
                        }
                    }
                }
                let tvar = tdecl.generics.types.pop().unwrap();
                tdecl.visit_vars(&mut SubstWithErrorVisitor(tvar.index));
            }
        }

        let _ = ctx
            .translated
            .drive_mut(&mut RemoveLastParamVisitor { types });
    }
}
