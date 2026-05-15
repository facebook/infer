use derive_generic_visitor::*;
use itertools::Itertools;
use std::collections::HashSet;

use crate::{ast::*, name_matcher::NamePattern};

use crate::transform::{TransformCtx, ctx::TransformPass};

#[derive(Visitor)]
struct RemoveMarkersVisitor {
    exclude: HashSet<TraitDeclId>,
}

impl RemoveMarkersVisitor {
    fn filter_trait_refs(&mut self, trait_refs: &mut IndexMap<TraitClauseId, TraitRef>) {
        for i in trait_refs.all_indices() {
            let tref = &trait_refs[i];
            if self.exclude.contains(&tref.trait_decl_ref.skip_binder.id) {
                trait_refs.remove(i);
            }
        }
    }

    fn filter_trait_clauses(&mut self, trait_clauses: &mut IndexMap<TraitClauseId, TraitParam>) {
        for i in trait_clauses.all_indices() {
            let clause = &trait_clauses[i];
            if self.exclude.contains(&clause.trait_.skip_binder.id) {
                trait_clauses.remove(i);
            }
        }
    }
}

// Remove clauses and trait refs that mention the offending traits. This relies on the fact that
// `IndexMap::remove` does not shift indices: it simply leaves an empty slot.
// FIXME: this is a footgun, it caused at least https://github.com/AeneasVerif/charon/issues/561.
impl VisitAstMut for RemoveMarkersVisitor {
    fn enter_generic_params(&mut self, args: &mut GenericParams) {
        self.filter_trait_clauses(&mut args.trait_clauses);
    }
    fn enter_generic_args(&mut self, args: &mut GenericArgs) {
        self.filter_trait_refs(&mut args.trait_refs);
    }
    fn enter_trait_decl(&mut self, tdecl: &mut TraitDecl) {
        self.filter_trait_clauses(&mut tdecl.implied_clauses);
    }
    fn enter_trait_impl(&mut self, timpl: &mut TraitImpl) {
        self.filter_trait_refs(&mut timpl.implied_trait_refs);
    }
    fn enter_trait_ref_kind(&mut self, x: &mut TraitRefKind) {
        if let TraitRefKind::BuiltinOrAuto {
            parent_trait_refs,
            types,
            ..
        } = x
        {
            self.filter_trait_refs(parent_trait_refs);
            for (_, assoc_ty) in types {
                self.filter_trait_refs(&mut assoc_ty.implied_trait_refs);
            }
        }
    }
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        // We remove any mention of these traits in generic parameters and arguments.
        let mut exclude = if ctx.options.hide_marker_traits {
            vec![
                "core::marker::Destruct",
                "core::marker::PointeeSized",
                "core::marker::MetaSized",
                "core::marker::Sized",
                "core::marker::Tuple",
                "core::marker::Send",
                "core::marker::Sync",
                "core::marker::Unpin",
                "core::clone::TrivialClone",
            ]
        } else {
            vec![]
        };
        if ctx.options.hide_allocator {
            exclude.push("core::alloc::Allocator");
        }
        if exclude.is_empty() {
            return;
        }

        let exclude: Vec<NamePattern> = exclude
            .into_iter()
            .map(|s| NamePattern::parse(s).unwrap())
            .collect_vec();
        let exclude: HashSet<TraitDeclId> = ctx
            .translated
            .item_names
            .iter()
            .filter(|(_, name)| exclude.iter().any(|p| p.matches(&ctx.translated, name)))
            .filter_map(|(id, _)| id.as_trait_decl())
            .copied()
            .collect();

        // Remove the marker traits and their methods.
        for &trait_id in &exclude {
            if let Some(tdecl) = &ctx.translated.trait_decls.get(trait_id) {
                for method in &tdecl.methods {
                    ctx.translated.fun_decls.remove(method.skip_binder.item.id);
                }
            }
            ctx.translated.trait_decls.remove(trait_id);
        }
        // Also remove any impls for these traits.
        for impl_id in ctx.translated.trait_impls.all_indices() {
            if let Some(timpl) = &ctx.translated.trait_impls.get(impl_id)
                && exclude.contains(&timpl.impl_trait.id)
            {
                for (_, method) in &timpl.methods {
                    ctx.translated.fun_decls.remove(method.skip_binder.id);
                }
                ctx.translated.trait_impls.remove(impl_id);
            }
        }

        let _ = ctx
            .translated
            .drive_mut(&mut RemoveMarkersVisitor { exclude });
    }
}
