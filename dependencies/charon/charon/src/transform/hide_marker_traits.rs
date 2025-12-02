use derive_generic_visitor::*;
use itertools::Itertools;
use std::collections::HashSet;

use crate::{ast::*, name_matcher::NamePattern};

use super::{TransformCtx, ctx::TransformPass};

#[derive(Visitor)]
struct RemoveMarkersVisitor {
    exclude: HashSet<TraitDeclId>,
}

impl RemoveMarkersVisitor {
    fn filter_trait_refs(&mut self, trait_refs: &mut Vector<TraitClauseId, TraitRef>) {
        for i in trait_refs.all_indices() {
            let tref = &trait_refs[i];
            if self.exclude.contains(&tref.trait_decl_ref.skip_binder.id) {
                trait_refs.remove(i);
            }
        }
    }

    fn filter_trait_clauses(&mut self, trait_clauses: &mut Vector<TraitClauseId, TraitClause>) {
        for i in trait_clauses.all_indices() {
            let clause = &trait_clauses[i];
            if self.exclude.contains(&clause.trait_.skip_binder.id) {
                trait_clauses.remove(i);
            }
        }
    }
}

// Remove clauses and trait refs that mention the offending traits. This relies on the fact that
// `Vector::remove` does not shift indices: it simply leaves an empty slot.
// FIXME: this is a footgun, it caused at least https://github.com/AeneasVerif/charon/issues/561.
impl VisitAstMut for RemoveMarkersVisitor {
    fn enter_generic_params(&mut self, args: &mut GenericParams) {
        self.filter_trait_clauses(&mut args.trait_clauses);
    }
    fn enter_generic_args(&mut self, args: &mut GenericArgs) {
        self.filter_trait_refs(&mut args.trait_refs);
    }
    fn enter_trait_decl(&mut self, tdecl: &mut TraitDecl) {
        self.filter_trait_clauses(&mut tdecl.parent_clauses);
    }
    fn enter_trait_impl(&mut self, timpl: &mut TraitImpl) {
        self.filter_trait_refs(&mut timpl.parent_trait_refs);
    }
    fn enter_trait_ref_kind(&mut self, x: &mut TraitRefKind) {
        if let TraitRefKind::BuiltinOrAuto {
            parent_trait_refs,
            types,
            ..
        } = x
        {
            self.filter_trait_refs(parent_trait_refs);
            for (_, _, trait_refs) in types {
                self.filter_trait_refs(trait_refs);
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

        for &id in &exclude {
            ctx.translated.trait_decls.remove(id);
        }

        let _ = ctx
            .translated
            .drive_mut(&mut RemoveMarkersVisitor { exclude });
    }
}
