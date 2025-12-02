//! Move clauses on associated types to be parent clauses. The distinction is not semantically
//! meaningful. We should ideally to this directly when translating but this is currently
//! difficult; instead we do this as a post-processing pass.
use std::collections::HashMap;
use std::mem;

use crate::{ast::*, ids::Vector};

use super::{TransformCtx, ctx::TransformPass};

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        // For each trait, we move the item-local clauses to be top-level parent clauses, and
        // record the mapping from the old to the new ids.
        let trait_item_clause_ids: Vector<
            TraitDeclId,
            HashMap<TraitItemName, Vector<TraitClauseId, TraitClauseId>>,
        > = ctx.translated.trait_decls.map_ref_mut(|decl| {
            mem::take(&mut decl.type_clauses)
                .into_iter()
                .map(|(name, clauses)| {
                    let id_map = clauses.map(|mut clause| {
                        decl.parent_clauses.push_with(|id| {
                            clause.clause_id = id;
                            clause
                        })
                    });
                    (name, id_map)
                })
                .collect()
        });

        // Move the item-local trait refs to match what we did in the trait declarations.
        for timpl in ctx.translated.trait_impls.iter_mut() {
            for (_, refs) in mem::take(&mut timpl.type_clauses) {
                for trait_ref in refs {
                    // Note: this assumes that we listed the types in the same order as in the trait
                    // decl, which we do.
                    timpl.parent_trait_refs.push(trait_ref);
                }
            }
        }

        // Update trait refs.
        ctx.translated.dyn_visit_mut(|trkind: &mut TraitRefKind| {
            use TraitRefKind::*;
            match trkind {
                ItemClause(..) => take_mut::take(trkind, |trkind| {
                    let ItemClause(tref, item_name, item_clause_id) = trkind else {
                        unreachable!()
                    };
                    let new_id = (|| {
                        let new_id = *trait_item_clause_ids
                            .get(tref.trait_decl_ref.skip_binder.id)?
                            .get(&item_name)?
                            .get(item_clause_id)?;
                        Some(new_id)
                    })();
                    match new_id {
                        Some(new_id) => ParentClause(tref, new_id),
                        None => ItemClause(tref, item_name, item_clause_id),
                    }
                }),
                BuiltinOrAuto {
                    parent_trait_refs,
                    types,
                    ..
                } => {
                    for (_, _, ty_trait_refs) in types {
                        for tref in std::mem::take(ty_trait_refs) {
                            // Note: this assumes that we listed the types in the same order as in
                            // the trait decl, which we do.
                            parent_trait_refs.push(tref);
                        }
                    }
                }
                _ => {}
            }
        });
    }
}
