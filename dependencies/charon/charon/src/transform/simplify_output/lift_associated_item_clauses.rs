//! Move clauses on non-generic associated types to be implied clauses of the trait. The
//! distinction is not semantically meaningful.
use std::collections::HashMap;
use std::mem;

use crate::{ast::*, ids::IndexMap};

use crate::transform::{TransformCtx, ctx::TransformPass};

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        // For each trait, we move the item-local clauses to be top-level parent clauses, and
        // record the mapping from the old to the new ids.
        let trait_item_clause_ids: IndexMap<
            TraitDeclId,
            HashMap<TraitItemName, IndexMap<TraitClauseId, TraitClauseId>>,
        > = ctx.translated.trait_decls.map_ref_mut(|decl| {
            decl.types
                .iter_mut()
                .filter(|assoc_ty| !assoc_ty.params.has_explicits())
                .map(|assoc_ty| {
                    let id_map =
                        mem::take(&mut assoc_ty.skip_binder.implied_clauses).map(|clause| {
                            let mut clause = clause.move_from_under_binder().unwrap();
                            decl.implied_clauses.push_with(|id| {
                                clause.clause_id = id;
                                clause
                            })
                        });
                    if assoc_ty.params.trait_clauses.is_empty() {
                        // Move non-trait-clause-predicates of non-GAT types to be predicates on
                        // the trait itself.
                        decl.generics.take_predicates_from(
                            mem::take(&mut assoc_ty.params)
                                .move_from_under_binder()
                                .unwrap(),
                        );
                    }
                    (assoc_ty.name().clone(), id_map)
                })
                .collect()
        });

        // Move the item-local trait refs to match what we did in the trait declarations.
        for timpl in ctx.translated.trait_impls.iter_mut() {
            for (_, assoc_ty) in &mut timpl.types {
                if !assoc_ty.params.has_explicits() {
                    for trait_ref in mem::take(&mut assoc_ty.skip_binder.implied_trait_refs) {
                        let trait_ref = trait_ref.move_from_under_binder().unwrap();
                        // Note: this assumes that we listed the types in the same order as in the
                        // trait decl, which we do.
                        timpl.implied_trait_refs.push(trait_ref);
                    }
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
                    for (_, assoc_ty) in types {
                        for tref in std::mem::take(&mut assoc_ty.implied_trait_refs) {
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
