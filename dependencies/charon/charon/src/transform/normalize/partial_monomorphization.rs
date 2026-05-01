//! This module implements partial monomorphization, which allows specializing generic items on
//! some specific instanciation patterns. This is used by Aeneas to avoid nested mutable borrows:
//! we transform `Iter<'a, &'b mut T>` to `{Iter::<_, &mut U>}<'a, 'b, T>`, where
//! ```ignore
//! struct {Iter::<'a, &'b mut U>}<'a, 'b, U> {
//!   // the field of `Iter` but instantiated with `T -> &'b mut U`.
//! }
//! ```
//!
//! Note: We may need to partial-mono the same item multiple times: `Foo::<&mut A, B>`, `Foo::<A,
//! &mut B>`. Note also that partial-mono is infectious: `Foo<Bar<&mut A>>` generates `Bar::<&mut
//! A>` then `Foo::<Bar::<&mut A>>``.
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Display;
use std::mem;

use derive_generic_visitor::Visitor;
use index_vec::Idx;

use crate::ast::types_utils::TyVisitable;
use crate::ast::visitor::{VisitWithBinderDepth, VisitorWithBinderDepth};
use crate::formatter::IntoFormatter;
use crate::options::MonomorphizeMut;
use crate::pretty::FmtWithCtx;
use crate::register_error;
use crate::transform::ctx::TransformPass;
use crate::{transform::TransformCtx, ullbc_ast::*};

type MutabilityShape = Binder<GenericArgs>;

/// See the docs of `MutabilityShapeBuilder::compute_shape`.
#[derive(Visitor)]
struct MutabilityShapeBuilder<'pm, 'ctx> {
    pm: &'pm PartialMonomorphizer<'ctx>,
    /// The parameters that will constitute the final binder.
    params: GenericParams,
    /// The arguments to pass to the final binder to recover the input arguments.
    extracted: GenericArgs,
    /// Current depth under which we're visiting.
    binder_depth: DeBruijnId,
}

impl<'pm, 'ctx> MutabilityShapeBuilder<'pm, 'ctx> {
    /// Compute the mutability "shape" of a set of generic arguments by factoring out the minimal
    /// amount of information that still allows reconstructing the original arguments while keeping the
    /// "shape arguments" free of mutable borrows.
    ///
    /// For example, for input:
    ///   <u32, &'a mut &'b A, Option::<&'a mut bool>>
    /// we want to build:
    ///   binder<'a, 'b, A, B, C> <A, &'a mut B, Option::<&'b mut C>>
    /// from which we can recover the original arguments by instantiating it with:
    ///   <'a, 'a, u32, &'b A, bool>
    ///
    /// Formally, given `let (shape, shape_args) = get_mutability_shape(args);`, we have the following:
    /// - `shape.substitute(shape_args) == args`;
    /// - `shape_args` contains no infected types;
    /// - `shape` is as shallow as possible (i.e. takes just enough to get all the infected types
    ///     and not more).
    ///
    /// Note: the input arguments are assumed to have been already partially monomorphized, in the
    /// sense that we won't recurse inside ADT args because we assume any ADT applied to infected
    /// args to have been replaced with a fresh infected ADT.
    fn compute_shape(
        pm: &'pm PartialMonomorphizer<'ctx>,
        target_params: &GenericParams,
        args: &GenericArgs,
    ) -> (MutabilityShape, GenericArgs) {
        // We start with the implicit parameters from the original item. We'll need to substitute
        // them once we've figured out the mapping of explicit parameters, but we'll also be adding
        // new trait clauses potentially so we can't leave the vector empty (the ids would be
        // wrong).
        let mut shape_contents = args.clone();
        let mut builder = Self {
            pm,
            params: GenericParams {
                regions: IndexMap::new(),
                types: IndexMap::new(),
                const_generics: IndexMap::new(),
                ..target_params.clone()
            },
            extracted: GenericArgs {
                regions: IndexMap::new(),
                types: IndexMap::new(),
                const_generics: IndexMap::new(),
                trait_refs: mem::take(&mut shape_contents.trait_refs),
            },
            binder_depth: DeBruijnId::zero(),
        };

        // Traverse the generics and replace any non-infected type, region or const generic with a
        // fresh variable.
        let _ = VisitWithBinderDepth::new(&mut builder).visit(&mut shape_contents);

        let shape_params = {
            let mut shape_params = builder.params;
            // Now the explicit params in `shape_params` are correct, and the implicit params are a mix
            // of the old params and new trait clauses. The old params may refer to the old explicit
            // params which is wrong and must be fixed up.
            shape_params.trait_clauses = shape_params.trait_clauses.map_indexed(|i, x| {
                if i.index() < target_params.trait_clauses.slot_count() {
                    x.substitute_explicits(&shape_contents)
                } else {
                    x
                }
            });
            shape_params.trait_type_constraints =
                shape_params.trait_type_constraints.map_indexed(|i, x| {
                    if i.index() < target_params.trait_type_constraints.slot_count() {
                        x.substitute_explicits(&shape_contents)
                    } else {
                        x
                    }
                });
            shape_params.regions_outlive = shape_params
                .regions_outlive
                .into_iter()
                .enumerate()
                .map(|(i, x)| {
                    if i < target_params.regions_outlive.len() {
                        x.substitute_explicits(&shape_contents)
                    } else {
                        x
                    }
                })
                .collect();
            shape_params.types_outlive = shape_params
                .types_outlive
                .into_iter()
                .enumerate()
                .map(|(i, x)| {
                    if i < target_params.types_outlive.len() {
                        x.substitute_explicits(&shape_contents)
                    } else {
                        x
                    }
                })
                .collect();
            shape_params
        };

        // The first half of the trait params correspond to the original item clauses so we can
        // pass them unmodified.
        shape_contents.trait_refs = shape_params.identity_args().trait_refs;
        shape_contents
            .trait_refs
            .truncate(target_params.trait_clauses.slot_count());

        let shape_args = builder.extracted;
        let shape = Binder::new(BinderKind::Other, shape_params, shape_contents);
        (shape, shape_args)
    }

    /// Replace this value with a fresh variable, and record that we did so.
    fn replace_with_fresh_var<Id, Param, Arg>(
        &mut self,
        val: &mut Arg,
        mk_param: impl FnOnce(Id) -> Param,
        mk_value: impl FnOnce(DeBruijnVar<Id>) -> Arg,
    ) where
        Id: Idx + Display,
        Arg: TyVisitable + Clone,
        GenericParams: HasIdxMapOf<Id, Output = Param>,
        GenericArgs: HasIdxMapOf<Id, Output = Arg>,
    {
        let Some(shifted_val) = val.clone().move_from_under_binders(self.binder_depth) else {
            // Give up on this value.
            return;
        };
        // Record the mapping in the output `GenericArgs`.
        self.extracted.get_idx_map_mut().push(shifted_val);
        // Put a fresh param in place of `val`.
        let id = self.params.get_idx_map_mut().push_with(mk_param);
        *val = mk_value(DeBruijnVar::bound(self.binder_depth, id));
    }
}

impl<'pm, 'ctx> VisitorWithBinderDepth for MutabilityShapeBuilder<'pm, 'ctx> {
    fn binder_depth_mut(&mut self) -> &mut DeBruijnId {
        &mut self.binder_depth
    }
}

impl<'pm, 'ctx> VisitAstMut for MutabilityShapeBuilder<'pm, 'ctx> {
    fn visit<'a, T: AstVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
        VisitWithBinderDepth::new(self).visit(x)
    }

    fn enter_ty(&mut self, ty: &mut Ty) {
        if !self.pm.is_infected(ty) {
            self.replace_with_fresh_var(
                ty,
                |id| TypeParam::new(id, format!("T{id}")),
                |v| v.into(),
            );
        }
    }
    fn exit_ty_kind(&mut self, kind: &mut TyKind) {
        if let TyKind::Adt(TypeDeclRef {
            id: TypeId::Adt(id),
            generics,
        }) = kind
        {
            // Since the type was not replaced with a type var, it's an infected type. We've
            // traversed it so we have its final explicit arguments. Now we need to satisfy its
            // predicates. For that we add all its predicates to the new item, and pass those new
            // trait clauses to it.
            let Some(target_params) = self.pm.generic_params.get(&(*id).into()) else {
                return;
            };
            let Some(shifted_generics) =
                generics.clone().move_from_under_binders(self.binder_depth)
            else {
                // Give up on this value.
                return;
            };

            // Add the target predicates (properly substituted) to the new item params.
            let num_clauses_before_merge = self.params.trait_clauses.slot_count();
            self.params.merge_predicates_from(
                target_params
                    .clone()
                    .substitute_explicits(&shifted_generics),
            );

            // Record the trait arguments in the output `GenericArgs`.
            self.extracted
                .trait_refs
                .extend(shifted_generics.trait_refs);

            // Replace each trait ref with a clause var.
            for (target_clause_id, tref) in generics.trait_refs.iter_mut_indexed() {
                let clause_id = target_clause_id + num_clauses_before_merge;
                *tref =
                    self.params.trait_clauses[clause_id].identity_tref_at_depth(self.binder_depth);
            }
        }
    }
    fn enter_region(&mut self, r: &mut Region) {
        self.replace_with_fresh_var(r, |id| RegionParam::new(id, None), |v| v.into());
    }
    // TODO: we're missing type info for this
    // fn enter_const_generic(&mut self, cg: &mut ConstGeneric) {
    //     self.replace_with_fresh_var(cg, |id| {
    //         ConstGenericParam::new(id, format!("N{id}"), cg.ty().clone())
    //     });
    // }
    fn visit_trait_ref(&mut self, _tref: &mut TraitRef) -> ControlFlow<Self::Break> {
        // We don't touch trait refs or we'd risk adding duplicated extra params. Instead, we fix
        // them up in `exit_ty_kind` and `compute_shape`.
        ControlFlow::Continue(())
    }

    fn visit_constant_expr(
        &mut self,
        _: &mut ConstantExpr,
    ) -> ::std::ops::ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }
}

#[derive(Visitor)]
struct PartialMonomorphizer<'a> {
    ctx: &'a mut TransformCtx,
    /// Tracks the closest span to emit useful errors.
    span: Span,
    /// Whether we partially-monomorphize type declarations.
    specialize_adts: bool,
    /// Types that contain mutable references.
    infected_types: HashSet<TypeDeclId>,
    /// Map of generic params for each item. We can't use `ctx.translated` because while iterating
    /// over items the current item isn't available anymore, which would break recursive types.
    /// This also makes it possible to record the generics of our to-be-added items without adding
    /// them.
    generic_params: HashMap<ItemId, GenericParams>,
    /// Map of partial monomorphizations. The source item applied with the generic params gives the
    /// target item. The resulting partially-monomorphized item will have the binder params as
    /// generic params.
    partial_mono_shapes: SeqHashMap<(ItemId, MutabilityShape), ItemId>,
    /// Reverse of `partial_mono_shapes`.
    reverse_shape_map: HashMap<ItemId, (ItemId, MutabilityShape)>,
    /// Items that need to be processed.
    to_process: VecDeque<ItemId>,
}

impl<'a> PartialMonomorphizer<'a> {
    pub fn new(ctx: &'a mut TransformCtx, specialize_adts: bool) -> Self {
        // Compute the types that contain `&mut` (even indirectly). We actually can ignore
        // `&'static mut`, so we simply rely on our "lifetime mutability" computation.
        let infected_types: HashSet<_> = ctx
            .translated
            .type_decls
            .iter()
            .filter(|tdecl| {
                tdecl
                    .generics
                    .regions
                    .iter()
                    .any(|r| r.mutability.is_mutable())
            })
            .map(|tdecl| tdecl.def_id)
            .collect();

        // Record the generic params of all items.
        let generic_params: HashMap<ItemId, GenericParams> = ctx
            .translated
            .all_items()
            .map(|item| (item.id(), item.generic_params().clone()))
            .collect();

        // Enqueue all items to be processed.
        let to_process = ctx.translated.all_ids().collect();
        PartialMonomorphizer {
            ctx,
            span: Span::dummy(),
            specialize_adts,
            infected_types,
            generic_params,
            to_process,
            partial_mono_shapes: SeqHashMap::default(),
            reverse_shape_map: Default::default(),
        }
    }

    /// Whether this type is or contains a `&mut`. This assumes that we've already visited this
    /// type and partially monomorphized any ADT references.
    fn is_infected(&self, ty: &Ty) -> bool {
        match ty.kind() {
            TyKind::Ref(_, _, RefKind::Mut) => true,
            TyKind::Ref(_, ty, _)
            | TyKind::RawPtr(ty, _)
            | TyKind::Array(ty, _)
            | TyKind::Slice(ty) => self.is_infected(ty),
            TyKind::Adt(tref) if let TypeId::Adt(id) = tref.id => {
                let ty_infected = self.infected_types.contains(&id);
                let args_infected = if self.specialize_adts {
                    // Since we make sure to only call the method on a processed type, any type
                    // with infected arguments would have been replaced with a fresh instantiated
                    // (and infected type). Hence we don't need to check the arguments here, only
                    // the type id.
                    false
                } else {
                    tref.generics.types.iter().any(|ty| self.is_infected(ty))
                };
                ty_infected || args_infected
            }
            TyKind::Adt(..) => false,
            // A function pointer/item by itself doesn't carry any mutable reference, even if it
            // uses some in its signature. Compare with closures: a closure without captures
            // doesn't trigger partial mono regardless of its signature.
            TyKind::FnDef(..) | TyKind::FnPtr(..) => false,
            TyKind::DynTrait(_) => {
                register_error!(
                    self.ctx,
                    self.span,
                    "`dyn Trait` is unsupported with `--monomorphize-mut`"
                );
                false
            }
            TyKind::TypeVar(..)
            | TyKind::Literal(..)
            | TyKind::Never
            | TyKind::TraitType(..)
            | TyKind::PtrMetadata(..)
            | TyKind::Error(_) => false,
        }
    }

    /// Given that `generics` apply to item `id`, if any of the generics is infected we generate a
    /// reference to a new item obtained by partially instantiating item `id`. (That new item isn't
    /// added immediately but is added to the `to_process` queue to be created later).
    fn process_generics(&mut self, id: ItemId, generics: &GenericArgs) -> Option<DeclRef<ItemId>> {
        if !generics.types.iter().any(|ty| self.is_infected(ty)) {
            return None;
        }

        // If the type is already an instantiation, transform this reference into a reference to
        // the original type so we don't instantiate the instantiation.
        let mut new_generics;
        let (id, generics) = if let Some(&(base_id, ref shape)) = self.reverse_shape_map.get(&id) {
            new_generics = shape.clone().apply(generics);
            let _ = self.visit(&mut new_generics); // New instantiation may require cleanup.
            (base_id, &new_generics)
        } else {
            (id, generics)
        };

        // Split the args between the infected part and the non-infected part.
        let item_params = self.generic_params.get(&id)?;
        let (shape, shape_args) =
            MutabilityShapeBuilder::compute_shape(self, item_params, generics);

        // Create a new type id.
        let new_params = shape.params.clone();
        let key: (ItemId, MutabilityShape) = (id, shape);
        let new_id = *self
            .partial_mono_shapes
            .entry(key.clone())
            .or_insert_with(|| {
                let new_id = match id {
                    ItemId::Type(_) => {
                        let new_id = self.ctx.translated.type_decls.reserve_slot();
                        self.infected_types.insert(new_id);
                        new_id.into()
                    }
                    ItemId::Fun(_) => self.ctx.translated.fun_decls.reserve_slot().into(),
                    ItemId::Global(_) => self.ctx.translated.global_decls.reserve_slot().into(),
                    ItemId::TraitDecl(_) => self.ctx.translated.trait_decls.reserve_slot().into(),
                    ItemId::TraitImpl(_) => self.ctx.translated.trait_impls.reserve_slot().into(),
                };
                self.generic_params.insert(new_id, new_params);
                self.reverse_shape_map.insert(new_id, key);
                self.to_process.push_back(new_id);
                new_id
            });

        let fmt_ctx = self.ctx.into_fmt();
        trace!(
            "processing {}{}\n output: {}{}",
            id.with_ctx(&fmt_ctx),
            generics.with_ctx(&fmt_ctx),
            new_id.with_ctx(&fmt_ctx),
            shape_args.with_ctx(&fmt_ctx),
        );
        Some(DeclRef {
            id: new_id,
            generics: Box::new(shape_args),
            trait_ref: None,
        })
    }

    /// Traverse the item, replacing any type instantiations we don't want with references to
    /// soon-to-be-created partially-monomorphized types. This does not access the items in
    /// `self.translated`, which may be missing since we took `item` out for processing.
    pub fn process_item(&mut self, item: &mut ItemRefMut<'_>) {
        let _ = item.drive_mut(self);
    }

    /// Creates the item corresponding to this id by instantiating the item it is based on.
    ///
    /// This accesses the items in `self.translated`, which must therefore all be there.
    /// That's why items are created outside of `process_item`.
    pub fn create_pending_instantiation(&mut self, new_id: ItemId) -> ItemByVal {
        let (orig_id, shape) = &self.reverse_shape_map[&new_id];
        let mut decl = self
            .ctx
            .translated
            .get_item(*orig_id)
            .unwrap()
            .clone()
            .substitute_with_self(&shape.skip_binder, &TraitRefKind::SelfId);

        let mut decl_mut = decl.as_mut();
        decl_mut.set_id(new_id);
        *decl_mut.generic_params() = shape.params.clone();

        let name_ref = &mut decl_mut.item_meta().name;
        *name_ref = mem::take(name_ref).instantiate(shape.clone());
        self.ctx
            .translated
            .item_names
            .insert(new_id, decl.as_ref().item_meta().name.clone());

        decl
    }
}

impl VisitorWithSpan for PartialMonomorphizer<'_> {
    fn current_span(&mut self) -> &mut Span {
        &mut self.span
    }
}
impl VisitAstMut for PartialMonomorphizer<'_> {
    fn visit<'a, T: AstVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
        // Track a useful enclosing span, for error messages.
        VisitWithSpan::new(self).visit(x)
    }

    fn exit_type_decl_ref(&mut self, x: &mut TypeDeclRef) {
        if self.specialize_adts
            && let TypeId::Adt(id) = x.id
            && let Some(new_decl_ref) = self.process_generics(id.into(), &x.generics)
        {
            *x = new_decl_ref.try_into().unwrap()
        }
    }
    fn exit_fn_ptr(&mut self, x: &mut FnPtr) {
        // TODO: methods. any `Trait::method<&mut A>` requires monomorphizing all the instances of
        // that method just in case :>>>
        if let FnPtrKind::Fun(FunId::Regular(id)) = *x.kind
            && let Some(new_decl_ref) = self.process_generics(id.into(), &x.generics)
        {
            *x = new_decl_ref.try_into().unwrap()
        }
    }
    fn exit_fun_decl_ref(&mut self, x: &mut FunDeclRef) {
        if let Some(new_decl_ref) = self.process_generics(x.id.into(), &x.generics) {
            *x = new_decl_ref.try_into().unwrap()
        }
    }
    fn exit_global_decl_ref(&mut self, x: &mut GlobalDeclRef) {
        if let Some(new_decl_ref) = self.process_generics(x.id.into(), &x.generics) {
            *x = new_decl_ref.try_into().unwrap()
        }
    }
    fn exit_trait_decl_ref(&mut self, x: &mut TraitDeclRef) {
        if let Some(new_decl_ref) = self.process_generics(x.id.into(), &x.generics) {
            *x = new_decl_ref.try_into().unwrap()
        }
    }
    fn exit_trait_impl_ref(&mut self, x: &mut TraitImplRef) {
        if let Some(new_decl_ref) = self.process_generics(x.id.into(), &x.generics) {
            *x = new_decl_ref.try_into().unwrap()
        }
    }
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        let Some(include_types) = ctx.options.monomorphize_mut else {
            return;
        };
        // TODO: test name matcher, also with methods
        let mut visitor =
            PartialMonomorphizer::new(ctx, matches!(include_types, MonomorphizeMut::All));
        while let Some(id) = visitor.to_process.pop_front() {
            // Get the item corresponding to this id, either by creating it or by getting an
            // existing one.
            let mut decl = if visitor.reverse_shape_map.get(&id).is_some() {
                // Create the required item by instantiating the item it's based on.
                visitor.create_pending_instantiation(id)
            } else {
                // Take the item out so we can modify it. Warning: don't look up other items in the
                // meantime as this would break in recursive cases.
                match visitor.ctx.translated.remove_item(id) {
                    Some(decl) => decl,
                    None => continue,
                }
            };
            // Visit the item, replacing type instantiations with references to soon-to-be-created
            // partially-monomorphized types.
            visitor.process_item(&mut decl.as_mut());
            // Put the item back.
            visitor.ctx.translated.set_item_slot(id, decl);
        }
    }
}
