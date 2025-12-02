//! # Micro-pass: monomorphize all functions and types; at the end of this pass, all functions and types are monomorphic.
use derive_generic_visitor::*;
use indexmap::IndexMap;
use std::collections::HashSet;

use crate::ast::*;
use crate::transform::TransformCtx;
use std::fmt::Debug;

use super::ctx::TransformPass;

enum OptionHint<T, H> {
    Some(T),
    None,
    Hint(H),
}

impl<T, H> OptionHint<T, H> {
    fn is_some(&self) -> bool {
        match self {
            OptionHint::Some(_) => true,
            OptionHint::None => false,
            OptionHint::Hint(_) => false,
        }
    }

    fn hint_or<'a>(&'a self, hint: &'a H) -> &'a H {
        match self {
            OptionHint::Some(_) => hint,
            OptionHint::None => hint,
            OptionHint::Hint(h) => h,
        }
    }
}

#[derive(Default)]
struct PassData {
    // Map of (poly item, generic args) -> mono item
    // None indicates the item hasn't been monomorphized yet
    items: IndexMap<(AnyTransId, GenericArgs), OptionHint<AnyTransId, (AnyTransId, BoxedArgs)>>,
    worklist: Vec<AnyTransId>,
    visited: HashSet<AnyTransId>,
}

impl PassData {
    fn new() -> Self {
        Self::default()
    }
}

impl TranslatedCrate {
    // FIXME(Nadrieril): implement type&tref normalization and use that instead
    fn find_trait_impl_and_gargs(
        self: &Self,
        tref: &TraitRef,
    ) -> Option<(&TraitImpl, GenericArgs)> {
        match &tref.kind {
            TraitRefKind::TraitImpl(impl_ref) => {
                let trait_impl = self.trait_impls.get(impl_ref.id)?;
                Some((trait_impl, impl_ref.generics.as_ref().clone()))
            }
            TraitRefKind::ParentClause(p, clause) => {
                let (trait_impl, _) = self.find_trait_impl_and_gargs(p)?;
                let t_ref = trait_impl.parent_trait_refs.get(*clause)?;
                self.find_trait_impl_and_gargs(t_ref)
            }
            _ => None,
        }
    }
}

#[derive(Visitor)]
struct UsageVisitor<'a> {
    data: &'a mut PassData,
    krate: &'a TranslatedCrate,
}
impl UsageVisitor<'_> {
    fn found_use(
        &mut self,
        id: &AnyTransId,
        gargs: &GenericArgs,
        default: OptionHint<AnyTransId, (AnyTransId, BoxedArgs)>,
    ) {
        trace!("Mono: Found use: {:?} / {:?}", id, gargs);
        self.data
            .items
            .entry((*id, gargs.clone()))
            .or_insert(default);
    }
    fn found_use_ty(&mut self, tref: &TypeDeclRef) {
        match tref.id {
            TypeId::Adt(id) => {
                self.found_use(&AnyTransId::Type(id), &tref.generics, OptionHint::None)
            }
            _ => {}
        }
    }
    fn found_use_fn(&mut self, id: &FunDeclId, gargs: &GenericArgs) {
        self.found_use(&AnyTransId::Fun(*id), gargs, OptionHint::None);
    }
    fn found_use_global_decl_ref(&mut self, id: &GlobalDeclId, gargs: &GenericArgs) {
        self.found_use(&AnyTransId::Global(*id), gargs, OptionHint::None);
    }
    fn found_use_fn_hinted(
        &mut self,
        id: &FunDeclId,
        gargs: &GenericArgs,
        (h_id, h_args): (FunDeclId, BoxedArgs),
    ) {
        self.found_use(
            &AnyTransId::Fun(*id),
            gargs,
            OptionHint::Hint((AnyTransId::Fun(h_id), h_args)),
        );
    }
}
impl VisitAst for UsageVisitor<'_> {
    // we need to skip ItemMeta, as we don't want to collect the types in PathElem::Impl
    fn visit_item_meta(&mut self, _: &ItemMeta) -> ControlFlow<Infallible> {
        Continue(())
    }

    fn enter_aggregate_kind(&mut self, kind: &AggregateKind) {
        match kind {
            AggregateKind::Adt(tref, _, _) => self.found_use_ty(tref),
            _ => {}
        }
    }

    fn visit_ty_kind(&mut self, kind: &TyKind) -> ControlFlow<Infallible> {
        match kind {
            TyKind::Adt(tref) => {
                self.found_use_ty(tref);
            }
            TyKind::FnDef(binder) => {
                // we don't want to visit inside the binder, as it will have regions that
                // haven't been erased; instead we visit the erased version, and skip
                // the default "visit_inner" behaviour
                let _ = self.visit(&binder.clone().erase());
                return Continue(());
            }
            _ => {}
        };
        self.visit_inner(kind)
    }

    fn enter_fn_ptr(&mut self, fn_ptr: &FnPtr) {
        match fn_ptr.func.as_ref() {
            FunIdOrTraitMethodRef::Fun(FunId::Regular(id)) => {
                self.found_use_fn(&id, &fn_ptr.generics)
            }
            FunIdOrTraitMethodRef::Trait(t_ref, name, id) => {
                let Some((trait_impl, impl_gargs)) = self.krate.find_trait_impl_and_gargs(t_ref)
                else {
                    return;
                };
                let (_, bound_fn) = trait_impl.methods().find(|(n, _)| n == name).unwrap();
                let fn_ref: Binder<Binder<FunDeclRef>> = Binder::new(
                    BinderKind::Other,
                    trait_impl.generics.clone(),
                    bound_fn.clone(),
                );
                // This is the actual function we need to call!
                // Whereas id is the trait method reference(?)
                let fn_ref = fn_ref.apply(&impl_gargs).apply(&fn_ptr.generics);
                let gargs_key = fn_ptr
                    .generics
                    .clone()
                    .concat(&t_ref.trait_decl_ref.skip_binder.generics);
                self.found_use_fn_hinted(&id, &gargs_key, (fn_ref.id, fn_ref.generics))
            }
            // These can't be monomorphized, since they're builtins
            FunIdOrTraitMethodRef::Fun(FunId::Builtin(..)) => {}
        }
    }

    fn enter_global_decl_ref(&mut self, glob: &GlobalDeclRef) {
        self.found_use_global_decl_ref(&glob.id, &glob.generics);
    }
}

// Akin to UsageVisitor, but substitutes all uses of generics with the monomorphized versions
// This is a two-step process, because we can't mutate the translation context with new definitions
// while also mutating the existing definitions.
#[derive(Visitor)]
struct SubstVisitor<'a> {
    data: &'a PassData,
}
impl SubstVisitor<'_> {
    fn subst_use<T, F>(&mut self, id: &mut T, gargs: &mut GenericArgs, of: F)
    where
        T: Into<AnyTransId> + Debug + Copy,
        F: Fn(&AnyTransId) -> Option<&T>,
    {
        trace!("Mono: Subst use: {:?} / {:?}", id, gargs);
        // Erase regions.
        gargs.regions.iter_mut().for_each(|r| *r = Region::Erased);
        let key = ((*id).into(), gargs.clone());
        let subst = self.data.items.get(&key);
        if let Some(OptionHint::Some(any_id)) = subst
            && let Some(subst_id) = of(any_id)
        {
            *id = *subst_id;
            *gargs = GenericArgs::empty();
        } else {
            warn!("Substitution missing for {:?} / {:?}", id, gargs);
        }
    }
    fn subst_use_ty(&mut self, tref: &mut TypeDeclRef) {
        match &mut tref.id {
            TypeId::Adt(id) => {
                self.subst_use(id, &mut tref.generics, AnyTransId::as_type);
            }
            _ => {}
        }
    }
    fn subst_use_fun(&mut self, id: &mut FunDeclId, gargs: &mut GenericArgs) {
        self.subst_use(id, gargs, AnyTransId::as_fun);
    }
    fn subst_use_glob(&mut self, id: &mut GlobalDeclId, gargs: &mut GenericArgs) {
        self.subst_use(id, gargs, AnyTransId::as_global);
    }
}

impl VisitAstMut for SubstVisitor<'_> {
    fn enter_aggregate_kind(&mut self, kind: &mut AggregateKind) {
        match kind {
            AggregateKind::Adt(tref, _, _) => self.subst_use_ty(tref),
            _ => {}
        }
    }

    fn enter_ty_kind(&mut self, kind: &mut TyKind) {
        match kind {
            TyKind::Adt(tref) => self.subst_use_ty(tref),
            TyKind::FnDef(binder) => {
                // erase the FnPtr binder, as we'll monomorphise its content
                if let FnPtr {
                    func: box FunIdOrTraitMethodRef::Fun(FunId::Regular(id)),
                    generics,
                } = binder.clone().erase()
                {
                    *binder = RegionBinder::empty(FnPtr {
                        func: Box::new(FunIdOrTraitMethodRef::Fun(FunId::Regular(id))),
                        generics,
                    });
                }
            }
            _ => {}
        }
    }

    fn enter_fn_ptr(&mut self, fn_ptr: &mut FnPtr) {
        match fn_ptr.func.as_mut() {
            FunIdOrTraitMethodRef::Fun(FunId::Regular(fun_id)) => {
                self.subst_use_fun(fun_id, &mut fn_ptr.generics)
            }
            FunIdOrTraitMethodRef::Trait(t_ref, _, fun_id) => {
                let mut gargs_key = fn_ptr
                    .generics
                    .clone()
                    .concat(&t_ref.trait_decl_ref.skip_binder.generics);
                self.subst_use_fun(fun_id, &mut gargs_key);
                fn_ptr.generics = Box::new(gargs_key);
            }
            // These can't be monomorphized, since they're builtins
            FunIdOrTraitMethodRef::Fun(FunId::Builtin(..)) => {}
        }
    }

    fn exit_place(&mut self, place: &mut Place) {
        match &mut place.kind {
            // FIXME(Nadrieril): remove this id, replace with a helper fn
            PlaceKind::Projection(inner, ProjectionElem::Field(FieldProjKind::Adt(id, _), _)) => {
                // Trick, we don't know the generics but the projected place does, so
                // we substitute it there, then update our current id.
                let tref = inner.ty.as_adt().unwrap();
                *id = *tref.id.as_adt().unwrap()
            }
            _ => {}
        }
    }

    fn enter_global_decl_ref(&mut self, glob: &mut GlobalDeclRef) {
        self.subst_use_glob(&mut glob.id, &mut glob.generics);
    }
}

#[derive(Visitor)]
#[allow(dead_code)]
struct MissingIndexChecker<'a> {
    krate: &'a TranslatedCrate,
    current_item: Option<AnyTransItem<'a>>,
}
impl VisitAst for MissingIndexChecker<'_> {
    fn enter_fun_decl_id(&mut self, id: &FunDeclId) {
        if self.krate.fun_decls.get(*id).is_none() {
            panic!(
                "Missing function declaration for id: {:?}, in {:?}",
                id, self.current_item
            );
        }
    }

    fn enter_trait_impl_id(&mut self, id: &TraitImplId) {
        if self.krate.trait_impls.get(*id).is_none() {
            panic!(
                "Missing trait implementation for id: {:?}, in {:?}",
                id, self.current_item
            );
        }
    }

    fn enter_trait_decl_id(&mut self, id: &TraitDeclId) {
        if self.krate.trait_decls.get(*id).is_none() {
            panic!(
                "Missing trait declaration for id: {:?}, in {:?}",
                id, self.current_item
            );
        }
    }

    fn enter_type_decl_id(&mut self, id: &TypeDeclId) {
        if self.krate.type_decls.get(*id).is_none() {
            panic!(
                "Missing type declaration for id: {:?}, in {:?}",
                id, self.current_item
            );
        }
    }
}

fn find_uses(data: &mut PassData, krate: &TranslatedCrate, item: &AnyTransItem) {
    let mut visitor = UsageVisitor { data, krate };
    let _ = item.drive(&mut visitor);
}

fn subst_uses<T: AstVisitable + Debug>(data: &PassData, item: &mut T) {
    let mut visitor = SubstVisitor { data };
    let _ = item.drive_mut(&mut visitor);
}

// fn check_missing_indices(krate: &TranslatedCrate) {
//     let mut visitor = MissingIndexChecker {
//         krate,
//         current_item: None,
//     };
//     for item in krate.all_items() {
//         visitor.current_item = Some(item);
//         item.drive(&mut visitor);
//     }
// }

// fn path_for_generics(gargs: &GenericArgs) -> PathElem {
//     PathElem::Ident(gargs.to_string(), Disambiguator::ZERO)
// }

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        // Check the option which instructs to ignore this pass
        if !ctx.options.monomorphize_as_pass {
            return;
        }

        // From https://doc.rust-lang.org/nightly/nightly-rustc/rustc_monomorphize/collector/index.html#general-algorithm
        //
        // The purpose of the algorithm implemented in this module is to build the mono item
        // graph for the current crate. It runs in two phases:
        // 1. Discover the roots of the graph by traversing the HIR of the crate.
        // 2. Starting from the roots, find uses by inspecting the MIR representation of the
        //    item corresponding to a given node, until no more new nodes are found.
        //
        // The roots of the mono item graph correspond to the public non-generic syntactic
        // items in the source code. We find them by walking the HIR of the crate, and whenever
        // we hit upon a public function, method, or static item, we create a mono item
        // consisting of the items DefId and, since we only consider non-generic items, an
        // empty type-parameters set.
        //
        // Given a mono item node, we can discover uses by inspecting its MIR. We walk the MIR
        // to find other mono items used by each mono item. Since the mono item we are
        // currently at is always monomorphic, we also know the concrete type arguments of its
        // used mono items. The specific forms a use can take in MIR are quite diverse: it
        // includes calling functions/methods, taking a reference to a function/method, drop
        // glue, and unsizing casts.

        // In our version of the algorithm, we do the following:
        // 1. Find all the roots, adding them to the worklist.
        // 2. For each item in the worklist:
        //    a. Find all the items it uses, adding them to the worklist and the generic
        //      arguments to the item.
        //    b. Mark the item as visited

        // Final list of monomorphized items: { (poly item, generic args) -> mono item }
        let mut data = PassData::new();

        let empty_gargs = GenericArgs::empty();

        // Find the roots of the mono item graph
        for (id, item) in ctx.translated.all_items_with_ids() {
            match item {
                AnyTransItem::Fun(f) if f.signature.generics.is_empty() => {
                    data.items
                        .insert((id, empty_gargs.clone()), OptionHint::Some(id));
                    data.worklist.push(id);
                }
                _ => {}
            }
        }

        // Iterate over worklist -- these items are always monomorphic!
        while let Some(id) = data.worklist.pop() {
            if data.visited.contains(&id) {
                continue;
            }
            data.visited.insert(id);

            // 1. Find new uses
            let Some(item) = ctx.translated.get_item(id) else {
                trace!("Couldn't find item {:} in translated items?", id);
                continue;
            };
            find_uses(&mut data, &ctx.translated, &item);

            // 2. Iterate through all newly discovered uses
            for ((id, gargs), mono) in data.items.iter_mut() {
                if mono.is_some() {
                    continue;
                }

                // a. Monomorphize the items if they're polymorphic, add them to the worklist
                let new_mono = if gargs.is_empty() {
                    *id
                } else {
                    match id {
                        AnyTransId::Fun(_) => {
                            let key_pair = (id.clone(), Box::new(gargs.clone()));
                            let (AnyTransId::Fun(fun_id), gargs) = mono.hint_or(&key_pair) else {
                                panic!("Unexpected ID type in hint_or");
                            };
                            let fun = ctx.translated.fun_decls.get(*fun_id).unwrap();
                            let mut fun_sub = fun.clone().substitute(gargs);
                            fun_sub.signature.generics = GenericParams::empty();
                            fun_sub
                                .item_meta
                                .name
                                .name
                                .push(PathElem::Monomorphized(gargs.clone()));

                            let fun_id_sub = ctx.translated.fun_decls.push_with(|id| {
                                fun_sub.def_id = id;
                                fun_sub
                            });

                            AnyTransId::Fun(fun_id_sub)
                        }
                        AnyTransId::Type(typ_id) => {
                            let typ = ctx.translated.type_decls.get(*typ_id).unwrap();
                            let mut typ_sub = typ.clone().substitute(gargs);
                            typ_sub.generics = GenericParams::empty();
                            typ_sub
                                .item_meta
                                .name
                                .name
                                .push(PathElem::Monomorphized(gargs.clone().into()));

                            let typ_id_sub = ctx.translated.type_decls.push_with(|id| {
                                typ_sub.def_id = id;
                                typ_sub
                            });

                            AnyTransId::Type(typ_id_sub)
                        }
                        AnyTransId::Global(g_id) => {
                            let Some(glob) = ctx.translated.global_decls.get(*g_id) else {
                                // Something odd happened -- we ignore and move on
                                *mono = OptionHint::Some(*id);
                                warn!("Found a global that has no associated declaration");
                                continue;
                            };
                            let mut glob_sub = glob.clone().substitute(gargs);
                            glob_sub.generics = GenericParams::empty();
                            glob_sub
                                .item_meta
                                .name
                                .name
                                .push(PathElem::Monomorphized(gargs.clone().into()));

                            let init = ctx.translated.fun_decls.get(glob.init).unwrap();
                            let mut init_sub = init.clone().substitute(gargs);
                            init_sub.signature.generics = GenericParams::empty();
                            init_sub
                                .item_meta
                                .name
                                .name
                                .push(PathElem::Monomorphized(gargs.clone().into()));

                            let init_id_sub = ctx.translated.fun_decls.push_with(|id| {
                                init_sub.def_id = id;
                                glob_sub.init = id;
                                init_sub
                            });

                            let g_id_sub = ctx.translated.global_decls.push_with(|id| {
                                glob_sub.def_id = id;
                                glob_sub
                            });

                            data.worklist.push(AnyTransId::Fun(init_id_sub));

                            AnyTransId::Global(g_id_sub)
                        }
                        _ => todo!("Unhandled monomorphization target ID {:?}", id),
                    }
                };
                trace!(
                    "Mono: Monomorphized {:?} with {:?} to {:?}",
                    id, gargs, new_mono
                );
                if id != &new_mono {
                    trace!(" - From {:?}", ctx.translated.get_item(id.clone()));
                    trace!(" - To {:?}", ctx.translated.get_item(new_mono.clone()));
                }
                *mono = OptionHint::Some(new_mono);
                data.worklist.push(new_mono);

                let Some(item) = ctx.translated.get_item(new_mono) else {
                    trace!("Missing monomorphised item {new_mono:?}");
                    continue;
                };
                ctx.translated
                    .item_names
                    .insert(new_mono, item.item_meta().name.clone());
            }

            // 3. Substitute all generics with the monomorphized versions
            let Some(item) = ctx.translated.get_item_mut(id) else {
                panic!("Couldn't find item {:} in translated items.", id)
            };
            match item {
                AnyTransItemMut::Fun(f) => subst_uses(&data, f),
                AnyTransItemMut::Type(t) => subst_uses(&data, t),
                AnyTransItemMut::TraitImpl(t) => subst_uses(&data, t),
                AnyTransItemMut::Global(g) => subst_uses(&data, g),
                AnyTransItemMut::TraitDecl(t) => subst_uses(&data, t),
            };
        }

        // Now, remove all polymorphic items from the translation context, as all their
        // uses have been monomorphized and substituted
        ctx.translated
            .fun_decls
            .retain(|f| data.visited.contains(&AnyTransId::Fun(f.def_id)));
        ctx.translated
            .type_decls
            .retain(|t| data.visited.contains(&AnyTransId::Type(t.def_id)));
        ctx.translated
            .global_decls
            .retain(|g| data.visited.contains(&AnyTransId::Global(g.def_id)));
        // ctx.translated.trait_impls.retain(|t| t.generics.is_empty());

        // TODO: Currently we don't update all TraitImpls/TraitDecls with the monomorphized versions
        //       and removing the polymorphic ones, so this fails.
        // Finally, ensure we didn't leave any IDs un-replaced
        // check_missing_indices(&ctx.translated);
    }
}
