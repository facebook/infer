//! Change trait associated types to be type parameters instead. E.g.
//! ```rust,ignore
//! trait Iterator {
//!     type Item;
//! }
//! fn merge<I, J>(...) -> ...
//! where
//!     I: Iterator,
//!     J: Iterator<Item = I::Item>,
//! {}
//! // becomes
//! trait Iterator<Item> {
//! }
//! fn merge<I, J, Item>(...) -> ...
//! where
//!     I: Iterator<Item>,
//!     J: Iterator<Item>,
//! {}
//! ```
//!
//! This pass only transforms traits that are selected with the `--remove-associated-types` option.
//! The pass also normalizes associated types everywhere.
//!
//! The pass works as follows:
//! 1. We compute for each item the list of associated types that need to be provided a value,
//!    represented with `AssocTypePath`. E.g. the above example gives us one extra param for
//!    `Iterator`, corresponding to `Self::Item`.
//!    If the associated type was constrained by a `Trait::Type = T` clause, we remember that.
//! 2. For each item, for each such associated type for which we don't have a value, we add a type
//!    parameter to the item. We then update each `GenericArgs`,`TraitRef` and `TyKind::TraitType`
//!    pointing to a modified item. This includes going into function and type bodies to replace
//!    all trait type references with our new parameters.
//!
//! Note that the first step is recursive: if a trait has parent clauses, types needed by these
//! parent clauses will be needed by the trait itself too.
//! ```rust,ignore
//! trait Foo {
//!     type Item: Bar;
//! }
//! trait Bar {
//!     type Output;
//! }
//! // becomes:
//! trait Foo<Item, Bar_Output>
//! where Item: Bar<Bar_Output> {}
//! trait Bar<Output> {}
//! ```
//!
//! In this process we detect recursive cases that we can't handle and skip them. For example:
//! ```rust,ignore
//! trait Bar {
//!     type BarTy;
//! }
//! trait Foo {
//!     type FooTy: Foo + Bar;
//! }
//! // becomes:
//! trait Bar<BarTy> {}
//! trait Foo {
//!     type FooTy: Foo + Bar<Self::FooTy_BarTy>;
//!     // We need to supply an argument to `Bar` but we can't add a type parameter, so we add a
//!     // new associated type.
//!     type FooTy_BarTy;
//! }
//! ```
//!
//! Limitations:
//! - we're missing assoc type info in `dyn Trait`, so we can't fixup the generics there.
//! - we don't track bound lifetimes in quantified clauses properly (<https://github.com/AeneasVerif/charon/issues/534>).
//! - type aliases don't have the correct clauses in scope (<https://github.com/AeneasVerif/charon/issues/531>).
//! - we don't take into account unicity of trait implementations. This means we won't detect type
//! equalities due to the same trait predicate appearing twice, or a trait predicate coinciding
//! with an existing trait impl. See the `dictionary_passing_style_woes.rs` test file.
use derive_generic_visitor::*;
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    mem,
};

use macros::EnumAsGetters;

use crate::{ast::*, formatter::IntoFormatter, ids::Vector, pretty::FmtWithCtx, register_error};

use super::{TransformCtx, ctx::TransformPass, utils::GenericsSource};

/// Represent some `TraitRef`s as paths for easier manipulation.
use trait_ref_path::*;
mod trait_ref_path {
    use macros::{EnumIsA, EnumToGetters};

    use crate::ast::*;

    /// A base clause: the special `Self: Trait` clause present in trait declarations, or a local
    /// clause.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIsA, EnumToGetters)]
    pub enum BaseClause {
        SelfClause,
        Local(DeBruijnVar<TraitClauseId>),
    }

    /// A `TraitRef` represented as a path.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TraitRefPath {
        /// The base clause we start from.
        pub base: BaseClause,
        /// Starting from `base`, recursively take the ith parent clause of the current clause. Each id
        /// corresponds to a parent of the previous one, e.g.
        /// ```rust,ignore
        /// trait Foo {
        ///     type FooTy;
        /// }
        /// trait Bar: Foo {} // `Self: Foo` is parent clause 0
        /// trait Baz: Copy // `Self: Copy` is parent clause 0
        /// {
        ///     // `Self::BazTy: Bar` is parent clause 1
        ///     // `<Self::BazTy as Foo>::FooTy = bool` is type constraint 0
        ///     // The `TraitRefPath` for `<Self::BazTy as Foo>` has base `Self` and `parent_path == [1, 0]`.
        ///     type BazTy: Bar<FooTy = bool>;
        /// }
        /// ```
        pub parent_path: Vec<TraitClauseId>,
    }

    impl TraitRefPath {
        /// Make a trait ref that refers to the special `Self` clause.
        pub fn self_ref() -> Self {
            Self {
                base: BaseClause::SelfClause,
                parent_path: vec![],
            }
        }
        /// Make a trait ref that refers to the given clause at depth 0.
        pub fn local_clause(clause_id: TraitClauseId) -> Self {
            Self {
                base: BaseClause::Local(DeBruijnVar::new_at_zero(clause_id)),
                parent_path: vec![],
            }
        }
        /// Make a trait ref that refers to the given parent clause on `Self`.
        /// Given a ref on a local clause, move it to refer to `Self`. This is used when we generate
        /// paths for the `GenericParams` of a trait: within the `GenericParams` we don't know whether
        /// we're talking about local clauses of trait implied clauses; we fix this here.
        pub fn parent_clause(id: TraitClauseId) -> Self {
            Self {
                base: BaseClause::SelfClause,
                parent_path: vec![id],
            }
        }

        pub fn with_assoc_type(self, type_name: TraitItemName) -> AssocTypePath {
            AssocTypePath {
                tref: self,
                type_name,
            }
        }

        /// Given a ref on `Self`, make it into a ref on the given clause.
        pub fn on_local_clause(&self, id: TraitClauseId) -> Self {
            assert!(matches!(self.base, BaseClause::SelfClause));
            let mut new_ref = self.clone();
            new_ref.base = BaseClause::Local(DeBruijnVar::new_at_zero(id));
            new_ref
        }

        /// Given a ref on `Self`, apply it on top of given ref.
        pub fn on_tref(&self, tref: &TraitRefPath) -> TraitRefPath {
            assert!(matches!(self.base, BaseClause::SelfClause));
            let mut new_ref = tref.clone();
            new_ref.parent_path.extend(self.parent_path.iter().copied());
            new_ref
        }

        /// References the same clause one level up.
        pub fn pop_base(&self) -> Self {
            let mut new_ref = self.clone();
            match self.base {
                BaseClause::Local(_) => {
                    new_ref.base = BaseClause::SelfClause;
                }
                BaseClause::SelfClause => panic!(),
            }
            new_ref
        }

        /// For a ref based on `Self` parent clauses, this returns the first parent id and the rest of
        /// the ref (that applies to the parent trait).
        pub fn pop_first_parent(&self) -> Option<(TraitClauseId, Self)> {
            assert!(self.base.is_self_clause());
            if self.parent_path.is_empty() {
                None
            } else {
                let mut this = self.clone();
                let clause_id = this.parent_path.remove(0);
                Some((clause_id, this))
            }
        }

        fn move_from_under_binders(mut self, depth: DeBruijnId) -> Option<Self> {
            match &mut self.base {
                BaseClause::SelfClause => {}
                BaseClause::Local(var) => *var = var.move_from_under_binders(depth)?,
            }
            Some(self)
        }
    }

    impl TraitRefKind {
        pub fn to_path(&self) -> Option<TraitRefPath> {
            match self {
                TraitRefKind::SelfId => Some(TraitRefPath {
                    base: BaseClause::SelfClause,
                    parent_path: vec![],
                }),
                TraitRefKind::Clause(id) => Some(TraitRefPath {
                    base: BaseClause::Local(*id),
                    parent_path: vec![],
                }),
                TraitRefKind::ParentClause(tref, id) => {
                    let mut path = tref.to_path()?;
                    path.parent_path.push(*id);
                    Some(path)
                }
                TraitRefKind::ItemClause(..) => unreachable!(),
                TraitRefKind::TraitImpl(..)
                | TraitRefKind::BuiltinOrAuto { .. }
                | TraitRefKind::Dyn(..)
                | TraitRefKind::Unknown(..) => None,
            }
        }
    }

    impl TraitRef {
        pub fn to_path(&self) -> Option<TraitRefPath> {
            self.kind.to_path()
        }
    }

    /// The path to an associated type that depends on a local clause.
    #[derive(Debug, Clone)]
    pub struct AssocTypePath {
        /// The trait clause that has the associated type.
        pub tref: TraitRefPath,
        /// The name of the associated type.
        pub type_name: TraitItemName,
    }

    impl AssocTypePath {
        /// See [`TraitRefPath::on_local_clause`].
        pub fn on_local_clause(&self, id: TraitClauseId) -> AssocTypePath {
            Self {
                tref: self.tref.on_local_clause(id),
                type_name: self.type_name.clone(),
            }
        }

        /// See [`TraitRefPath::on_tref`].
        pub fn on_tref(&self, tref: &TraitRefPath) -> AssocTypePath {
            Self {
                tref: self.tref.on_tref(&tref),
                type_name: self.type_name.clone(),
            }
        }

        /// See [`TraitRefPath::pop_base`].
        pub fn pop_base(&self) -> Self {
            Self {
                tref: self.tref.pop_base(),
                type_name: self.type_name.clone(),
            }
        }

        /// For a ref based on `Self` parent clauses, this returns the first parent id and the rest of
        /// the ref (that applies to the parent trait).
        pub fn pop_first_parent(&self) -> Option<(TraitClauseId, Self)> {
            let (clause_id, sub_tref) = self.tref.pop_first_parent()?;
            let sub_path = Self {
                tref: sub_tref,
                type_name: self.type_name.clone(),
            };
            Some((clause_id, sub_path))
        }

        /// Transform a trait type constraint into a (path, ty) pair. If the constraint does not refer
        /// to a local clause, we return it unchanged.
        pub fn from_constraint(cstr: &RegionBinder<TraitTypeConstraint>) -> Option<Self> {
            // `unwrap()` is ok because a path only contains clause variables, which can't be bound
            // in a predicate binder.
            let tref = cstr
                .skip_binder
                .trait_ref
                .to_path()?
                .move_from_under_binders(DeBruijnId::one())
                .unwrap();
            Some(AssocTypePath {
                tref,
                type_name: cstr.skip_binder.type_name.clone(),
            })
        }

        /// Construct a name that can be used to name a new type parameter/associated type
        /// corresponding to this path.
        pub fn to_name(&self) -> String {
            use std::fmt::Write;
            let mut buf = match &self.tref.base {
                BaseClause::SelfClause => "Self".to_string(),
                BaseClause::Local(var) => {
                    format!("Clause{}", var.bound_at_depth(DeBruijnId::zero()).unwrap())
                }
            };
            for id in &self.tref.parent_path {
                let _ = write!(&mut buf, "_Clause{id}");
            }
            let _ = write!(&mut buf, "_{}", self.type_name);
            buf
        }
    }

    impl std::fmt::Display for AssocTypePath {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self.tref.base {
                BaseClause::SelfClause => write!(f, "Self")?,
                BaseClause::Local(var) => write!(f, "Clause{var}")?,
            };
            for id in &self.tref.parent_path {
                write!(f, "::Clause{id}")?;
            }
            write!(f, "::{}", self.type_name)?;
            Ok(())
        }
    }
}

/// Efficient representation of a set of `<Type as Trait>::Type = T` constraints.
use type_constraint_set::*;
mod type_constraint_set {
    use std::collections::HashMap;

    use super::trait_ref_path::*;
    use crate::ast::*;

    /// A set of local `TraitTypeConstraint`s, represented as a trie.
    #[derive(Default, Clone)]
    pub struct TypeConstraintSet {
        /// For each base clause, a sub-trie of type constraints.
        clauses: HashMap<BaseClause, TypeConstraintSetInner>,
    }

    /// A set of `TraitTypeConstraint`s that apply to a trait.
    #[derive(Debug, Default, Clone)]
    struct TypeConstraintSetInner {
        /// The types that correspond to `Self::<Name>` for each `Name`. We also remember the id of the
        /// original constraint if applicable.
        assoc_tys: HashMap<TraitItemName, (Option<TraitTypeConstraintId>, Ty)>,
        /// The types constraints that depend on the ith parent clause.
        parent_clauses: Vector<TraitClauseId, Self>,
    }

    impl TypeConstraintSet {
        pub fn from_constraints(
            constraints: &Vector<TraitTypeConstraintId, RegionBinder<TraitTypeConstraint>>,
        ) -> Self {
            let mut this = TypeConstraintSet::default();
            for (i, c) in constraints.iter_indexed() {
                this.insert_type_constraint(i, c);
            }
            this
        }

        /// Add a type constraint to the set.
        fn insert_inner(
            &mut self,
            path: &AssocTypePath,
            cid: Option<TraitTypeConstraintId>,
            ty: Ty,
        ) {
            let mut trie = self.clauses.entry(path.tref.base).or_default();
            for id in &path.tref.parent_path {
                trie = trie
                    .parent_clauses
                    .get_or_extend_and_insert(*id, Default::default);
            }
            trie.assoc_tys.insert(path.type_name.clone(), (cid, ty));
        }

        /// Add a type constraint to the set that doesn't come from a `TraitTypeConstraint`.
        pub fn insert_path(&mut self, path: &AssocTypePath, ty: Ty) {
            self.insert_inner(path, None, ty);
        }

        /// Add a type constraint to the set.
        fn insert_type_constraint(
            &mut self,
            cid: TraitTypeConstraintId,
            cstr: &RegionBinder<TraitTypeConstraint>,
        ) {
            let Some(path) = AssocTypePath::from_constraint(cstr) else {
                // We ignore non-local constraints.
                return;
            };
            // Erase bound lifetimes; handling them correctly would require tracking a lot more
            // info.
            let ty = cstr.map_ref(|cstr| cstr.ty.clone()).erase();
            self.insert_inner(&path, Some(cid), ty);
        }

        /// Find the entry at that path from the trie, if it exists.
        pub fn find(&self, path: &AssocTypePath) -> Option<(Option<TraitTypeConstraintId>, &Ty)> {
            if let BaseClause::Local(var) = path.tref.base {
                // Only lookup at level zero because replacements are always at level zero relative to
                // the item.
                assert!(var.bound_at_depth(DeBruijnId::zero()).is_some());
            }
            let mut trie = self.clauses.get(&path.tref.base)?;
            for id in &path.tref.parent_path {
                trie = trie.parent_clauses.get(*id)?;
            }
            trie.assoc_tys
                .get(&path.type_name)
                .map(|(id, ty)| (*id, ty))
        }

        pub fn iter(&self) -> impl Iterator<Item = (AssocTypePath, Ty)> {
            let mut vec = vec![];
            for (base, inner) in &self.clauses {
                inner.to_vec_inner(
                    &mut vec,
                    &TraitRefPath {
                        base: *base,
                        parent_path: vec![],
                    },
                )
            }
            vec.into_iter()
        }

        /// List the constraints that start with `Self::`.
        pub fn iter_self_paths(&self) -> impl Iterator<Item = (AssocTypePath, Ty)> {
            self.iter()
                .filter(|(path, _ty)| matches!(path.tref.base, BaseClause::SelfClause))
        }

        /// Apply a substitution to the output of `iter_self_paths`.
        ///
        /// We only provide this method for `Self::` paths (i.e. implied clauses) because
        /// constraints on local clauses must always be proven from the environment already hence
        /// they would be redundant outside of the defining trait/impl.
        pub fn iter_self_paths_subst<'a>(
            &'a self,
            args: &'a GenericArgs,
        ) -> impl Iterator<Item = (AssocTypePath, Ty)> + use<'a> {
            self.iter_self_paths()
                .map(move |(path, ty)| (path, ty.substitute(args)))
        }
    }

    impl TypeConstraintSetInner {
        fn debug_fmt(
            &self,
            f: &mut std::fmt::Formatter<'_>,
            base: BaseClause,
            parents: &[TraitClauseId],
        ) -> std::fmt::Result {
            for (name, (_, ty)) in &self.assoc_tys {
                match base {
                    BaseClause::SelfClause => write!(f, "Self")?,
                    BaseClause::Local(id) => write!(f, "Clause{id}")?,
                }
                for id in parents {
                    write!(f, "::Clause{id}")?;
                }
                write!(f, "::{name} = {ty:?}, ")?;
            }
            for (parent_id, parent_trie) in self.parent_clauses.iter_indexed() {
                let mut new_parents = parents.to_vec();
                new_parents.push(parent_id);
                parent_trie.debug_fmt(f, base, &new_parents)?;
            }
            Ok(())
        }

        fn to_vec_inner(&self, vec: &mut Vec<(AssocTypePath, Ty)>, base_path: &TraitRefPath) {
            for (name, (_, ty)) in &self.assoc_tys {
                vec.push((base_path.clone().with_assoc_type(name.clone()), ty.clone()));
            }
            for (clause_id, inner) in self.parent_clauses.iter_indexed() {
                let mut base_path = base_path.clone();
                base_path.parent_path.push(clause_id);
                inner.to_vec_inner(vec, &base_path);
            }
        }
    }

    impl std::fmt::Debug for TypeConstraintSet {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("{ ")?;
            for (base, inner) in self.clauses.iter() {
                inner.debug_fmt(f, *base, &[])?;
            }
            f.write_str("}")?;
            Ok(())
        }
    }
}

/// Records the modifications we are operating on the given item.
#[derive(Debug, Clone)]
struct ItemModifications {
    /// The constraints on associated types for this item.
    type_constraints: TypeConstraintSet,
    /// Associated item paths for which we don't know a type they correspond to, so we will have to
    /// add a new type parameter to the signature of the item.
    required_extra_paths: Vec<AssocTypePath>,
    /// These constraints refer to now-removed associated types. We have taken them into account,
    /// they should be removed.
    remove_constraints: HashSet<TraitTypeConstraintId>,
    /// For trait declarations only: if `true`, we turn its associated types into parameters and
    /// turn any extra required params into new params. If `false`, we keep its associated types
    /// and turn extra require params into new associated types.
    /// For non-traits, this is always `true`.
    add_type_params: bool,
}

impl ItemModifications {
    fn from_constraint_set(type_constraints: TypeConstraintSet, add_type_params: bool) -> Self {
        Self {
            type_constraints,
            required_extra_paths: Default::default(),
            remove_constraints: Default::default(),
            add_type_params,
        }
    }

    /// Record that we must replace this path. If there is a relevant type constraint we can use
    /// it, otherwise we will need to add a new type parameter to supply a value for this path.
    fn replace_path(&mut self, path: AssocTypePath) {
        if let Some((cstr_id, _)) = self.type_constraints.find(&path) {
            // The local constraints already give us a value for this associated type; we
            // use that.
            if let Some(cstr_id) = cstr_id {
                self.remove_constraints.insert(cstr_id);
            }
            // The path is already in the set, no need to replace it.
        } else {
            // We don't have a value for this associated type; we add a new type parameter
            // to supply it.
            self.required_extra_paths.push(path);
        }
    }

    /// Iterate over the paths that need to be filled.
    fn required_extra_paths(&self) -> impl Iterator<Item = &AssocTypePath> {
        self.required_extra_paths.iter()
    }

    /// Iterate over the paths that require adding new type parameters.
    fn required_extra_params(&self) -> impl Iterator<Item = &AssocTypePath> {
        if self.add_type_params {
            Some(self.required_extra_paths())
        } else {
            None
        }
        .into_iter()
        .flatten()
    }

    /// Iterate over the paths that require adding new associated types.
    fn required_extra_assoc_types(&self) -> impl Iterator<Item = &AssocTypePath> {
        if self.add_type_params {
            None
        } else {
            Some(self.required_extra_paths())
        }
        .into_iter()
        .flatten()
    }

    /// Compute the type replacements needed to update the body of this item. We use the provided
    /// function to make new types for each missing type. That function will typically add new type
    /// parameters to the item signature.
    fn compute_replacements(&self, mut f: impl FnMut(&AssocTypePath) -> Ty) -> TypeConstraintSet {
        let mut set = self.type_constraints.clone();
        for path in &self.required_extra_paths {
            let ty = f(path);
            set.insert_path(path, ty);
        }
        set
    }
}

/// An enum to manage potentially-cyclic computations.
#[derive(Debug, EnumAsGetters)]
enum Processing<T> {
    /// We haven't analyzed this yet.
    Unprocessed,
    /// Sentinel value that we set when starting the computation on an item. If we ever encounter
    /// this, we know we encountered a loop that we can't handle.
    Processing,
    /// Sentinel value we put when encountering a cycle, so we can know that happened.
    Cyclic,
    /// The final result of the computation.
    Processed(T),
}

struct ComputeItemModifications<'a> {
    ctx: &'a TransformCtx,
    /// Records the modifications for each trait.
    trait_modifications: Vector<TraitDeclId, Processing<ItemModifications>>,
    /// Records the set of known associated types for this trait impl, including parent impls and
    /// associated type constraints.
    impl_assoc_tys: Vector<TraitImplId, Processing<TypeConstraintSet>>,
}

impl<'a> ComputeItemModifications<'a> {
    fn new(ctx: &'a TransformCtx) -> Self {
        ComputeItemModifications {
            ctx,
            trait_modifications: ctx
                .translated
                .trait_decls
                .map_ref_opt(|_| Some(Processing::Unprocessed)),
            impl_assoc_tys: ctx
                .translated
                .trait_impls
                .map_ref_opt(|_| Some(Processing::Unprocessed)),
        }
    }

    fn compute_item_modifications(&mut self, item: AnyTransItem<'_>) -> ItemModifications {
        if let AnyTransItem::TraitDecl(tdecl) = item {
            // The complex case is traits: we call `compute_extra_params_for_trait` to
            // compute the right thing.
            let id = tdecl.def_id;
            let _ = self.compute_extra_params_for_trait(id);
            self.trait_modifications[id].as_processed().unwrap().clone()
        } else if let AnyTransItem::TraitImpl(timpl) = item {
            let _ = self.compute_assoc_tys_for_impl(timpl.def_id);
            let type_constraints = self.impl_assoc_tys[timpl.def_id]
                .as_processed()
                .unwrap()
                .clone();
            let mut modifications = ItemModifications::from_constraint_set(type_constraints, true);
            for clause in &timpl.generics.trait_clauses {
                for path in self.compute_extra_params_for_clause(clause, TraitRefPath::local_clause)
                {
                    modifications.replace_path(path);
                }
            }
            modifications
        } else {
            self.compute_non_trait_modifications(item.generic_params())
        }
    }

    /// Compute the initial constraint set for this item.
    fn compute_constraint_set(&mut self, params: &GenericParams) -> TypeConstraintSet {
        let mut type_constraints =
            TypeConstraintSet::from_constraints(&params.trait_type_constraints);
        // Clauses may provide more type constraints.
        for clause in &params.trait_clauses {
            let tref = clause.identity_tref();
            for (path, ty) in
                self.compute_assoc_tys_for_tref(clause.span.unwrap_or_default(), &tref)
            {
                type_constraints.insert_path(&path, ty);
            }
        }
        type_constraints
    }

    fn compute_non_trait_modifications(&mut self, params: &GenericParams) -> ItemModifications {
        // For non-traits, we simply iterate through the clauses of the item and
        // collect new paths to replace.
        let type_constraints = self.compute_constraint_set(params);
        let mut modifications = ItemModifications::from_constraint_set(type_constraints, true);
        // For each clause, we need to supply types for the new parameters. `replace_path` either
        // finds the right type in the type constraints, or records that we need to add new
        // parameters to this trait's signature.
        for clause in &params.trait_clauses {
            for path in self.compute_extra_params_for_clause(clause, TraitRefPath::local_clause) {
                modifications.replace_path(path);
            }
        }
        modifications
    }

    /// Returns the extra parameters that we add to the given trait. If we hadn't processed this
    /// trait before, we modify it to add the parameters in question and remove its associated
    /// types.
    fn compute_extra_params_for_trait(
        &mut self,
        id: TraitDeclId,
    ) -> impl Iterator<Item = &AssocTypePath> {
        if let Processing::Unprocessed = self.trait_modifications[id] {
            if let Some(tr) = self.ctx.translated.trait_decls.get(id) {
                // Put the sentinel value to detect loops.
                self.trait_modifications[id] = Processing::Processing;

                // The heart of the recursion: we process the trait clauses, which recursively computes
                // the extra parameters needed for each corresponding trait. Each of the referenced
                // traits may need to be supplied extra type parameters. `replace_path` either finds
                // the right type in the type constraints, or records that we need to add new
                // parameters to this trait's signature.
                let mut replace = vec![];
                for clause in &tr.generics.trait_clauses {
                    for path in
                        self.compute_extra_params_for_clause(clause, TraitRefPath::local_clause)
                    {
                        replace.push(path);
                    }
                }
                for clause in &tr.parent_clauses {
                    for path in
                        self.compute_extra_params_for_clause(clause, TraitRefPath::parent_clause)
                    {
                        replace.push(path);
                    }
                }

                let is_self_referential = match &self.trait_modifications[id] {
                    Processing::Unprocessed | Processing::Processed(_) => unreachable!(),
                    Processing::Processing => false,
                    Processing::Cyclic => true,
                };
                // These traits carry a built-in associated type that we can't replace with
                // anything else than itself, so we keep it as an associated type.
                let has_builtin_assoc_ty = matches!(
                    tr.item_meta.lang_item.as_deref(),
                    Some("discriminant_kind" | "pointee_trait")
                );
                let remove_assoc_type = !has_builtin_assoc_ty
                    && self
                        .ctx
                        .options
                        .remove_associated_types
                        .iter()
                        .any(|pat| pat.matches(&self.ctx.translated, &tr.item_meta.name));
                let remove_assoc_types = !is_self_referential && remove_assoc_type;
                let type_constraints = self.compute_constraint_set(&tr.generics);
                let mut modifications =
                    ItemModifications::from_constraint_set(type_constraints, remove_assoc_types);
                if modifications.add_type_params {
                    // Remove all associated types and turn them into new parameters. We add these
                    // before the paths in `replace` because this gives a better output.
                    for type_name in &tr.types {
                        let path = TraitRefPath::self_ref().with_assoc_type(type_name.clone());
                        modifications.replace_path(path);
                    }
                }
                for path in replace {
                    modifications.replace_path(path);
                }

                self.trait_modifications[id] = Processing::Processed(modifications);
            }
        } else if let Processing::Processing = self.trait_modifications[id] {
            // This is already being processed: we encountered a cycle.
            self.trait_modifications[id] = Processing::Cyclic;
        }

        match &self.trait_modifications[id] {
            Processing::Unprocessed => None,
            Processing::Processing | Processing::Cyclic => {
                // We're recursively processing ourselves. We already know we won't add new type
                // parameters, so we correctly return an empty iterator.
                None
            }
            Processing::Processed(modifs) => Some(modifs.required_extra_params()),
        }
        .into_iter()
        .flatten()
    }

    fn compute_extra_params_for_clause<'b>(
        &'b mut self,
        clause: &TraitClause,
        clause_to_path: fn(TraitClauseId) -> TraitRefPath,
    ) -> impl Iterator<Item = AssocTypePath> + use<'a, 'b> {
        let trait_id = clause.trait_.skip_binder.id;
        let clause_path = clause_to_path(clause.clause_id);
        self.compute_extra_params_for_trait(trait_id)
            .map(move |path| path.on_tref(&clause_path))
    }

    /// Returns any extra constraints implied by this predicate. If the trait is cyclic, the set
    /// will not be exhaustive.
    fn compute_implied_constraints_for_predicate<'b>(
        &'b mut self,
        pred: &'b TraitDeclRef,
    ) -> impl Iterator<Item = (AssocTypePath, Ty)> + use<'b> {
        let _ = self.compute_extra_params_for_trait(pred.id);
        self.trait_modifications[pred.id]
            .as_processed()
            .into_iter()
            .flat_map(|mods| mods.type_constraints.iter_self_paths_subst(&pred.generics))
    }

    fn compute_implied_constraints_for_poly_predicate<'b>(
        &'b mut self,
        poly_pred: &'b PolyTraitDeclRef,
    ) -> Vec<(AssocTypePath, Ty)> {
        if let Some(pred) = poly_pred.skip_binder.clone().move_from_under_binder() {
            self.compute_implied_constraints_for_predicate(&pred)
                .collect()
        } else {
            vec![]
        }
    }

    /// Returns the associated types values known for the given impl. If the impl is cyclic, the
    /// set will not be exhaustive.
    fn compute_assoc_tys_for_impl(&mut self, id: TraitImplId) -> Option<&TypeConstraintSet> {
        if let Processing::Unprocessed = self.impl_assoc_tys[id] {
            if let Some(timpl) = self.ctx.translated.trait_impls.get(id) {
                // Put the sentinel value to detect loops.
                self.impl_assoc_tys[id] = Processing::Processing;

                let mut set = self.compute_constraint_set(&timpl.generics);
                for (name, ty) in &timpl.types {
                    let path = TraitRefPath::self_ref().with_assoc_type(name.clone());
                    set.insert_path(&path, ty.clone());
                }
                for (clause_id, tref) in timpl.parent_trait_refs.iter_indexed() {
                    let clause_path = TraitRefPath::parent_clause(clause_id);
                    for (path, ty) in self.compute_assoc_tys_for_tref(timpl.item_meta.span, tref) {
                        let path = path.on_tref(&clause_path);
                        set.insert_path(&path, ty);
                    }
                }

                self.impl_assoc_tys[id] = Processing::Processed(set);
            }
        }
        self.impl_assoc_tys[id].as_processed()
    }

    /// Returns the associated types values known for the given trait ref.
    // TODO: also extract refs from `dyn Trait` refs.
    fn compute_assoc_tys_for_tref_kind(
        &mut self,
        span: Span,
        tref: &TraitRefKind,
    ) -> Vec<(AssocTypePath, Ty)> {
        let mut out = vec![];
        match tref {
            TraitRefKind::Clause(..)
            | TraitRefKind::ParentClause(..)
            | TraitRefKind::ItemClause(..)
            | TraitRefKind::SelfId
            | TraitRefKind::Unknown(_) => {}
            TraitRefKind::TraitImpl(impl_ref) => {
                if let Some(set_for_clause) = self.compute_assoc_tys_for_impl(impl_ref.id) {
                    out.extend(set_for_clause.iter_self_paths_subst(&impl_ref.generics));
                }
            }
            TraitRefKind::BuiltinOrAuto {
                parent_trait_refs,
                types,
                ..
            } => {
                for (name, ty, _) in types.iter().cloned() {
                    let path = TraitRefPath::self_ref().with_assoc_type(name);
                    out.push((path, ty));
                }
                for (parent_clause_id, parent_ref) in parent_trait_refs.iter_indexed() {
                    let clause_path = TraitRefPath::parent_clause(parent_clause_id);
                    out.extend(
                        self.compute_assoc_tys_for_tref(span, parent_ref)
                            .into_iter()
                            .map(|(path, ty)| {
                                let path = path.on_tref(&clause_path);
                                (path, ty)
                            }),
                    )
                }
            }
            TraitRefKind::Dyn(..) => {
                register_error!(
                    self.ctx,
                    span,
                    "Can't process associated types for `dyn Trait`"
                );
            }
        }
        out
    }

    fn compute_assoc_tys_for_tref(
        &mut self,
        span: Span,
        tref: &TraitRef,
    ) -> Vec<(AssocTypePath, Ty)> {
        let mut tys = self.compute_assoc_tys_for_tref_kind(span, &tref.kind);
        if let Some(base_path) = tref.to_path() {
            for (path, ty) in
                self.compute_implied_constraints_for_poly_predicate(&tref.trait_decl_ref)
            {
                tys.push((path.on_tref(&base_path), ty));
            }
        }
        tys
    }
}

/// Visitor that will traverse item bodies and update `GenericArgs` to match the new item
/// signatures. This also replaces `TyKind::TraitType`s for which we have a replacement.
///
/// The tricky part is that to update `GenericArgs` that apply to traits, we need to know the
/// `TraitRef` they apply to in order to get the values of the appropriate types. To do this,
/// `visit_generic_args` will skip `GenericArgs` meant for traits, and we must manually catch
#[derive(Visitor)]
struct UpdateItemBody<'a> {
    /// Warning: we keep the ctx around but we can't meaningfully use it for inspecting items as we
    /// remporarily remove them for in-place modification.
    ctx: &'a TransformCtx,
    span: Span,
    item_modifications: &'a HashMap<GenericsSource, ItemModifications>,
    // It's a reversed stack, for when we go under binders.
    type_replacements: BindingStack<TypeConstraintSet>,
}

impl UpdateItemBody<'_> {
    fn under_binder<R>(&mut self, repls: TypeConstraintSet, f: impl FnOnce(&mut Self) -> R) -> R {
        self.type_replacements.push(repls);
        let ret = f(self);
        self.type_replacements.pop();
        ret
    }

    fn lookup_type_replacement(&self, path: &AssocTypePath) -> Option<Ty> {
        let mut path = path.clone();
        let dbid = match &mut path.tref.base {
            BaseClause::Local(DeBruijnVar::Bound(dbid, _)) => {
                // Make the variable relative to the target item. Where clauses are always at
                // binder level zero.
                mem::replace(dbid, DeBruijnId::zero())
            }
            _ => self.type_replacements.depth(),
        };
        let ty = self.type_replacements[dbid].find(&path)?.1;
        Some(ty.clone().move_under_binders(dbid))
    }

    fn lookup_path_on_trait_ref(&self, path: &AssocTypePath, tref: &TraitRefKind) -> Option<Ty> {
        assert!(path.tref.base.is_self_clause());
        match tref {
            TraitRefKind::TraitImpl(impl_ref) => {
                // The type constraints of trait impls already contain all relevant type
                // equalities.
                let (_, ty) = self
                    .item_modifications
                    .get(&GenericsSource::item(impl_ref.id))?
                    .type_constraints
                    .find(path)?;
                Some(
                    ItemBinder::new(impl_ref.id, ty.clone())
                        .substitute(ItemBinder::new(CurrentItem, &impl_ref.generics))
                        .under_current_binder(),
                )
            }
            TraitRefKind::Clause(..) | TraitRefKind::SelfId => {
                let path = path.on_tref(&tref.to_path().unwrap());
                self.lookup_type_replacement(&path)
            }
            TraitRefKind::ParentClause(parent, clause_id) => {
                let path = path.on_tref(&TraitRefPath::parent_clause(*clause_id));
                self.lookup_path_on_trait_ref(&path, &parent.kind)
            }
            TraitRefKind::ItemClause(..) => {
                register_error!(
                    self.ctx,
                    self.span,
                    "Found unhandled item clause; \
                    this is a bug unless the clause is coming from a GAT."
                );
                None
            }
            TraitRefKind::BuiltinOrAuto {
                parent_trait_refs,
                types,
                ..
            } => match path.pop_first_parent() {
                None => types
                    .iter()
                    .find(|(name, _, _)| name == &path.type_name)
                    .map(|(_, ty, _)| ty.clone()),
                Some((parent_clause_id, sub_path)) => {
                    let parent_ref = &parent_trait_refs[parent_clause_id];
                    self.lookup_path_on_trait_ref(&sub_path, &parent_ref.kind)
                }
            },
            // TODO: add enough info to recover assoc types.
            TraitRefKind::Dyn(..) => None,
            TraitRefKind::Unknown(..) => None,
        }
    }

    fn update_generics(
        &mut self,
        args: &mut GenericArgs,
        target: GenericsSource,
        self_path: Option<TraitRefKind>,
    ) {
        let Some(modifications) = self.item_modifications.get(&target) else {
            return;
        };
        for path in modifications.required_extra_params() {
            let mut path = path.clone();
            let base_tref = match path.tref.base {
                BaseClause::SelfClause => self_path.as_ref().unwrap(),
                BaseClause::Local(var) => {
                    let clause_id = var
                        .bound_at_depth(DeBruijnId::zero())
                        .expect("found replacement not at binder level 0?");
                    path = path.pop_base();
                    &args.trait_refs[clause_id].kind
                }
            };
            let ty = if let Some(ty) = self.lookup_path_on_trait_ref(&path, &base_tref) {
                ty.clone()
            } else {
                if let Some(tref) = base_tref.to_path() {
                    path = path.on_tref(&tref);
                }
                let fmt_ctx = &self.ctx.into_fmt();
                let item_name = target.item_name(&self.ctx.translated, fmt_ctx);
                register_error!(
                    self.ctx,
                    self.span,
                    "Could not compute the value of {path} ({self_path:?}) needed to update \
                    generics {args:?} for item {item_name}.\
                    \nConstraints in scope:\n{}",
                    self.type_replacements
                        .iter()
                        .flat_map(|x| x.iter())
                        .map(|(path, ty)| format!("  - {path} = {}", ty.with_ctx(fmt_ctx)))
                        .join("\n"),
                );
                TyKind::Error(format!("Can't compute {path}")).into_ty()
            };
            args.types.push(ty);
        }
    }
    fn update_item_generics(&mut self, id: impl Into<AnyTransId>, args: &mut GenericArgs) {
        self.update_generics(args, GenericsSource::item(id), None);
    }

    /// A `TraitDeclRef` must always be paired with a path in order to get access to the
    /// appropriate associated types. We therefore ignore the trait args in `enter_generic_args`,
    /// and must carefully call `process_trait_decl_ref` on all the right places of the AST to
    /// catch the `GenericArgs` we skipped.
    /// `TraitDeclRef`s exist in three places: in `TraitClause`, `TraitRef`, and directly in
    /// `TraitImpl`.
    fn process_trait_decl_ref(&mut self, tref: &mut TraitDeclRef, self_path: TraitRefKind) {
        trace!("{tref:?}");
        let target = GenericsSource::item(tref.id);
        self.update_generics(&mut tref.generics, target, Some(self_path));
    }

    /// See `process_trait_decl_ref`.
    fn process_poly_trait_decl_ref(
        &mut self,
        tref: &mut PolyTraitDeclRef,
        self_path: TraitRefKind,
    ) {
        trace!("{tref:?}");
        self.under_binder(Default::default(), |this| {
            let self_path = self_path.move_under_binder();
            this.process_trait_decl_ref(&mut tref.skip_binder, self_path)
        });
    }
}

impl VisitAstMut for UpdateItemBody<'_> {
    // Track binding levels.
    fn visit_binder<T: AstVisitable>(
        &mut self,
        binder: &mut Binder<T>,
    ) -> ControlFlow<Self::Break> {
        let generics = &mut binder.params;
        // An inner binder. Apart from the case of trait method declarations, this is not a
        // globally-addressable item, so we haven't computed appropriate modifications yet. Hence
        // we compute them here.
        let mut type_constraints =
            TypeConstraintSet::from_constraints(&generics.trait_type_constraints);
        // Clauses may provide more type constraints.
        for clause in &generics.trait_clauses {
            if let Some(pred) = clause.trait_.skip_binder.clone().move_from_under_binder() {
                if let Some(tmods) = self.item_modifications.get(&GenericsSource::item(pred.id)) {
                    for (path, ty) in tmods.type_constraints.iter_self_paths_subst(&pred.generics) {
                        let path = path.on_local_clause(clause.clause_id);
                        type_constraints.insert_path(&path, ty);
                    }
                }
            }
        }
        let mut modifications = ItemModifications::from_constraint_set(type_constraints, true);
        for clause in &generics.trait_clauses {
            let trait_id = clause.trait_.skip_binder.id;
            if let Some(tmods) = self.item_modifications.get(&GenericsSource::item(trait_id)) {
                for path in tmods.required_extra_params() {
                    let path = path.on_local_clause(clause.clause_id);
                    modifications.replace_path(path);
                }
            }
        }
        // Remove used constraints.
        for cid in &modifications.remove_constraints {
            generics.trait_type_constraints.remove(*cid);
        }
        let replacements = modifications.compute_replacements(|path| {
            let var_id = generics
                .types
                .push_with(|id| TypeVar::new(id, path.to_name()));
            TyKind::TypeVar(DeBruijnVar::new_at_zero(var_id)).into_ty()
        });
        self.under_binder(replacements, |this| this.visit_inner(binder))
    }
    fn visit_region_binder<T: AstVisitable>(
        &mut self,
        binder: &mut RegionBinder<T>,
    ) -> ControlFlow<Self::Break> {
        self.under_binder(Default::default(), |this| this.visit_inner(binder))
    }

    // Process trait refs
    fn enter_trait_ref(&mut self, tref: &mut TraitRef) {
        self.process_poly_trait_decl_ref(&mut tref.trait_decl_ref, tref.kind.clone());
    }
    fn enter_trait_ref_kind(&mut self, kind: &mut TraitRefKind) {
        // There are some stray `TraitDeclRef`s that we have to update.
        // TODO: we should make `TraitRefKind` recursively contain full `TraitRef`s so we have the
        // implemented trait info at each level. This would require hax to give us more info.
        let kind_clone = kind.clone();
        match kind {
            TraitRefKind::Dyn(tref) => {
                self.process_poly_trait_decl_ref(tref, kind_clone);
            }
            TraitRefKind::BuiltinOrAuto {
                trait_decl_ref: tref,
                types,
                ..
            } => {
                self.process_poly_trait_decl_ref(tref, kind_clone);
                let target = GenericsSource::item(tref.skip_binder.id);
                if let Some(decl_modifs) = self.item_modifications.get(&target) {
                    assert!(decl_modifs.required_extra_assoc_types().count() == 0);
                    if decl_modifs.add_type_params {
                        types.clear();
                    }
                }
            }
            _ => {}
        }
    }
    fn enter_trait_impl(&mut self, timpl: &mut TraitImpl) {
        self.process_trait_decl_ref(&mut timpl.impl_trait, TraitRefKind::SelfId);
    }
    fn enter_trait_decl(&mut self, tdecl: &mut TraitDecl) {
        let self_tref = TraitRef {
            kind: TraitRefKind::SelfId,
            trait_decl_ref: RegionBinder::empty(TraitDeclRef {
                id: tdecl.def_id,
                generics: Box::new(tdecl.generics.identity_args()),
            }),
        };
        for (clause_id, clause) in tdecl.parent_clauses.iter_mut_indexed() {
            let self_path = TraitRefKind::ParentClause(Box::new(self_tref.clone()), clause_id);
            self.process_poly_trait_decl_ref(&mut clause.trait_, self_path);
        }
    }
    fn enter_generic_params(&mut self, params: &mut GenericParams) {
        for (clause_id, clause) in params.trait_clauses.iter_mut_indexed() {
            let self_path = TraitRefKind::Clause(DeBruijnVar::new_at_zero(clause_id));
            self.process_poly_trait_decl_ref(&mut clause.trait_, self_path);
        }
    }
    fn enter_item_kind(&mut self, kind: &mut ItemKind) {
        match kind {
            ItemKind::TopLevel
            | ItemKind::Closure { .. }
            | ItemKind::VTableTy { .. }
            | ItemKind::VTableInstance { .. } => {}
            // Inside method declarations, the implicit `Self` clause is the first clause.
            ItemKind::TraitDecl { trait_ref, .. } => self.process_trait_decl_ref(
                trait_ref,
                TraitRefKind::Clause(DeBruijnVar::new_at_zero(TraitClauseId::ZERO)),
            ),
            ItemKind::TraitImpl {
                impl_ref,
                trait_ref,
                ..
            } => self.process_trait_decl_ref(trait_ref, TraitRefKind::TraitImpl(impl_ref.clone())),
        }
    }

    // Update item generics.
    fn enter_type_decl_ref(&mut self, x: &mut TypeDeclRef) {
        match x.id {
            TypeId::Adt(id) => self.update_item_generics(id, &mut x.generics),
            TypeId::Tuple => {}
            TypeId::Builtin(_) => {}
        }
    }
    fn enter_fun_decl_ref(&mut self, x: &mut FunDeclRef) {
        self.update_item_generics(x.id, &mut x.generics);
    }
    fn enter_fn_ptr(&mut self, x: &mut FnPtr) {
        match x.func.as_ref() {
            FunIdOrTraitMethodRef::Fun(FunId::Regular(id)) => {
                self.update_item_generics(*id, &mut x.generics)
            }
            FunIdOrTraitMethodRef::Fun(FunId::Builtin(_)) => {}
            FunIdOrTraitMethodRef::Trait(trait_ref, method_name, _) => {
                let trait_id = trait_ref.trait_decl_ref.skip_binder.id;
                self.update_generics(
                    &mut x.generics,
                    GenericsSource::Method(trait_id, method_name.clone()),
                    None,
                );
            }
        }
    }
    fn enter_global_decl_ref(&mut self, x: &mut GlobalDeclRef) {
        self.update_item_generics(x.id, &mut x.generics);
    }
    fn enter_trait_impl_ref(&mut self, x: &mut TraitImplRef) {
        self.update_item_generics(x.id, &mut x.generics);
    }

    // Normalize associated types.
    fn enter_ty_kind(&mut self, kind: &mut TyKind) {
        if let TyKind::TraitType(tref, name) = kind {
            let path = TraitRefPath::self_ref().with_assoc_type(name.clone());
            if let Some(new_ty) = self.lookup_path_on_trait_ref(&path, &tref.kind) {
                *kind = new_ty.kind().clone();
                // Fix the newly-substituted type.
                let _ = self.visit(kind);
            }
        }
    }

    // Track span for more precise error messages.
    fn visit_ullbc_statement(&mut self, st: &mut ullbc_ast::Statement) -> ControlFlow<Self::Break> {
        let old_span = self.span;
        self.span = st.span;
        self.visit_inner(st)?;
        self.span = old_span;
        Continue(())
    }
    fn visit_llbc_statement(&mut self, st: &mut llbc_ast::Statement) -> ControlFlow<Self::Break> {
        let old_span = self.span;
        self.span = st.span;
        self.visit_inner(st)?;
        self.span = old_span;
        Continue(())
    }
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        // Compute all the necessary type info to be able to replace and normalize associated
        // types.
        let item_modifications: HashMap<GenericsSource, ItemModifications> = {
            let mut computer = ComputeItemModifications::new(ctx);
            let mut item_modifications = HashMap::new();
            for (id, item) in ctx.translated.all_items_with_ids() {
                let modifications = computer.compute_item_modifications(item);
                item_modifications.insert(GenericsSource::Item(id), modifications);
                if let AnyTransItem::TraitDecl(tdecl) = item {
                    for (name, bound_fn) in &tdecl.methods {
                        let modifications =
                            computer.compute_non_trait_modifications(&bound_fn.params);
                        item_modifications.insert(
                            GenericsSource::Method(tdecl.def_id, name.clone()),
                            modifications,
                        );
                    }
                }
            }
            item_modifications
        };

        // Apply the computed modifications to each item.
        ctx.for_each_item_mut(|ctx, mut item| {
            let id = item.as_ref().id();
            let span = item.as_ref().item_meta().span;
            let modifications = item_modifications.get(&GenericsSource::Item(id)).unwrap();

            // Add new parameters or associated types in order to have types to fill in all the
            // replaced paths. We then collect the replaced paths and their associated value.
            let type_replacements: TypeConstraintSet = if let AnyTransItemMut::TraitDecl(tr) =
                &mut item
                && !modifications.add_type_params
            {
                // If we're self-referential, instead of adding new type parameters to pass to our
                // parent clauses, we add new associated types. That way the parameter list of the
                // trait stays unchanged.
                let self_tref = TraitRef {
                    kind: TraitRefKind::SelfId,
                    trait_decl_ref: RegionBinder::empty(TraitDeclRef {
                        id: tr.def_id,
                        generics: Box::new(tr.generics.identity_args()),
                    }),
                };
                modifications.compute_replacements(|path| {
                    let new_type_name = TraitItemName(path.to_name());
                    tr.types.push(new_type_name.clone());
                    TyKind::TraitType(self_tref.clone(), new_type_name).into_ty()
                })
            } else {
                modifications.compute_replacements(|path| {
                    let var_id = item
                        .generic_params()
                        .types
                        .push_with(|id| TypeVar::new(id, path.to_name()));
                    TyKind::TypeVar(DeBruijnVar::new_at_zero(var_id)).into_ty()
                })
            };

            // Remove used constraints.
            for cid in &modifications.remove_constraints {
                item.generic_params().trait_type_constraints.remove(*cid);
            }

            // Remove trait associated types.
            if let AnyTransItemMut::TraitDecl(tr) = &mut item
                && modifications.add_type_params
            {
                tr.types.clear();
            }

            // Adjust impl associated types.
            if let AnyTransItemMut::TraitImpl(timpl) = &mut item {
                let trait_id = timpl.impl_trait.id;
                if let Some(decl_modifs) = item_modifications.get(&GenericsSource::item(trait_id)) {
                    if decl_modifs.add_type_params {
                        timpl.types.clear();
                    }
                    for path in decl_modifs.required_extra_assoc_types() {
                        let new_type_name = TraitItemName(path.to_name());
                        if let Some((_, ty)) = type_replacements.find(&path) {
                            trace!("Adding associated type {new_type_name} = {ty:?}");
                            timpl.types.push((new_type_name, ty.clone()));
                        } else {
                            register_error!(
                                ctx,
                                span,
                                "Could not figure out type for new associated type {new_type_name}.\
                                \nAvailable: {:?}",
                                type_replacements
                            );
                        }
                    }
                }
            }

            // Update the rest of the items: all the `GenericArgs` and the non-top-level binders
            // (trait methods and inherent impls in `Name`s).
            let _ = item.drive_mut(&mut UpdateItemBody {
                ctx,
                span,
                item_modifications: &item_modifications,
                type_replacements: BindingStack::new(type_replacements),
            });
        });
    }
}
