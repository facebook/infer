//! This file groups everything which is linked to implementations about [crate::types]
use crate::ast::*;
use crate::ids::IndexMap;
use derive_generic_visitor::*;
use itertools::Itertools;
use std::borrow::Cow;
use std::collections::HashSet;
use std::convert::Infallible;
use std::fmt::Debug;
use std::iter::Iterator;
use std::mem;

impl TraitParam {
    /// Constructs the trait ref that refers to this clause.
    pub fn identity_tref(&self) -> TraitRef {
        self.identity_tref_at_depth(DeBruijnId::zero())
    }

    /// Like `identity_tref` but uses variables bound at the given depth.
    pub fn identity_tref_at_depth(&self, depth: DeBruijnId) -> TraitRef {
        TraitRef::new(
            TraitRefKind::Clause(DeBruijnVar::bound(depth, self.clause_id)),
            self.trait_.clone().move_under_binders(depth),
        )
    }
}

impl GenericParams {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Whether this has any explicit arguments (types, regions or const generics).
    pub fn has_explicits(&self) -> bool {
        !self.regions.is_empty() || !self.types.is_empty() || !self.const_generics.is_empty()
    }
    /// Whether this has any implicit arguments (trait clauses, outlives relations, associated type
    /// equality constraints).
    pub fn has_predicates(&self) -> bool {
        !self.trait_clauses.is_empty()
            || !self.types_outlive.is_empty()
            || !self.regions_outlive.is_empty()
            || !self.trait_type_constraints.is_empty()
    }

    /// Run some sanity checks.
    pub fn check_consistency(&self) {
        // Sanity check: check the clause ids are consistent.
        assert!(
            self.trait_clauses
                .iter()
                .enumerate()
                .all(|(i, c)| c.clause_id.index() == i)
        );

        // Sanity check: region names are pairwise distinct (this caused trouble when generating
        // names for the backward functions in Aeneas): at some point, Rustc introduced names equal
        // to `Some("'_")` for the anonymous regions, instead of using `None` (we now check in
        // [translate_region_name] and ignore names equal to "'_").
        let mut s = HashSet::new();
        for r in &self.regions {
            if let Some(name) = &r.name {
                assert!(
                    !s.contains(name),
                    "Name \"{}\" reused for two different lifetimes",
                    name
                );
                s.insert(name);
            }
        }
    }

    pub fn len(&self) -> usize {
        let GenericParams {
            regions,
            types,
            const_generics,
            trait_clauses,
            regions_outlive,
            types_outlive,
            trait_type_constraints,
        } = self;
        regions.elem_count()
            + types.elem_count()
            + const_generics.elem_count()
            + trait_clauses.elem_count()
            + regions_outlive.len()
            + types_outlive.len()
            + trait_type_constraints.elem_count()
    }

    /// Construct a set of generic arguments in the scope of `self` that matches `self` and feeds
    /// each required parameter with itself. E.g. given parameters for `<T, U> where U:
    /// PartialEq<T>`, the arguments would be `<T, U>[@TraitClause0]`.
    pub fn identity_args(&self) -> GenericArgs {
        self.identity_args_at_depth(DeBruijnId::zero())
    }

    /// Like `identity_args` but uses variables bound at the given depth.
    pub fn identity_args_at_depth(&self, depth: DeBruijnId) -> GenericArgs {
        GenericArgs {
            regions: self
                .regions
                .map_ref_indexed(|id, _| Region::Var(DeBruijnVar::bound(depth, id))),
            types: self
                .types
                .map_ref_indexed(|id, _| TyKind::TypeVar(DeBruijnVar::bound(depth, id)).into_ty()),
            const_generics: self.const_generics.map_ref_indexed(|id, c| ConstantExpr {
                ty: c.ty.clone(),
                kind: ConstantExprKind::Var(DeBruijnVar::bound(depth, id)),
            }),
            trait_refs: self
                .trait_clauses
                .map_ref(|clause| clause.identity_tref_at_depth(depth)),
        }
    }

    /// Take the predicates from the another `GenericParams`. This assumes the clause ids etc are
    /// already consistent.
    pub fn take_predicates_from(&mut self, other: GenericParams) {
        assert!(!other.has_explicits());
        let num_clauses = self.trait_clauses.slot_count();
        let GenericParams {
            regions: _,
            types: _,
            const_generics: _,
            trait_clauses,
            regions_outlive,
            types_outlive,
            trait_type_constraints,
        } = other;
        self.trait_clauses
            .extend(trait_clauses.into_iter().update(|clause| {
                clause.clause_id += num_clauses;
            }));
        self.regions_outlive.extend(regions_outlive);
        self.types_outlive.extend(types_outlive);
        self.trait_type_constraints.extend(trait_type_constraints);
    }

    /// Take the predicates from the another `GenericParams`. This assumes that the two
    /// `GenericParams` are independent, hence will shift clause ids if `other` has any
    /// trait refs that reference its own clauses.
    pub fn merge_predicates_from(&mut self, mut other: GenericParams) {
        // Drop the explicits params.
        other.types.clear();
        other.regions.clear();
        other.const_generics.clear();
        // The contents of `other` may refer to its own trait clauses, so we must shift clause ids.
        struct ShiftClausesVisitor(usize);
        impl VarsVisitor for ShiftClausesVisitor {
            fn visit_clause_var(&mut self, v: ClauseDbVar) -> Option<TraitRefKind> {
                if let DeBruijnVar::Bound(DeBruijnId::ZERO, clause_id) = v {
                    // Replace clause 0 and decrement the others.
                    Some(TraitRefKind::Clause(DeBruijnVar::Bound(
                        DeBruijnId::ZERO,
                        clause_id + self.0,
                    )))
                } else {
                    None
                }
            }
        }
        let num_clauses = self.trait_clauses.slot_count();
        other.visit_vars(&mut ShiftClausesVisitor(num_clauses));
        self.take_predicates_from(other);
    }
}

impl<T> Binder<T> {
    /// Wrap the value in an empty binder, shifting variables appropriately.
    pub fn empty(kind: BinderKind, x: T) -> Self
    where
        T: TyVisitable,
    {
        Binder {
            params: Default::default(),
            skip_binder: x.move_under_binder(),
            kind,
        }
    }
    pub fn new(kind: BinderKind, params: GenericParams, skip_binder: T) -> Self {
        Self {
            params,
            skip_binder,
            kind,
        }
    }

    /// Whether this binder binds any variables.
    pub fn binds_anything(&self) -> bool {
        !self.params.is_empty()
    }

    /// Retreive the contents of this binder if the binder binds no variables. This is the invers
    /// of `Binder::empty`.
    pub fn get_if_binds_nothing(&self) -> Option<T>
    where
        T: TyVisitable + Clone,
    {
        self.params
            .is_empty()
            .then(|| self.skip_binder.clone().move_from_under_binder().unwrap())
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Binder<U> {
        Binder {
            params: self.params,
            skip_binder: f(self.skip_binder),
            kind: self.kind.clone(),
        }
    }

    pub fn map_ref<U>(&self, f: impl FnOnce(&T) -> U) -> Binder<U> {
        Binder {
            params: self.params.clone(),
            skip_binder: f(&self.skip_binder),
            kind: self.kind.clone(),
        }
    }

    /// Substitute the provided arguments for the variables bound in this binder and return the
    /// substituted inner value.
    pub fn apply(self, args: &GenericArgs) -> T
    where
        T: TyVisitable,
    {
        self.skip_binder.substitute(args)
    }
}

impl<T: AstVisitable> Binder<Binder<T>> {
    /// Flatten two levels of binders into a single one.
    pub fn flatten(self) -> Binder<T> {
        #[derive(Visitor)]
        struct FlattenVisitor<'a> {
            shift_by: &'a GenericParams,
            binder_depth: DeBruijnId,
        }
        impl VisitorWithBinderDepth for FlattenVisitor<'_> {
            fn binder_depth_mut(&mut self) -> &mut DeBruijnId {
                &mut self.binder_depth
            }
        }
        impl VisitAstMut for FlattenVisitor<'_> {
            fn visit<'a, T: AstVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
                VisitWithBinderDepth::new(self).visit(x)
            }

            fn enter_de_bruijn_id(&mut self, db_id: &mut DeBruijnId) {
                if *db_id > self.binder_depth {
                    // We started visiting at the inner binder, so in this branch we're either
                    // mentioning the outer binder or a binder further beyond. Either way we
                    // decrease the depth; variables that point to the outer binder don't have to
                    // be shifted.
                    *db_id = db_id.decr();
                }
            }
            fn enter_region(&mut self, x: &mut Region) {
                if let Region::Var(var) = x
                    && let Some(id) = var.bound_at_depth_mut(self.binder_depth)
                {
                    *id += self.shift_by.regions.slot_count();
                }
            }
            fn enter_ty_kind(&mut self, x: &mut TyKind) {
                if let TyKind::TypeVar(var) = x
                    && let Some(id) = var.bound_at_depth_mut(self.binder_depth)
                {
                    *id += self.shift_by.types.slot_count();
                }
            }
            fn enter_constant_expr(&mut self, x: &mut ConstantExpr) {
                if let ConstantExprKind::Var(ref mut var) = x.kind
                    && let Some(id) = var.bound_at_depth_mut(self.binder_depth)
                {
                    *id += self.shift_by.const_generics.slot_count();
                }
            }
            fn enter_trait_ref_kind(&mut self, x: &mut TraitRefKind) {
                if let TraitRefKind::Clause(var) = x
                    && let Some(id) = var.bound_at_depth_mut(self.binder_depth)
                {
                    *id += self.shift_by.trait_clauses.slot_count();
                }
            }
        }

        // We will concatenate both sets of params.
        let mut outer_params = self.params;

        // The inner value needs to change:
        // - at binder level 0 we shift all variable ids to match the concatenated params;
        // - at binder level > 0 we decrease binding level because there's one fewer binder.
        let mut bound_value = self.skip_binder.skip_binder;
        let _ = bound_value.drive_mut(&mut FlattenVisitor {
            shift_by: &outer_params,
            binder_depth: Default::default(),
        });

        // The inner params must also be updated, as they can refer to themselves and the outer
        // one.
        let mut inner_params = self.skip_binder.params;
        let _ = inner_params.drive_mut(&mut FlattenVisitor {
            shift_by: &outer_params,
            binder_depth: Default::default(),
        });
        inner_params
            .regions
            .iter_mut()
            .for_each(|v| v.index += outer_params.regions.slot_count());
        inner_params
            .types
            .iter_mut()
            .for_each(|v| v.index += outer_params.types.slot_count());
        inner_params
            .const_generics
            .iter_mut()
            .for_each(|v| v.index += outer_params.const_generics.slot_count());
        inner_params
            .trait_clauses
            .iter_mut()
            .for_each(|v| v.clause_id += outer_params.trait_clauses.slot_count());

        let GenericParams {
            regions,
            types,
            const_generics,
            trait_clauses,
            regions_outlive,
            types_outlive,
            trait_type_constraints,
        } = &inner_params;
        outer_params.regions.extend_from_slice(regions);
        outer_params.types.extend_from_slice(types);
        outer_params
            .const_generics
            .extend_from_slice(const_generics);
        outer_params.trait_clauses.extend_from_slice(trait_clauses);
        outer_params
            .regions_outlive
            .extend_from_slice(regions_outlive);
        outer_params.types_outlive.extend_from_slice(types_outlive);
        outer_params
            .trait_type_constraints
            .extend_from_slice(trait_type_constraints);

        Binder {
            params: outer_params,
            skip_binder: bound_value,
            kind: BinderKind::Other,
        }
    }
}

impl<T> RegionBinder<T> {
    /// Wrap the value in an empty region binder, shifting variables appropriately.
    pub fn empty(x: T) -> Self
    where
        T: TyVisitable,
    {
        RegionBinder {
            regions: Default::default(),
            skip_binder: x.move_under_binder(),
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> RegionBinder<U> {
        RegionBinder {
            regions: self.regions,
            skip_binder: f(self.skip_binder),
        }
    }

    pub fn map_ref<U>(&self, f: impl FnOnce(&T) -> U) -> RegionBinder<U> {
        RegionBinder {
            regions: self.regions.clone(),
            skip_binder: f(&self.skip_binder),
        }
    }

    /// Substitute the bound variables with the given lifetimes.
    pub fn apply(self, regions: IndexMap<RegionId, Region>) -> T
    where
        T: TyVisitable,
    {
        assert_eq!(regions.slot_count(), self.regions.slot_count());
        let args = GenericArgs {
            regions,
            ..GenericArgs::empty()
        };
        self.skip_binder.substitute_inner_binder(&args)
    }

    /// Substitute the bound variables with erased lifetimes.
    pub fn erase(self) -> T
    where
        T: TyVisitable,
    {
        let regions = self.regions.map_ref_indexed(|_, _| Region::Erased);
        self.apply(regions)
    }
}

impl GenericArgs {
    pub fn len(&self) -> usize {
        let GenericArgs {
            regions,
            types,
            const_generics,
            trait_refs,
        } = self;
        regions.elem_count()
            + types.elem_count()
            + const_generics.elem_count()
            + trait_refs.elem_count()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Whether this has any explicit arguments (types, regions or const generics).
    pub fn has_explicits(&self) -> bool {
        !self.regions.is_empty() || !self.types.is_empty() || !self.const_generics.is_empty()
    }
    /// Whether this has any implicit arguments (trait refs).
    pub fn has_implicits(&self) -> bool {
        !self.trait_refs.is_empty()
    }

    pub fn empty() -> Self {
        GenericArgs {
            regions: Default::default(),
            types: Default::default(),
            const_generics: Default::default(),
            trait_refs: Default::default(),
        }
    }

    pub fn new_for_builtin(types: IndexMap<TypeVarId, Ty>) -> Self {
        GenericArgs {
            types,
            ..Self::empty()
        }
    }

    pub fn new(
        regions: IndexMap<RegionId, Region>,
        types: IndexMap<TypeVarId, Ty>,
        const_generics: IndexMap<ConstGenericVarId, ConstantExpr>,
        trait_refs: IndexMap<TraitClauseId, TraitRef>,
    ) -> Self {
        Self {
            regions,
            types,
            const_generics,
            trait_refs,
        }
    }

    pub fn new_types(types: IndexMap<TypeVarId, Ty>) -> Self {
        Self {
            types,
            ..Self::empty()
        }
    }

    /// Check whether this matches the given `GenericParams`.
    /// TODO: check more things, e.g. that the trait refs use the correct trait and generics.
    pub fn matches(&self, params: &GenericParams) -> bool {
        params.regions.elem_count() == self.regions.elem_count()
            && params.types.elem_count() == self.types.elem_count()
            && params.const_generics.elem_count() == self.const_generics.elem_count()
            && params.trait_clauses.elem_count() == self.trait_refs.elem_count()
    }

    /// Return the same generics, but where we pop the first type arguments.
    /// This is useful for trait references (for pretty printing for instance),
    /// because the first type argument is the type for which the trait is
    /// implemented.
    pub fn pop_first_type_arg(&self) -> (Ty, Self) {
        let mut generics = self.clone();
        let mut it = mem::take(&mut generics.types).into_iter();
        let ty = it.next().unwrap();
        generics.types = it.collect();
        (ty, generics)
    }

    /// Concatenate this set of arguments with another one. Use with care, you must manage the
    /// order of arguments correctly.
    pub fn concat(mut self, other: &Self) -> Self {
        let Self {
            regions,
            types,
            const_generics,
            trait_refs,
        } = other;
        self.regions.extend_from_slice(regions);
        self.types.extend_from_slice(types);
        self.const_generics.extend_from_slice(const_generics);
        self.trait_refs.extend_from_slice(trait_refs);
        self
    }
}

impl IntTy {
    /// Important: this returns the target byte count for the types.
    /// Must not be used for host types from rustc.
    pub fn target_size(&self, ptr_size: ByteCount) -> usize {
        match self {
            IntTy::Isize => ptr_size as usize,
            IntTy::I8 => size_of::<i8>(),
            IntTy::I16 => size_of::<i16>(),
            IntTy::I32 => size_of::<i32>(),
            IntTy::I64 => size_of::<i64>(),
            IntTy::I128 => size_of::<i128>(),
        }
    }
}
impl UIntTy {
    /// Important: this returns the target byte count for the types.
    /// Must not be used for host types from rustc.
    pub fn target_size(&self, ptr_size: ByteCount) -> usize {
        match self {
            UIntTy::Usize => ptr_size as usize,
            UIntTy::U8 => size_of::<u8>(),
            UIntTy::U16 => size_of::<u16>(),
            UIntTy::U32 => size_of::<u32>(),
            UIntTy::U64 => size_of::<u64>(),
            UIntTy::U128 => size_of::<u128>(),
        }
    }
}
impl FloatTy {
    /// Important: this returns the target byte count for the types.
    /// Must not be used for host types from rustc.
    pub fn target_size(&self) -> usize {
        match self {
            FloatTy::F16 => size_of::<u16>(),
            FloatTy::F32 => size_of::<u32>(),
            FloatTy::F64 => size_of::<u64>(),
            FloatTy::F128 => size_of::<u128>(),
        }
    }
}

impl IntegerTy {
    pub fn to_unsigned(&self) -> Self {
        match self {
            IntegerTy::Signed(IntTy::Isize) => IntegerTy::Unsigned(UIntTy::Usize),
            IntegerTy::Signed(IntTy::I8) => IntegerTy::Unsigned(UIntTy::U8),
            IntegerTy::Signed(IntTy::I16) => IntegerTy::Unsigned(UIntTy::U16),
            IntegerTy::Signed(IntTy::I32) => IntegerTy::Unsigned(UIntTy::U32),
            IntegerTy::Signed(IntTy::I64) => IntegerTy::Unsigned(UIntTy::U64),
            IntegerTy::Signed(IntTy::I128) => IntegerTy::Unsigned(UIntTy::U128),
            _ => *self,
        }
    }

    /// Important: this returns the target byte count for the types.
    /// Must not be used for host types from rustc.
    pub fn target_size(&self, ptr_size: ByteCount) -> usize {
        match self {
            IntegerTy::Signed(ty) => ty.target_size(ptr_size),
            IntegerTy::Unsigned(ty) => ty.target_size(ptr_size),
        }
    }
}

impl LiteralTy {
    pub fn to_integer_ty(&self) -> Option<IntegerTy> {
        match self {
            Self::Int(int_ty) => Some(IntegerTy::Signed(*int_ty)),
            Self::UInt(uint_ty) => Some(IntegerTy::Unsigned(*uint_ty)),
            _ => None,
        }
    }

    /// Important: this returns the target byte count for the types.
    /// Must not be used for host types from rustc.
    pub fn target_size(&self, ptr_size: ByteCount) -> usize {
        match self {
            LiteralTy::Int(int_ty) => int_ty.target_size(ptr_size),
            LiteralTy::UInt(uint_ty) => uint_ty.target_size(ptr_size),
            LiteralTy::Float(float_ty) => float_ty.target_size(),
            LiteralTy::Char => 4,
            LiteralTy::Bool => 1,
        }
    }
}

impl From<LiteralTy> for Ty {
    fn from(value: LiteralTy) -> Self {
        TyKind::Literal(value).into_ty()
    }
}

/// A value of type `T` bound by the generic parameters of item
/// `item`. Used when dealing with multiple items at a time, to
/// ensure we don't mix up generics.
///
/// To get the value, use `under_binder_of` or `subst_for`.
#[derive(Debug, Clone, Copy)]
pub struct ItemBinder<ItemId, T> {
    pub item_id: ItemId,
    val: T,
}

impl<ItemId, T> ItemBinder<ItemId, T>
where
    ItemId: Debug + Copy + PartialEq,
{
    pub fn new(item_id: ItemId, val: T) -> Self {
        Self { item_id, val }
    }

    pub fn as_ref(&self) -> ItemBinder<ItemId, &T> {
        ItemBinder {
            item_id: self.item_id,
            val: &self.val,
        }
    }

    pub fn map_bound<U>(self, f: impl FnOnce(T) -> U) -> ItemBinder<ItemId, U> {
        ItemBinder {
            item_id: self.item_id,
            val: f(self.val),
        }
    }

    fn assert_item_id(&self, item_id: ItemId) {
        assert_eq!(
            self.item_id, item_id,
            "Trying to use item bound for {:?} as if it belonged to {:?}",
            self.item_id, item_id
        );
    }

    /// Assert that the value is bound for item `item_id`, and returns it. This is used when we
    /// plan to store the returned value inside that item.
    pub fn under_binder_of(self, item_id: ItemId) -> T {
        self.assert_item_id(item_id);
        self.val
    }

    /// Given generic args for `item_id`, assert that the value is bound for `item_id` and
    /// substitute it with the provided generic arguments. Because the arguments are bound in the
    /// context of another item, so it the resulting substituted value.
    pub fn substitute<OtherItem: Debug + Copy + PartialEq>(
        self,
        args: ItemBinder<OtherItem, &GenericArgs>,
    ) -> ItemBinder<OtherItem, T>
    where
        ItemId: Into<ItemId>,
        T: TyVisitable,
    {
        args.map_bound(|args| self.val.substitute(args))
    }
}

/// Dummy item identifier that represents the current item when not ambiguous.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CurrentItem;

impl<T> ItemBinder<CurrentItem, T> {
    pub fn under_current_binder(self) -> T {
        self.val
    }
}

impl Ty {
    pub fn new(kind: TyKind) -> Self {
        Ty(HashConsed::new(kind))
    }

    pub fn kind(&self) -> &TyKind {
        self.0.inner()
    }

    pub fn with_kind_mut<R>(&mut self, f: impl FnOnce(&mut TyKind) -> R) -> R {
        self.0.with_inner_mut(f)
    }

    /// Return the unit type
    pub fn mk_unit() -> Ty {
        Self::mk_tuple(vec![])
    }

    pub fn mk_bool() -> Ty {
        TyKind::Literal(LiteralTy::Bool).into()
    }

    pub fn mk_usize() -> Ty {
        TyKind::Literal(LiteralTy::UInt(UIntTy::Usize)).into()
    }

    pub fn mk_tuple(tys: Vec<Ty>) -> Ty {
        TyKind::Adt(TypeDeclRef {
            id: TypeId::Tuple,
            generics: Box::new(GenericArgs::new_for_builtin(tys.into())),
        })
        .into_ty()
    }

    pub fn mk_array(ty: Ty, len: ConstantExpr) -> Ty {
        TyKind::Array(ty, Box::new(len)).into_ty()
    }

    pub fn mk_slice(ty: Ty) -> Ty {
        TyKind::Slice(ty).into_ty()
    }
    /// Return true if it is actually unit (i.e.: 0-tuple)
    pub fn is_unit(&self) -> bool {
        match self.as_tuple() {
            Some(tys) => tys.is_empty(),
            None => false,
        }
    }

    /// Return true if this is a scalar type
    pub fn is_scalar(&self) -> bool {
        match self.kind() {
            TyKind::Literal(kind) => kind.is_int() || kind.is_uint(),
            _ => false,
        }
    }

    pub fn is_unsigned_scalar(&self) -> bool {
        matches!(self.kind(), TyKind::Literal(LiteralTy::UInt(_)))
    }

    pub fn is_signed_scalar(&self) -> bool {
        matches!(self.kind(), TyKind::Literal(LiteralTy::Int(_)))
    }

    pub fn is_str(&self) -> bool {
        match self.kind() {
            TyKind::Adt(ty_ref) if let TypeId::Builtin(BuiltinTy::Str) = ty_ref.id => true,
            _ => false,
        }
    }

    /// Return true if the type is Box
    pub fn is_box(&self) -> bool {
        match self.kind() {
            TyKind::Adt(ty_ref) if let TypeId::Builtin(BuiltinTy::Box) = ty_ref.id => true,
            _ => false,
        }
    }

    pub fn as_box(&self) -> Option<&Ty> {
        match self.kind() {
            TyKind::Adt(ty_ref) if let TypeId::Builtin(BuiltinTy::Box) = ty_ref.id => {
                Some(&ty_ref.generics.types[0])
            }
            _ => None,
        }
    }

    pub fn get_ptr_metadata(&self, translated: &TranslatedCrate) -> PtrMetadata {
        let ref ty_decls = translated.type_decls;
        match self.kind() {
            TyKind::Adt(ty_ref) => {
                // there are two cases:
                // 1. if the declared type has a fixed metadata, just returns it
                // 2. if it depends on some other types or the generic itself
                match ty_ref.id {
                    TypeId::Adt(type_decl_id) => {
                        let Some(decl) = ty_decls.get(type_decl_id) else {
                            return PtrMetadata::InheritFrom(self.clone());
                        };
                        match decl.ptr_metadata.clone().substitute(&ty_ref.generics) {
                            // if it depends on some type, recursion with the binding env
                            PtrMetadata::InheritFrom(ty) => ty.get_ptr_metadata(translated),
                            // otherwise, simply return it
                            meta => meta,
                        }
                    }
                    // the metadata of a tuple is simply the last field
                    TypeId::Tuple => {
                        match ty_ref.generics.types.iter().last() {
                            // `None` refers to the unit type `()`
                            None => PtrMetadata::None,
                            // Otherwise, simply recurse
                            Some(ty) => ty.get_ptr_metadata(translated),
                        }
                    }
                    // Box is a pointer like ref & raw ptr, hence no metadata
                    TypeId::Builtin(BuiltinTy::Box) => PtrMetadata::None,
                    // `str` has metadata length
                    TypeId::Builtin(BuiltinTy::Str) => PtrMetadata::Length,
                }
            }
            TyKind::DynTrait(pred) => match pred.vtable_ref(translated) {
                Some(vtable) => PtrMetadata::VTable(vtable),
                None => PtrMetadata::InheritFrom(self.clone()),
            },
            // `[T]` has metadata length
            TyKind::Slice(..) => PtrMetadata::Length,
            TyKind::TraitType(..) | TyKind::TypeVar(_) => PtrMetadata::InheritFrom(self.clone()),
            TyKind::Literal(_)
            | TyKind::Never
            | TyKind::Ref(..)
            | TyKind::RawPtr(..)
            | TyKind::FnPtr(..)
            | TyKind::FnDef(..)
            | TyKind::Array(..)
            | TyKind::Error(_) => PtrMetadata::None,
            // The metadata itself must be Sized, hence must with `PtrMetadata::None`
            TyKind::PtrMetadata(_) => PtrMetadata::None,
        }
    }

    pub fn as_ref_or_ptr(&self) -> Option<&Ty> {
        match self.kind() {
            TyKind::RawPtr(ty, _) | TyKind::Ref(_, ty, _) => Some(ty),
            _ => None,
        }
    }

    pub fn as_array_or_slice(&self) -> Option<&Ty> {
        match self.kind() {
            TyKind::Slice(ty) | TyKind::Array(ty, _) => Some(ty),
            _ => None,
        }
    }

    pub fn as_tuple(&self) -> Option<&IndexMap<TypeVarId, Ty>> {
        match self.kind() {
            TyKind::Adt(ty_ref) if let TypeId::Tuple = ty_ref.id => Some(&ty_ref.generics.types),
            _ => None,
        }
    }

    pub fn as_adt(&self) -> Option<&TypeDeclRef> {
        self.kind().as_adt()
    }
}

impl TyKind {
    pub fn into_ty(self) -> Ty {
        Ty::new(self)
    }
}

impl From<TyKind> for Ty {
    fn from(kind: TyKind) -> Ty {
        kind.into_ty()
    }
}

/// Convenience for migration purposes.
impl std::ops::Deref for Ty {
    type Target = TyKind;

    fn deref(&self) -> &Self::Target {
        self.kind()
    }
}
/// For deref patterns.
unsafe impl std::ops::DerefPure for Ty {}

impl TypeDeclRef {
    pub fn new(id: TypeId, generics: GenericArgs) -> Self {
        Self {
            id,
            generics: Box::new(generics),
        }
    }
}

impl TraitDeclRef {
    pub fn self_ty<'a>(&'a self, krate: &'a TranslatedCrate) -> Option<&'a Ty> {
        match self.generics.types.iter().next() {
            Some(ty) => return Some(ty),
            // TODO(mono): A monomorphized trait takes no arguments.
            None => {
                let name = krate.item_name(self.id)?;
                let args = name.name.last()?.as_monomorphized()?;
                args.types.iter().next()
            }
        }
    }
}

impl TraitRef {
    pub fn new(kind: TraitRefKind, trait_decl_ref: PolyTraitDeclRef) -> Self {
        TraitRefContents {
            kind,
            trait_decl_ref,
        }
        .intern()
    }

    pub fn new_builtin(
        trait_id: TraitDeclId,
        ty: Ty,
        parents: IndexMap<TraitClauseId, TraitRef>,
        builtin_data: BuiltinImplData,
    ) -> Self {
        let trait_decl_ref = RegionBinder::empty(TraitDeclRef {
            id: trait_id,
            generics: Box::new(GenericArgs::new_types([ty].into())),
        });
        Self::new(
            TraitRefKind::BuiltinOrAuto {
                builtin_data,
                parent_trait_refs: parents,
                types: Default::default(),
            },
            trait_decl_ref,
        )
    }

    pub fn trait_id(&self) -> TraitDeclId {
        self.trait_decl_ref.skip_binder.id
    }

    /// Get mutable access to the contents. This cloned the value and will re-intern the modified
    /// value at the end of the function.
    pub fn with_contents_mut<R>(&mut self, f: impl FnOnce(&mut TraitRefContents) -> R) -> R {
        self.0.with_inner_mut(f)
    }
}
impl TraitRefContents {
    pub fn intern(self) -> TraitRef {
        TraitRef(HashConsed::new(self))
    }
}

impl std::ops::Deref for TraitRef {
    type Target = TraitRefContents;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl BuiltinImplData {
    pub fn as_closure_kind(&self) -> Option<ClosureKind> {
        match self {
            BuiltinImplData::FnOnce => Some(ClosureKind::FnOnce),
            BuiltinImplData::FnMut => Some(ClosureKind::FnMut),
            BuiltinImplData::Fn => Some(ClosureKind::Fn),
            _ => None,
        }
    }
}

impl PtrMetadata {
    pub fn into_type(self) -> Ty {
        match self {
            PtrMetadata::None => Ty::mk_unit(),
            PtrMetadata::Length => Ty::mk_usize(),
            PtrMetadata::VTable(type_decl_ref) => Ty::new(TyKind::Ref(
                Region::Static,
                Ty::new(TyKind::Adt(type_decl_ref)),
                RefKind::Shared,
            )),
            PtrMetadata::InheritFrom(ty) => Ty::new(TyKind::PtrMetadata(ty)),
        }
    }
}

impl Field {
    /// The new name for this field, as suggested by the `#[charon::rename]` attribute.
    pub fn renamed_name(&self) -> Option<&str> {
        self.attr_info.rename.as_deref().or(self.name.as_deref())
    }

    /// Whether this field has a `#[charon::opaque]` annotation.
    pub fn is_opaque(&self) -> bool {
        self.attr_info
            .attributes
            .iter()
            .any(|attr| attr.is_opaque())
    }
}

impl Variant {
    /// The new name for this variant, as suggested by the `#[charon::rename]` and
    /// `#[charon::variants_prefix]` attributes.
    pub fn renamed_name(&self) -> &str {
        self.attr_info
            .rename
            .as_deref()
            .unwrap_or(self.name.as_ref())
    }

    /// Whether this variant has a `#[charon::opaque]` annotation.
    pub fn is_opaque(&self) -> bool {
        self.attr_info
            .attributes
            .iter()
            .any(|attr| attr.is_opaque())
    }
}

impl DynPredicate {
    /// Get a reference to the vtable type that corresponds to this predicate.
    pub fn vtable_ref(&self, translated: &TranslatedCrate) -> Option<TypeDeclRef> {
        let dyn_ty = TyKind::DynTrait(self.clone()).into_ty();
        // The first clause is the one relevant for the vtable. We're extracting it from our binder
        // so must give a value for the `Self` type.
        let relevant_tref = self.binder.params.trait_clauses[0]
            .trait_
            .clone()
            .erase()
            .substitute(&GenericArgs::new_types([dyn_ty].into_iter().collect()));

        // Get the vtable ref from the trait decl
        let trait_decl = translated.trait_decls.get(relevant_tref.id)?;
        let vtable_ref = trait_decl
            .vtable
            .clone()?
            .substitute_with_self(&relevant_tref.generics, &TraitRefKind::Dyn);
        Some(vtable_ref)
    }
}

impl RefKind {
    pub fn mutable(x: bool) -> Self {
        if x { Self::Mut } else { Self::Shared }
    }
}

/// Visitor for type-level variables. Used to visit the variables contained in a value, as seen
/// from the outside of the value. This means that any variable bound inside the value will be
/// skipped, and all the seen De Bruijn indices will count from the outside of the value. The
/// returned value, if any, will be put in place of the variable.
pub trait VarsVisitor {
    fn visit_erased_region(&mut self) -> Option<Region> {
        None
    }
    fn visit_region_var(&mut self, _v: RegionDbVar) -> Option<Region> {
        None
    }
    fn visit_type_var(&mut self, _v: TypeDbVar) -> Option<Ty> {
        None
    }
    fn visit_const_generic_var(&mut self, _v: ConstGenericDbVar) -> Option<ConstantExprKind> {
        None
    }
    fn visit_clause_var(&mut self, _v: ClauseDbVar) -> Option<TraitRefKind> {
        None
    }
    fn visit_self_clause(&mut self) -> Option<TraitRefKind> {
        None
    }
}

/// Visitor for the [TyVisitable::substitute] function.
/// This substitutes variables bound at the level where we start to substitute (level 0).
#[derive(Visitor)]
pub(crate) struct SubstVisitor<'a> {
    generics: &'a GenericArgs,
    self_ref: Option<&'a TraitRefKind>,
    /// Whether to substitute explicit variables only (types, regions, const generics).
    explicits_only: bool,
    had_error: bool,
}
impl<'a> SubstVisitor<'a> {
    pub(crate) fn new(
        generics: &'a GenericArgs,
        self_ref: Option<&'a TraitRefKind>,
        explicits_only: bool,
    ) -> Self {
        Self {
            generics,
            self_ref,
            explicits_only,
            had_error: false,
        }
    }

    pub fn visit<T: TyVisitable>(mut self, mut x: T) -> Result<T, GenericsMismatch> {
        let _ = x.visit_vars(&mut self);
        if self.had_error {
            Err(GenericsMismatch)
        } else {
            Ok(x)
        }
    }

    /// Returns the value for this variable, if any.
    fn process_var<Id, T>(
        &mut self,
        var: DeBruijnVar<Id>,
        get: impl Fn(Id) -> Option<&'a T>,
    ) -> Option<T>
    where
        Id: Copy,
        T: Clone + TyVisitable,
        DeBruijnVar<Id>: Into<T>,
    {
        match var {
            DeBruijnVar::Bound(dbid, varid) => {
                Some(if let Some(dbid) = dbid.sub(DeBruijnId::one()) {
                    // This is bound outside the binder we're substituting for.
                    DeBruijnVar::Bound(dbid, varid).into()
                } else {
                    match get(varid) {
                        Some(v) => v.clone(),
                        None => {
                            self.had_error = true;
                            return None;
                        }
                    }
                })
            }
            DeBruijnVar::Free(..) => None,
        }
    }
}
impl VarsVisitor for SubstVisitor<'_> {
    fn visit_region_var(&mut self, v: RegionDbVar) -> Option<Region> {
        self.process_var(v, |id| self.generics.regions.get(id))
    }
    fn visit_type_var(&mut self, v: TypeDbVar) -> Option<Ty> {
        self.process_var(v, |id| self.generics.types.get(id))
    }
    fn visit_const_generic_var(&mut self, v: ConstGenericDbVar) -> Option<ConstantExprKind> {
        self.process_var(v, |id| {
            self.generics.const_generics.get(id).map(|c| &c.kind)
        })
    }
    fn visit_clause_var(&mut self, v: ClauseDbVar) -> Option<TraitRefKind> {
        if self.explicits_only {
            None
        } else {
            self.process_var(v, |id| Some(&self.generics.trait_refs.get(id)?.kind))
        }
    }
    fn visit_self_clause(&mut self) -> Option<TraitRefKind> {
        Some(self.self_ref.cloned().expect(
            "used `substitute` on an item coming from a trait; \
            use `substitute_with_self` or `substitute_inner_binder` instead.",
        ))
    }
}

#[derive(Debug)]
pub struct GenericsMismatch;

/// Types that are involved at the type-level and may be substituted around.
pub trait TyVisitable: Sized + AstVisitable {
    /// Visit the variables contained in `self`, as seen from the outside of `self`. This means
    /// that any variable bound inside `self` will be skipped, and all the seen De Bruijn indices
    /// will count from the outside of `self`.
    fn visit_vars(&mut self, v: &mut impl VarsVisitor) {
        #[derive(Visitor)]
        struct Wrap<'v, V> {
            v: &'v mut V,
            depth: DeBruijnId,
        }
        impl<V> VisitorWithBinderDepth for Wrap<'_, V> {
            fn binder_depth_mut(&mut self) -> &mut DeBruijnId {
                &mut self.depth
            }
        }
        impl<V: VarsVisitor> VisitAstMut for Wrap<'_, V> {
            fn visit<'a, T: AstVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
                VisitWithBinderDepth::new(self).visit(x)
            }

            fn exit_region(&mut self, r: &mut Region) {
                match r {
                    Region::Var(var)
                        if let Some(var) = var.move_out_from_depth(self.depth)
                            && let Some(new_r) = self.v.visit_region_var(var) =>
                    {
                        *r = new_r.move_under_binders(self.depth);
                    }
                    Region::Erased | Region::Body(..)
                        if let Some(new_r) = self.v.visit_erased_region() =>
                    {
                        *r = new_r.move_under_binders(self.depth);
                    }
                    _ => (),
                }
            }
            fn exit_ty(&mut self, ty: &mut Ty) {
                if let TyKind::TypeVar(var) = ty.kind()
                    && let Some(var) = var.move_out_from_depth(self.depth)
                    && let Some(new_ty) = self.v.visit_type_var(var)
                {
                    *ty = new_ty.move_under_binders(self.depth);
                }
            }
            fn exit_constant_expr(&mut self, ce: &mut ConstantExpr) {
                if let ConstantExprKind::Var(var) = &mut ce.kind
                    && let Some(var) = var.move_out_from_depth(self.depth)
                    && let Some(new_cg) = self.v.visit_const_generic_var(var)
                {
                    ce.kind = new_cg.move_under_binders(self.depth);
                }
            }
            fn exit_trait_ref_kind(&mut self, kind: &mut TraitRefKind) {
                match kind {
                    TraitRefKind::SelfId => {
                        if let Some(new_kind) = self.v.visit_self_clause() {
                            *kind = new_kind.move_under_binders(self.depth);
                        }
                    }
                    TraitRefKind::Clause(var) => {
                        if let Some(var) = var.move_out_from_depth(self.depth)
                            && let Some(new_kind) = self.v.visit_clause_var(var)
                        {
                            *kind = new_kind.move_under_binders(self.depth);
                        }
                    }
                    _ => {}
                }
            }
        }
        Wrap {
            v,
            depth: DeBruijnId::zero(),
        }
        .visit(self);
    }

    /// Substitute the generic variables inside `self` by replacing them with the provided values.
    /// Note: if `self` is an item that comes from a `TraitDecl`, you must use
    /// `substitute_with_self` or `substitute_inner_binder`, otherwise you'll get panics.
    fn substitute(self, generics: &GenericArgs) -> Self {
        SubstVisitor::new(generics, None, false)
            .visit(self)
            .unwrap()
    }
    /// Substitute the generic variables inside `self` by replacing them with the provided values.
    /// This is appropriate when substituting an inner binder.
    fn substitute_inner_binder(self, generics: &GenericArgs) -> Self {
        self.substitute_with_self(generics, &TraitRefKind::SelfId)
    }
    /// Substitute only the type, region and const generic args.
    fn substitute_explicits(self, generics: &GenericArgs) -> Self {
        SubstVisitor::new(generics, None, true).visit(self).unwrap()
    }
    /// Substitute the generic variables as well as the `TraitRefKind::SelfId` trait ref.
    fn substitute_with_self(self, generics: &GenericArgs, self_ref: &TraitRefKind) -> Self {
        self.try_substitute_with_self(generics, self_ref).unwrap()
    }
    /// Substitute the generic variables as well as the `TraitRefKind::SelfId` trait ref.
    fn substitute_with_tref(self, tref: &TraitRef) -> Self {
        let pred = tref.trait_decl_ref.clone().erase();
        self.substitute_with_self(&pred.generics, &tref.kind)
    }

    fn try_substitute(self, generics: &GenericArgs) -> Result<Self, GenericsMismatch> {
        SubstVisitor::new(generics, None, false).visit(self)
    }
    fn try_substitute_with_self(
        self,
        generics: &GenericArgs,
        self_ref: &TraitRefKind,
    ) -> Result<Self, GenericsMismatch> {
        SubstVisitor::new(generics, Some(self_ref), false).visit(self)
    }

    /// Move under one binder.
    fn move_under_binder(self) -> Self {
        self.move_under_binders(DeBruijnId::one())
    }

    /// Move under `depth` binders.
    fn move_under_binders(mut self, depth: DeBruijnId) -> Self {
        if !depth.is_zero() {
            let Continue(()) = self.visit_db_id::<Infallible>(|id| {
                *id = id.plus(depth);
                Continue(())
            });
        }
        self
    }

    /// Move from under one binder.
    fn move_from_under_binder(self) -> Option<Self> {
        self.move_from_under_binders(DeBruijnId::one())
    }

    /// Move the value out of `depth` binders. Returns `None` if it contains a variable bound in
    /// one of these `depth` binders.
    fn move_from_under_binders(mut self, depth: DeBruijnId) -> Option<Self> {
        self.visit_db_id::<()>(|id| match id.sub(depth) {
            Some(sub) => {
                *id = sub;
                Continue(())
            }
            None => Break(()),
        })
        .is_continue()
        .then_some(self)
    }

    /// Visit the de Bruijn ids contained in `self`, as seen from the outside of `self`. This means
    /// that any variable bound inside `self` will be skipped, and all the seen indices will count
    /// from the outside of self.
    fn visit_db_id<B>(
        &mut self,
        f: impl FnMut(&mut DeBruijnId) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        struct Wrap<F> {
            f: F,
            depth: DeBruijnId,
        }
        impl<B, F> Visitor for Wrap<F>
        where
            F: FnMut(&mut DeBruijnId) -> ControlFlow<B>,
        {
            type Break = B;
        }
        impl<B, F> VisitAstMut for Wrap<F>
        where
            F: FnMut(&mut DeBruijnId) -> ControlFlow<B>,
        {
            fn enter_region_binder<T: AstVisitable>(&mut self, _: &mut RegionBinder<T>) {
                self.depth = self.depth.incr()
            }
            fn exit_region_binder<T: AstVisitable>(&mut self, _: &mut RegionBinder<T>) {
                self.depth = self.depth.decr()
            }
            fn enter_binder<T: AstVisitable>(&mut self, _: &mut Binder<T>) {
                self.depth = self.depth.incr()
            }
            fn exit_binder<T: AstVisitable>(&mut self, _: &mut Binder<T>) {
                self.depth = self.depth.decr()
            }

            fn visit_de_bruijn_id(&mut self, x: &mut DeBruijnId) -> ControlFlow<Self::Break> {
                if let Some(mut shifted) = x.sub(self.depth) {
                    (self.f)(&mut shifted)?;
                    *x = shifted.plus(self.depth)
                }
                Continue(())
            }
        }
        self.drive_mut(&mut Wrap {
            f,
            depth: DeBruijnId::zero(),
        })
    }

    /// Replace all the erased regions by the output of the provided function. Binders levels are
    /// handled automatically.
    fn replace_erased_regions(mut self, f: impl FnMut() -> Region) -> Self {
        #[derive(Visitor)]
        struct RefreshErasedRegions<F>(F);
        impl<F: FnMut() -> Region> VarsVisitor for RefreshErasedRegions<F> {
            fn visit_erased_region(&mut self) -> Option<Region> {
                Some((self.0)())
            }
        }
        self.visit_vars(&mut RefreshErasedRegions(f));
        self
    }
}

/// A value of type `T` applied to some `GenericArgs`, except we havent applied them yet to avoid a
/// deep clone.
#[derive(Debug, Clone)]
pub struct Substituted<'a, T> {
    pub val: &'a T,
    pub generics: Cow<'a, GenericArgs>,
    pub trait_self: Option<&'a TraitRefKind>,
}

impl<'a, T> Substituted<'a, T> {
    pub fn new(val: &'a T, generics: &'a GenericArgs) -> Self {
        Self {
            val,
            generics: Cow::Borrowed(generics),
            trait_self: None,
        }
    }
    pub fn new_for_trait(
        val: &'a T,
        generics: &'a GenericArgs,
        trait_self: &'a TraitRefKind,
    ) -> Self {
        Self {
            val,
            generics: Cow::Borrowed(generics),
            trait_self: Some(trait_self),
        }
    }
    pub fn new_for_trait_ref(val: &'a T, tref: &'a TraitRef) -> Self {
        Self {
            val,
            generics: Cow::Owned(*tref.trait_decl_ref.clone().erase().generics),
            trait_self: Some(&tref.kind),
        }
    }

    pub fn rebind<U>(&self, val: &'a U) -> Substituted<'a, U> {
        Substituted {
            val,
            generics: self.generics.clone(),
            trait_self: self.trait_self.clone(),
        }
    }

    pub fn substitute(&self) -> T
    where
        T: TyVisitable + Clone,
    {
        self.try_substitute().unwrap()
    }
    pub fn try_substitute(&self) -> Result<T, GenericsMismatch>
    where
        T: TyVisitable + Clone,
    {
        match self.trait_self {
            None => self.val.clone().try_substitute(&self.generics),
            Some(trait_self) => self
                .val
                .clone()
                .try_substitute_with_self(&self.generics, trait_self),
        }
    }

    pub fn iter<Item: 'a>(&self) -> impl Iterator<Item = Substituted<'a, Item>>
    where
        &'a T: IntoIterator<Item = &'a Item>,
    {
        self.val.into_iter().map(move |x| self.rebind(x))
    }
}

impl TypeDecl {
    /// Looks up the variant corresponding to the tag (i.e. the in-memory bytes that represent the discriminant).
    /// Returns `None` for types that don't have a relevant discriminant (e.g. uninhabited types).
    ///
    /// If the `tag` does not correspond to any valid discriminant but there is a niche,
    /// the resulting `VariantId` will be for the untagged variant [`TagEncoding::Niche::untagged_variant`].
    pub fn get_variant_from_tag(&self, tag: ScalarValue) -> Option<VariantId> {
        let layout = self.layout.as_ref()?;
        if layout.uninhabited {
            return None;
        };
        let discr_layout = layout.discriminant_layout.as_ref()?;

        let variant_for_tag =
            layout
                .variant_layouts
                .iter_enumerated()
                .find_map(|(id, variant_layout)| {
                    if variant_layout.tag == Some(tag) {
                        Some(id)
                    } else {
                        None
                    }
                });

        match &discr_layout.encoding {
            TagEncoding::Direct => {
                assert_eq!(tag.get_integer_ty(), discr_layout.tag_ty);
                variant_for_tag
            }
            TagEncoding::Niche { untagged_variant } => variant_for_tag.or(Some(*untagged_variant)),
        }
    }

    pub fn is_c_repr(&self) -> bool {
        self.repr
            .as_ref()
            .is_some_and(|repr| repr.repr_algo == ReprAlgorithm::C)
    }

    pub fn get_field_by_name(
        &self,
        variant: Option<VariantId>,
        field_name: &str,
    ) -> Option<(FieldId, &Field)> {
        let fields = match &self.kind {
            TypeDeclKind::Struct(fields) | TypeDeclKind::Union(fields) => fields,
            TypeDeclKind::Enum(variants) => &variants[variant.unwrap()].fields,
            _ => return None,
        };
        fields
            .iter_indexed()
            .find(|(_, field)| field.name.as_deref() == Some(field_name))
    }
}

impl Layout {
    pub fn is_variant_uninhabited(&self, variant_id: VariantId) -> bool {
        if let Some(v) = self.variant_layouts.get(variant_id) {
            v.uninhabited
        } else {
            false
        }
    }
}

impl ReprOptions {
    /// Whether this representation options guarantee a fixed
    /// field ordering for the type.
    ///
    /// Since we don't support `repr(simd)` or `repr(linear)` yet, this is
    /// the case if it's either `repr(C)` or an explicit discriminant type for
    /// an enum with fields (if it doesn't have fields, this obviously doesn't matter anyway).
    ///
    /// Cf. <https://doc.rust-lang.org/reference/type-layout.html#r-layout.repr.c.struct>
    /// and <https://doc.rust-lang.org/reference/type-layout.html#r-layout.repr.primitive.adt>.
    pub fn guarantees_fixed_field_order(&self) -> bool {
        self.repr_algo == ReprAlgorithm::C || self.explicit_discr_type
    }
}

impl<T: AstVisitable> TyVisitable for T {}

impl Eq for TraitParam {}

mk_index_impls!(GenericArgs.regions[RegionId]: Region);
mk_index_impls!(GenericArgs.types[TypeVarId]: Ty);
mk_index_impls!(GenericArgs.const_generics[ConstGenericVarId]: ConstantExpr);
mk_index_impls!(GenericArgs.trait_refs[TraitClauseId]: TraitRef);
mk_index_impls!(GenericParams.regions[RegionId]: RegionParam);
mk_index_impls!(GenericParams.types[TypeVarId]: TypeParam);
mk_index_impls!(GenericParams.const_generics[ConstGenericVarId]: ConstGenericParam);
mk_index_impls!(GenericParams.trait_clauses[TraitClauseId]: TraitParam);
