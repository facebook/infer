//! Copies of the relevant type-level types. These are semantically-rich representations of
//! type-level concepts such as types and trait references.
use crate::prelude::*;
use crate::sinto_as_usize;
use crate::sinto_todo;

use rustc_middle::ty;
use rustc_span::def_id::DefId as RDefId;

/// Generic container for decorating items with a type, a span,
/// attributes and other meta-data.

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Decorated<T> {
    pub ty: Ty,
    pub contents: Box<T>,
}

/// Reflects [`ty::ParamTy`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::ParamTy, state: S as gstate)]
pub struct ParamTy {
    pub index: u32,
    pub name: Symbol,
}

/// Reflects [`ty::ParamConst`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<S>, from: ty::ParamConst, state: S as gstate)]
pub struct ParamConst {
    pub index: u32,
    pub name: Symbol,
}

/// A predicate without `Self`, for use in `dyn Trait`.
///
/// Reflects [`ty::ExistentialPredicate`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::ExistentialPredicate<'tcx>, state: S as state)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ExistentialPredicate {
    /// E.g. `From<u64>`. Note that this isn't `T: From<u64>` with a given `T`, this is just
    /// `From<u64>`. Could be written `?: From<u64>`.
    Trait(ExistentialTraitRef),
    /// E.g. `Iterator::Item = u64`. Could be written `<? as Iterator>::Item = u64`.
    Projection(ExistentialProjection),
    /// E.g. `Send`.
    AutoTrait(DefId),
}

/// Reflects [`rustc_type_ir::ExistentialTraitRef`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: rustc_type_ir::ExistentialTraitRef<ty::TyCtxt<'tcx>>, state: S as state)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExistentialTraitRef {
    pub def_id: DefId,
    pub args: Vec<GenericArg>,
}

/// Reflects [`rustc_type_ir::ExistentialProjection`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: rustc_type_ir::ExistentialProjection<ty::TyCtxt<'tcx>>, state: S as state)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExistentialProjection {
    pub def_id: DefId,
    pub args: Vec<GenericArg>,
    pub term: Term,
}

/// Reflects [`ty::BoundTyKind`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::BoundTyKind<'tcx>, state: S as s)]
pub enum BoundTyKind {
    Anon,
    #[custom_arm(&FROM_TYPE::Param(def_id) => TO_TYPE::Param(def_id.sinto(s), s.base().tcx.item_name(def_id).sinto(s)),)]
    Param(DefId, Symbol),
}

/// Reflects [`ty::BoundTy`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::BoundTy<'tcx>, state: S as s)]
pub struct BoundTy {
    pub var: BoundVar,
    pub kind: BoundTyKind,
}

sinto_as_usize!(rustc_middle::ty, BoundVar);

/// Reflects [`ty::BoundRegionKind`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::BoundRegionKind<'tcx>, state: S as s)]
pub enum BoundRegionKind {
    Anon,
    NamedForPrinting(Symbol),
    #[custom_arm(&FROM_TYPE::Named(def_id) => TO_TYPE::Named(def_id.sinto(s), s.base().tcx.item_name(def_id).sinto(s)),)]
    Named(DefId, Symbol),
    ClosureEnv,
}

/// Reflects [`ty::BoundRegion`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::BoundRegion<'tcx>, state: S as s)]
pub struct BoundRegion {
    pub var: BoundVar,
    pub kind: BoundRegionKind,
}

/// Reflects [`ty::PlaceholderRegion`]
pub type PlaceholderRegion = Placeholder<BoundRegion>;
/// Reflects [`ty::PlaceholderConst`]
pub type PlaceholderConst = Placeholder<BoundVar>;
/// Reflects [`ty::PlaceholderType`]
pub type PlaceholderType = Placeholder<BoundTy>;

/// Reflects [`ty::Placeholder`]

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Placeholder<T> {
    pub bound: T,
}

impl<'tcx, S: UnderOwnerState<'tcx>, T: SInto<S, U>, U> SInto<S, Placeholder<U>>
    for ty::Placeholder<ty::TyCtxt<'tcx>, T>
{
    fn sinto(&self, s: &S) -> Placeholder<U> {
        Placeholder {
            bound: self.bound.sinto(s),
        }
    }
}

/// Reflects [`rustc_middle::infer::canonical::Canonical`]

#[derive(Clone, Debug)]
pub struct Canonical<T> {
    pub value: T,
}
/// Reflects [`ty::CanonicalUserType`]
pub type CanonicalUserType = Canonical<UserType>;

impl<'tcx, S: UnderOwnerState<'tcx>, T: SInto<S, U>, U> SInto<S, Canonical<U>>
    for rustc_middle::infer::canonical::Canonical<'tcx, T>
{
    fn sinto(&self, s: &S) -> Canonical<U> {
        Canonical {
            value: self.value.sinto(s),
        }
    }
}

/// Reflects [`ty::UserSelfTy`]

#[derive(AdtInto, Clone, Debug)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::UserSelfTy<'tcx>, state: S as gstate)]
pub struct UserSelfTy {
    pub impl_def_id: DefId,
    pub self_ty: Ty,
}

/// Reflects [`ty::UserArgs`]

#[derive(AdtInto, Clone, Debug)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::UserArgs<'tcx>, state: S as gstate)]
pub struct UserArgs {
    pub args: Vec<GenericArg>,
    pub user_self_ty: Option<UserSelfTy>,
}

/// Reflects [`ty::UserType`]: this is currently
/// disabled, and everything is printed as debug in the
/// [`UserType::Todo`] variant.

#[derive(AdtInto, Clone, Debug)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::UserType<'tcx>, state: S as _s)]
pub enum UserType {
    // TODO: for now, we don't use user types at all.
    // We disable it for now, since it cause the following to fail:
    //
    //    pub const MY_VAL: u16 = 5;
    //    pub type Alias = MyStruct<MY_VAL>; // Using the literal 5, it goes through
    //
    //    pub struct MyStruct<const VAL: u16> {}
    //
    //    impl<const VAL: u16> MyStruct<VAL> {
    //        pub const MY_CONST: u16 = VAL;
    //    }
    //
    //    pub fn do_something() -> u32 {
    //        u32::from(Alias::MY_CONST)
    //    }
    //
    // In this case, we get a [ty::ConstKind::Bound] in
    // [do_something], which we are not able to translate.
    // See: https://github.com/hacspec/hax/pull/209

    // Ty(Ty),
    // TypeOf(DefId, UserArgs),
    #[todo]
    Todo(String),
}

/// Reflects [`ty::VariantDiscr`]

#[derive(AdtInto, Clone, Debug)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::VariantDiscr, state: S as gstate)]
pub enum DiscriminantDefinition {
    Explicit(DefId),
    Relative(u32),
}

/// Reflects [`ty::util::Discr`]

#[derive(AdtInto, Clone, Debug)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::util::Discr<'tcx>, state: S as gstate)]
pub struct DiscriminantValue {
    pub val: u128,
    pub ty: Ty,
}

/// Reflects [`ty::Visibility`]

#[derive(Clone, Debug)]
pub enum Visibility<Id> {
    Public,
    Restricted(Id),
}

impl<S, T: SInto<S, U>, U> SInto<S, Visibility<U>> for ty::Visibility<T> {
    fn sinto(&self, s: &S) -> Visibility<U> {
        use ty::Visibility as T;
        match self {
            T::Public => Visibility::Public,
            T::Restricted(id) => Visibility::Restricted(id.sinto(s)),
        }
    }
}

/// Reflects [`ty::FieldDef`]

#[derive(Clone, Debug)]
pub struct FieldDef {
    pub did: DefId,
    /// Field definition of [tuple
    /// structs](https://doc.rust-lang.org/book/ch05-01-defining-structs.html#using-tuple-structs-without-named-fields-to-create-different-types)
    /// are anonymous, in that case `name` is [`None`].
    pub name: Option<Symbol>,
    pub vis: Visibility<DefId>,
    pub ty: Ty,
    pub span: Span,
}

impl FieldDef {
    pub fn sfrom<'tcx, S: UnderOwnerState<'tcx>>(
        s: &S,
        fdef: &ty::FieldDef,
        instantiate: ty::GenericArgsRef<'tcx>,
    ) -> FieldDef {
        let tcx = s.base().tcx;
        let ty = fdef.ty(tcx, instantiate).sinto(s);
        let name = {
            let name = fdef.name.sinto(s);
            let is_user_provided = {
                // SH: Note that the only way I found of checking if the user wrote the name or if it
                // is just an integer generated by rustc is by checking if it is just made of
                // numerals...
                name.to_string().parse::<usize>().is_err()
            };
            is_user_provided.then_some(name)
        };

        FieldDef {
            did: fdef.did.sinto(s),
            name,
            vis: fdef.vis.sinto(s),
            ty,
            span: tcx.def_span(fdef.did).sinto(s),
        }
    }
}

/// Reflects [`ty::VariantDef`]

#[derive(Clone, Debug)]
pub struct VariantDef {
    pub def_id: DefId,
    pub ctor: Option<(CtorKind, DefId)>,
    pub name: Symbol,
    pub discr_def: DiscriminantDefinition,
    pub discr_val: DiscriminantValue,
    /// The definitions of the fields on this variant. In case of [tuple
    /// structs/variants](https://doc.rust-lang.org/book/ch05-01-defining-structs.html#using-tuple-structs-without-named-fields-to-create-different-types),
    /// the fields are anonymous, otherwise fields are named.
    pub fields: IndexVec<FieldIdx, FieldDef>,
    /// Span of the definition of the variant
    pub span: Span,
}

impl VariantDef {
    pub(crate) fn sfrom<'tcx, S: UnderOwnerState<'tcx>>(
        s: &S,
        def: &ty::VariantDef,
        discr_val: ty::util::Discr<'tcx>,
        instantiate: Option<ty::GenericArgsRef<'tcx>>,
    ) -> Self {
        let tcx = s.base().tcx;
        let instantiate =
            instantiate.unwrap_or_else(|| ty::GenericArgs::identity_for_item(tcx, def.def_id));
        VariantDef {
            def_id: def.def_id.sinto(s),
            ctor: def.ctor.sinto(s),
            name: def.name.sinto(s),
            discr_def: def.discr.sinto(s),
            discr_val: discr_val.sinto(s),
            fields: def
                .fields
                .iter()
                .map(|f| FieldDef::sfrom(s, f, instantiate))
                .collect(),
            span: s.base().tcx.def_span(def.def_id).sinto(s),
        }
    }
}

/// Reflects [`ty::EarlyParamRegion`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::EarlyParamRegion, state: S as s)]
pub struct EarlyParamRegion {
    pub index: u32,
    pub name: Symbol,
}

/// Reflects [`ty::LateParamRegion`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::LateParamRegion, state: S as s)]
pub struct LateParamRegion {
    pub scope: DefId,
    pub kind: LateParamRegionKind,
}

/// Reflects [`ty::LateParamRegionKind`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::LateParamRegionKind, state: S as s)]
pub enum LateParamRegionKind {
    Anon(u32),
    NamedAnon(u32, Symbol),
    #[custom_arm(&FROM_TYPE::Named(def_id) => TO_TYPE::Named(def_id.sinto(s), s.base().tcx.item_name(def_id).sinto(s)),)]
    Named(DefId, Symbol),
    ClosureEnv,
}

/// Reflects [`ty::RegionKind`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::RegionKind<'tcx>, state: S as gstate)]
pub enum RegionKind {
    ReEarlyParam(EarlyParamRegion),
    ReBound(BoundVarIndexKind, BoundRegion),
    ReLateParam(LateParamRegion),
    ReStatic,
    ReVar(RegionVid),
    RePlaceholder(PlaceholderRegion),
    ReErased,
    ReError(ErrorGuaranteed),
}

/// Reflects [`ty::BoundVarIndexKind`]

#[derive(AdtInto, Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::BoundVarIndexKind, state: S as gstate)]
pub enum BoundVarIndexKind {
    Bound(DebruijnIndex),
    Canonical,
}

sinto_as_usize!(rustc_middle::ty, DebruijnIndex);
sinto_as_usize!(rustc_middle::ty, RegionVid);

/// Reflects [`ty::Region`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::Region<'tcx>, state: S as s)]
pub struct Region {
    #[value(self.kind().sinto(s))]
    pub kind: RegionKind,
}

/// Reflects both [`ty::GenericArg`] and [`ty::GenericArgKind`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::GenericArgKind<'tcx>, state: S as s)]
pub enum GenericArg {
    Lifetime(Region),
    Type(Ty),
    Const(ConstantExpr),
}

/// Contents of `ItemRef`.

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ItemRefContents {
    /// The item being refered to.
    pub def_id: DefId,
    /// The generics passed to the item. If `in_trait` is `Some`, these are only the generics of
    /// the method/type/const itself; generics for the traits are available in
    /// `in_trait.unwrap().trait`.
    pub generic_args: Vec<GenericArg>,
    /// Witnesses of the trait clauses required by the item, e.g. `T: Sized` for `Option<T>` or `B:
    /// ToOwned` for `Cow<'a, B>`. Same as above, for associated items this only includes clauses
    /// for the item itself.
    pub impl_exprs: Vec<ImplExpr>,
    /// If we're referring to a trait associated item, this gives the trait clause/impl we're
    /// referring to.
    pub in_trait: Option<ImplExpr>,
    /// Whether this contains any reference to a type/lifetime/const parameter.
    pub has_param: bool,
    /// Whether this contains any reference to a type/const parameter.
    pub has_non_lt_param: bool,
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, GenericArg> for ty::GenericArg<'tcx> {
    fn sinto(&self, s: &S) -> GenericArg {
        self.kind().sinto(s)
    }
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, Vec<GenericArg>> for ty::GenericArgsRef<'tcx> {
    fn sinto(&self, s: &S) -> Vec<GenericArg> {
        self.iter().map(|v| v.kind().sinto(s)).collect()
    }
}

/// Reflects both [`ty::GenericArg`] and [`ty::GenericArgKind`]
#[derive(AdtInto)]
#[args(<'tcx, S: BaseState<'tcx>>, from: rustc_ast::ast::LitIntType, state: S as gstate)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LitIntType {
    Signed(IntTy),
    Unsigned(UintTy),
    Unsuffixed,
}

/// Reflects partially [`ty::InferTy`]

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S>, from: ty::InferTy, state: S as gstate)]
pub enum InferTy {
    #[custom_arm(FROM_TYPE::TyVar(..) => TO_TYPE::TyVar,)]
    TyVar, /*TODO?*/
    #[custom_arm(FROM_TYPE::IntVar(..) => TO_TYPE::IntVar,)]
    IntVar, /*TODO?*/
    #[custom_arm(FROM_TYPE::FloatVar(..) => TO_TYPE::FloatVar,)]
    FloatVar, /*TODO?*/
    FreshTy(u32),
    FreshIntTy(u32),
    FreshFloatTy(u32),
}

/// Reflects [`rustc_type_ir::IntTy`]
#[derive(AdtInto)]
#[args(<S>, from: rustc_type_ir::IntTy, state: S as _s)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

/// Reflects [`rustc_type_ir::FloatTy`]
#[derive(AdtInto)]
#[args(<S>, from: rustc_type_ir::FloatTy, state: S as _s)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FloatTy {
    F16,
    F32,
    F64,
    F128,
}

/// Reflects [`rustc_type_ir::UintTy`]
#[derive(AdtInto)]
#[args(<S>, from: rustc_type_ir::UintTy, state: S as _s)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

impl ToString for IntTy {
    fn to_string(&self) -> String {
        use IntTy::*;
        match self {
            Isize => "isize".to_string(),
            I8 => "i8".to_string(),
            I16 => "i16".to_string(),
            I32 => "i32".to_string(),
            I64 => "i64".to_string(),
            I128 => "i128".to_string(),
        }
    }
}

impl ToString for UintTy {
    fn to_string(&self) -> String {
        use UintTy::*;
        match self {
            Usize => "usize".to_string(),
            U8 => "u8".to_string(),
            U16 => "u16".to_string(),
            U32 => "u32".to_string(),
            U64 => "u64".to_string(),
            U128 => "u128".to_string(),
        }
    }
}

/// Reflects [`ty::TypeAndMut`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::TypeAndMut<'tcx>, state: S as gstate)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeAndMut {
    pub ty: Box<Ty>,
    pub mutbl: Mutability,
}

impl<S, U, T: SInto<S, U>> SInto<S, Vec<U>> for ty::List<T> {
    fn sinto(&self, s: &S) -> Vec<U> {
        self.iter().map(|x| x.sinto(s)).collect()
    }
}

/// Reflects [`ty::Variance`]
#[derive(AdtInto)]
#[args(<S>, from: ty::Variance, state: S as _s)]
#[derive(Clone, Debug)]
pub enum Variance {
    Covariant,
    Invariant,
    Contravariant,
    Bivariant,
}

/// Reflects [`ty::GenericParamDef`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::GenericParamDef, state: S as s)]
#[derive(Clone, Debug)]
pub struct GenericParamDef {
    pub name: Symbol,
    pub def_id: DefId,
    pub index: u32,
    pub pure_wrt_drop: bool,
    #[value(
        match self.kind {
            ty::GenericParamDefKind::Lifetime => GenericParamDefKind::Lifetime,
            ty::GenericParamDefKind::Type { has_default, synthetic } => GenericParamDefKind::Type { has_default, synthetic },
            ty::GenericParamDefKind::Const { has_default, .. } => {
                let ty = s.base().tcx.type_of(self.def_id).instantiate_identity().sinto(s);
                GenericParamDefKind::Const { has_default, ty }
            },
        }
    )]
    pub kind: GenericParamDefKind,
    /// Variance of this type parameter, if sensible.
    #[value({
        use rustc_hir::def::DefKind::*;
        let tcx = s.base().tcx;
        let parent = tcx.parent(self.def_id);
        match tcx.def_kind(parent) {
            Fn | AssocFn | Enum | Struct | Union | Ctor(..) | OpaqueTy => {
                tcx.variances_of(parent).get(self.index as usize).sinto(s)
            }
            _ => None
        }
    })]
    pub variance: Option<Variance>,
}

/// Reflects [`ty::GenericParamDefKind`]

#[derive(Clone, Debug)]
pub enum GenericParamDefKind {
    Lifetime,
    Type { has_default: bool, synthetic: bool },
    Const { has_default: bool, ty: Ty },
}

/// Reflects [`ty::Generics`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::Generics, state: S as state)]
#[derive(Clone, Debug)]
pub struct TyGenerics {
    pub parent: Option<DefId>,
    pub parent_count: usize,
    #[from(own_params)]
    pub params: Vec<GenericParamDef>,
    // pub param_def_id_to_index: FxHashMap<DefId, u32>,
    pub has_self: bool,
    pub has_late_bound_regions: Option<Span>,
}

impl TyGenerics {
    pub(crate) fn count_total_params(&self) -> usize {
        self.parent_count + self.params.len()
    }
}

/// This type merges the information from
/// `rustc_type_ir::AliasKind` and `ty::AliasTy`

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Alias {
    pub kind: AliasKind,
    pub args: Vec<GenericArg>,
    pub def_id: DefId,
}

/// Reflects [`ty::AliasKind`]

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AliasKind {
    /// The projection of a trait type: `<Ty as Trait<...>>::Type<...>`
    Projection {
        /// The `impl Trait for Ty` in `Ty: Trait<..., Type = U>`.
        impl_expr: ImplExpr,
        /// The `Type` in `Ty: Trait<..., Type = U>`.
        assoc_item: AssocItem,
    },
    /// An associated type in an inherent impl.
    Inherent,
    /// An `impl Trait` opaque type.
    Opaque {
        /// The real type hidden inside this opaque type.
        hidden_ty: Ty,
    },
    /// A type alias that references opaque types. Likely to always be normalized away.
    Free,
}

impl Alias {
    #[tracing::instrument(level = "trace", skip(s))]
    fn from<'tcx, S: UnderOwnerState<'tcx>>(
        s: &S,
        alias_kind: &rustc_type_ir::AliasTyKind,
        alias_ty: &ty::AliasTy<'tcx>,
    ) -> TyKind {
        let tcx = s.base().tcx;
        let typing_env = s.typing_env();
        use rustc_type_ir::AliasTyKind as RustAliasKind;

        // Try to normalize the alias first.
        let ty = ty::Ty::new_alias(tcx, *alias_kind, *alias_ty);
        let ty = crate::traits::normalize(tcx, typing_env, ty);
        let ty::Alias(alias_kind, alias_ty) = ty.kind() else {
            let ty: Ty = ty.sinto(s);
            return ty.kind().clone();
        };

        let kind = match alias_kind {
            RustAliasKind::Projection => {
                let trait_ref = alias_ty.trait_ref(tcx);
                // In a case like:
                // ```
                // impl<T, U> Trait for Result<T, U>
                // where
                //     for<'a> &'a Result<T, U>: IntoIterator,
                //     for<'a> <&'a Result<T, U> as IntoIterator>::Item: Copy,
                // {}
                // ```
                // the `&'a Result<T, U> as IntoIterator` trait ref has escaping bound variables
                // yet we dont have a binder around (could even be several). Binding this correctly
                // is therefore difficult. Since our trait resolution ignores lifetimes anyway, we
                // just erase them. See also https://github.com/hacspec/hax/issues/747.
                let trait_ref = crate::traits::erase_free_regions(tcx, trait_ref);
                let item = tcx.associated_item(alias_ty.def_id);
                AliasKind::Projection {
                    assoc_item: AssocItem::sfrom(s, &item),
                    impl_expr: solve_trait(s, ty::Binder::dummy(trait_ref)),
                }
            }
            RustAliasKind::Inherent => AliasKind::Inherent,
            RustAliasKind::Opaque => {
                // Reveal the underlying `impl Trait` type.
                let ty = tcx.type_of(alias_ty.def_id).instantiate(tcx, alias_ty.args);
                AliasKind::Opaque {
                    hidden_ty: ty.sinto(s),
                }
            }
            RustAliasKind::Free => AliasKind::Free,
        };
        TyKind::Alias(Alias {
            kind,
            args: alias_ty.args.sinto(s),
            def_id: alias_ty.def_id.sinto(s),
        })
    }
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, Box<Ty>> for ty::Ty<'tcx> {
    fn sinto(&self, s: &S) -> Box<Ty> {
        Box::new(self.sinto(s))
    }
}

/// Reflects [`rustc_middle::ty::Ty`]

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ty {
    pub(crate) kind: id_table::hash_consing::HashConsed<TyKind>,
}

impl Ty {
    pub fn new<'tcx, S: BaseState<'tcx>>(_s: &S, kind: TyKind) -> Self {
        let kind = id_table::hash_consing::HashConsed::new(kind);
        Ty { kind }
    }

    pub fn kind(&self) -> &TyKind {
        self.kind.inner()
    }
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, Ty> for rustc_middle::ty::Ty<'tcx> {
    fn sinto(&self, s: &S) -> Ty {
        if let Some(ty) = s.with_cache(|cache| cache.tys.get(self).cloned()) {
            return ty;
        }
        let kind: TyKind = self.kind().sinto(s);
        let ty = Ty::new(s, kind);
        s.with_cache(|cache| {
            cache.tys.insert(*self, ty.clone());
        });
        ty
    }
}

/// Reflects [`ty::TyKind`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::TyKind<'tcx>, state: S as s)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TyKind {
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),

    #[custom_arm(
        ty::TyKind::FnDef(fun_id, generics) => {
            let item = translate_item_ref(s, *fun_id, generics);
            let tcx = s.base().tcx;
            let fn_sig = tcx.fn_sig(*fun_id).instantiate(tcx, generics);
            let fn_sig = Box::new(fn_sig.sinto(s));
            TyKind::FnDef { item, fn_sig }
        },
    )]
    /// Reflects [`ty::TyKind::FnDef`]
    FnDef {
        item: ItemRef,
        fn_sig: Box<PolyFnSig>,
    },

    #[custom_arm(
        ty::TyKind::FnPtr(tys, header) => {
            let sig = tys.map_bound(|tys| ty::FnSig {
                inputs_and_output: tys.inputs_and_output,
                c_variadic: header.c_variadic,
                safety: header.safety,
                abi: header.abi,
            });
            TyKind::Arrow(Box::new(sig.sinto(s)))
        },
    )]
    /// Reflects [`ty::TyKind::FnPtr`]
    Arrow(Box<PolyFnSig>),

    #[custom_arm(
        ty::TyKind::Closure (def_id, generics) => {
            TyKind::Closure(ClosureArgs::sfrom(s, *def_id, generics))
        },
    )]
    Closure(ClosureArgs),

    #[custom_arm(FROM_TYPE::Adt(adt_def, generics) => TO_TYPE::Adt(translate_item_ref(s, adt_def.did(), generics)),)]
    Adt(ItemRef),
    #[custom_arm(FROM_TYPE::Foreign(def_id) => TO_TYPE::Foreign(translate_item_ref(s, *def_id, Default::default())),)]
    Foreign(ItemRef),
    /// The `ItemRef` uses the fake `Array` def_id.
    #[custom_arm(FROM_TYPE::Array(ty, len) => TO_TYPE::Array({
        let args = s.base().tcx.mk_args(&[(*ty).into(), (*len).into()]);
        ItemRef::translate_synthetic(s, SyntheticItem::Array, args)
    }),)]
    Array(ItemRef),
    /// The `ItemRef` uses the fake `Slice` def_id.
    #[custom_arm(FROM_TYPE::Slice(ty) => TO_TYPE::Slice({
        let args = s.base().tcx.mk_args(&[(*ty).into()]);
        ItemRef::translate_synthetic(s, SyntheticItem::Slice, args)
    }),)]
    Slice(ItemRef),
    /// The `ItemRef` uses the fake `Tuple` def_id.
    #[custom_arm(FROM_TYPE::Tuple(tys) => TO_TYPE::Tuple({
        let args = s.base().tcx.mk_args_from_iter(tys.into_iter().map(ty::GenericArg::from));
        ItemRef::translate_synthetic(s, SyntheticItem::Tuple(tys.len()), args)
    }),)]
    Tuple(ItemRef),
    Str,
    RawPtr(Box<Ty>, Mutability),
    Ref(Region, Box<Ty>, Mutability),
    #[custom_arm(FROM_TYPE::Dynamic(preds, region) => TyKind::Dynamic(resolve_for_dyn(s, preds, |_, _| ()), region.sinto(s)),)]
    Dynamic(DynBinder<()>, Region),
    #[custom_arm(FROM_TYPE::Coroutine(def_id, generics) => TO_TYPE::Coroutine(translate_item_ref(s, *def_id, generics)),)]
    Coroutine(ItemRef),
    Never,
    #[custom_arm(FROM_TYPE::Alias(alias_kind, alias_ty) => Alias::from(s, alias_kind, alias_ty),)]
    Alias(Alias),
    Param(ParamTy),
    Bound(BoundVarIndexKind, BoundTy),
    Placeholder(PlaceholderType),
    Infer(InferTy),
    #[custom_arm(FROM_TYPE::Error(..) => TO_TYPE::Error,)]
    Error,
    #[todo]
    Todo(String),
}

/// A representation of `exists<T: Trait1 + Trait2>(value)`: we create a fresh type id and the
/// appropriate trait clauses. The contained value may refer to the fresh ty and the in-scope trait
/// clauses. This is used to represent types related to `dyn Trait`.

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct DynBinder<T> {
    /// Fresh type parameter that we use as the `Self` type in the prediates below.
    pub existential_ty: ParamTy,
    /// Clauses that define the trait object. These clauses use the fresh type parameter above
    /// as `Self` type.
    pub predicates: GenericPredicates,
    /// The value inside the binder.
    pub val: T,
}

/// Do trait resolution in the context of the clauses of a `dyn Trait` type.
fn resolve_for_dyn<'tcx, S: UnderOwnerState<'tcx>, R>(
    s: &S,
    // The predicates in the context.
    epreds: &'tcx ty::List<ty::Binder<'tcx, ty::ExistentialPredicate<'tcx>>>,
    f: impl FnOnce(&mut PredicateSearcher<'tcx>, ty::Ty<'tcx>) -> R,
) -> DynBinder<R> {
    fn searcher_for_traits<'tcx, S: UnderOwnerState<'tcx>>(
        s: &S,
        preds: &ItemPredicates<'tcx>,
    ) -> PredicateSearcher<'tcx> {
        let tcx = s.base().tcx;
        // Populate a predicate searcher that knows about the `dyn` clauses.
        let mut predicate_searcher = s.with_predicate_searcher(|ps| ps.clone());
        predicate_searcher.insert_bound_predicates(preds.iter());
        predicate_searcher.set_param_env(
            rustc_trait_selection::traits::normalize_param_env_or_error(
                tcx,
                ty::ParamEnv::new(
                    tcx.mk_clauses_from_iter(
                        s.param_env()
                            .caller_bounds()
                            .iter()
                            .chain(preds.iter().map(|pred| pred.clause)),
                    ),
                ),
                rustc_trait_selection::traits::ObligationCause::dummy(),
            ),
        );
        predicate_searcher
    }

    fn fresh_param_ty<'tcx, S: UnderOwnerState<'tcx>>(s: &S) -> ty::ParamTy {
        let tcx = s.base().tcx;
        let def_id = s.owner_id();
        let generics = tcx.generics_of(def_id);
        let param_count = generics.parent_count + generics.own_params.len();
        ty::ParamTy::new(param_count as u32 + 1, rustc_span::Symbol::intern("_dyn"))
    }

    let tcx = s.base().tcx;
    let span = rustc_span::DUMMY_SP.sinto(s);

    // Pretend there's an extra type in the environment.
    let new_param_ty = fresh_param_ty(s);
    let new_ty = new_param_ty.to_ty(tcx);

    // Set the new type as the `Self` parameter of our predicates.
    let predicates = epreds.iter().map(|epred| epred.with_self_ty(tcx, new_ty));
    let predicates: ItemPredicates<'_> = ItemPredicates::new_unmapped(span, predicates);

    // Populate a predicate searcher that knows about the `dyn` clauses.
    let mut predicate_searcher = searcher_for_traits(s, &predicates);
    let val = f(&mut predicate_searcher, new_ty);

    // Using the predicate searcher, translate the predicates. Only the projection predicates need
    // to be handled specially.
    let predicates = predicates
        .iter()
        .map(|pred| {
            match pred.clause.as_projection_clause() {
                // Translate normally
                None => pred.sinto(s),
                // Translate by hand using our predicate searcher. This does the same as
                // `clause.sinto(s)` except that it uses our predicate searcher to resolve the
                // projection `ImplExpr`.
                Some(proj) => {
                    let bound_vars = proj.bound_vars().sinto(s);
                    let proj = {
                        let alias_ty = &proj.skip_binder().projection_term.expect_ty(tcx);
                        let impl_expr = {
                            let poly_trait_ref = proj.rebind(alias_ty.trait_ref(tcx));
                            predicate_searcher
                                .resolve(&poly_trait_ref, &|_| {})
                                .s_unwrap(s)
                                .sinto(s)
                        };
                        let Term::Ty(ty) = proj.skip_binder().term.sinto(s) else {
                            unreachable!()
                        };
                        let item = tcx.associated_item(alias_ty.def_id);
                        ProjectionPredicate {
                            impl_expr,
                            assoc_item: AssocItem::sfrom(s, &item),
                            ty,
                        }
                    };
                    let kind = Binder {
                        value: ClauseKind::Projection(proj),
                        bound_vars,
                    };
                    let clause = Clause { kind };
                    GenericPredicate {
                        id: pred.id.sinto(s),
                        clause,
                        span,
                    }
                }
            }
        })
        .collect();

    let predicates = GenericPredicates { predicates };
    DynBinder {
        existential_ty: new_param_ty.sinto(s),
        predicates,
        val,
    }
}

/// Reflects [`ty::CanonicalUserTypeAnnotation`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::CanonicalUserTypeAnnotation<'tcx>, state: S as gstate)]
#[derive(Clone, Debug)]
pub struct CanonicalUserTypeAnnotation {
    pub user_ty: CanonicalUserType,
    pub span: Span,
    pub inferred_ty: Ty,
}

/// Reflects [`ty::AdtKind`]

#[derive(Copy, Clone, Debug)]
pub enum AdtKind {
    Struct,
    Union,
    Enum,
    /// We sometimes pretend arrays are an ADT and generate a `FullDef` for them.
    Array,
    /// We sometimes pretend slices are an ADT and generate a `FullDef` for them.
    Slice,
    /// We sometimes pretend tuples are an ADT and generate a `FullDef` for them.
    Tuple,
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, AdtKind> for ty::AdtKind {
    fn sinto(&self, _s: &S) -> AdtKind {
        match self {
            ty::AdtKind::Struct => AdtKind::Struct,
            ty::AdtKind::Union => AdtKind::Union,
            ty::AdtKind::Enum => AdtKind::Enum,
        }
    }
}

sinto_todo!(rustc_middle::ty, AdtFlags);

/// Reflects [`ty::ReprOptions`]

#[derive(AdtInto, Clone, Debug)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: rustc_abi::ReprOptions, state: S as s)]
pub struct ReprOptions {
    /// Whether an explicit integer representation was specified.
    #[value(self.int.is_some())]
    pub int_specified: bool,
    /// The actual discriminant type resulting from the representation options.
    #[value({
        use crate::rustc_middle::ty::util::IntTypeExt;
        self.discr_type().to_ty(s.base().tcx).sinto(s)
    })]
    pub typ: Ty,
    pub align: Option<Align>,
    pub pack: Option<Align>,
    #[value(ReprFlags { is_c: self.c(), is_transparent: self.transparent(), is_simd: self.simd() })]
    pub flags: ReprFlags,
}

/// The representation flags without the ones irrelevant outside of rustc.

#[derive(Default, Clone, Debug)]
pub struct ReprFlags {
    pub is_c: bool,
    pub is_transparent: bool,
    pub is_simd: bool,
}

/// Reflects [`ty::Align`], but directly stores the number of bytes as a u64.

#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: BaseState<'tcx>>, from: rustc_abi::Align, state: S as _s)]
pub struct Align {
    #[value({
        self.bytes()
    })]
    pub bytes: u64,
}

/// The metadata to attach to the newly-unsized ptr.
#[derive(Clone, Debug)]
pub enum UnsizingMetadata {
    /// Unsize an array to a slice, storing the length as metadata.
    Length(ConstantExpr),
    /// Unsize a non-dyn type to a dyn type, adding a vtable pointer as metadata.
    DirectVTable(ImplExpr),
    /// Unsize a dyn-type to another dyn-type, (optionally) indexing within the current vtable.
    NestedVTable(DynBinder<ImplExpr>),
    /// Couldn't compute
    Unknown,
}

pub fn compute_unsizing_metadata<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    src_ty: ty::Ty<'tcx>,
    tgt_ty: ty::Ty<'tcx>,
) -> UnsizingMetadata {
    // TODO: to properly find out what field we want, we should use the query
    // `coerce_unsized_info`, which we call recursively to get the list of fields
    // to go into until we reach a pointer/reference.
    // We should also pass this list of field IDs in the unsizing metadata.

    let (Some(src_ty), Some(tgt_ty)) = (src_ty.builtin_deref(true), tgt_ty.builtin_deref(true))
    else {
        return UnsizingMetadata::Unknown;
    };

    let tcx = s.base().tcx;
    let typing_env = s.typing_env();
    let (src_ty, tgt_ty) =
        tcx.struct_lockstep_tails_raw(src_ty, tgt_ty, |ty| normalize(tcx, typing_env, ty));

    match (&src_ty.kind(), &tgt_ty.kind()) {
        (ty::Array(_, len), ty::Slice(_)) => {
            let len = len.sinto(s);
            UnsizingMetadata::Length(len)
        }
        (ty::Dynamic(from_preds, _), ty::Dynamic(to_preds, ..)) => {
            let impl_expr = resolve_for_dyn(s, from_preds, |searcher, fresh_ty| {
                let to_pred = to_preds.principal().unwrap().with_self_ty(tcx, fresh_ty);
                searcher.resolve(&to_pred, &|_| {}).s_unwrap(s).sinto(s)
            });
            UnsizingMetadata::NestedVTable(impl_expr)
        }
        (_, ty::Dynamic(preds, ..)) => {
            let pred = preds[0].with_self_ty(tcx, src_ty);
            let clause = pred.as_trait_clause().expect(
                "the first `ExistentialPredicate` of `TyKind::Dynamic` \\
                                        should be a trait clause",
            );
            let tref = clause.rebind(clause.skip_binder().trait_ref);
            let impl_expr = solve_trait(s, tref);

            UnsizingMetadata::DirectVTable(impl_expr)
        }
        _ => UnsizingMetadata::Unknown,
    }
}

/// Reflects [`rustc_abi::ExternAbi`]
#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: BaseState<'tcx>>, from: rustc_abi::ExternAbi, state: S as s)]
pub enum ExternAbi {
    Rust,
    C {
        unwind: bool,
    },
    #[todo]
    Other(String),
}

/// Reflects [`ty::FnSig`]
#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::FnSig<'tcx>, state: S as s)]
pub struct TyFnSig {
    #[value(self.inputs().sinto(s))]
    pub inputs: Vec<Ty>,
    #[value(self.output().sinto(s))]
    pub output: Ty,
    pub c_variadic: bool,
    pub safety: Safety,
    pub abi: ExternAbi,
}

/// Reflects [`ty::PolyFnSig`]
pub type PolyFnSig = Binder<TyFnSig>;

/// Reflects [`ty::TraitRef`]
/// Contains the def_id and arguments passed to the trait. The first type argument is the `Self`
/// type. The `ImplExprs` are the _required_ predicate for this trait; currently they are always
/// empty because we consider all trait predicates as implied.
/// `self.in_trait` is always `None` because a trait can't be associated to another one.
pub type TraitRef = ItemRef;

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, TraitRef> for ty::TraitRef<'tcx> {
    fn sinto(&self, s: &S) -> TraitRef {
        translate_item_ref(s, self.def_id, self.args)
    }
}

/// Reflects [`ty::TraitPredicate`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::TraitPredicate<'tcx>, state: S as tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TraitPredicate {
    pub trait_ref: TraitRef,
    #[map(*x == ty::PredicatePolarity::Positive)]
    #[from(polarity)]
    pub is_positive: bool,
}

/// Reflects [`ty::OutlivesPredicate`] as a named struct
/// instead of a tuple struct. This is because the script converting
/// JSONSchema types to OCaml doesn't support tuple structs, and this
/// is the only tuple struct in the whole AST.

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct OutlivesPredicate<T> {
    pub lhs: T,
    pub rhs: Region,
}

impl<'tcx, S: UnderOwnerState<'tcx>, T, U> SInto<S, OutlivesPredicate<U>>
    for ty::OutlivesPredicate<'tcx, T>
where
    T: SInto<S, U>,
{
    fn sinto(&self, s: &S) -> OutlivesPredicate<U> where {
        OutlivesPredicate {
            lhs: self.0.sinto(s),
            rhs: self.1.sinto(s),
        }
    }
}

/// Reflects [`ty::RegionOutlivesPredicate`]
pub type RegionOutlivesPredicate = OutlivesPredicate<Region>;
/// Reflects [`ty::TypeOutlivesPredicate`]
pub type TypeOutlivesPredicate = OutlivesPredicate<Ty>;

/// Reflects [`ty::Term`]

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Term {
    Ty(Ty),
    Const(ConstantExpr),
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, Term> for ty::Term<'tcx> {
    fn sinto(&self, s: &S) -> Term {
        use ty::TermKind;
        match self.kind() {
            TermKind::Ty(ty) => Term::Ty(ty.sinto(s)),
            TermKind::Const(c) => Term::Const(c.sinto(s)),
        }
    }
}

/// Expresses a constraints over an associated type.
///
/// For instance:
/// ```text
/// fn f<T : Foo<S = String>>(...)
///              ^^^^^^^^^^
/// ```
/// (provided the trait `Foo` has an associated type `S`).

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ProjectionPredicate {
    /// The `impl Trait for Ty` in `Ty: Trait<..., Type = U>`.
    pub impl_expr: ImplExpr,
    /// The `Type` in `Ty: Trait<..., Type = U>`.
    pub assoc_item: AssocItem,
    /// The type `U` in `Ty: Trait<..., Type = U>`.
    pub ty: Ty,
}

impl<'tcx, S: UnderBinderState<'tcx>> SInto<S, ProjectionPredicate>
    for ty::ProjectionPredicate<'tcx>
{
    fn sinto(&self, s: &S) -> ProjectionPredicate {
        let tcx = s.base().tcx;
        let alias_ty = &self.projection_term.expect_ty(tcx);
        let poly_trait_ref = s.binder().rebind(alias_ty.trait_ref(tcx));
        let Term::Ty(ty) = self.term.sinto(s) else {
            unreachable!()
        };
        let item = tcx.associated_item(alias_ty.def_id);
        ProjectionPredicate {
            impl_expr: solve_trait(s, poly_trait_ref),
            assoc_item: AssocItem::sfrom(s, &item),
            ty,
        }
    }
}

/// Reflects [`ty::ClauseKind`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderBinderState<'tcx>>, from: ty::ClauseKind<'tcx>, state: S as tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ClauseKind {
    Trait(TraitPredicate),
    RegionOutlives(RegionOutlivesPredicate),
    TypeOutlives(TypeOutlivesPredicate),
    Projection(ProjectionPredicate),
    ConstArgHasType(ConstantExpr, Ty),
    WellFormed(Term),
    ConstEvaluatable(ConstantExpr),
    HostEffect(HostEffectPredicate),
    UnstableFeature(Symbol),
}

sinto_todo!(rustc_middle::ty, HostEffectPredicate<'tcx>);

/// Reflects [`ty::Clause`] and adds a hash-consed predicate identifier.

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Clause {
    pub kind: Binder<ClauseKind>,
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, Clause> for ty::Clause<'tcx> {
    fn sinto(&self, s: &S) -> Clause {
        let kind = self.kind().sinto(s);
        Clause { kind }
    }
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, Clause> for ty::PolyTraitPredicate<'tcx> {
    fn sinto(&self, s: &S) -> Clause {
        let kind: Binder<_> = self.sinto(s);
        let kind: Binder<ClauseKind> = kind.map(ClauseKind::Trait);
        Clause { kind }
    }
}

/// Reflects [`ty::Predicate`] and adds a hash-consed predicate identifier.

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Predicate {
    pub kind: Binder<PredicateKind>,
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, Predicate> for ty::Predicate<'tcx> {
    fn sinto(&self, s: &S) -> Predicate {
        let kind = self.kind().sinto(s);
        Predicate { kind }
    }
}

/// Reflects [`ty::BoundVariableKind`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::BoundVariableKind<'tcx>, state: S as tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BoundVariableKind {
    Ty(BoundTyKind),
    Region(BoundRegionKind),
    Const,
}

/// Reflects [`ty::Binder`]

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Binder<T> {
    pub value: T,
    pub bound_vars: Vec<BoundVariableKind>,
}

impl<T> Binder<T> {
    pub fn as_ref(&self) -> Binder<&T> {
        Binder {
            value: &self.value,
            bound_vars: self.bound_vars.clone(),
        }
    }

    pub fn hax_skip_binder(self) -> T {
        self.value
    }

    pub fn hax_skip_binder_ref(&self) -> &T {
        &self.value
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Binder<U> {
        Binder {
            value: f(self.value),
            bound_vars: self.bound_vars,
        }
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.value
    }

    pub fn rebind<U>(&self, value: U) -> Binder<U> {
        self.as_ref().map(|_| value)
    }
}

/// Uniquely identifies a predicate.
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: traits::ItemPredicateId, state: S as s)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum GenericPredicateId {
    /// A predicate that counts as "input" for an item, e.g. `where` clauses on a function or impl.
    /// Numbered in some arbitrary but consistent order.
    Required(DefId, u32),
    /// A predicate that counts as "output" of an item, e.g. supertrait clauses in a trait. Note
    /// that we count `where` clauses on a trait as implied.
    /// Numbered in some arbitrary but consistent order.
    Implied(DefId, u32),
    /// Predicate inside a non-item binder, e.g. within a `dyn Trait`.
    /// Numbered in some arbitrary but consistent order.
    Unmapped(u32),
    /// The special `Self: Trait` clause available within trait `Trait`.
    TraitSelf,
}

#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: traits::ItemPredicate<'tcx>, state: S as s)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GenericPredicate {
    pub id: GenericPredicateId,
    pub clause: Clause,
    pub span: Span,
}

/// Reflects [`ty::GenericPredicates`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: traits::ItemPredicates<'tcx>, state: S as s)]
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct GenericPredicates {
    pub predicates: Vec<GenericPredicate>,
}

impl GenericPredicates {
    pub fn iter(&self) -> impl Iterator<Item = &GenericPredicate> {
        self.predicates.iter()
    }
    /// Iter only on trait clauses.
    pub fn iter_trait_clauses(&self) -> impl Iterator<Item = &GenericPredicate> {
        self.iter()
            .filter(|pred| matches!(pred.clause.kind.hax_skip_binder_ref(), ClauseKind::Trait(_)))
    }
}

impl<'tcx, S: UnderOwnerState<'tcx>, T1, T2> SInto<S, Binder<T2>> for ty::Binder<'tcx, T1>
where
    T1: SInto<StateWithBinder<'tcx>, T2>,
{
    fn sinto(&self, s: &S) -> Binder<T2> {
        let bound_vars = self.bound_vars().sinto(s);
        let value = {
            let under_binder_s = &s.with_binder(self.as_ref().map_bound(|_| ()));
            self.as_ref().skip_binder().sinto(under_binder_s)
        };
        Binder { value, bound_vars }
    }
}

/// Reflects [`ty::SubtypePredicate`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::SubtypePredicate<'tcx>, state: S as tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SubtypePredicate {
    pub a_is_expected: bool,
    pub a: Ty,
    pub b: Ty,
}

/// Reflects [`ty::CoercePredicate`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::CoercePredicate<'tcx>, state: S as tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CoercePredicate {
    pub a: Ty,
    pub b: Ty,
}

/// Reflects [`ty::AliasRelationDirection`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::AliasRelationDirection, state: S as _tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AliasRelationDirection {
    Equate,
    Subtype,
}

/// Reflects [`ty::ClosureArgs`]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]

pub struct ClosureArgs {
    pub item: ItemRef,
    /// The base kind of this closure. The kinds are ordered by inclusion: any `Fn` works as an
    /// `FnMut`, and any `FnMut` works as an `FnOnce`.
    pub kind: ClosureKind,
    /// The signature of the function that the closure implements, e.g. `fn(A, B, C) -> D`.
    pub fn_sig: PolyFnSig,
    /// The set of captured variables. Together they form the state of the closure.
    pub upvar_tys: Vec<Ty>,
}

impl ClosureArgs {
    /// Iterate over the upvars that are borrows with erased regions. These may require allocating
    /// fresh regions.
    pub fn iter_upvar_borrows(&self) -> impl Iterator<Item = &Ty> {
        self.upvar_tys.iter().filter(|ty| {
            matches!(
                ty.kind(),
                TyKind::Ref(
                    Region {
                        kind: RegionKind::ReErased
                    },
                    ..
                )
            )
        })
    }
}

impl ClosureArgs {
    // Manual implementation because we need the `def_id` of the closure.
    pub fn sfrom<'tcx, S>(s: &S, def_id: RDefId, from: ty::GenericArgsRef<'tcx>) -> Self
    where
        S: UnderOwnerState<'tcx>,
    {
        use rustc_middle::ty;
        use rustc_type_ir::TypeFoldable;
        use rustc_type_ir::TypeSuperFoldable;

        struct RegionUnEraserVisitor<'tcx> {
            tcx: ty::TyCtxt<'tcx>,
            depth: u32,
            bound_vars: Vec<ty::BoundVariableKind<'tcx>>,
        }

        impl<'tcx> ty::TypeFolder<ty::TyCtxt<'tcx>> for RegionUnEraserVisitor<'tcx> {
            fn cx(&self) -> ty::TyCtxt<'tcx> {
                self.tcx
            }

            fn fold_ty(&mut self, ty: ty::Ty<'tcx>) -> ty::Ty<'tcx> {
                ty.super_fold_with(self)
            }

            fn fold_binder<T>(&mut self, t: ty::Binder<'tcx, T>) -> ty::Binder<'tcx, T>
            where
                T: ty::TypeFoldable<ty::TyCtxt<'tcx>>,
            {
                self.depth += 1;
                let t = t.super_fold_with(self);
                self.depth -= 1;
                t
            }

            fn fold_region(&mut self, r: ty::Region<'tcx>) -> ty::Region<'tcx> {
                // Replace erased regions with fresh bound regions.
                if r.is_erased() {
                    let bound_region = ty::BoundRegion {
                        var: ty::BoundVar::from_usize(self.bound_vars.len()),
                        kind: ty::BoundRegionKind::Anon,
                    };
                    self.bound_vars
                        .push(ty::BoundVariableKind::Region(bound_region.kind));
                    ty::Region::new_bound(
                        self.tcx,
                        ty::DebruijnIndex::from(self.depth),
                        bound_region,
                    )
                } else {
                    r
                }
            }
        }

        let tcx = s.base().tcx;
        let closure = from.as_closure();
        let item = {
            // The closure has no generics of its own: it inherits its parent generics and could
            // have late-bound args but these are part of the signature.
            let parent_args = tcx.mk_args(closure.parent_args());
            translate_item_ref(s, def_id, parent_args)
        };
        let sig = closure.sig();
        let sig = tcx.signature_unclosure(sig, rustc_hir::Safety::Safe);
        // Add bound variables for each erased region in the signature.
        let sig = {
            let mut visitor = RegionUnEraserVisitor {
                tcx,
                depth: 0,
                bound_vars: sig.bound_vars().iter().collect(),
            };
            let unbound_sig = sig.skip_binder().fold_with(&mut visitor);
            let bound_vars = tcx.mk_bound_variable_kinds(&visitor.bound_vars);
            ty::Binder::bind_with_vars(unbound_sig, bound_vars)
        };
        ClosureArgs {
            item,
            kind: closure.kind().sinto(s),
            fn_sig: sig.sinto(s),
            upvar_tys: closure.upvar_tys().sinto(s),
        }
    }
}

/// Reflects [`ty::ClosureKind`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: ty::ClosureKind, state: S as _tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ClosureKind {
    Fn,
    FnMut,
    FnOnce,
}

sinto_todo!(rustc_middle::ty, NormalizesTo<'tcx>);

/// Reflects [`ty::PredicateKind`]
#[derive(AdtInto)]
#[args(<'tcx, S: UnderBinderState<'tcx>>, from: ty::PredicateKind<'tcx>, state: S as tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PredicateKind {
    Clause(ClauseKind),
    DynCompatible(DefId),
    Subtype(SubtypePredicate),
    Coerce(CoercePredicate),
    ConstEquate(ConstantExpr, ConstantExpr),
    Ambiguous,
    AliasRelate(Term, Term, AliasRelationDirection),
    NormalizesTo(NormalizesTo),
}

/// Reflects [`ty::AssocItem`]

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AssocItem {
    pub def_id: DefId,
    /// This is `None` for RPTITs.
    pub name: Option<Symbol>,
    pub kind: AssocKind,
    pub container: AssocItemContainer,
    /// Whether this item has a value (e.g. this is `false` for trait methods without default
    /// implementations).
    pub has_value: bool,
}

impl AssocItem {
    pub fn sfrom<'tcx, S: BaseState<'tcx>>(s: &S, item: &ty::AssocItem) -> AssocItem {
        Self::sfrom_instantiated(s, item, None)
    }

    /// Translate an `AssocItem` and optionally instantiate it with the provided arguments.
    pub fn sfrom_instantiated<'tcx, S: BaseState<'tcx>>(
        s: &S,
        item: &ty::AssocItem,
        item_args: Option<ty::GenericArgsRef<'tcx>>,
    ) -> AssocItem {
        let tcx = s.base().tcx;
        // We want to solve traits in the context of this item.
        let s = &s.with_rustc_owner(item.def_id);
        let item_args =
            item_args.unwrap_or_else(|| ty::GenericArgs::identity_for_item(tcx, item.def_id));
        let container_id = item.container_id(tcx);
        let container_args = item_args.truncate_to(tcx, tcx.generics_of(container_id));
        let container = match item.container {
            ty::AssocContainer::Trait => {
                let trait_ref =
                    ty::TraitRef::new_from_args(tcx, container_id, container_args).sinto(s);
                AssocItemContainer::TraitContainer { trait_ref }
            }
            ty::AssocContainer::TraitImpl(implemented_item_id) => {
                let implemented_item_id = implemented_item_id.unwrap();
                let item = translate_item_ref(s, container_id, container_args);
                let implemented_trait_ref = tcx
                    .impl_trait_ref(container_id)
                    .instantiate(tcx, container_args);
                let implemented_trait_item = translate_item_ref(
                    s,
                    implemented_item_id,
                    item_args.rebase_onto(tcx, container_id, implemented_trait_ref.args),
                );
                AssocItemContainer::TraitImplContainer {
                    impl_: item,
                    implemented_trait_ref: implemented_trait_ref.sinto(s),
                    implemented_trait_item,
                    overrides_default: tcx.defaultness(implemented_item_id).has_value(),
                }
            }
            ty::AssocContainer::InherentImpl => AssocItemContainer::InherentImplContainer {
                impl_id: container_id.sinto(s),
            },
        };
        let name = match item.opt_name() {
            None if let ty::AssocKind::Type { data } = item.kind
                && let ty::AssocTypeData::Rpitit(rpitit) = data =>
            {
                let (ty::ImplTraitInTraitData::Trait { fn_def_id, .. }
                | ty::ImplTraitInTraitData::Impl { fn_def_id, .. }) = rpitit;
                let fn_name = tcx.item_name(fn_def_id);
                let name = Symbol::intern(&format!("{fn_name}_ty"));
                Some(name)
            }
            opt_name => opt_name,
        };
        AssocItem {
            def_id: item.def_id.sinto(s),
            name,
            kind: item.kind.sinto(s),
            container,
            has_value: item.defaultness(tcx).has_value(),
        }
    }
}

/// Reflects [`ty::AssocKind`]
#[derive(AdtInto)]
#[args(<'tcx, S: BaseState<'tcx>>, from: ty::AssocKind, state: S as _tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AssocKind {
    Const { name: Symbol },
    Fn { name: Symbol, has_self: bool },
    Type { data: AssocTypeData },
}

/// Reflects [`ty::AssocTypeData`]
#[derive(AdtInto)]
#[args(<'tcx, S: BaseState<'tcx>>, from: ty::AssocTypeData, state: S as _tcx)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AssocTypeData {
    Normal(Symbol),
    Rpitit(ImplTraitInTraitData),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AssocItemContainer {
    TraitContainer {
        trait_ref: TraitRef,
    },
    TraitImplContainer {
        /// Reference to the def_id of the impl block.
        impl_: ItemRef,
        /// The trait ref implemented by the impl block.
        implemented_trait_ref: TraitRef,
        /// The the associated item (in the trait declaration) that is being implemented.
        implemented_trait_item: ItemRef,
        /// Whether the corresponding trait item had a default (and therefore this one overrides
        /// it).
        overrides_default: bool,
    },
    InherentImplContainer {
        impl_id: DefId,
    },
}

/// Reflects [`ty::ImplTraitInTraitData`]
#[derive(AdtInto)]
#[args(<'tcx, S: BaseState<'tcx>>, from: ty::ImplTraitInTraitData, state: S as _s)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImplTraitInTraitData {
    Trait {
        fn_def_id: DefId,
        opaque_def_id: DefId,
    },
    Impl {
        fn_def_id: DefId,
    },
}
