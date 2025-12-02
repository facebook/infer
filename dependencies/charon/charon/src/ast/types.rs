use crate::ids::Vector;
use crate::{ast::*, common::hash_consing::HashConsed};
use derive_generic_visitor::*;
use macros::{EnumAsGetters, EnumIsA, EnumToGetters, VariantIndexArity, VariantName};
use serde::{Deserialize, Serialize};

mod vars;
pub use vars::*;

#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    Hash,
    PartialOrd,
    Ord,
    EnumIsA,
    EnumAsGetters,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("R")]
pub enum Region {
    /// Region variable. See `DeBruijnVar` for details.
    Var(RegionDbVar),
    /// Static region
    Static,
    /// Erased region
    Erased,
}

/// Identifier of a trait instance.
/// This is derived from the trait resolution.
///
/// Should be read as a path inside the trait clauses which apply to the current
/// definition. Note that every path designated by `TraitInstanceId` refers
/// to a *trait instance*, which is why the [`TraitRefKind::Clause`] variant may seem redundant
/// with some of the other variants.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Drive, DriveMut)]
#[charon::rename("TraitInstanceId")]
pub enum TraitRefKind {
    /// A specific top-level implementation item.
    TraitImpl(TraitImplRef),

    /// One of the local clauses.
    ///
    /// Example:
    /// ```text
    /// fn f<T>(...) where T : Foo
    ///                    ^^^^^^^
    ///                    Clause(0)
    /// ```
    Clause(ClauseDbVar),

    /// A parent clause
    ///
    /// Example:
    /// ```text
    /// trait Foo1 {}
    /// trait Foo2 { fn f(); }
    ///
    /// trait Bar : Foo1 + Foo2 {}
    ///             ^^^^   ^^^^
    ///                    parent clause 1
    ///     parent clause 0
    ///
    /// fn g<T : Bar>(x : T) {
    ///   x.f()
    ///   ^^^^^
    ///   Parent(Clause(0), 1)::f(x)
    ///                     ^
    ///                     parent clause 1 of clause 0
    /// }
    /// ```
    ParentClause(Box<TraitRef>, TraitClauseId),

    /// A clause defined on an associated type. This variant is only used during translation; after
    /// the `lift_associated_item_clauses` pass, clauses on items become `ParentClause`s.
    ///
    /// Example:
    /// ```text
    /// trait Foo {
    ///   type W: Bar0 + Bar1 // Bar1 contains a method bar1
    ///                  ^^^^
    ///               this is the clause 1 applying to W
    /// }
    ///
    /// fn f<T : Foo>(x : T::W) {
    ///   x.bar1();
    ///   ^^^^^^^
    ///   ItemClause(Clause(0), W, 1)
    ///                         ^^^^
    ///                         clause 1 from item W (from local clause 0)
    /// }
    /// ```
    #[charon::opaque]
    ItemClause(Box<TraitRef>, TraitItemName, TraitClauseId),

    /// The implicit `Self: Trait` clause. Present inside trait declarations, including trait
    /// method declarations. Not present in trait implementations as we can use `TraitImpl` intead.
    #[charon::rename("Self")]
    SelfId,

    /// A trait implementation that is computed by the compiler, such as for built-in trait
    /// `Sized`. This morally points to an invisible `impl` block; as such it contains
    /// the information we may need from one.
    BuiltinOrAuto {
        trait_decl_ref: PolyTraitDeclRef,
        /// Exactly like the same field on `TraitImpl`: the `TraitRef`s required to satisfy the
        /// implied predicates on the trait declaration. E.g. since `FnMut: FnOnce`, a built-in `T:
        /// FnMut` impl would have a `TraitRef` for `T: FnOnce`.
        parent_trait_refs: Vector<TraitClauseId, TraitRef>,
        /// The values of the associated types for this trait.
        types: Vec<(TraitItemName, Ty, Vector<TraitClauseId, TraitRef>)>,
    },

    /// The automatically-generated implementation for `dyn Trait`.
    Dyn(PolyTraitDeclRef),

    /// For error reporting.
    #[charon::rename("UnknownTrait")]
    #[drive(skip)]
    Unknown(String),
}

/// A reference to a trait
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct TraitRef {
    #[charon::rename("trait_id")]
    pub kind: TraitRefKind,
    /// Not necessary, but useful
    pub trait_decl_ref: PolyTraitDeclRef,
}

/// A predicate of the form `Type: Trait<Args>`.
///
/// About the generics, if we write:
/// ```text
/// impl Foo<bool> for String { ... }
/// ```
///
/// The substitution is: `[String, bool]`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct TraitDeclRef {
    pub id: TraitDeclId,
    pub generics: BoxedArgs,
}

/// A quantified trait predicate, e.g. `for<'a> Type<'a>: Trait<'a, Args>`.
pub type PolyTraitDeclRef = RegionBinder<TraitDeclRef>;

/// A reference to a tait impl, using the provided arguments.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct TraitImplRef {
    pub id: TraitImplId,
    pub generics: BoxedArgs,
}

/// .0 outlives .1
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct OutlivesPred<T, U>(pub T, pub U);

pub type RegionOutlives = OutlivesPred<Region, Region>;
pub type TypeOutlives = OutlivesPred<Ty, Region>;

/// A constraint over a trait associated type.
///
/// Example:
/// ```text
/// T : Foo<S = String>
///         ^^^^^^^^^^
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct TraitTypeConstraint {
    pub trait_ref: TraitRef,
    pub type_name: TraitItemName,
    pub ty: Ty,
}

/// A set of generic arguments.
#[derive(Clone, Eq, PartialEq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct GenericArgs {
    pub regions: Vector<RegionId, Region>,
    pub types: Vector<TypeVarId, Ty>,
    pub const_generics: Vector<ConstGenericVarId, ConstGeneric>,
    // TODO: rename to match [GenericParams]?
    pub trait_refs: Vector<TraitClauseId, TraitRef>,
}

pub type BoxedArgs = Box<GenericArgs>;

/// A value of type `T` bound by regions. We should use `binder` instead but this causes name clash
/// issues in the derived ocaml visitors.
/// TODO: merge with `binder`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct RegionBinder<T> {
    #[charon::rename("binder_regions")]
    pub regions: Vector<RegionId, RegionVar>,
    /// Named this way to highlight accesses to the inner value that might be handling parameters
    /// incorrectly. Prefer using helper methods.
    #[charon::rename("binder_value")]
    pub skip_binder: T,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
#[charon::variants_prefix("BK")]
pub enum BinderKind {
    /// The parameters of a trait method. Used in the `methods` lists in trait decls and trait
    /// impls.
    TraitMethod(TraitDeclId, TraitItemName),
    /// The parameters bound in a non-trait `impl` block. Used in the `Name`s of inherent methods.
    InherentImplBlock,
    /// Binder used for `dyn Trait` existential predicates.
    Dyn,
    /// Some other use of a binder outside the main Charon ast.
    Other,
}

/// A value of type `T` bound by generic parameters. Used in any context where we're adding generic
/// parameters that aren't on the top-level item, e.g. `for<'a>` clauses (uses `RegionBinder` for
/// now), trait methods, GATs (TODO).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct Binder<T> {
    #[charon::rename("binder_params")]
    pub params: GenericParams,
    /// Named this way to highlight accesses to the inner value that might be handling parameters
    /// incorrectly. Prefer using helper methods.
    #[charon::rename("binder_value")]
    pub skip_binder: T,
    /// The kind of binder this is.
    #[charon::opaque]
    #[drive(skip)]
    pub kind: BinderKind,
}

/// Generic parameters for a declaration.
/// We group the generics which come from the Rust compiler substitutions
/// (the regions, types and const generics) as well as the trait clauses.
/// The reason is that we consider that those are parameters that need to
/// be filled. We group in a different place the predicates which are not
/// trait clauses, because those enforce constraints but do not need to
/// be filled with witnesses/instances.
#[derive(Default, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct GenericParams {
    pub regions: Vector<RegionId, RegionVar>,
    pub types: Vector<TypeVarId, TypeVar>,
    pub const_generics: Vector<ConstGenericVarId, ConstGenericVar>,
    // TODO: rename to match [GenericArgs]?
    pub trait_clauses: Vector<TraitClauseId, TraitClause>,
    /// The first region in the pair outlives the second region
    pub regions_outlive: Vec<RegionBinder<RegionOutlives>>,
    /// The type outlives the region
    pub types_outlive: Vec<RegionBinder<TypeOutlives>>,
    /// Constraints over trait associated types
    pub trait_type_constraints: Vector<TraitTypeConstraintId, RegionBinder<TraitTypeConstraint>>,
}

/// Where a given predicate came from.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub enum PredicateOrigin {
    // Note: we use this for globals too, but that's only available with an unstable feature.
    // ```
    // fn function<T: Clone>() {}
    // fn function<T>() where T: Clone {}
    // const NONE<T: Copy>: Option<T> = None;
    // ```
    WhereClauseOnFn,
    // ```
    // struct Struct<T: Clone> {}
    // struct Struct<T> where T: Clone {}
    // type TypeAlias<T: Clone> = ...;
    // ```
    WhereClauseOnType,
    // Note: this is both trait impls and inherent impl blocks.
    // ```
    // impl<T: Clone> Type<T> {}
    // impl<T> Type<T> where T: Clone {}
    // impl<T> Trait for Type<T> where T: Clone {}
    // ```
    WhereClauseOnImpl,
    // The special `Self: Trait` clause which is in scope inside the definition of `Foo` or an
    // implementation of it.
    // ```
    // trait Trait {}
    // ```
    TraitSelf,
    // Note: this also includes supertrait constraints.
    // ```
    // trait Trait<T: Clone> {}
    // trait Trait<T> where T: Clone {}
    // trait Trait: Clone {}
    // ```
    WhereClauseOnTrait,
    // ```
    // trait Trait {
    //     type AssocType: Clone;
    // }
    // ```
    TraitItem(TraitItemName),
    /// Clauses that are part of a `dyn Trait` type.
    Dyn,
}

// rustc counts bytes in layouts as u64
pub type ByteCount = u64;

/// Simplified layout of a single variant.
///
/// Maps fields to their offset within the layout.
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
pub struct VariantLayout {
    /// The offset of each field.
    #[drive(skip)]
    pub field_offsets: Vector<FieldId, ByteCount>,
    /// Whether the variant is uninhabited, i.e. has any valid possible value.
    /// Note that uninhabited types can have arbitrary layouts.
    #[drive(skip)]
    pub uninhabited: bool,
    /// The memory representation of the discriminant corresponding to this
    /// variant. It must be of the same type as the corresponding [`DiscriminantLayout::tag_ty`].
    ///
    /// If it's `None`, then this variant is either:
    /// - the untagged variant (cf. [`TagEncoding::Niche::untagged_variant`]) of a niched enum;
    /// - the single variant of a struct;
    /// - uninhabited.
    #[drive(skip)]
    pub tag: Option<ScalarValue>,
}

/// Describes how we represent the active enum variant in memory.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TagEncoding {
    /// Represents the direct encoding of the discriminant as the tag via integer casts.
    Direct,
    /// Represents the encoding of the discriminant in the niche of variant `untagged_variant`.
    Niche { untagged_variant: VariantId },
}

/// Layout of the discriminant.
/// Describes the offset of the discriminant field as well as its encoding
/// as `tag` in memory.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DiscriminantLayout {
    /// The offset of the discriminant in bytes.
    pub offset: ByteCount,
    /// The representation type of the discriminant.
    pub tag_ty: IntegerTy,
    /// How the tag is encoding in memory.
    pub encoding: TagEncoding,
    // FIXME: Should probably contain the valid range of the tag, too.
}

/// Simplified type layout information.
///
/// Does not include information about niches.
/// If the type does not have a fully known layout (e.g. it is ?Sized)
/// some of the layout parts are not available.
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
pub struct Layout {
    /// The size of the type in bytes.
    #[drive(skip)]
    pub size: Option<ByteCount>,
    /// The alignment, in bytes.
    #[drive(skip)]
    pub align: Option<ByteCount>,
    /// The discriminant's layout, if any. Only relevant for types with multiple variants.
    ///
    #[drive(skip)]
    pub discriminant_layout: Option<DiscriminantLayout>,
    /// Whether the type is uninhabited, i.e. has any valid value at all.
    /// Note that uninhabited types can have arbitrary layouts: `(u32, !)` has space for the `u32`
    /// and `enum E2 { A, B(!), C(i32, !) }` may have space for a discriminant.
    #[drive(skip)]
    pub uninhabited: bool,
    /// Map from `VariantId` to the corresponding field layouts. Structs are modeled as having
    /// exactly one variant, unions as having no variant.
    pub variant_layouts: Vector<VariantId, VariantLayout>,
}

/// A placeholder for the vtable of a trait object.
/// To be implemented in the future when `dyn Trait` is fully supported.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
pub struct VTable;

/// The metadata stored in a pointer. That's the information stored in pointers alongside
/// their address. It's empty for `Sized` types, and interesting for unsized
/// aka dynamically-sized types.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
pub enum PtrMetadata {
    /// Types that need no metadata, namely `T: Sized` types.
    #[charon::rename("NoMetadata")]
    None,
    /// Metadata for `[T]`, `str`, and user-defined types
    /// that directly or indirectly contain one of these two.
    Length,
    /// Metadata for `dyn Trait` and user-defined types
    /// that directly or indirectly contain a `dyn Trait`.
    VTable(VTable),
}

/// A type declaration.
///
/// Types can be opaque or transparent.
///
/// Transparent types are local types not marked as opaque.
/// Opaque types are the others: local types marked as opaque, and non-local
/// types (coming from external dependencies).
///
/// In case the type is transparent, the declaration also contains the
/// type definition (see [TypeDeclKind]).
///
/// A type can only be an ADT (structure or enumeration), as type aliases are
/// inlined in MIR.
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct TypeDecl {
    #[drive(skip)]
    pub def_id: TypeDeclId,
    /// Meta information associated with the item.
    pub item_meta: ItemMeta,
    pub generics: GenericParams,
    /// The context of the type: distinguishes top-level items from closure-related items.
    pub src: ItemKind,
    /// The type kind: enum, struct, or opaque.
    pub kind: TypeDeclKind,
    /// The layout of the type. Information may be partial because of generics or dynamically-
    /// sized types. If rustc cannot compute a layout, it is `None`.
    pub layout: Option<Layout>,
    /// The metadata associated with a pointer to the type.
    /// This is `None` if we could not compute it because of generics.
    /// The information is *accurate* if it is `Some`
    ///     while if it is `None`, it may still be theoretically computable
    ///     but due to some limitation to be fixed, we are unable to obtain the info.
    /// See `translate_types::{impl ItemTransCtx}::translate_ptr_metadata` for more details.
    pub ptr_metadata: Option<PtrMetadata>,
}

generate_index_type!(VariantId, "Variant");
generate_index_type!(FieldId, "Field");

#[derive(Debug, Clone, EnumIsA, EnumAsGetters, Serialize, Deserialize, Drive, DriveMut)]
pub enum TypeDeclKind {
    Struct(Vector<FieldId, Field>),
    Enum(Vector<VariantId, Variant>),
    Union(Vector<FieldId, Field>),
    /// An opaque type.
    ///
    /// Either a local type marked as opaque, or an external type.
    Opaque,
    /// An alias to another type. This only shows up in the top-level list of items, as rustc
    /// inlines uses of type aliases everywhere else.
    Alias(Ty),
    /// Used if an error happened during the extraction, and we don't panic
    /// on error.
    #[charon::rename("TDeclError")]
    #[drive(skip)]
    Error(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct Variant {
    pub span: Span,
    #[drive(skip)]
    pub attr_info: AttrInfo,
    #[charon::rename("variant_name")]
    #[drive(skip)]
    pub name: String,
    pub fields: Vector<FieldId, Field>,
    /// The discriminant value outputted by `std::mem::discriminant` for this variant. This is
    /// different than the discriminant stored in memory (the one controlled by `repr`).
    /// That one is described by [`DiscriminantLayout`] and [`TagEncoding`].
    pub discriminant: ScalarValue,
}

#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct Field {
    pub span: Span,
    #[drive(skip)]
    pub attr_info: AttrInfo,
    #[charon::rename("field_name")]
    #[drive(skip)]
    pub name: Option<String>,
    #[charon::rename("field_ty")]
    pub ty: Ty,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    EnumIsA,
    VariantName,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    Hash,
    Ord,
    PartialOrd,
)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    EnumIsA,
    VariantName,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    Hash,
    Ord,
    PartialOrd,
)]
pub enum UIntTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    EnumIsA,
    VariantName,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    Hash,
    Ord,
    PartialOrd,
)]
#[charon::rename("IntegerType")]
pub enum IntegerTy {
    Signed(IntTy),
    Unsigned(UIntTy),
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    EnumIsA,
    VariantName,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    Hash,
    Ord,
    PartialOrd,
)]
#[charon::rename("FloatType")]
pub enum FloatTy {
    F16,
    F32,
    F64,
    F128,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Hash,
    VariantName,
    EnumIsA,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    Ord,
    PartialOrd,
)]
#[charon::variants_prefix("R")]
pub enum RefKind {
    Mut,
    Shared,
}

/// Type identifier.
///
/// Allows us to factorize the code for built-in types, adts and tuples
#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    VariantName,
    EnumAsGetters,
    EnumIsA,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    Hash,
    Ord,
    PartialOrd,
)]
#[charon::variants_prefix("T")]
pub enum TypeId {
    /// A "regular" ADT type.
    ///
    /// Includes transparent ADTs and opaque ADTs (local ADTs marked as opaque,
    /// and external ADTs).
    #[charon::rename("TAdtId")]
    Adt(TypeDeclId),
    Tuple,
    /// Built-in type. Either a primitive type like array or slice, or a
    /// non-primitive type coming from a standard library
    /// and that we handle like a primitive type. Types falling into this
    /// category include: Box, Vec, Cell...
    /// The Array and Slice types were initially modelled as primitive in
    /// the [Ty] type. We decided to move them to built-in types as it allows
    /// for more uniform treatment throughout the codebase.
    #[charon::rename("TBuiltin")]
    Builtin(BuiltinTy),
}

/// Reference to a type declaration or builtin type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct TypeDeclRef {
    pub id: TypeId,
    pub generics: BoxedArgs,
}

/// Types of primitive values. Either an integer, bool, char
#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    VariantName,
    EnumIsA,
    EnumAsGetters,
    VariantIndexArity,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    Hash,
    Ord,
    PartialOrd,
)]
#[charon::rename("LiteralType")]
#[charon::variants_prefix("T")]
pub enum LiteralTy {
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    Bool,
    Char,
}

/// Const Generic Values. Either a primitive value, or a variable corresponding to a primitve value
#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    VariantName,
    EnumIsA,
    EnumAsGetters,
    VariantIndexArity,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    Hash,
)]
#[charon::variants_prefix("Cg")]
pub enum ConstGeneric {
    /// A global constant
    Global(GlobalDeclId),
    /// A const generic variable
    Var(ConstGenericDbVar),
    /// A concrete value
    Value(Literal),
}

/// A type.
///
/// Warning: the `DriveMut` impls of `Ty` needs to clone and re-hash the modified type to maintain
/// the hash-consing invariant. This is expensive, avoid visiting types mutably when not needed.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Ty(HashConsed<TyKind>);

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
}

impl<'s, V: Visit<'s, TyKind>> Drive<'s, V> for Ty {
    fn drive_inner(&'s self, v: &mut V) -> std::ops::ControlFlow<V::Break> {
        self.0.drive_inner(v)
    }
}
/// This explores the type mutably by cloning and re-hashing afterwards.
impl<'s, V> DriveMut<'s, V> for Ty
where
    for<'a> V: VisitMut<'a, TyKind>,
{
    fn drive_inner_mut(&'s mut self, v: &mut V) -> std::ops::ControlFlow<V::Break> {
        self.0.drive_inner_mut(v)
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    VariantName,
    EnumIsA,
    EnumAsGetters,
    EnumToGetters,
    VariantIndexArity,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("T")]
#[charon::rename("Ty")]
pub enum TyKind {
    /// An ADT.
    /// Note that here ADTs are very general. They can be:
    /// - user-defined ADTs
    /// - tuples (including `unit`, which is a 0-tuple)
    /// - built-in types (includes some primitive types, e.g., arrays or slices)
    /// The information on the nature of the ADT is stored in (`TypeId`)[TypeId].
    /// The last list is used encode const generics, e.g., the size of an array
    ///
    /// Note: this is incorrectly named: this can refer to any valid `TypeDecl` including extern
    /// types.
    Adt(TypeDeclRef),
    #[charon::rename("TVar")]
    TypeVar(TypeDbVar),
    Literal(LiteralTy),
    /// The never type, for computations which don't return. It is sometimes
    /// necessary for intermediate variables. For instance, if we do (coming
    /// from the rust documentation):
    /// ```text
    /// let num: u32 = match get_a_number() {
    ///     Some(num) => num,
    ///     None => break,
    /// };
    /// ```
    /// the second branch will have type `Never`. Also note that `Never`
    /// can be coerced to any type.
    ///
    /// Note that we eliminate the variables which have this type in a micro-pass.
    /// As statements don't have types, this type disappears eventually disappears
    /// from the AST.
    Never,
    // We don't support floating point numbers on purpose (for now)
    /// A borrow
    Ref(Region, Ty, RefKind),
    /// A raw pointer.
    RawPtr(Ty, RefKind),
    /// A trait associated type
    ///
    /// Ex.:
    /// ```text
    /// trait Foo {
    ///   type Bar; // type associated to the trait Foo
    /// }
    /// ```
    TraitType(TraitRef, TraitItemName),
    /// `dyn Trait`
    DynTrait(DynPredicate),
    /// Function pointer type. This is a literal pointer to a region of memory that
    /// contains a callable function.
    /// This is a function signature with limited generics: it only supports lifetime generics, not
    /// other kinds of generics.
    FnPtr(RegionBinder<(Vec<Ty>, Ty)>),
    /// The unique type associated with each function item. Each function item is given
    /// a unique generic type that takes as input the function's early-bound generics. This type
    /// is not generally nameable in Rust; it's a ZST (there's a unique value), and a value of that type
    /// can be cast to a function pointer or passed to functions that expect `FnOnce`/`FnMut`/`Fn` parameters.
    /// There's a binder here because charon function items take both early and late-bound
    /// lifetimes as arguments; given that the type here is polymorpohic in the late-bound
    /// variables (those that could appear in a function pointer type like `for<'a> fn(&'a u32)`),
    /// we need to bind them here.
    FnDef(RegionBinder<FnPtr>),
    /// A type that could not be computed or was incorrect.
    #[drive(skip)]
    Error(String),
}

/// Builtin types identifiers.
///
/// WARNING: for now, all the built-in types are covariant in the generic
/// parameters (if there are). Adding types which don't satisfy this
/// will require to update the code abstracting the signatures (to properly
/// take into account the lifetime constraints).
///
/// TODO: update to not hardcode the types (except `Box` maybe) and be more
/// modular.
/// TODO: move to builtins.rs?
#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    EnumIsA,
    EnumAsGetters,
    VariantName,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    Hash,
    Ord,
    PartialOrd,
)]
#[charon::variants_prefix("T")]
pub enum BuiltinTy {
    /// Boxes are de facto a primitive type.
    Box,
    /// Primitive type
    Array,
    /// Primitive type
    Slice,
    /// Primitive type
    Str,
}

#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
pub enum ClosureKind {
    Fn,
    FnMut,
    FnOnce,
}

impl ClosureKind {
    // pub fn trait_name(self) -> &'static str {}
    pub fn method_name(self) -> &'static str {
        match self {
            ClosureKind::FnOnce => "call_once",
            ClosureKind::FnMut => "call_mut",
            ClosureKind::Fn => "call",
        }
    }
}

/// Additional information for closures.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
pub struct ClosureInfo {
    pub kind: ClosureKind,
    /// The `FnOnce` implementation of this closure -- always exists.
    pub fn_once_impl: RegionBinder<TraitImplRef>,
    /// The `FnMut` implementation of this closure, if any.
    pub fn_mut_impl: Option<RegionBinder<TraitImplRef>>,
    /// The `Fn` implementation of this closure, if any.
    pub fn_impl: Option<RegionBinder<TraitImplRef>>,
    /// The signature of the function that this closure represents.
    pub signature: RegionBinder<(Vec<Ty>, Ty)>,
}

/// A function signature.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
pub struct FunSig {
    /// Is the function unsafe or not
    #[drive(skip)]
    pub is_unsafe: bool,
    pub generics: GenericParams,
    pub inputs: Vec<Ty>,
    pub output: Ty,
}

/// The contents of a `dyn Trait` type.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
pub struct DynPredicate {
    /// This binder binds a single type `T`, which is considered existentially quantified. The
    /// predicates in the binder apply to `T` and represent the `dyn Trait` constraints.
    /// E.g. `dyn Iterator<Item=u32> + Send` is represented as `exists<T: Iterator<Item=u32> + Send> T`.
    ///
    /// Only the first trait clause may have methods. We use the vtable of this trait in the `dyn
    /// Trait` pointer metadata.
    pub binder: Binder<Ty>,
}
