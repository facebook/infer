//! Implements expressions: paths, operands, rvalues, lvalues

use crate::ast::*;
use derive_generic_visitor::{Drive, DriveMut};
use macros::{EnumAsGetters, EnumIsA, EnumToGetters, VariantIndexArity, VariantName};
use serde::{Deserialize, Serialize};
use serde_state::{DeserializeState, SerializeState};
use std::vec::Vec;

#[derive(Debug, PartialEq, Eq, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
#[serde_state(state_implements = HashConsSerializerState)] // Avoid corecursive impls due to perfect derive
pub struct Place {
    pub kind: PlaceKind,
    pub ty: Ty,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    EnumIsA,
    EnumAsGetters,
    EnumToGetters,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("Place")]
pub enum PlaceKind {
    /// A local variable in a function body.
    Local(LocalId),
    /// A subplace of a place.
    Projection(Box<Place>, ProjectionElem),
    /// A global (const or static).
    /// Not present in MIR; introduced in [simplify_constants.rs].
    Global(GlobalDeclRef),
}

/// Note that we don't have the equivalent of "downcasts".
/// Downcasts are actually necessary, for instance when initializing enumeration
/// values: the value is initially `Bottom`, and we need a way of knowing the
/// variant.
/// For example:
/// `((_0 as Right).0: T2) = move _1;`
/// In MIR, downcasts always happen before field projections: in our internal
/// language, we thus merge downcasts and field projections.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    EnumIsA,
    EnumAsGetters,
    EnumToGetters,
    VariantName,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
pub enum ProjectionElem {
    /// Dereference a shared/mutable reference, a box, or a raw pointer.
    Deref,
    /// Projection from ADTs (variants, structures).
    /// We allow projections to be used as left-values and right-values.
    /// We should never have projections to fields of symbolic variants (they
    /// should have been expanded before through a match).
    Field(FieldProjKind, FieldId),
    /// A built-in pointer (a reference, raw pointer, or `Box`) in Rust is always a fat pointer: it
    /// contains an address and metadata for the pointed-to place. This metadata is empty for sized
    /// types, it's the length for slices, and the vtable for `dyn Trait`.
    ///
    /// We consider such pointers to be like a struct with two fields; this represent access to the
    /// metadata "field".
    PtrMetadata,
    /// MIR imposes that the argument to an index projection be a local variable, meaning
    /// that even constant indices into arrays are let-bound as separate variables.
    /// We **eliminate** this variant in a micro-pass for LLBC.
    #[charon::rename("ProjIndex")]
    Index {
        offset: Box<Operand>,
        #[drive(skip)]
        from_end: bool,
    },
    /// Take a subslice of a slice or array. If `from_end` is `true` this is
    /// `slice[from..slice.len() - to]`, otherwise this is `slice[from..to]`.
    /// We **eliminate** this variant in a micro-pass for LLBC.
    Subslice {
        from: Box<Operand>,
        to: Box<Operand>,
        #[drive(skip)]
        from_end: bool,
    },
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    EnumIsA,
    EnumAsGetters,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("Proj")]
pub enum FieldProjKind {
    Adt(TypeDeclId, Option<VariantId>),
    /// If we project from a tuple, the projection kind gives the arity of the tuple.
    #[drive(skip)]
    Tuple(usize),
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    EnumIsA,
    EnumAsGetters,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("B")]
pub enum BorrowKind {
    Shared,
    Mut,
    /// See <https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/enum.MutBorrowKind.html#variant.TwoPhaseBorrow>
    /// and <https://rustc-dev-guide.rust-lang.org/borrow_check/two_phase_borrows.html>
    TwoPhaseMut,
    /// Those are typically introduced when using guards in matches, to make sure guards don't
    /// change the variant of an enum value while me match over it.
    ///
    /// See <https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/enum.FakeBorrowKind.html#variant.Shallow>.
    Shallow,
    /// Data must be immutable but not aliasable. In other words you can't mutate the data but you
    /// can mutate *through it*, e.g. if it points to a `&mut T`. This is only used in closure
    /// captures, e.g.
    /// ```rust,ignore
    /// let mut z = 3;
    /// let x: &mut isize = &mut z;
    /// let y = || *x += 5;
    /// ```
    /// Here the captured variable can't be `&mut &mut x` since the `x` binding is not mutable, yet
    /// we must be able to mutate what it points to.
    ///
    /// See <https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/enum.MutBorrowKind.html#variant.ClosureCapture>.
    UniqueImmutable,
}

/// Unary operation
#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    EnumIsA,
    VariantName,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[charon::rename("Unop")]
pub enum UnOp {
    Not,
    /// This can overflow, for `-i::MIN`.
    #[drive(skip)]
    #[serde_state(stateless)]
    Neg(OverflowMode),
    /// Casts are rvalues in MIR, but we treat them as unops.
    Cast(CastKind),
}

/// Nullary operation
#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    EnumIsA,
    VariantName,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[charon::rename("Nullop")]
pub enum NullOp {
    SizeOf,
    AlignOf,
    OffsetOf(TypeDeclRef, Option<VariantId>, FieldId),
    UbChecks,
    OverflowChecks,
    ContractChecks,
}

/// For all the variants: the first type gives the source type, the second one gives
/// the destination type.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    EnumIsA,
    VariantName,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("Cast")]
pub enum CastKind {
    /// Conversion between types in `{Integer, Bool}`
    /// Remark: for now we don't support conversions with Char.
    Scalar(LiteralTy, LiteralTy),
    RawPtr(Ty, Ty),
    FnPtr(Ty, Ty),
    /// [Unsize coercion](https://doc.rust-lang.org/std/ops/trait.CoerceUnsized.html). This is
    /// either `[T; N]` -> `[T]` or `T: Trait` -> `dyn Trait` coercions, behind a pointer
    /// (reference, `Box`, or other type that implements `CoerceUnsized`).
    ///
    /// The special case of `&[T; N]` -> `&[T]` coercion is caught by `UnOp::ArrayToSlice`.
    Unsize(Ty, Ty, UnsizingMetadata),
    /// Reinterprets the bits of a value of one type as another type, i.e. exactly what
    /// [`std::mem::transmute`] does.
    Transmute(Ty, Ty),
    /// Converts a receiver type with `dyn Trait<...>` to a concrete type `T`, used in vtable method shims.
    /// Valid conversions are references, raw pointers, and (optionally) boxes:
    /// - `&[mut] dyn Trait<...>` -> `&[mut] T`
    /// - `*[mut] dyn Trait<...>` -> `*[mut] T`
    /// - `Box<dyn Trait<...>>` -> `Box<T>` when no `--raw-boxes`
    ///
    /// For possible receivers, see: <https://doc.rust-lang.org/reference/items/traits.html#dyn-compatibility>.
    /// Other receivers, e.g., `Rc` should be unpacked before the cast and re-boxed after.
    /// FIXME(ssyram): but this is not implemented yet, namely, there may still be
    ///     something like `Rc<dyn Trait<...>> -> Rc<T>` in the types.
    Concretize(Ty, Ty),
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    EnumIsA,
    VariantName,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
    Hash,
)]
#[charon::variants_prefix("Meta")]
pub enum UnsizingMetadata {
    /// Cast from `[T; N]` to `[T]`.
    Length(Box<ConstantExpr>),
    /// Cast from a sized value to a `dyn Trait` value. The `TraitRef` is the proof of the `dyn
    /// Trait` predicate; the constant expression is a reference to the vtable `static` value.
    VTable(TraitRef, Box<ConstantExpr>),
    /// Cast from `dyn Trait` to `dyn OtherTrait`. The fields indicate how to retreive the vtable:
    /// it's always either the same we already had, or the vtable for a (possibly nested) supertrait.
    ///
    /// Note that we cheat in one case: when upcasting to a marker trait (e.g. `dyn Trait -> dyn
    /// Sized`), we keep the current vtable.
    VTableUpcast(Vec<FieldId>),
    Unknown,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Serialize, Deserialize)]
#[charon::variants_prefix("O")]
pub enum OverflowMode {
    /// If this operation overflows, it panics. Only exists in debug mode, for instance in
    /// `a + b`, and only if `--reconstruct-fallible-operations` is passed to Charon. Otherwise the
    /// bound check will be explicit.
    Panic,
    /// If this operation overflows, it is UB; for instance in `core::num::unchecked_add`. This can
    /// exists in safe code, but will always be preceded by a bounds check.
    UB,
    /// If this operation overflows, it wraps around for instance in `core::num::wrapping_add`,
    /// or `a + b` in release mode.
    Wrap,
}

/// Binary operations.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    EnumIsA,
    VariantName,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[charon::rename("Binop")]
#[serde_state(stateless)]
pub enum BinOp {
    BitXor,
    BitAnd,
    BitOr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    #[drive(skip)]
    Add(OverflowMode),
    #[drive(skip)]
    Sub(OverflowMode),
    #[drive(skip)]
    Mul(OverflowMode),
    #[drive(skip)]
    Div(OverflowMode),
    #[drive(skip)]
    Rem(OverflowMode),
    /// Returns `(result, did_overflow)`, where `result` is the result of the operation with
    /// wrapping semantics, and `did_overflow` is a boolean that indicates whether the operation
    /// overflowed. This operation does not fail.
    AddChecked,
    /// Like `AddChecked`.
    SubChecked,
    /// Like `AddChecked`.
    MulChecked,
    /// Fails if the shift is bigger than the bit-size of the type.
    #[drive(skip)]
    Shl(OverflowMode),
    /// Fails if the shift is bigger than the bit-size of the type.
    #[drive(skip)]
    Shr(OverflowMode),
    /// `BinOp(Offset, ptr, n)` for `ptr` a pointer to type `T` offsets `ptr` by `n * size_of::<T>()`.
    Offset,
    /// `BinOp(Cmp, a, b)` returns `-1u8` if `a < b`, `0u8` if `a == b`, and `1u8` if `a > b`.
    Cmp,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    EnumIsA,
    EnumToGetters,
    EnumAsGetters,
    VariantName,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[serde_state(state_implements = HashConsSerializerState)] // Avoid corecursive impls due to perfect derive
pub enum Operand {
    Copy(Place),
    Move(Place),
    /// Constant value (including constant and static variables)
    #[charon::rename("Constant")]
    Const(Box<ConstantExpr>),
}

/// A function identifier. See [crate::ullbc_ast::Terminator]
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    EnumIsA,
    EnumAsGetters,
    VariantName,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("F")]
#[serde_state(stateless)]
pub enum FunId {
    /// A "regular" function (function local to the crate, external function
    /// not treated as a primitive one).
    Regular(FunDeclId),
    /// A primitive function, coming from a standard library (for instance:
    /// `alloc::boxed::Box::new`).
    /// TODO: rename to "Primitive"
    #[charon::rename("FBuiltin")]
    Builtin(BuiltinFunId),
}

impl From<FunDeclId> for FunId {
    fn from(id: FunDeclId) -> Self {
        Self::Regular(id)
    }
}
impl From<BuiltinFunId> for FunId {
    fn from(id: BuiltinFunId) -> Self {
        Self::Builtin(id)
    }
}

/// An built-in function identifier, identifying a function coming from a
/// standard library.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    EnumIsA,
    EnumAsGetters,
    VariantName,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
pub enum BuiltinFunId {
    /// Used instead of `alloc::boxed::Box::new` when `--treat-box-as-builtin` is set.
    BoxNew,
    /// Cast `&[T; N]` to `&[T]`.
    ///
    /// This is used instead of unsizing coercions when `--ops-to-function-calls` is set.
    ArrayToSliceShared,
    /// Cast `&mut [T; N]` to `&mut [T]`.
    ///
    /// This is used instead of unsizing coercions when `--ops-to-function-calls` is set.
    ArrayToSliceMut,
    /// `repeat(n, x)` returns an array where `x` has been replicated `n` times.
    ///
    /// This is used instead of `Rvalue::ArrayRepeat` when `--ops-to-function-calls` is set.
    ArrayRepeat,
    /// A built-in funciton introduced instead of array/slice place indexing when
    /// `--index-to-function-calls` is set. The signature depends on the parameters. It could look
    /// like:
    /// - `fn ArrayIndexShared<T,N>(&[T;N], usize) -> &T`
    /// - `fn SliceIndexShared<T>(&[T], usize) -> &T`
    /// - `fn ArraySubSliceShared<T,N>(&[T;N], usize, usize) -> &[T]`
    /// - `fn SliceSubSliceMut<T>(&mut [T], usize, usize) -> &mut [T]`
    /// - etc
    Index(BuiltinIndexOp),
    /// Build a raw pointer, from a data pointer and metadata. The metadata can be unit, if
    /// building a thin pointer.
    ///
    /// This is used instead of `AggregateKind::RawPtr` when `--ops-to-function-calls` is set.
    PtrFromParts(RefKind),
}

/// One of 8 built-in indexing operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct BuiltinIndexOp {
    /// Whether this is a slice or array.
    #[drive(skip)]
    pub is_array: bool,
    /// Whether we're indexing mutably or not. Determines the type ofreference of the input and
    /// output.
    pub mutability: RefKind,
    /// Whether we're indexing a single element or a subrange. If `true`, the function takes
    /// two indices and the output is a slice; otherwise, the function take one index and the
    /// output is a reference to a single element.
    #[drive(skip)]
    pub is_range: bool,
}

/// Reference to a function declaration or builtin function.
#[derive(Debug, Clone, SerializeState, DeserializeState, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct MaybeBuiltinFunDeclRef {
    pub id: FunId,
    pub generics: BoxedArgs,
    pub trait_ref: Option<TraitRef>,
}

/// Reference to a trait method.
#[derive(Debug, Clone, SerializeState, DeserializeState, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct TraitMethodRef {
    pub trait_ref: TraitRef,
    pub name: TraitItemName,
    pub generics: BoxedArgs,
    /// Reference to the method declaration; can be derived from the trait_ref, provided here for
    /// convenience. The generic args are for the method, not for this function.
    pub method_decl_id: FunDeclId,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    EnumAsGetters,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
    Hash,
)]
pub enum FnPtrKind {
    #[charon::rename("FunId")]
    Fun(FunId),
    /// If a trait: the reference to the trait and the id of the trait method.
    /// The fun decl id is not really necessary - we put it here for convenience
    /// purposes.
    #[charon::rename("TraitMethod")]
    Trait(TraitRef, TraitItemName, FunDeclId),
}

impl From<FunId> for FnPtrKind {
    fn from(id: FunId) -> Self {
        Self::Fun(id)
    }
}
impl From<FunDeclId> for FnPtrKind {
    fn from(id: FunDeclId) -> Self {
        Self::Fun(id.into())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, SerializeState, DeserializeState, Drive, DriveMut, Hash)]
pub struct FnPtr {
    pub kind: Box<FnPtrKind>,
    pub generics: BoxedArgs,
}

impl From<FunDeclRef> for FnPtr {
    fn from(fn_ref: FunDeclRef) -> Self {
        FnPtr::new(fn_ref.id.into(), fn_ref.generics)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, SerializeState, DeserializeState, Drive, DriveMut, Hash)]
#[charon::variants_prefix("Prov")]
pub enum Provenance {
    Global(GlobalDeclRef),
    Function(FunDeclRef),
    Unknown,
}

/// A byte, in the MiniRust sense: it can either be uninitialized, a concrete u8 value,
/// or part of a pointer with provenance (e.g. to a global or a function)
#[derive(Debug, PartialEq, Eq, Clone, SerializeState, DeserializeState, Drive, DriveMut, Hash)]
pub enum Byte {
    /// An uninitialized byte
    Uninit,
    /// A concrete byte value
    Value(u8),
    /// A byte that is part of a pointer with provenance. The u8 is the offset within the
    /// pointer. Note that we do not have an actual value for this pointer byte, unlike
    /// MiniRust, as that is non-deterministic.
    Provenance(Provenance, u8),
}

/// A constant expression.
///
/// Only the [`ConstantExprKind::Literal`] and [`ConstantExprKind::Var`]
/// cases are left in the final LLBC.
///
/// The other cases come from a straight translation from the MIR:
///
/// [`ConstantExprKind::Adt`] case:
/// It is a bit annoying, but rustc treats some ADT and tuple instances as
/// constants when generating MIR:
/// - an enumeration with one variant and no fields is a constant.
/// - a structure with no field is a constant.
/// - sometimes, Rust stores the initialization of an ADT as a constant
///   (if all the fields are constant) rather than as an aggregated value
/// We later desugar those to regular ADTs, see [regularize_constant_adts.rs].
///
/// [`ConstantExprKind::Global`] case: access to a global variable. We later desugar it to
/// a copy of a place global.
///
/// [`ConstantExprKind::Ref`] case: reference to a constant value. We later desugar it to a separate
/// statement.
///
/// [`ConstantExprKind::FnPtr`] case: a function pointer (to a top-level function).
///
/// Remark:
/// MIR seems to forbid more complex expressions like paths. For instance,
/// reading the constant `a.b` is translated to `{ _1 = const a; _2 = (_1.0) }`.
#[derive(
    Debug,
    Hash,
    PartialEq,
    Eq,
    Clone,
    VariantName,
    EnumIsA,
    EnumAsGetters,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("C")]
pub enum ConstantExprKind {
    #[serde_state(stateless)]
    Literal(Literal),
    /// In most situations:
    /// Enumeration with one variant with no fields, structure with
    /// no fields, unit (encoded as a 0-tuple).
    ///
    /// Less frequently: arbitrary ADT values.
    ///
    /// We eliminate this case in a micro-pass.
    Adt(Option<VariantId>, Vec<ConstantExpr>),
    Array(Vec<ConstantExpr>),
    /// The value is a top-level constant/static.
    ///
    /// We eliminate this case in a micro-pass.
    ///
    /// Remark: constants can actually have generic parameters.
    /// ```text
    /// struct V<const N: usize, T> {
    ///   x: [T; N],
    /// }
    ///
    /// impl<const N: usize, T> V<N, T> {
    ///   const LEN: usize = N; // This has generics <N, T>
    /// }
    ///
    /// fn use_v<const N: usize, T>(v: V<N, T>) {
    ///   let l = V::<N, T>::LEN; // We need to provided a substitution here
    /// }
    /// ```
    Global(GlobalDeclRef),
    /// A trait associated constant.
    ///
    /// Ex.:
    /// ```text
    /// impl Foo for Bar {
    ///   const C : usize = 32; // <-
    /// }
    /// ```
    TraitConst(TraitRef, TraitItemName),
    /// A reference to the vtable `static` item for this trait ref. This can be normalized for
    /// cases where we do emit a vtable item. That's not always the case for builtin traits, e.g.
    /// for `MetaSized`.
    VTableRef(TraitRef),
    /// A shared reference to a constant value.
    ///
    /// We eliminate this case in a micro-pass.
    Ref(Box<ConstantExpr>, Option<UnsizingMetadata>),
    /// A pointer to a mutable static.
    ///
    /// We eliminate this case in a micro-pass.
    Ptr(RefKind, Box<ConstantExpr>, Option<UnsizingMetadata>),
    /// A const generic var
    Var(ConstGenericDbVar),
    /// Function definition -- this is a ZST constant
    FnDef(FnPtr),
    /// A function pointer to a function item; this is an actual pointer to that function item.
    ///
    /// We eliminate this case in a micro-pass.
    FnPtr(FnPtr),
    /// A pointer with no provenance (e.g. 0 for the null pointer)
    ///
    /// We eliminate this case in a micro-pass.
    #[drive(skip)]
    PtrNoProvenance(u128),
    /// Raw memory value obtained from constant evaluation. Used when a more structured
    /// representation isn't possible (e.g. for unions) or just isn't implemented yet.
    #[drive(skip)]
    RawMemory(Vec<Byte>),
    /// A constant expression that Charon still doesn't handle, along with the reason why.
    #[drive(skip)]
    Opaque(String),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
#[serde_state(state_implements = HashConsSerializerState)] // Avoid corecursive impls due to perfect derive
pub struct ConstantExpr {
    pub kind: ConstantExprKind,
    pub ty: Ty,
}

/// TODO: we could factor out [Rvalue] and function calls (for LLBC, not ULLBC).
/// We can also factor out the unops, binops with the function calls.
/// TODO: move the aggregate kind to operands
/// TODO: we should prefix the type variants with "R" or "Rv", this would avoid collisions
#[derive(
    Debug,
    Clone,
    EnumToGetters,
    EnumAsGetters,
    EnumIsA,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
pub enum Rvalue {
    /// Lifts an operand as an rvalue.
    Use(Operand),
    /// Takes a reference to the given place.
    /// The `Operand` refers to the init value of the metadata, it is `()` if no metadata
    #[charon::rename("RvRef")]
    Ref {
        place: Place,
        #[serde_state(stateless)]
        kind: BorrowKind,
        ptr_metadata: Operand,
    },
    /// Takes a raw pointer with the given mutability to the given place. This is generated by
    /// pointer casts like `&v as *const _` or raw borrow expressions like `&raw const v.`
    /// Like `Ref`, the `Operand` refers to the init value of the metadata, it is `()` if no metadata.
    RawPtr {
        place: Place,
        kind: RefKind,
        ptr_metadata: Operand,
    },
    /// Binary operations (note that we merge "checked" and "unchecked" binops)
    BinaryOp(BinOp, Operand, Operand),
    /// Unary operation (e.g. not, neg)
    UnaryOp(UnOp, Operand),
    /// Nullary operation (e.g. `size_of`)
    NullaryOp(NullOp, Ty),
    /// Discriminant read. Reads the discriminant value of an enum. The place must have the type of
    /// an enum. The discriminant in question is the one in the `discriminant` field of the
    /// corresponding `Variant`. This can be different than the value stored in memory (called
    /// `tag`). That one is described by [`DiscriminantLayout`] and [`TagEncoding`].
    Discriminant(Place),
    /// Creates an aggregate value, like a tuple, a struct or an enum:
    /// ```text
    /// l = List::Cons { value:x, tail:tl };
    /// ```
    /// Note that in some MIR passes (like optimized MIR), aggregate values are
    /// decomposed, like below:
    /// ```text
    /// (l as List::Cons).value = x;
    /// (l as List::Cons).tail = tl;
    /// ```
    /// Because we may want to plug our translation mechanism at various
    /// places, we need to take both into accounts in the translation and in
    /// our semantics. Aggregate value initialization is easy, you might want
    /// to have a look at expansion of `Bottom` values for explanations about the
    /// other case.
    ///
    /// Remark: in case of closures, the aggregated value groups the closure id
    /// together with its state.
    Aggregate(AggregateKind, Vec<Operand>),
    /// Length of a place of type `[T]` or `[T; N]`. This applies to the place itself, not to a
    /// pointer value. This is inserted by rustc in a single case: slice patterns.
    /// ```text
    /// fn slice_pattern_4(x: &[()]) {
    ///     match x {
    ///         [_named] => (),
    ///         _ => (),
    ///     }
    /// }
    /// ```
    Len(Place, Ty, Option<Box<ConstantExpr>>),
    /// `Repeat(x, n)` creates an array where `x` is copied `n` times.
    ///
    /// We translate this to a function call for LLBC.
    Repeat(Operand, Ty, Box<ConstantExpr>),
    /// Transmutes a `*mut u8` (obtained from `malloc`) into shallow-initialized `Box<T>`. This
    /// only appears as part of lowering `Box::new()` in some cases. We reconstruct the original
    /// `Box::new()` call, but sometimes may fail to do so, leaking the expression.
    ShallowInitBox(Operand, Ty),
}

/// An aggregated ADT.
///
/// Note that ADTs are desaggregated at some point in MIR. For instance, if
/// we have in Rust:
/// ```ignore
///   let ls = Cons(hd, tl);
/// ```
///
/// In MIR we have (yes, the discriminant update happens *at the end* for some
/// reason):
/// ```text
///   (ls as Cons).0 = move hd;
///   (ls as Cons).1 = move tl;
///   discriminant(ls) = 0; // assuming `Cons` is the variant of index 0
/// ```
///
/// Rem.: in the Aeneas semantics, both cases are handled (in case of desaggregated
/// initialization, `ls` is initialized to `⊥`, then this `⊥` is expanded to
/// `Cons (⊥, ⊥)` upon the first assignment, at which point we can initialize
/// the field 0, etc.).
#[derive(Debug, Clone, VariantIndexArity, SerializeState, DeserializeState, Drive, DriveMut)]
#[charon::variants_prefix("Aggregated")]
pub enum AggregateKind {
    /// A struct, enum or union aggregate. The `VariantId`, if present, indicates this is an enum
    /// and the aggregate uses that variant. The `FieldId`, if present, indicates this is a union
    /// and the aggregate writes into that field. Otherwise this is a struct.
    Adt(TypeDeclRef, Option<VariantId>, Option<FieldId>),
    /// We don't put this with the ADT cas because this is the only built-in type
    /// with aggregates, and it is a primitive type. In particular, it makes
    /// sense to treat it differently because it has a variable number of fields.
    Array(Ty, Box<ConstantExpr>),
    /// Construct a raw pointer from a pointer value, and its metadata (can be unit, if building
    /// a thin pointer). The type is the type of the pointee.
    /// We lower this to a builtin function call for LLBC in [crate::transform::simplify_output::ops_to_function_calls].
    RawPtr(Ty, RefKind),
}
