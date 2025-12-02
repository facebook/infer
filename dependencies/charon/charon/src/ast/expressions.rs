//! Implements expressions: paths, operands, rvalues, lvalues

use crate::ast::*;
use derive_generic_visitor::{Drive, DriveMut};
use macros::{EnumAsGetters, EnumIsA, EnumToGetters, VariantIndexArity, VariantName};
use serde::{Deserialize, Serialize};
use std::vec::Vec;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Drive, DriveMut)]
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
    Serialize,
    Deserialize,
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
    Serialize,
    Deserialize,
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
    Serialize,
    Deserialize,
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
    Debug, PartialEq, Eq, Clone, EnumIsA, VariantName, Serialize, Deserialize, Drive, DriveMut,
)]
#[charon::rename("Unop")]
pub enum UnOp {
    Not,
    /// This can overflow, for `-i::MIN`.
    #[drive(skip)]
    Neg(OverflowMode),
    /// Retreive the metadata part of a fat pointer. For slices, this retreives their length.
    PtrMetadata,
    /// Casts are rvalues in MIR, but we treat them as unops.
    Cast(CastKind),
}

/// Nullary operation
#[derive(
    Debug, PartialEq, Eq, Clone, EnumIsA, VariantName, Serialize, Deserialize, Drive, DriveMut,
)]
#[charon::rename("Nullop")]
pub enum NullOp {
    SizeOf,
    AlignOf,
    #[drive(skip)]
    OffsetOf(Vec<(usize, FieldId)>),
    UbChecks,
}

/// For all the variants: the first type gives the source type, the second one gives
/// the destination type.
#[derive(
    Debug, PartialEq, Eq, Clone, EnumIsA, VariantName, Serialize, Deserialize, Drive, DriveMut,
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
}

#[derive(
    Debug, PartialEq, Eq, Clone, EnumIsA, VariantName, Serialize, Deserialize, Drive, DriveMut,
)]
#[charon::variants_prefix("Meta")]
pub enum UnsizingMetadata {
    Length(ConstGeneric),
    VTablePtr(TraitRef),
    Unknown,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Serialize, Deserialize)]
#[charon::variants_prefix("O")]
pub enum OverflowMode {
    /// If this operation overflows, it panics. Only exists in debug mode, for instance in
    /// `a + b`, and is introduced by the `remove_dynamic_checks` pass.
    Panic,
    /// If this operation overflows, it UBs, for instance in `core::num::unchecked_add`.
    UB,
    /// If this operation overflows, it wraps around, for instance in `core::num::wrapping_add`,
    /// or `a + b` in release mode.
    Wrap,
}

/// Binary operations.
#[derive(
    Debug, PartialEq, Eq, Copy, Clone, EnumIsA, VariantName, Serialize, Deserialize, Drive, DriveMut,
)]
#[charon::rename("Binop")]
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
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
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
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("F")]
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
    /// `alloc::boxed::Box::new`
    BoxNew,
    /// Cast an array as a slice.
    ///
    /// Converted from `UnOp::ArrayToSlice`
    ArrayToSliceShared,
    /// Cast an array as a slice.
    ///
    /// Converted from `UnOp::ArrayToSlice`
    ArrayToSliceMut,
    /// `repeat(n, x)` returns an array where `x` has been replicated `n` times.
    ///
    /// We introduce this when desugaring the `ArrayRepeat` rvalue.
    ArrayRepeat,
    /// Converted from indexing `ProjectionElem`s. The signature depends on the parameters. It
    /// could look like:
    /// - `fn ArrayIndexShared<T,N>(&[T;N], usize) -> &T`
    /// - `fn SliceIndexShared<T>(&[T], usize) -> &T`
    /// - `fn ArraySubSliceShared<T,N>(&[T;N], usize, usize) -> &[T]`
    /// - `fn SliceSubSliceMut<T>(&mut [T], usize, usize) -> &mut [T]`
    /// - etc
    Index(BuiltinIndexOp),
    /// Build a raw pointer, from a data pointer and metadata. The metadata can be unit, if
    /// building a thin pointer.
    ///
    /// Converted from [AggregateKind::RawPtr]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct MaybeBuiltinFunDeclRef {
    pub id: FunId,
    pub generics: BoxedArgs,
}

/// Reference to a trait method.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct TraitMethodRef {
    pub trait_ref: TraitRef,
    pub name: TraitItemName,
    pub generics: BoxedArgs,
    /// Reference to the method declaration; can be derived from the trait_ref, provided here for
    /// convenience. The generic args are for the method, not for this function.
    pub method_decl_id: FunDeclId,
}

#[derive(
    Debug, Clone, PartialEq, Eq, EnumAsGetters, Serialize, Deserialize, Drive, DriveMut, Hash,
)]
pub enum FunIdOrTraitMethodRef {
    #[charon::rename("FunId")]
    Fun(FunId),
    /// If a trait: the reference to the trait and the id of the trait method.
    /// The fun decl id is not really necessary - we put it here for convenience
    /// purposes.
    #[charon::rename("TraitMethod")]
    Trait(TraitRef, TraitItemName, FunDeclId),
}

impl From<FunId> for FunIdOrTraitMethodRef {
    fn from(id: FunId) -> Self {
        Self::Fun(id)
    }
}
impl From<FunDeclId> for FunIdOrTraitMethodRef {
    fn from(id: FunDeclId) -> Self {
        Self::Fun(id.into())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Drive, DriveMut, Hash)]
pub struct FnPtr {
    pub func: Box<FunIdOrTraitMethodRef>,
    pub generics: BoxedArgs,
}

impl From<FunDeclRef> for FnPtr {
    fn from(fn_ref: FunDeclRef) -> Self {
        FnPtr {
            func: Box::new(fn_ref.id.into()),
            generics: fn_ref.generics,
        }
    }
}

/// A constant expression.
///
/// Only the [`RawConstantExpr::Literal`] and [`RawConstantExpr::Var`]
/// cases are left in the final LLBC.
///
/// The other cases come from a straight translation from the MIR:
///
/// [`RawConstantExpr::Adt`] case:
/// It is a bit annoying, but rustc treats some ADT and tuple instances as
/// constants when generating MIR:
/// - an enumeration with one variant and no fields is a constant.
/// - a structure with no field is a constant.
/// - sometimes, Rust stores the initialization of an ADT as a constant
///   (if all the fields are constant) rather than as an aggregated value
/// We later desugar those to regular ADTs, see [regularize_constant_adts.rs].
///
/// [`RawConstantExpr::Global`] case: access to a global variable. We later desugar it to
/// a copy of a place global.
///
/// [`RawConstantExpr::Ref`] case: reference to a constant value. We later desugar it to a separate
/// statement.
///
/// [`RawConstantExpr::FnPtr`] case: a function pointer (to a top-level function).
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
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("C")]
pub enum RawConstantExpr {
    Literal(Literal),
    /// In most situations:
    /// Enumeration with one variant with no fields, structure with
    /// no fields, unit (encoded as a 0-tuple).
    ///
    /// Less frequently: arbitrary ADT values.
    ///
    /// We eliminate this case in a micro-pass.
    #[charon::opaque]
    Adt(Option<VariantId>, Vec<ConstantExpr>),
    #[charon::opaque]
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
    #[charon::opaque]
    Global(GlobalDeclRef),
    ///
    /// A trait constant.
    ///
    /// Ex.:
    /// ```text
    /// impl Foo for Bar {
    ///   const C : usize = 32; // <-
    /// }
    /// ```
    ///
    /// Remark: trait constants can not be used in types, they are necessarily
    /// values.
    TraitConst(TraitRef, TraitItemName),
    /// A shared reference to a constant value.
    ///
    /// We eliminate this case in a micro-pass.
    #[charon::opaque]
    Ref(Box<ConstantExpr>),
    /// A pointer to a mutable static.
    ///
    /// We eliminate this case in a micro-pass.
    #[charon::opaque]
    Ptr(RefKind, Box<ConstantExpr>),
    /// A const generic var
    Var(ConstGenericDbVar),
    /// Function pointer
    FnPtr(FnPtr),
    /// A pointer with no provenance (e.g. 0 for the null pointer)
    ///
    /// We eliminate this case in a micro-pass.
    #[drive(skip)]
    #[charon::opaque]
    PtrNoProvenance(u128),
    /// Raw memory value obtained from constant evaluation. Used when a more structured
    /// representation isn't possible (e.g. for unions) or just isn't implemented yet.
    #[drive(skip)]
    RawMemory(Vec<u8>),
    /// A constant expression that Charon still doesn't handle, along with the reason why.
    #[drive(skip)]
    Opaque(String),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct ConstantExpr {
    pub value: RawConstantExpr,
    pub ty: Ty,
}

/// TODO: we could factor out [Rvalue] and function calls (for LLBC, not ULLBC).
/// We can also factor out the unops, binops with the function calls.
/// TODO: move the aggregate kind to operands
/// TODO: we should prefix the type variants with "R" or "Rv", this would avoid collisions
#[derive(
    Debug, Clone, EnumToGetters, EnumAsGetters, EnumIsA, Serialize, Deserialize, Drive, DriveMut,
)]
pub enum Rvalue {
    /// Lifts an operand as an rvalue.
    Use(Operand),
    /// Takes a reference to the given place.
    #[charon::rename("RvRef")]
    Ref(Place, BorrowKind),
    /// Takes a raw pointer with the given mutability to the given place. This is generated by
    /// pointer casts like `&v as *const _` or raw borrow expressions like `&raw const v.`
    RawPtr(Place, RefKind),
    /// Binary operations (note that we merge "checked" and "unchecked" binops)
    BinaryOp(BinOp, Operand, Operand),
    /// Unary operation (e.g. not, neg)
    UnaryOp(UnOp, Operand),
    /// Nullary operation (e.g. `size_of`)
    NullaryOp(NullOp, Ty),
    /// Discriminant read. Reads the discriminant value of an enum. The place must have the type of
    /// an enum.
    ///
    /// This case is filtered in [crate::transform::remove_read_discriminant]
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
    /// Length of a memory location. The run-time length of e.g. a vector or a slice is
    /// represented differently (but pretty-prints the same, FIXME).
    /// Should be seen as a function of signature:
    /// - `fn<T;N>(&[T;N]) -> usize`
    /// - `fn<T>(&[T]) -> usize`
    ///
    /// We store the type argument and the const generic (the latter only for arrays).
    ///
    /// `Len` is automatically introduced by rustc, notably for the bound checks:
    /// we eliminate it together with the bounds checks whenever possible.
    /// There are however occurrences that we don't eliminate (yet).
    /// For instance, for the following Rust code:
    /// ```text
    /// fn slice_pattern_4(x: &[()]) {
    ///     match x {
    ///         [_named] => (),
    ///         _ => (),
    ///     }
    /// }
    /// ```
    /// rustc introduces a check that the length of the slice is exactly equal
    /// to 1 and that we preserve.
    Len(Place, Ty, Option<ConstGeneric>),
    /// `Repeat(x, n)` creates an array where `x` is copied `n` times.
    ///
    /// We translate this to a function call for LLBC.
    Repeat(Operand, Ty, ConstGeneric),
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
#[derive(Debug, Clone, VariantIndexArity, Serialize, Deserialize, Drive, DriveMut)]
#[charon::variants_prefix("Aggregated")]
pub enum AggregateKind {
    /// A struct, enum or union aggregate. The `VariantId`, if present, indicates this is an enum
    /// and the aggregate uses that variant. The `FieldId`, if present, indicates this is a union
    /// and the aggregate writes into that field. Otherwise this is a struct.
    Adt(TypeDeclRef, Option<VariantId>, Option<FieldId>),
    /// We don't put this with the ADT cas because this is the only built-in type
    /// with aggregates, and it is a primitive type. In particular, it makes
    /// sense to treat it differently because it has a variable number of fields.
    Array(Ty, ConstGeneric),
    /// Construct a raw pointer from a pointer value, and its metadata (can be unit, if building
    /// a thin pointer). The type is the type of the pointee.
    /// We lower this to a builtin function call for LLBC in [crate::transform::ops_to_function_calls].
    RawPtr(Ty, RefKind),
}
