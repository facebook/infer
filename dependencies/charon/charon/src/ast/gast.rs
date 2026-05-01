//! Definitions common to [crate::ullbc_ast] and [crate::llbc_ast]
use crate::ast::*;
use crate::ids::IndexMap;
use crate::ids::IndexVec;
use crate::llbc_ast;
use crate::ullbc_ast;
use derive_generic_visitor::{Drive, DriveMut};
use macros::EnumAsGetters;
use macros::{EnumIsA, EnumToGetters};
use serde_state::DeserializeState;
use serde_state::SerializeState;

/// A variable
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct Local {
    /// Unique index identifying the variable
    pub index: LocalId,
    /// Variable name - may be `None` if the variable was introduced by Rust
    /// through desugaring.
    #[drive(skip)]
    pub name: Option<String>,
    /// Span of the variable declaration.
    pub span: Span,
    /// The variable type
    #[charon::rename("local_ty")]
    pub ty: Ty,
}
#[deprecated(note = "use `Local` intead")]
pub type Var = Local;
#[deprecated(note = "use `LocalId` intead")]
pub type VarId = LocalId;

/// The local variables of a body.
#[derive(Debug, Default, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct Locals {
    /// The number of local variables used for the input arguments.
    #[drive(skip)]
    pub arg_count: usize,
    /// The local variables.
    /// We always have, in the following order:
    /// - the local used for the return value (index 0)
    /// - the `arg_count` input arguments
    /// - the remaining locals, used for the intermediate computations
    pub locals: IndexVec<LocalId, Local>,
}

/// An expression body.
/// TODO: arg_count should be stored in GFunDecl below. But then,
///       the print is obfuscated and Aeneas may need some refactoring.
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
#[charon::rename("GexprBody")]
pub struct GExprBody<T> {
    pub span: Span,
    /// The number of regions existentially bound in this body. We introduce fresh such regions
    /// during translation instead of the erased regions that rustc gives us.
    #[drive(skip)]
    pub bound_body_regions: usize,
    /// The local variables.
    pub locals: Locals,
    /// The statements and blocks that compose this body.
    pub body: T,
    /// For each line inside the body, we record any whole-line `//` comments found before it. They
    /// are added to statements in the late `recover_body_comments` pass.
    #[charon::opaque]
    #[drive(skip)]
    pub comments: Vec<(usize, Vec<String>)>,
}

/// The body of a function.
#[derive(
    Debug,
    Clone,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
    EnumIsA,
    EnumAsGetters,
    EnumToGetters,
)]
pub enum Body {
    /// Body represented as a CFG. This is what ullbc is made of, and what we get after translating MIR.
    Unstructured(ullbc_ast::ExprBody),
    /// Body represented with structured control flow. This is what llbc is made of. We restructure
    /// the control flow in the `ullbc_to_llbc` pass.
    Structured(llbc_ast::ExprBody),
    /// The body of the function item we add for each trait method declaration, if the trait
    /// doesn't provide a default for that method.
    TraitMethodWithoutDefault,
    /// A body that the user chose not to translate, based on opacity settings like
    /// `--include`/`--opaque`.
    Opaque,
    /// A body that was not available. Typically that's function bodies for non-generic and
    /// non-inlineable std functions, as these are not present in the compiled standard library
    /// `.rmeta` file shipped with a rust toolchain.
    Missing,
    /// We encountered an error while translating this body.
    #[drive(skip)]
    #[serde_state(stateless)]
    Error(Error),
}

/// Item kind: whether this function/const is part of a trait declaration, trait implementation, or
/// neither.
///
/// Example:
/// ```text
/// trait Foo {
///     fn bar(x : u32) -> u32; // trait item decl without default
///
///     fn baz(x : bool) -> bool { x } // trait item decl with default
/// }
///
/// impl Foo for ... {
///     fn bar(x : u32) -> u32 { x } // trait item implementation
/// }
///
/// fn test(...) { ... } // regular
///
/// impl Type {
///     fn test(...) { ... } // regular
/// }
/// ```
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut, PartialEq, Eq)]
#[charon::variants_suffix("Item")]
pub enum ItemSource {
    /// This item stands on its own.
    TopLevel,
    /// This is a closure in a function body.
    Closure {
        info: ClosureInfo,
    },
    /// This is an associated item in a trait declaration. It has a body if and only if the trait
    /// provided a default implementation.
    TraitDecl {
        /// The trait declaration this item belongs to.
        trait_ref: TraitDeclRef,
        /// The name of the item.
        // TODO: also include method generics so we can recover a full `FnPtr::TraitMethod`
        #[drive(skip)]
        item_name: TraitItemName,
        /// Whether the trait declaration provides a default implementation.
        #[drive(skip)]
        has_default: bool,
    },
    /// This is an associated item in a trait implementation.
    TraitImpl {
        /// The trait implementation the method belongs to.
        impl_ref: TraitImplRef,
        /// The trait declaration that the impl block implements.
        trait_ref: TraitDeclRef,
        /// The name of the item
        // TODO: also include method generics so we can recover a full `FnPtr::TraitMethod`
        #[drive(skip)]
        item_name: TraitItemName,
        /// True if the trait decl had a default implementation for this function/const and this
        /// item is a copy of the default item.
        #[drive(skip)]
        reuses_default: bool,
    },
    /// This is a vtable struct for a trait.
    VTableTy {
        /// The `dyn Trait` predicate implemented by this vtable.
        dyn_predicate: DynPredicate,
        /// Record what each vtable field means.
        field_map: IndexVec<FieldId, VTableField>,
        /// For each implied clause that is also a supertrait clause, reords which field id
        /// corresponds to it.
        supertrait_map: IndexMap<TraitClauseId, Option<FieldId>>,
    },
    /// This is a vtable value for an impl.
    VTableInstance {
        impl_ref: TraitImplRef,
    },
    /// The method shim wraps a concrete implementation of a method into a function that takes `dyn
    /// Trait` as its `Self` type. This shim casts the receiver to the known concrete type and
    /// calls the real method.
    VTableMethodShim,
    VTableInstanceMono,
    VTableMethodPreShim(TraitDeclId, TraitItemName, Vec<Ty>),
}

#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut, PartialEq, Eq)]
#[charon::variants_prefix("VTable")]
pub enum VTableField {
    Size,
    Align,
    Drop,
    Method(TraitItemName),
    SuperTrait(TraitClauseId),
}

/// A function definition
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct FunDecl {
    #[drive(skip)]
    pub def_id: FunDeclId,
    /// The meta data associated with the declaration.
    pub item_meta: ItemMeta,
    pub generics: GenericParams,
    /// The signature contains the inputs/output types *with* non-erased regions.
    /// It also contains the list of region and type parameters.
    pub signature: FunSig,
    /// The function kind: "regular" function, trait method declaration, etc.
    pub src: ItemSource,
    /// Whether this function is in fact the body of a constant/static that we turned into an
    /// initializer function.
    pub is_global_initializer: Option<GlobalDeclId>,
    /// The function body, unless the function is opaque.
    /// Opaque functions are: external functions, or local functions tagged
    /// as opaque.
    pub body: Body,
}

/// Reference to a function declaration.
#[derive(Debug, Clone, SerializeState, DeserializeState, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct FunDeclRef {
    pub id: FunDeclId,
    /// Generic arguments passed to the function.
    pub generics: BoxedArgs,
}

#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub enum GlobalKind {
    /// A static.
    Static,
    /// A const with a name (either top-level or an associated const in a trait).
    NamedConst,
    /// A const without a name:
    /// - An inline const expression (`const { 1 + 1 }`);
    /// - A const expression in a type (`[u8; sizeof::<T>()]`);
    /// - A promoted constant, automatically lifted from a body (`&0`).
    AnonConst,
}

/// A global variable definition (constant or static).
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct GlobalDecl {
    #[drive(skip)]
    pub def_id: GlobalDeclId,
    /// The meta data associated with the declaration.
    pub item_meta: ItemMeta,
    pub generics: GenericParams,
    pub ty: Ty,
    /// The context of the global: distinguishes top-level items from trait-associated items.
    pub src: ItemSource,
    /// The kind of global (static or const).
    #[drive(skip)]
    pub global_kind: GlobalKind,
    /// The initializer function used to compute the initial value for this constant/static. It
    /// uses the same generic parameters as the global.
    pub init: FunDeclId,
}

/// Reference to a global declaration.
#[derive(Debug, Clone, SerializeState, DeserializeState, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct GlobalDeclRef {
    pub id: GlobalDeclId,
    pub generics: BoxedArgs,
}

#[derive(
    Debug,
    Clone,
    Copy,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
)]
#[drive(skip)]
#[serde_state(stateless)]
pub struct TraitItemName(pub ustr::Ustr);

/// A trait **declaration**.
///
/// For instance:
/// ```text
/// trait Foo {
///   type Bar;
///
///   fn baz(...); // required method (see below)
///
///   fn test() -> bool { true } // provided method (see below)
/// }
/// ```
///
/// In case of a trait declaration, we don't include the provided methods (the methods
/// with a default implementation): they will be translated on a per-need basis. This is
/// important for two reasons:
/// - this makes the trait definitions a lot smaller (the Iterator trait
///   has *one* declared function and more than 70 provided functions)
/// - this is important for the external traits, whose provided methods
///   often use features we don't support yet
///
/// Remark:
/// In Aeneas, we still translate the provided methods on an individual basis,
/// and in such a way thay they take as input a trait instance. This means that
/// we can use default methods *but*:
/// - implementations of required methods shoudln't call default methods
/// - trait implementations shouldn't redefine required methods
/// The use case we have in mind is [std::iter::Iterator]: it declares one required
/// method (`next`) that should be implemented for every iterator, and defines many
/// helpers like `all`, `map`, etc. that shouldn't be re-implemented.
/// Of course, this forbids other useful use cases such as visitors implemented
/// by means of traits.
#[allow(clippy::type_complexity)]
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct TraitDecl {
    #[drive(skip)]
    pub def_id: TraitDeclId,
    pub item_meta: ItemMeta,
    pub generics: GenericParams,
    /// The "parent" clauses: the supertraits.
    ///
    /// Supertraits are actually regular where clauses, but we decided to have
    /// a custom treatment.
    /// ```text
    /// trait Foo : Bar {
    ///             ^^^
    ///         supertrait, that we treat as a parent predicate
    /// }
    /// ```
    /// TODO: actually, as of today, we consider that all trait clauses of
    /// trait declarations are parent clauses.
    pub implied_clauses: IndexMap<TraitClauseId, TraitParam>,
    /// The associated constants declared in the trait.
    pub consts: Vec<TraitAssocConst>,
    /// The associated types declared in the trait. The binder binds the generic parameters of the
    /// type if it is a GAT (Generic Associated Type). For a plain associated type the binder binds
    /// nothing.
    pub types: Vec<Binder<TraitAssocTy>>,
    /// The methods declared by the trait. The binder binds the generic parameters of the method.
    ///
    /// ```rust
    /// trait Trait<T> {
    ///   // The `Binder` for this method binds `'a` and `U`.
    ///   fn method<'a, U>(x: &'a U);
    /// }
    /// ```
    pub methods: Vec<Binder<TraitMethod>>,
    /// The virtual table struct for this trait, if it has one.
    /// It is guaranteed that the trait has a vtable iff it is dyn-compatible.
    pub vtable: Option<TypeDeclRef>,
}

/// An associated constant in a trait.
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct TraitAssocConst {
    pub name: TraitItemName,
    #[drive(skip)]
    #[serde_state(stateless)]
    pub attr_info: AttrInfo,
    pub ty: Ty,
    pub default: Option<GlobalDeclRef>,
}

/// An associated type in a trait.
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct TraitAssocTy {
    pub name: TraitItemName,
    #[drive(skip)]
    #[serde_state(stateless)]
    pub attr_info: AttrInfo,
    pub default: Option<TraitAssocTyImpl>,
    /// List of trait clauses that apply to this type.
    pub implied_clauses: IndexMap<TraitClauseId, TraitParam>,
}

/// A trait method.
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct TraitMethod {
    pub name: TraitItemName,
    #[drive(skip)]
    #[serde_state(stateless)]
    pub attr_info: AttrInfo,
    /// Each method declaration is represented by a function item. That function contains the
    /// signature of the method as well as information like attributes. It has a body iff the
    /// method declaration has a default implementation; otherwise it has an `Opaque` body.
    pub item: FunDeclRef,
}

/// A trait **implementation**.
///
/// For instance:
/// ```text
/// impl Foo for List {
///   type Bar = ...
///
///   fn baz(...) { ... }
/// }
/// ```
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct TraitImpl {
    #[drive(skip)]
    pub def_id: TraitImplId,
    pub item_meta: ItemMeta,
    /// The information about the implemented trait.
    /// Note that this contains the instantiation of the "parent"
    /// clauses.
    pub impl_trait: TraitDeclRef,
    pub generics: GenericParams,
    /// The trait references for the parent clauses (see [TraitDecl]).
    pub implied_trait_refs: IndexMap<TraitClauseId, TraitRef>,
    /// The implemented associated constants.
    pub consts: Vec<(TraitItemName, GlobalDeclRef)>,
    /// The implemented associated types.
    pub types: Vec<(TraitItemName, Binder<TraitAssocTyImpl>)>,
    /// The implemented methods
    pub methods: Vec<(TraitItemName, Binder<FunDeclRef>)>,
    /// The virtual table instance for this trait implementation. This is `Some` iff the trait is
    /// dyn-compatible.
    pub vtable: Option<GlobalDeclRef>,
}

/// The value of a trait associated type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct TraitAssocTyImpl {
    pub value: Ty,
    /// This matches the corresponding vector in `TraitAssocTy`. In the same way, this is empty
    /// after the `lift_associated_item_clauses` pass.
    #[charon::opaque]
    pub implied_trait_refs: IndexMap<TraitClauseId, TraitRef>,
}

/// A function operand is used in function calls.
/// It either designates a top-level function, or a place in case
/// we are using function pointers stored in local variables.
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
#[charon::variants_prefix("FnOp")]
pub enum FnOperand {
    /// Regular case: call to a top-level function, trait method, etc.
    Regular(FnPtr),
    /// Use of a function pointer.
    Dynamic(Operand),
}

#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct Call {
    pub func: FnOperand,
    pub args: Vec<Operand>,
    pub dest: Place,
}

#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct CopyNonOverlapping {
    pub src: Operand,
    pub dst: Operand,
    pub count: Operand,
}

/// The kind of a built-in assertion, which may panic and unwind. These are removed
/// by `reconstruct_fallible_operations` because they're implicit in the semantics of (U)LLBC.
/// This kind should only be used for error-reporting purposes, as the check itself
/// is performed in the instructions preceding the assert.
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub enum BuiltinAssertKind {
    BoundsCheck { len: Operand, index: Operand },
    Overflow(BinOp, Operand, Operand),
    OverflowNeg(Operand),
    DivisionByZero(Operand),
    RemainderByZero(Operand),
    MisalignedPointerDereference { required: Operand, found: Operand },
    NullPointerDereference,
    InvalidEnumConstruction(Operand),
}

/// (U)LLBC is a language with side-effects: a statement may abort in a way that isn't tracked by
/// control-flow. The three kinds of abort are:
/// - Panic
/// - Undefined behavior (caused by an "assume")
/// - Unwind termination
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub enum AbortKind {
    /// A built-in panicking function, or a panic due to a failed built-in check (e.g. for out-of-bounds accesses).
    Panic(Option<Name>),
    /// Undefined behavior in the rust abstract machine.
    UndefinedBehavior,
    /// Unwind had to stop for ABI reasons or because cleanup code panicked again.
    UnwindTerminate,
}

/// A `Drop` statement/terminator can mean two things, depending on what MIR phase we retrieved
/// from rustc: it could be a real drop, or it could be a "conditional drop", which is where drop
/// may happen depending on whether the borrow-checker determines a drop is needed.
#[derive(Debug, Clone, Copy, SerializeState, DeserializeState, Drive, DriveMut)]
pub enum DropKind {
    /// A real drop. This calls `<T as Destruct>::drop_in_place(&raw mut place)` and marks the
    /// place as moved-out-of. Use `--desugar-drops` to transform all such drops to an actual
    /// function call.
    ///
    /// The `drop_in_place` method is added by Charon to the `Destruct` trait to make it possible
    /// to track drop code in polymorphic code. It contains the same code as the
    /// `core::ptr::drop_in_place<T>` builtin would.
    ///
    /// Drop are precise in MIR `elaborated` and `optimized`.
    Precise,
    /// A conditional drop, which may or may not end up running drop code depending on the code
    /// path that led to it. A conditional drop may also become a partial drop (dropping only the
    /// subplaces that haven't been moved out of), may be conditional on the code path that led to
    /// it, or become an async drop. The exact semantics are left intentionally unspecified by
    /// rustc developers. To elaborate such drops into precise drops, pass `--precise-drops` to
    /// Charon.
    ///
    /// A conditional drop may also be passed an unaligned place when dropping fields of packed
    /// structs. Such a thing is UB for a precise drop.
    ///
    /// Drop are conditional in MIR `built` and `promoted`.
    Conditional,
}

/// Check the value of an operand and abort if the value is not expected. This is introduced to
/// avoid a lot of small branches.
///
/// We translate MIR asserts (introduced for out-of-bounds accesses or divisions by zero for
/// instance) to this. We then eliminate them in [crate::transform::resugar::reconstruct_fallible_operations],
/// because they're implicit in the semantics of our array accesses etc. Finally we introduce new asserts in
/// [crate::transform::resugar::reconstruct_asserts].
#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
#[charon::rename("Assertion")]
pub struct Assert {
    pub cond: Operand,
    /// The value that the operand should evaluate to for the assert to succeed.
    #[drive(skip)]
    pub expected: bool,
    /// The kind of check performed by this assert. This is only used for error reporting, as the check
    /// is actually performed by the instructions preceding the assert.
    pub check_kind: Option<BuiltinAssertKind>,
}

/// A generic `*DeclRef`-shaped struct, used when we're generic over the type of item.
#[derive(Debug, Clone, Drive, DriveMut)]
pub struct DeclRef<Id> {
    pub id: Id,
    pub generics: BoxedArgs,
    /// If the item is a trait associated item, `generics` are only those of the item, and this
    /// contains a reference to the trait.
    pub trait_ref: Option<TraitRef>,
}

impl DeclRef<ItemId> {
    pub fn try_convert_id<Id>(self) -> Result<DeclRef<Id>, <ItemId as TryInto<Id>>::Error>
    where
        ItemId: TryInto<Id>,
    {
        Ok(DeclRef {
            id: self.id.try_into()?,
            generics: self.generics,
            trait_ref: self.trait_ref,
        })
    }
}

// Implement `DeclRef<_>` -> `FooDeclRef` conversions.
macro_rules! convert_item_ref {
    ($item_ref_ty:ident($id:ident)) => {
        impl TryFrom<DeclRef<ItemId>> for $item_ref_ty {
            type Error = ();
            fn try_from(item: DeclRef<ItemId>) -> Result<Self, ()> {
                assert!(item.trait_ref.is_none());
                Ok($item_ref_ty {
                    id: item.id.try_into()?,
                    generics: item.generics,
                })
            }
        }
        impl From<DeclRef<$id>> for $item_ref_ty {
            fn from(item: DeclRef<$id>) -> Self {
                assert!(item.trait_ref.is_none());
                $item_ref_ty {
                    id: item.id,
                    generics: item.generics,
                }
            }
        }
    };
}
convert_item_ref!(TypeDeclRef(TypeId));
convert_item_ref!(FunDeclRef(FunDeclId));
convert_item_ref!(GlobalDeclRef(GlobalDeclId));
convert_item_ref!(TraitDeclRef(TraitDeclId));
convert_item_ref!(TraitImplRef(TraitImplId));
impl TryFrom<DeclRef<ItemId>> for FnPtr {
    type Error = ();
    fn try_from(item: DeclRef<ItemId>) -> Result<Self, ()> {
        let id: FunId = item.id.try_into()?;
        Ok(FnPtr::new(id.into(), item.generics))
    }
}

impl TryFrom<DeclRef<ItemId>> for MaybeBuiltinFunDeclRef {
    type Error = ();
    fn try_from(item: DeclRef<ItemId>) -> Result<Self, ()> {
        Ok(item.try_convert_id::<FunId>()?.into())
    }
}
impl From<DeclRef<FunId>> for MaybeBuiltinFunDeclRef {
    fn from(item: DeclRef<FunId>) -> Self {
        MaybeBuiltinFunDeclRef {
            id: item.id,
            generics: item.generics,
            trait_ref: item.trait_ref,
        }
    }
}
