//! Definitions common to [crate::ullbc_ast] and [crate::llbc_ast]
use crate::ast::*;
use crate::ids::Vector;
use crate::llbc_ast;
use crate::ullbc_ast;
use derive_generic_visitor::{Drive, DriveMut};
use indexmap::IndexMap;
use macros::{EnumIsA, EnumToGetters};
use serde::{Deserialize, Serialize};

/// A variable
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct Local {
    /// Unique index identifying the variable
    pub index: LocalId,
    /// Variable name - may be `None` if the variable was introduced by Rust
    /// through desugaring.
    #[drive(skip)]
    pub name: Option<String>,
    /// The variable type
    #[charon::rename("var_ty")]
    pub ty: Ty,
}
#[deprecated(note = "use `Local` intead")]
pub type Var = Local;
#[deprecated(note = "use `LocalId` intead")]
pub type VarId = LocalId;

/// Marker to indicate that a declaration is opaque (i.e. we don't inspect its body).
#[derive(Debug, Copy, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct Opaque;

/// The local variables of a body.
#[derive(Debug, Default, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct Locals {
    /// The number of local variables used for the input arguments.
    #[drive(skip)]
    pub arg_count: usize,
    /// The local variables.
    /// We always have, in the following order:
    /// - the local used for the return value (index 0)
    /// - the `arg_count` input arguments
    /// - the remaining locals, used for the intermediate computations
    pub locals: Vector<LocalId, Local>,
}

/// An expression body.
/// TODO: arg_count should be stored in GFunDecl below. But then,
///       the print is obfuscated and Aeneas may need some refactoring.
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
#[charon::rename("GexprBody")]
pub struct GExprBody<T> {
    pub span: Span,
    /// The local variables.
    pub locals: Locals,
    /// For each line inside the body, we record any whole-line `//` comments found before it. They
    /// are added to statements in the late `recover_body_comments` pass.
    #[charon::opaque]
    #[drive(skip)]
    pub comments: Vec<(usize, Vec<String>)>,
    pub body: T,
}

/// The body of a function or a constant.
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut, EnumIsA, EnumToGetters)]
pub enum Body {
    /// Body represented as a CFG. This is what ullbc is made of, and what we get after translating MIR.
    Unstructured(ullbc_ast::ExprBody),
    /// Body represented with structured control flow. This is what llbc is made of. We restructure
    /// the control flow in `ullbc_to_llbc`.
    Structured(llbc_ast::ExprBody),
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
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut, PartialEq, Eq)]
#[charon::variants_suffix("Item")]
pub enum ItemKind {
    /// This item stands on its own.
    TopLevel,
    /// This is a closure in a function body.
    Closure { info: ClosureInfo },
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
    },
    /// This is a vtable value for an impl.
    VTableInstance { impl_ref: TraitImplRef },
}

/// A function definition
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct FunDecl {
    #[drive(skip)]
    pub def_id: FunDeclId,
    /// The meta data associated with the declaration.
    pub item_meta: ItemMeta,
    /// The signature contains the inputs/output types *with* non-erased regions.
    /// It also contains the list of region and type parameters.
    pub signature: FunSig,
    /// The function kind: "regular" function, trait method declaration, etc.
    pub kind: ItemKind,
    /// Whether this function is in fact the body of a constant/static that we turned into an
    /// initializer function.
    pub is_global_initializer: Option<GlobalDeclId>,
    /// The function body, unless the function is opaque.
    /// Opaque functions are: external functions, or local functions tagged
    /// as opaque.
    pub body: Result<Body, Opaque>,
}

/// Reference to a function declaration.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct FunDeclRef {
    pub id: FunDeclId,
    /// Generic arguments passed to the function.
    pub generics: BoxedArgs,
}

#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
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
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct GlobalDecl {
    #[drive(skip)]
    pub def_id: GlobalDeclId,
    /// The meta data associated with the declaration.
    pub item_meta: ItemMeta,
    pub generics: GenericParams,
    pub ty: Ty,
    /// The context of the global: distinguishes top-level items from trait-associated items.
    pub kind: ItemKind,
    /// The kind of global (static or const).
    #[drive(skip)]
    pub global_kind: GlobalKind,
    /// The initializer function used to compute the initial value for this constant/static. It
    /// uses the same generic parameters as the global.
    #[charon::rename("body")]
    pub init: FunDeclId,
}

/// Reference to a global declaration.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Drive, DriveMut)]
pub struct GlobalDeclRef {
    pub id: GlobalDeclId,
    pub generics: BoxedArgs,
}

#[derive(
    Debug, Clone, Serialize, Deserialize, Drive, DriveMut, PartialEq, Eq, Hash, PartialOrd, Ord,
)]
#[drive(skip)]
pub struct TraitItemName(pub String);

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
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
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
    pub parent_clauses: Vector<TraitClauseId, TraitClause>,
    /// The associated constants declared in the trait, along with their type.
    pub consts: Vec<(TraitItemName, Ty)>,
    /// Records associated constants that have a default value.
    #[charon::opaque]
    pub const_defaults: IndexMap<TraitItemName, GlobalDeclRef>,
    /// The associated types declared in the trait.
    pub types: Vec<TraitItemName>,
    /// Records associated types that have a default value.
    #[charon::opaque]
    pub type_defaults: IndexMap<TraitItemName, Ty>,
    /// List of trait clauses that apply to each associated type. This is used during translation,
    /// but the `lift_associated_item_clauses` pass moves them to be parent clauses later. Hence
    /// this is empty after that pass.
    /// TODO: Do this as we translate to avoid the need to store this vector.
    #[charon::opaque]
    pub type_clauses: Vec<(TraitItemName, Vector<TraitClauseId, TraitClause>)>,
    /// The methods declared by the trait. The signature of the methods can be found in each
    /// corresponding `FunDecl`. These `FunDecl` may have a body if the trait provided a default
    /// implementation for that method; otherwise it has an `Opaque` body.
    ///
    /// The binder contains the type parameters specific to the method. The `FunDeclRef` then
    /// provides a full list of arguments to the pointed-to function.
    pub methods: Vec<(TraitItemName, Binder<FunDeclRef>)>,
    /// The virtual table struct for this trait, if it has one.
    /// It is guaranteed that the trait has a vtable iff it is dyn-compatible.
    pub vtable: Option<TypeDeclRef>,
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
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
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
    pub parent_trait_refs: Vector<TraitClauseId, TraitRef>,
    /// The associated constants declared in the trait.
    pub consts: Vec<(TraitItemName, GlobalDeclRef)>,
    /// The associated types declared in the trait.
    pub types: Vec<(TraitItemName, Ty)>,
    /// The `Vec` corresponds to the same `Vector` in `TraitDecl`. In the same way, this is
    /// empty after the `lift_associated_item_clauses` pass.
    #[charon::opaque]
    pub type_clauses: Vec<(TraitItemName, Vector<TraitClauseId, TraitRef>)>,
    /// The implemented methods
    pub methods: Vec<(TraitItemName, Binder<FunDeclRef>)>,
    /// The virtual table instance for this trait implementation. This is `Some` iff the trait is
    /// dyn-compatible.
    pub vtable: Option<GlobalDeclRef>,
}

/// A function operand is used in function calls.
/// It either designates a top-level function, or a place in case
/// we are using function pointers stored in local variables.
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
#[charon::variants_prefix("FnOp")]
pub enum FnOperand {
    /// Regular case: call to a top-level function, trait method, etc.
    Regular(FnPtr),
    /// Use of a function pointer stored in a local variable
    Move(Place),
}

#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct Call {
    pub func: FnOperand,
    pub args: Vec<Operand>,
    pub dest: Place,
}

#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct CopyNonOverlapping {
    pub src: Operand,
    pub dst: Operand,
    pub count: Operand,
}

/// (U)LLBC is a language with side-effects: a statement may abort in a way that isn't tracked by
/// control-flow. The two kinds of abort are:
/// - Panic (may unwind or not depending on compilation setting);
/// - Undefined behavior:
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub enum AbortKind {
    /// A built-in panicking function.
    Panic(Option<Name>),
    /// Undefined behavior in the rust abstract machine.
    UndefinedBehavior,
    /// Unwind had to stop for Abi reasons or because cleanup code panicked again.
    UnwindTerminate,
}

/// Check the value of an operand and abort if the value is not expected. This is introduced to
/// avoid a lot of small branches.
///
/// We translate MIR asserts (introduced for out-of-bounds accesses or divisions by zero for
/// instance) to this. We then eliminate them in [crate::transform::remove_dynamic_checks],
/// because they're implicit in the semantics of our array accesses etc. Finally we introduce new asserts in
/// [crate::transform::reconstruct_asserts].
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
#[charon::rename("Assertion")]
pub struct Assert {
    pub cond: Operand,
    /// The value that the operand should evaluate to for the assert to succeed.
    #[drive(skip)]
    pub expected: bool,
    /// What kind of abort happens on assert failure.
    pub on_failure: AbortKind,
}
