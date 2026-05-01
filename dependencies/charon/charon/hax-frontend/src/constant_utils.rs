use crate::prelude::*;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstantInt {
    Int(i128, IntTy),
    Uint(u128, UintTy),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstantLiteral {
    Bool(bool),
    Char(char),
    Float(String, FloatTy),
    Int(ConstantInt),
    PtrNoProvenance(u128),
    Str(String),
    ByteStr(Vec<u8>),
}

sinto_reexport!(rustc_abi::VariantIdx);

/// Describe the kind of a variant
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum VariantKind {
    Struct,
    Union,
    Enum { index: VariantIdx },
}

/// The subset of [Expr] that corresponds to constants.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstantExprKind {
    Literal(ConstantLiteral),
    // Adts (structs, enums, unions) or closures.
    Adt {
        kind: VariantKind,
        fields: Vec<ConstantFieldExpr>,
    },
    Array {
        fields: Vec<ConstantExpr>,
    },
    Tuple {
        fields: Vec<ConstantExpr>,
    },
    /// A top-level or associated constant.
    ///
    /// Remark: constants *can* have generic parameters.
    /// Example:
    /// ```text
    /// struct V<const N: usize, T> {
    ///   x: [T; N],
    /// }
    ///
    /// impl<const N: usize, T> V<N, T> {
    ///   const LEN: usize = N; // This has generics <N, T>
    /// }
    ///
    /// impl Foo for Bar {
    ///   const C : usize = 32; // <-
    /// }
    /// ```
    ///
    /// If `options.inline_anon_consts` is `false`, this is also used for inline const blocks and
    /// advanced const generics expressions.
    NamedGlobal(ItemRef),
    /// A shared reference to a static variable.
    Borrow(ConstantExpr),
    /// A raw borrow (`*const` or `*mut`).
    RawBorrow {
        mutability: Mutability,
        arg: ConstantExpr,
    },
    /// A cast `<source> as <type>`, `<type>` is stored as the type of
    /// the current constant expression. Currently, this is only used
    /// to represent `lit as *mut T` or `lit as *const T`, where `lit`
    /// is a `usize` literal.
    Cast {
        source: ConstantExpr,
    },
    ConstRef {
        id: ParamConst,
    },
    /// A function definition, corresponding to a particular item. This is a ZST, unlike `FnPtr`.
    FnDef(ItemRef),
    /// A function pointer. This is an actual pointer to that function.
    FnPtr(ItemRef),
    /// A blob of memory containing the byte representation of the value. This can occur when
    /// evaluating MIR constants. Interpreting this back to a structured value is left as an
    /// exercice to the consumer.
    Memory(Vec<u8>),
    Todo(String),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConstantFieldExpr {
    pub field: DefId,
    pub value: ConstantExpr,
}

/// Rustc has different representation for constants: one for MIR
/// ([`rustc_middle::mir::Const`]), one for the type system
/// ([`rustc_middle::ty::ConstKind`]). For simplicity hax maps those
/// two construct to one same `ConstantExpr` type.
pub type ConstantExpr = Decorated<ConstantExprKind>;

// For ConstantKind we merge all the cases (Ty, Val, Unevaluated) into one
pub type ConstantKind = ConstantExpr;

pub use self::uneval::*;
mod uneval;
