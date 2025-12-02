//! "Unstructured LLBC" ast (ULLBC). This is LLBC before the control-flow
//! reconstruction. In effect, this is a cleaned up version of MIR.
pub use crate::ast::*;
use derive_generic_visitor::{Drive, DriveMut};
use macros::{EnumAsGetters, EnumIsA, VariantIndexArity, VariantName};
use serde::{Deserialize, Serialize};

// Block identifier. Similar to rust's `BasicBlock`.
generate_index_type!(BlockId, "Block");

// The entry block of a function is always the block with id 0
pub static START_BLOCK_ID: BlockId = BlockId::ZERO;

#[charon::rename("Blocks")]
pub type BodyContents = Vector<BlockId, BlockData>;
pub type ExprBody = GExprBody<BodyContents>;

/// A raw statement: a statement without meta data.
#[derive(
    Debug, Clone, EnumIsA, EnumAsGetters, VariantName, Serialize, Deserialize, Drive, DriveMut,
)]
pub enum RawStatement {
    Assign(Place, Rvalue),
    /// A call. For now, we don't support dynamic calls (i.e. to a function pointer in memory).
    SetDiscriminant(Place, VariantId),
    /// Equivalent to std::intrinsics::copy_nonoverlapping; this is not modelled as a function
    /// call as it cannot diverge
    CopyNonOverlapping(Box<CopyNonOverlapping>),
    /// Indicates that this local should be allocated; if it is already allocated, this frees
    /// the local and re-allocates it. The return value and arguments do not receive a
    /// `StorageLive`. We ensure in the micro-pass `insert_storage_lives` that all other locals
    /// have a `StorageLive` associated with them.
    StorageLive(LocalId),
    /// Indicates that this local should be deallocated; if it is already deallocated, this is
    /// a no-op. A local may not have a `StorageDead` in the function's body, in which case it
    /// is implicitly deallocated at the end of the function.
    StorageDead(LocalId),
    Deinit(Place),
    Drop(Place, TraitRef),
    /// A built-in assert, which corresponds to runtime checks that we remove, namely: bounds
    /// checks, over/underflow checks, div/rem by zero checks, pointer alignement check.
    Assert(Assert),
    /// Does nothing. Useful for passes.
    Nop,
    #[charon::opaque]
    #[drive(skip)]
    Error(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct Statement {
    pub span: Span,
    pub content: RawStatement,
    /// Comments that precede this statement.
    // This is filled in a late pass after all the control-flow manipulation.
    #[drive(skip)]
    pub comments_before: Vec<String>,
}

#[derive(
    Debug,
    Clone,
    EnumIsA,
    EnumAsGetters,
    VariantName,
    VariantIndexArity,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[charon::rename("Switch")]
pub enum SwitchTargets {
    /// Gives the `if` block and the `else` block
    If(BlockId, BlockId),
    /// Gives the integer type, a map linking values to switch branches, and the
    /// otherwise block. Note that matches over enumerations are performed by
    /// switching over the discriminant, which is an integer.
    SwitchInt(IntegerTy, Vec<(ScalarValue, BlockId)>, BlockId),
}

/// A raw terminator: a terminator without meta data.
#[derive(Debug, Clone, EnumIsA, EnumAsGetters, Serialize, Deserialize, Drive, DriveMut)]
pub enum RawTerminator {
    Goto {
        target: BlockId,
    },
    Switch {
        discr: Operand,
        targets: SwitchTargets,
    },
    Call {
        call: Call,
        target: BlockId,
        on_unwind: BlockId,
    },
    /// Handles panics and impossible cases.
    Abort(AbortKind),
    Return,
    UnwindResume,
}

#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct Terminator {
    pub span: Span,
    pub content: RawTerminator,
    /// Comments that precede this terminator.
    // This is filled in a late pass after all the control-flow manipulation.
    #[drive(skip)]
    pub comments_before: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
#[charon::rename("Block")]
pub struct BlockData {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}
