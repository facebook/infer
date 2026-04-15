//! "Unstructured LLBC" ast (ULLBC). This is LLBC before the control-flow
//! reconstruction. In effect, this is a cleaned up version of MIR.
pub use crate::ast::*;
use crate::ids::IndexVec;
use derive_generic_visitor::{Drive, DriveMut};
use macros::{EnumAsGetters, EnumIsA, VariantIndexArity, VariantName};
use serde_state::{DeserializeState, SerializeState};

// Block identifier. Similar to rust's `BasicBlock`.
generate_index_type!(BlockId, "Block");

// The entry block of a function is always the block with id 0
pub static START_BLOCK_ID: BlockId = BlockId::ZERO;

#[charon::rename("Blocks")]
pub type BodyContents = IndexVec<BlockId, BlockData>;
pub type ExprBody = GExprBody<BodyContents>;

/// A raw statement: a statement without meta data.
#[derive(
    Debug,
    Clone,
    EnumIsA,
    EnumAsGetters,
    VariantName,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
pub enum StatementKind {
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
    /// A place is mentioned, but not accessed. The place itself must still be valid though, so
    /// this statement is not a no-op: it can trigger UB if the place's projections are not valid
    /// (e.g. because they go out of bounds).
    PlaceMention(Place),
    /// A non-diverging runtime check for a condition. This can be either:
    /// - Emitted for inlined "assumes" (which cause UB on failure)
    /// - Reconstructed from `if b { panic() }` if `--reconstruct-asserts` is set.
    /// This statement comes with the effect that happens when the check fails
    /// (rather than representing it as an unwinding edge).
    Assert {
        assert: Assert,
        on_failure: AbortKind,
    },
    /// Does nothing. Useful for passes.
    Nop,
}

#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
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
    SerializeState,
    DeserializeState,
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
    SwitchInt(LiteralTy, Vec<(Literal, BlockId)>, BlockId),
}

/// A raw terminator: a terminator without meta data.
#[derive(
    Debug, Clone, EnumIsA, EnumAsGetters, SerializeState, DeserializeState, Drive, DriveMut,
)]
pub enum TerminatorKind {
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
    /// Drop the value at the given place.
    ///
    /// Depending on `DropKind`, this may be a real call to `drop_in_place`, or a conditional call
    /// that should only happen if the place has not been moved out of. See the docs of `DropKind`
    /// for more details; to get precise drops use `--precise-drops`.
    Drop {
        #[drive(skip)]
        kind: DropKind,
        place: Place,
        tref: TraitRef,
        target: BlockId,
        on_unwind: BlockId,
    },
    /// Assert that the given condition holds, and if not, unwind to the given block. This is used for
    /// bounds checks, overflow checks, etc.
    #[charon::rename("TAssert")]
    Assert {
        assert: Assert,
        target: BlockId,
        on_unwind: BlockId,
    },
    /// Handles panics and impossible cases.
    Abort(AbortKind),
    Return,
    UnwindResume,
}

#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct Terminator {
    pub span: Span,
    pub kind: TerminatorKind,
    /// Comments that precede this terminator.
    // This is filled in a late pass after all the control-flow manipulation.
    #[drive(skip)]
    pub comments_before: Vec<String>,
}

#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
#[charon::rename("Block")]
pub struct BlockData {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}
