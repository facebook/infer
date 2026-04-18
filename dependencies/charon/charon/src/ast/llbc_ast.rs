//! LLBC
//!
//! MIR code where we have rebuilt the control-flow (`if ... then ... else ...`,
//! `while ...`, ...).
//!
//! Also note that we completely break the definitions Statement and Terminator
//! from MIR to use Statement only.

pub use super::llbc_ast_utils::*;
pub use crate::ast::*;
use derive_generic_visitor::{Drive, DriveMut};
use macros::{EnumAsGetters, EnumIsA, EnumToGetters, VariantIndexArity, VariantName};
use serde_state::{DeserializeState, SerializeState};

generate_index_type!(StatementId);

/// A raw statement: a statement without meta data.
#[derive(
    Debug,
    Clone,
    EnumIsA,
    EnumToGetters,
    EnumAsGetters,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
pub enum StatementKind {
    /// Assigns an `Rvalue` to a `Place`. e.g. `let y = x;` could become
    /// `y := move x` which is represented as `Assign(y, Rvalue::Use(Operand::Move(x)))`.
    Assign(Place, Rvalue),
    /// Not used today because we take MIR built.
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
    /// Drop the value at the given place.
    ///
    /// Depending on `DropKind`, this may be a real call to `drop_in_place`, or a conditional call
    /// that should only happen if the place has not been moved out of. See the docs of `DropKind`
    /// for more details; to get precise drops use `--precise-drops`.
    Drop(Place, TraitRef, #[drive(skip)] DropKind),
    Assert {
        assert: Assert,
        on_failure: AbortKind,
    },
    Call(Call),
    /// Panic also handles "unreachable". We keep the name of the panicking function that was
    /// called.
    Abort(AbortKind),
    Return,
    /// Break to outer loops.
    /// The `usize` gives the index of the outer loop to break to:
    /// * 0: break to first outer loop (the current loop)
    /// * 1: break to second outer loop
    /// * ...
    #[drive(skip)]
    Break(usize),
    /// Continue to outer loops.
    /// The `usize` gives the index of the outer loop to continue to:
    /// * 0: continue to first outer loop (the current loop)
    /// * 1: continue to second outer loop
    /// * ...
    #[drive(skip)]
    Continue(usize),
    /// No-op.
    Nop,
    Switch(Switch),
    Loop(Block),
    #[drive(skip)]
    Error(String),
}

#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
pub struct Statement {
    pub span: Span,
    /// Integer uniquely identifying this statement among the statmeents in the current body. To
    /// simplify things we generate globally-fresh ids when creating a new `Statement`.
    #[charon::rename("statement_id")]
    pub id: StatementId,
    pub kind: StatementKind,
    /// Comments that precede this statement.
    // This is filled in a late pass after all the control-flow manipulation.
    #[drive(skip)]
    pub comments_before: Vec<String>,
}

#[derive(Debug, Clone, SerializeState, DeserializeState, Drive, DriveMut)]
#[serde_state(state_implements = HashConsSerializerState)] // Avoid corecursive impls due to perfect derive
pub struct Block {
    pub span: Span,
    pub statements: Vec<Statement>,
}

#[derive(
    Debug,
    Clone,
    EnumIsA,
    EnumToGetters,
    EnumAsGetters,
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
    VariantName,
    VariantIndexArity,
)]
pub enum Switch {
    /// Gives the `if` block and the `else` block. The `Operand` is the condition of the `if`, e.g. `if (y == 0)` could become
    /// ```text
    /// v@3 := copy y; // Represented as `Assign(v@3, Use(Copy(y))`
    /// v@2 := move v@3 == 0; // Represented as `Assign(v@2, BinOp(BinOp::Eq, Move(y), Const(0)))`
    /// if (move v@2) { // Represented as `If(Move(v@2), <then branch>, <else branch>)`
    /// ```
    If(Operand, Block, Block),
    /// Gives the integer type, a map linking values to switch branches, and the
    /// otherwise block. Note that matches over enumerations are performed by
    /// switching over the discriminant, which is an integer.
    /// Also, we use a `Vec` to make sure the order of the switch
    /// branches is preserved.
    ///
    /// Rk.: we use a vector of values, because some of the branches may
    /// be grouped together, like for the following code:
    /// ```text
    /// match e {
    ///   E::V1 | E::V2 => ..., // Grouped
    ///   E::V3 => ...
    /// }
    /// ```
    SwitchInt(Operand, LiteralTy, Vec<(Vec<Literal>, Block)>, Block),
    /// A match over an ADT.
    ///
    /// The match statement is introduced in [crate::transform::resugar::reconstruct_matches]
    /// (whenever we find a discriminant read, we merge it with the subsequent
    /// switch into a match).
    Match(Place, Vec<(Vec<VariantId>, Block)>, Option<Block>),
}

pub type ExprBody = GExprBody<Block>;
