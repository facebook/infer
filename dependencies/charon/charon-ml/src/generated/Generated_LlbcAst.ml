open GAst
open Types
open Values
open Expressions
open Meta
open Identifiers
module StatementId = IdGen ()

type block = { span : span; statements : statement list }

and statement = {
  span : span;
  statement_id : statement_id;
      (** Integer uniquely identifying this statement among the statmeents in
          the current body. To simplify things we generate globally-fresh ids
          when creating a new [Statement]. *)
  kind : statement_kind;
  comments_before : string list;  (** Comments that precede this statement. *)
}

and statement_id = (StatementId.id[@visitors.opaque])

(** A raw statement: a statement without meta data. *)
and statement_kind =
  | Assign of place * rvalue
      (** Assigns an [Rvalue] to a [Place]. e.g. [let y = x;] could become
          [y := move x] which is represented as
          [Assign(y, Rvalue::Use(Operand::Move(x)))]. *)
  | SetDiscriminant of place * variant_id
      (** Not used today because we take MIR built. *)
  | CopyNonOverlapping of copy_non_overlapping
      (** Equivalent to std::intrinsics::copy_nonoverlapping; this is not
          modelled as a function call as it cannot diverge *)
  | StorageLive of local_id
      (** Indicates that this local should be allocated; if it is already
          allocated, this frees the local and re-allocates it. The return value
          and arguments do not receive a [StorageLive]. We ensure in the
          micro-pass [insert_storage_lives] that all other locals have a
          [StorageLive] associated with them. *)
  | StorageDead of local_id
      (** Indicates that this local should be deallocated; if it is already
          deallocated, this is a no-op. A local may not have a [StorageDead] in
          the function's body, in which case it is implicitly deallocated at the
          end of the function. *)
  | PlaceMention of place
      (** A place is mentioned, but not accessed. The place itself must still be
          valid though, so this statement is not a no-op: it can trigger UB if
          the place's projections are not valid (e.g. because they go out of
          bounds). *)
  | Drop of place * trait_ref * drop_kind
      (** Drop the value at the given place.

          Depending on [DropKind], this may be a real call to [drop_in_place],
          or a conditional call that should only happen if the place has not
          been moved out of. See the docs of [DropKind] for more details; to get
          precise drops use [--precise-drops]. *)
  | Assert of assertion * abort_kind
      (** Fields:
          - [assert]
          - [on_failure] *)
  | Call of call
  | Abort of abort_kind
      (** Panic also handles "unreachable". We keep the name of the panicking
          function that was called. *)
  | Return
  | Break of int
      (** Break to outer loops. The [usize] gives the index of the outer loop to
          break to: * 0: break to first outer loop (the current loop) * 1: break
          to second outer loop * ... *)
  | Continue of int
      (** Continue to outer loops. The [usize] gives the index of the outer loop
          to continue to: * 0: continue to first outer loop (the current loop) *
          1: continue to second outer loop * ... *)
  | Nop  (** No-op. *)
  | Switch of switch
  | Loop of block
  | Error of string

and switch =
  | If of operand * block * block
      (** Gives the [if] block and the [else] block. The [Operand] is the
          condition of the [if], e.g. [if (y == 0)] could become
          {@rust[
            v@3 := copy y; // Represented as [Assign(v@3, Use(Copy(y))]
            v@2 := move v@3 == 0; // Represented as [Assign(v@2, BinOp(BinOp::Eq, Move(y), Const(0)))]
            if (move v@2) { // Represented as [If(Move(v@2), <then branch>, <else branch>)]
          ]} *)
  | SwitchInt of operand * literal_type * (literal list * block) list * block
      (** Gives the integer type, a map linking values to switch branches, and
          the otherwise block. Note that matches over enumerations are performed
          by switching over the discriminant, which is an integer. Also, we use
          a [Vec] to make sure the order of the switch branches is preserved.

          Rk.: we use a vector of values, because some of the branches may be
          grouped together, like for the following code:
          {@rust[
            match e {
              E::V1 | E::V2 => ..., // Grouped
              E::V3 => ...
            }
          ]} *)
  | Match of place * (variant_id list * block) list * block option
      (** A match over an ADT.

          The match statement is introduced in
          [crate::transform::resugar::reconstruct_matches] (whenever we find a
          discriminant read, we merge it with the subsequent switch into a
          match). *)
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_statement_base";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_trait_impl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_statement_base";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_trait_impl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]
