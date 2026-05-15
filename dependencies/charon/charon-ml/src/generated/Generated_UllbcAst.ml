open Types
open Values
open Expressions
open Meta
open Identifiers
open GAst
module BlockId = IdGen ()

type block = { statements : statement list; terminator : terminator }
and block_id = (BlockId.id[@visitors.opaque])
and blocks = block list

and statement = {
  span : span;
  kind : statement_kind;
  comments_before : string list;  (** Comments that precede this statement. *)
}

(** A raw statement: a statement without meta data. *)
and statement_kind =
  | Assign of place * rvalue
  | SetDiscriminant of place * variant_id
      (** A call. For now, we don't support dynamic calls (i.e. to a function
          pointer in memory). *)
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
  | Assert of assertion * abort_kind
      (** A non-diverging runtime check for a condition. This can be either:
          - Emitted for inlined "assumes" (which cause UB on failure)
          - Reconstructed from [if b { panic() }] if [--reconstruct-asserts] is
            set. This statement comes with the effect that happens when the
            check fails (rather than representing it as an unwinding edge).

          Fields:
          - [assert]
          - [on_failure] *)
  | Nop  (** Does nothing. Useful for passes. *)

and switch =
  | If of block_id * block_id  (** Gives the [if] block and the [else] block *)
  | SwitchInt of literal_type * (literal * block_id) list * block_id
      (** Gives the integer type, a map linking values to switch branches, and
          the otherwise block. Note that matches over enumerations are performed
          by switching over the discriminant, which is an integer. *)

and terminator = {
  span : span;
  kind : terminator_kind;
  comments_before : string list;  (** Comments that precede this terminator. *)
}

(** A raw terminator: a terminator without meta data. *)
and terminator_kind =
  | Goto of block_id
      (** Fields:
          - [target] *)
  | Switch of operand * switch
      (** Fields:
          - [discr]
          - [targets] *)
  | Call of call * block_id * block_id
      (** Fields:
          - [call]
          - [target]
          - [on_unwind] *)
  | Drop of drop_kind * place * trait_ref * block_id * block_id
      (** Drop the value at the given place.

          Depending on [DropKind], this may be a real call to [drop_in_place],
          or a conditional call that should only happen if the place has not
          been moved out of. See the docs of [DropKind] for more details; to get
          precise drops use [--precise-drops].

          Fields:
          - [kind]
          - [place]
          - [tref]
          - [target]
          - [on_unwind] *)
  | TAssert of assertion * block_id * block_id
      (** Assert that the given condition holds, and if not, unwind to the given
          block. This is used for bounds checks, overflow checks, etc.

          Fields:
          - [assert]
          - [target]
          - [on_unwind] *)
  | Abort of abort_kind  (** Handles panics and impossible cases. *)
  | Return
  | UnwindResume
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_ullbc_ast";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_trait_impl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_ullbc_ast";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_trait_impl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]

(* __REPLACE1__ *)
