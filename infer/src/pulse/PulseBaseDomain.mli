(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module Memory = PulseBaseMemory
module F = Format

type t = {heap: PulseBaseMemory.t; stack: PulseBaseStack.t; attrs: PulseBaseAddressAttributes.t}
[@@deriving compare, equal, yojson_of]

type cell = PulseBaseMemory.Edges.t * Attributes.t

val empty : t

val reachable_addresses : ?var_filter:(Var.t -> bool) -> t -> AbstractValue.Set.t
(** compute the set of abstract addresses that are "used" in the abstract state, i.e. reachable from
    the stack variables *)

val reachable_addresses_from :
  ?already_visited:AbstractValue.Set.t -> AbstractValue.t Seq.t -> t -> AbstractValue.Set.t
(** Compute the set of abstract addresses that are reachable from given abstract addresses. Use
    already_visited as initial set of visited values (empty by default). *)

type mapping

val empty_mapping : mapping

type isograph_relation =
  | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
  | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

val isograph_map : lhs:t -> rhs:t -> mapping -> isograph_relation

val is_isograph : lhs:t -> rhs:t -> mapping -> bool

val find_cell_opt : AbstractValue.t -> t -> cell option

val pp : F.formatter -> t -> unit

val subst_var : for_summary:bool -> AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t

module GraphVisit : sig
  val fold :
       var_filter:(Var.t -> bool)
    -> t
    -> init:'accum
    -> f:
         (   Var.t
          -> 'accum
          -> AbstractValue.t
          -> Memory.Access.t list
          -> ('accum, 'final) Base.Continue_or_stop.t )
    -> finish:('accum -> 'final)
    -> AbstractValue.Set.t * 'final
  (** Generic graph traversal of the memory starting from each variable in the stack that pass
      [var_filter], in order. Returns the result of folding over every address in the graph and the
      set of addresses that have been visited before [f] returned [Stop] or all reachable addresses
      were seen. [f] is passed each address together with the variable from which the address was
      reached and the access path from that variable to the address. *)

  val fold_from_addresses :
       AbstractValue.t Seq.t
    -> t
    -> init:'accum
    -> already_visited:AbstractValue.Set.t
    -> f:
         (   'accum
          -> AbstractValue.t
          -> Memory.Access.t list
          -> ('accum, 'final) Base.Continue_or_stop.t )
    -> finish:('accum -> 'final)
    -> AbstractValue.Set.t * 'final
  (** Similar to [fold], but start from given addresses, instead of stack variables. Use
      already_visited as initial set of visited values. *)
end
