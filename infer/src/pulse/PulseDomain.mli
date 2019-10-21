(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

module AbstractAddress : sig
  type t = private int [@@deriving compare]

  val equal : t -> t -> bool

  val init : unit -> unit

  val pp : F.formatter -> t -> unit [@@warning "-32"]

  val mk_fresh : unit -> t

  type state

  val get_state : unit -> state

  val set_state : state -> unit
end

module AbstractAddressSet : PrettyPrintable.PPSet with type elt = AbstractAddress.t

module AbstractAddressMap : PrettyPrintable.PPMap with type key = AbstractAddress.t

module AddrTracePair : sig
  type t = AbstractAddress.t * ValueHistory.t [@@deriving compare]
end

module Stack : sig
  include PrettyPrintable.MonoMap with type key = Var.t and type value = AddrTracePair.t

  (* need to shadow the declaration in [MonoMap] even though it is unused since [MapS.compare] has a
     different type *)
  val compare : t -> t -> int [@@warning "-32"]
end

module Memory : sig
  module Access :
    PrettyPrintable.PrintableOrderedType with type t = AbstractAddress.t HilExp.Access.t

  module Edges : PrettyPrintable.PPMap with type key = Access.t

  type edges = AddrTracePair.t Edges.t

  val pp_edges : F.formatter -> edges -> unit [@@warning "-32"]

  type cell = edges * Attributes.t

  type t

  val filter : (AbstractAddress.t -> bool) -> t -> t

  val filter_heap : (AbstractAddress.t -> edges -> bool) -> t -> t

  val find_opt : AbstractAddress.t -> t -> cell option

  val fold_attrs : (AbstractAddress.t -> Attributes.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

  val set_attrs : AbstractAddress.t -> Attributes.t -> t -> t

  val set_edges : AbstractAddress.t -> edges -> t -> t

  val set_cell : AbstractAddress.t -> cell -> t -> t

  val find_edges_opt : AbstractAddress.t -> t -> edges option

  val mem_edges : AbstractAddress.t -> t -> bool

  val register_address : AbstractAddress.t -> t -> t

  val add_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val find_edge_opt : AbstractAddress.t -> Access.t -> t -> AddrTracePair.t option

  val add_attribute : AbstractAddress.t -> Attribute.t -> t -> t

  val invalidate : AbstractAddress.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

  val check_valid : AbstractAddress.t -> t -> (unit, Invalidation.t Trace.t) result

  val get_closure_proc_name : AbstractAddress.t -> t -> Typ.Procname.t option

  val get_constant : AbstractAddress.t -> t -> Const.t option

  val std_vector_reserve : AbstractAddress.t -> t -> t

  val is_std_vector_reserved : AbstractAddress.t -> t -> bool
end

type t = {heap: Memory.t; stack: Stack.t}

val empty : t

include AbstractDomain.NoJoin with type t := t

val reachable_addresses : t -> AbstractAddressSet.t
(** compute the set of abstract addresses that are "used" in the abstract state, i.e. reachable
    from the stack variables *)

type mapping

val empty_mapping : mapping

type isograph_relation =
  | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
  | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

val isograph_map : lhs:t -> rhs:t -> mapping -> isograph_relation

val is_isograph : lhs:t -> rhs:t -> mapping -> bool
