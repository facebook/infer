(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

module AddrTracePair : sig
  type t = AbstractValue.t * ValueHistory.t [@@deriving compare]
end

module Stack : sig
  include PrettyPrintable.MonoMap with type key = Var.t and type value = AddrTracePair.t

  (* need to shadow the declaration in [MonoMap] even though it is unused since [MapS.compare] has a
     different type *)
  val compare : t -> t -> int [@@warning "-32"]
end

module Memory : sig
  module Access :
    PrettyPrintable.PrintableOrderedType with type t = AbstractValue.t HilExp.Access.t

  module Edges : PrettyPrintable.PPMap with type key = Access.t

  type edges = AddrTracePair.t Edges.t

  val pp_edges : F.formatter -> edges -> unit [@@warning "-32"]

  type cell = edges * Attributes.t

  type t

  val filter : (AbstractValue.t -> bool) -> t -> t

  val filter_heap : (AbstractValue.t -> edges -> bool) -> t -> t

  val find_opt : AbstractValue.t -> t -> cell option

  val fold_attrs : (AbstractValue.t -> Attributes.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

  val set_attrs : AbstractValue.t -> Attributes.t -> t -> t

  val set_edges : AbstractValue.t -> edges -> t -> t

  val set_cell : AbstractValue.t -> cell -> t -> t

  val find_edges_opt : AbstractValue.t -> t -> edges option

  val mem_edges : AbstractValue.t -> t -> bool

  val register_address : AbstractValue.t -> t -> t

  val add_edge : AbstractValue.t -> Access.t -> AddrTracePair.t -> t -> t

  val find_edge_opt : AbstractValue.t -> Access.t -> t -> AddrTracePair.t option

  val add_attribute : AbstractValue.t -> Attribute.t -> t -> t

  val invalidate : AbstractValue.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

  val check_valid : AbstractValue.t -> t -> (unit, Invalidation.t Trace.t) result

  val get_closure_proc_name : AbstractValue.t -> t -> Typ.Procname.t option

  val get_constant : AbstractValue.t -> t -> Const.t option

  val std_vector_reserve : AbstractValue.t -> t -> t

  val is_std_vector_reserved : AbstractValue.t -> t -> bool
end

type t = {heap: Memory.t; stack: Stack.t}

val empty : t

include AbstractDomain.NoJoin with type t := t

val reachable_addresses : t -> AbstractValue.Set.t
(** compute the set of abstract addresses that are "used" in the abstract state, i.e. reachable
    from the stack variables *)

type mapping

val empty_mapping : mapping

type isograph_relation =
  | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
  | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

val isograph_map : lhs:t -> rhs:t -> mapping -> isograph_relation

val is_isograph : lhs:t -> rhs:t -> mapping -> bool
