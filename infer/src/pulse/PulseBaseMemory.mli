(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

module Access : sig
  include PrettyPrintable.PrintableOrderedType with type t = AbstractValue.t HilExp.Access.t

  val equal : t -> t -> bool
end

module AddrTrace : sig
    type t = AbstractValue.t * ValueHistory.t
    val pp :  Format.formatter -> t -> unit
end

module Edges : RecencyMap.S with type key = Access.t and type value = AddrTrace.t

include PrettyPrintable.PPMonoMap with type key = AbstractValue.t and type value = Edges.t

val register_address : AbstractValue.t -> t -> t

val remove_address : AbstractValue.t -> t -> t
  
val add_edge : AbstractValue.t -> Access.t -> AddrTrace.t -> t -> t

val find_edge_opt : AbstractValue.t -> Access.t -> t -> AddrTrace.t option

val find_edges_opt : AbstractValue.t -> t -> Edges.t option

val exist_edge_dest : AbstractValue.t -> t -> bool

val yojson_of_t : t -> Yojson.Safe.t

val is_allocated : t -> AbstractValue.t -> bool
(** whether the address has a non-empty set of edges *)

val canonicalize : get_var_repr:(AbstractValue.t -> AbstractValue.t) -> t -> t SatUnsat.t
(** replace each address in the heap by its canonical representative according to the current
    equality relation, represented by [get_var_repr]; also remove addresses that point to empty
    edges *)
