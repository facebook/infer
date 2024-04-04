(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

module type S = sig
  type key

  type in_map_t

  type out_of_map_t

  module Access : Access.S with type key := key

  module Edges : sig
    include RecencyMap.S with type key = Access.t and type value = out_of_map_t

    val mapi : t -> f:(key -> out_of_map_t -> in_map_t) -> t

    val canonicalize : get_var_repr:(AbstractValue.t -> AbstractValue.t) -> t -> t
  end

  include PrettyPrintable.PPMonoMap with type key := key and type value = Edges.t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val register_address : key -> t -> t

  val add_edge : key -> Access.t -> in_map_t -> t -> t

  val find_edge_opt :
       ?get_var_repr:(AbstractValue.t -> AbstractValue.t)
    -> key
    -> Access.t
    -> t
    -> out_of_map_t option

  val has_edge : key -> Access.t -> t -> bool

  val is_allocated : t -> key -> bool
  (** whether the address has a non-empty set of edges *)
end

include
  S
    with type key := AbstractValue.t
     and type out_of_map_t := AbstractValue.t * ValueHistory.t
     and type in_map_t := AbstractValue.t * ValueHistory.t

val yojson_of_t : t -> Yojson.Safe.t

val canonicalize : get_var_repr:(AbstractValue.t -> AbstractValue.t) -> t -> t SatUnsat.t
(** replace each address in the heap by its canonical representative according to the current
    equality relation, represented by [get_var_repr]; also remove addresses that point to empty
    edges *)

val subst_var : for_summary:bool -> AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t
