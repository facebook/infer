(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PulseBasicInterface

module Access : sig
  include PrettyPrintable.PrintableOrderedType with type t = AbstractValue.t HilExp.Access.t

  val equal : t -> t -> bool
end

module Edges : PrettyPrintable.PPMap with type key = Access.t

type edges = (AbstractValue.t * ValueHistory.t) Edges.t

type cell = edges * Attributes.t

type t

val empty : t

val filter : (AbstractValue.t -> bool) -> t -> t

val filter_heap : (AbstractValue.t -> edges -> bool) -> t -> t

val find_opt : AbstractValue.t -> t -> cell option

val fold_attrs : (AbstractValue.t -> Attributes.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val set_attrs : AbstractValue.t -> Attributes.t -> t -> t

val set_edges : AbstractValue.t -> edges -> t -> t

val set_cell : AbstractValue.t -> cell -> t -> t

val find_edges_opt : AbstractValue.t -> t -> edges option

val mem_edges : AbstractValue.t -> t -> bool

val pp_heap : F.formatter -> t -> unit

val pp_attributes : F.formatter -> t -> unit

val register_address : AbstractValue.t -> t -> t

val add_edge : AbstractValue.t -> Access.t -> AbstractValue.t * ValueHistory.t -> t -> t

val find_edge_opt : AbstractValue.t -> Access.t -> t -> (AbstractValue.t * ValueHistory.t) option

val add_attribute : AbstractValue.t -> Attribute.t -> t -> t

val invalidate : AbstractValue.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

val check_valid : AbstractValue.t -> t -> (unit, Invalidation.t * Trace.t) result

val get_closure_proc_name : AbstractValue.t -> t -> Typ.Procname.t option

val get_arithmetic : AbstractValue.t -> t -> (Arithmetic.t * Trace.t) option

val get_must_be_valid : AbstractValue.t -> t -> Trace.t option

val std_vector_reserve : AbstractValue.t -> t -> t

val is_std_vector_reserved : AbstractValue.t -> t -> bool
