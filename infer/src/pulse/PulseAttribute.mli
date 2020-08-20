(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module Invalidation = PulseInvalidation
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

type t =
  | AddressOfCppTemporary of Var.t * ValueHistory.t
  | AddressOfStackVariable of Var.t * Location.t * ValueHistory.t
  | Allocated of Procname.t * Trace.t
  (** the {!Procname.t} is the function causing the allocation, eg [malloc] *)
  | AbdAllocated of Procname.t * Trace.t
  | Closure of Procname.t
  | DynamicType of Typ.Name.t
  | EndOfCollection
  | Invalid of Invalidation.t * Trace.t
  | MustBeValid of Trace.t
  | StdVectorReserve
  | WrittenTo of Trace.t
[@@deriving compare]

val pp : F.formatter -> t -> unit

val is_suitable_for_pre : t -> bool

val map_trace : f:(Trace.t -> Trace.t) -> t -> t
(** applies [f] to the traces found in attributes, leaving attributes without traces intact *)

module Set : PrettyPrintable.PPSet with type elt = t

module Attributes : sig
  include PrettyPrintable.PPUniqRankSet with type elt = t

  val get_address_of_stack_variable : t -> (Var.t * Location.t * ValueHistory.t) option

  val get_closure_proc_name : t -> Procname.t option

  val get_abdallocation : t -> (Procname.t * Trace.t) option

  val get_allocation : t -> (Procname.t * Trace.t) option

  val get_dynamic_type : t -> Typ.Name.t option

  val is_end_of_collection : t -> bool

  val get_invalid : t -> (Invalidation.t * Trace.t) option

  val get_must_be_valid : t -> Trace.t option

  val get_written_to : t -> Trace.t option

  val is_modified : t -> bool

  val is_std_vector_reserved : t -> bool

  val is_imply: t -> t -> bool

  val replace_abduce: t -> t -> t

  val is_not_empty_heap : t -> bool
    
end
