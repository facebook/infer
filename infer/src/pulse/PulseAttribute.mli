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
  | Closure of Procname.t
  | DynamicType of Typ.t
  | EndOfCollection
  | Invalid of Invalidation.t * Trace.t
  | ISLAbduced of Trace.t  (** The allocation is abduced so as the analysis could run normally *)
  | MustBeInitialized of Trace.t
  | MustBeValid of Trace.t
  | StdVectorReserve
  | Uninitialized
  | WrittenTo of Trace.t
[@@deriving compare]

val pp : F.formatter -> t -> unit

val is_suitable_for_pre : t -> bool

val is_suitable_for_post : t -> bool

val map_trace : f:(Trace.t -> Trace.t) -> t -> t
(** applies [f] to the traces found in attributes, leaving attributes without traces intact *)

module Attributes : sig
  include PrettyPrintable.PPUniqRankSet with type elt = t

  val get_address_of_stack_variable : t -> (Var.t * Location.t * ValueHistory.t) option

  val get_closure_proc_name : t -> Procname.t option

  val get_allocation : t -> (Procname.t * Trace.t) option

  val get_dynamic_type : t -> Typ.t option

  val is_end_of_collection : t -> bool

  val get_invalid : t -> (Invalidation.t * Trace.t) option

  val get_isl_abduced : t -> Trace.t option

  val get_must_be_valid : t -> Trace.t option

  val get_written_to : t -> Trace.t option

  val is_modified : t -> bool

  val is_std_vector_reserved : t -> bool

  val is_uninitialized : t -> bool

  val get_must_be_initialized : t -> Trace.t option

  val isl_subset : t -> t -> bool
  (** check whether for each attr in the second list, there exists a corresponding attr in the first
      according to {!Attributes.isl_equiv}. *)

  val replace_isl_abduced : t -> t -> t
  (** While applying a spec, replacing ISLAbduced by Allocated and Invalidation.Cfree by
      Invalidation.delete, if applicable *)
end
