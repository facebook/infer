(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module Invalidation = PulseInvalidation
module PathContext = PulsePathContext
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
  | MustBeInitialized of PathContext.timestamp * Trace.t
  | MustBeValid of PathContext.timestamp * Trace.t * Invalidation.must_be_valid_reason option
  | StdVectorReserve
  | Uninitialized
  | UnreachableAt of Location.t
      (** temporary marker to remember where a variable became unreachable; helps with accurately
          reporting leaks *)
  | WrittenTo of Trace.t
[@@deriving compare]

val pp : F.formatter -> t -> unit

val is_suitable_for_pre : t -> bool

val is_suitable_for_post : t -> bool

module Attributes : sig
  include PrettyPrintable.PPUniqRankSet with type elt = t

  val get_address_of_stack_variable : t -> (Var.t * Location.t * ValueHistory.t) option

  val get_closure_proc_name : t -> Procname.t option

  val get_allocation : t -> (Procname.t * Trace.t) option

  val get_dynamic_type : t -> Typ.t option

  val is_end_of_collection : t -> bool

  val get_invalid : t -> (Invalidation.t * Trace.t) option

  val get_isl_abduced : t -> Trace.t option

  val get_must_be_valid :
    t -> (PathContext.timestamp * Trace.t * Invalidation.must_be_valid_reason option) option

  val get_written_to : t -> Trace.t option

  val is_modified : t -> bool

  val is_std_vector_reserved : t -> bool

  val is_uninitialized : t -> bool

  val get_must_be_initialized : t -> (PathContext.timestamp * Trace.t) option

  val get_unreachable_at : t -> Location.t option

  val isl_subset : t -> t -> bool
  (** check whether for each attr in the second list, there exists a corresponding attr in the first
      according to {!Attributes.isl_equiv}. *)

  val replace_isl_abduced : t -> t -> t
  (** While applying a spec, replacing ISLAbduced by Allocated and Invalidation.Cfree by
      Invalidation.delete, if applicable *)

  val add_call : PathContext.t -> Procname.t -> Location.t -> ValueHistory.t -> t -> t
end
