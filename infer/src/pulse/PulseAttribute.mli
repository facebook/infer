(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module Arithmetic = PulseArithmetic
module Invalidation = PulseInvalidation
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

type t =
  | AddressOfCppTemporary of Var.t * ValueHistory.t
  | AddressOfStackVariable of Var.t * Location.t * ValueHistory.t
  | Arithmetic of Arithmetic.t * Trace.t
  | Closure of Typ.Procname.t
  | Invalid of Invalidation.t * Trace.t
  | MustBeValid of Trace.t
  | StdVectorReserve
  | WrittenTo of Trace.t
[@@deriving compare]

val pp : F.formatter -> t -> unit

module Attributes : sig
  include PrettyPrintable.PPUniqRankSet with type elt = t

  val get_address_of_stack_variable : t -> (Var.t * Location.t * ValueHistory.t) option

  val get_closure_proc_name : t -> Typ.Procname.t option

  val get_arithmetic : t -> (Arithmetic.t * Trace.t) option

  val get_invalid : t -> (Invalidation.t * Trace.t) option

  val get_must_be_valid : t -> Trace.t option

  val get_written_to : t -> Trace.t option

  val is_modified : t -> bool

  val is_std_vector_reserved : t -> bool
end
