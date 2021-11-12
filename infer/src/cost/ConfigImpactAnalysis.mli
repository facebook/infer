(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_in_strict_mode_paths : SourceFile.t -> bool

val strict_mode : bool

module Fields : AbstractDomain.FiniteSetS

module UncheckedCallee : sig
  type t

  val is_known_expensive : t -> bool

  val make_err_trace : t -> Errlog.loc_trace

  val pp_without_location_list : Format.formatter -> t list -> unit
end

module UncheckedCallees : sig
  include AbstractDomain.FiniteSetS with type elt = UncheckedCallee.t

  val encode : t -> string

  val decode : string -> t

  val pp_without_location : Format.formatter -> t -> unit
end

module ClassGateConditions : sig
  include AbstractDomain.S

  val is_gated : Fields.t -> t -> bool
end

module GatedClasses :
  AbstractDomain.MapS with type key = Typ.Name.t and type value = ClassGateConditions.t

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val get_config_fields : t -> Fields.t

  val get_gated_classes : t -> GatedClasses.t

  val get_unchecked_callees : t -> UncheckedCallees.t

  val instantiate_unchecked_callees_cond : all_config_fields:Fields.t -> t -> t
end

val checker :
     (BufferOverrunAnalysisSummary.t option * Summary.t option * CostDomain.summary option)
     InterproceduralAnalysis.t
  -> Summary.t option
