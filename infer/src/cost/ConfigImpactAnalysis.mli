(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type mode = Jsonconfigimpact_t.config_impact_mode [@@deriving equal]

val pp_mode : Format.formatter -> mode -> unit

val is_in_strict_mode_paths : SourceFile.t -> bool

val mode : mode

module LatentConfig : sig
  type t
end

module LatentConfigs : AbstractDomain.FiniteSetS with type elt = LatentConfig.t

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

  val is_gated : LatentConfigs.t -> t -> bool
end

module GatedClasses :
  AbstractDomain.MapS with type key = Typ.Name.t and type value = ClassGateConditions.t

module LatentConfigAlias : sig
  include AbstractDomain.WithBottom

  val empty : t

  val union : t -> t -> t

  val get_all : LatentConfig.t -> t -> LatentConfig.t list
end

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val get_configs : t -> LatentConfigs.t

  val get_gated_classes : t -> GatedClasses.t

  val get_unchecked_callees : t -> UncheckedCallees.t

  val get_latent_config_alias : t -> LatentConfigAlias.t

  val instantiate_unchecked_callees_cond : all_configs:LatentConfigs.t -> t -> t
end

val checker : Summary.t InterproceduralAnalysis.t -> Summary.t option
