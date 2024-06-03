(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type pre_post_list = ExecutionDomain.summary list [@@deriving yojson_of]

type summary = {pre_post_list: pre_post_list; non_disj: NonDisjDomain.Summary.t}
[@@deriving yojson_of]

type t = {main: summary; specialized: summary Specialization.Pulse.Map.t} [@@deriving yojson_of]

val of_posts :
     t InterproceduralAnalysis.t
  -> Specialization.Pulse.t option
  -> Location.t
  -> ExecutionDomain.t list
  -> NonDisjDomain.t
  -> summary

val add_disjunctive_pre_post : ExecutionDomain.summary -> summary -> summary

val empty : summary

val join : summary -> summary -> summary

val force_exit_program :
     t InterproceduralAnalysis.t
  -> Location.t
  -> ExecutionDomain.t
  -> _ ExecutionDomain.base_t SatUnsat.t

val pp : Format.formatter -> t -> unit

val append_objc_actual_self_positive :
     Procname.t
  -> ProcAttributes.t
  -> ((AbstractValue.t * ValueHistory.t) * Typ.t) option
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t SatUnsat.t

val initial_with_positive_self : ProcAttributes.t -> AbductiveDomain.t -> AbductiveDomain.t
(** The initial state of the analysis, with the additional path condition [self > 0] for Objective-C
    and [this>0] for C++ instance methods. *)

val mk_objc_nil_messaging_summary : Tenv.t -> ProcAttributes.t -> ExecutionDomain.summary option

val merge : t -> t -> t
(** Merge specialized summaries. *)

val get_missed_captures : get_summary:(Procname.t -> t option) -> Procname.t list -> Typ.Name.Set.t
