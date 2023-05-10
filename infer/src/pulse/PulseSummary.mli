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

type t = {main: pre_post_list; specialized: pre_post_list Specialization.Pulse.Map.t}
[@@deriving yojson_of]

val of_posts :
  Tenv.t -> Procdesc.t -> Errlog.t -> Location.t -> ExecutionDomain.t list -> pre_post_list

val force_exit_program :
     Tenv.t
  -> Procdesc.t
  -> Errlog.t
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

val initial_with_positive_self :
  Procname.t -> ProcAttributes.t -> AbductiveDomain.t -> AbductiveDomain.t
(** The initial state of the analysis, with the additional path condition [self > 0] for Objective-C
    and [this>0] for C++ instance methods. *)

val mk_objc_nil_messaging_summary :
  Tenv.t -> Procname.t -> ProcAttributes.t -> ExecutionDomain.summary option
