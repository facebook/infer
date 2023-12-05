(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val record_load : Exp.t -> Location.t -> PulseExecutionDomain.t list -> PulseExecutionDomain.t list

val record_call :
  Tenv.t -> Procname.t option -> Location.t -> PulseAbductiveDomain.t -> PulseAbductiveDomain.t

val report_errors : Tenv.t -> Procdesc.t -> Errlog.t -> PulseSummary.summary -> unit
