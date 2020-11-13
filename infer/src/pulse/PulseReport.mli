(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type 'a access_result = ('a, Diagnostic.t * AbductiveDomain.t) result

type 'a path_feasibility = InfeasiblePath | FeasiblePath of 'a

val report_error :
     Procdesc.t
  -> Errlog.t
  -> 'ok access_result
  -> ('ok, _ ExecutionDomain.base_t path_feasibility) result
