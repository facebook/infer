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

val report_error :
  Procdesc.t -> Errlog.t -> 'ok access_result -> ('ok, _ ExecutionDomain.base_t SatUnsat.t) result
