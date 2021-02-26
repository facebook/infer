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

val report_list_result :
     PulseSummary.t InterproceduralAnalysis.t
  -> AbductiveDomain.t list access_result
  -> ExecutionDomain.t list

val report_results :
     PulseSummary.t InterproceduralAnalysis.t
  -> ExecutionDomain.t access_result list
  -> ExecutionDomain.t list
