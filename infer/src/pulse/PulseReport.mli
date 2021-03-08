(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface

val report_result :
     PulseSummary.t InterproceduralAnalysis.t
  -> AbductiveDomain.t AccessResult.t
  -> ExecutionDomain.t list

val report_results :
     PulseSummary.t InterproceduralAnalysis.t
  -> AbductiveDomain.t AccessResult.t list
  -> ExecutionDomain.t list

val report_exec_results :
     PulseSummary.t InterproceduralAnalysis.t
  -> ExecutionDomain.t AccessResult.t list
  -> ExecutionDomain.t list
