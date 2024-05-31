(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface

[@@@warning "-unused-value-declaration"]

val report :
  _ InterproceduralAnalysis.t -> is_suppressed:bool -> latent:bool -> Diagnostic.t -> unit

val report_summary_error :
     _ InterproceduralAnalysis.t
  -> AccessResult.error * AbductiveDomain.Summary.t
  -> _ ExecutionDomain.base_t option
(** [None] means that the execution can continue but we could not compute the continuation state
    (because this only takes a [AccessResult.error], which doesn't have the ok state) *)

val report_result :
     _ InterproceduralAnalysis.t
  -> Location.t
  -> AbductiveDomain.t AccessResult.t
  -> ExecutionDomain.t list

val report_results :
     _ InterproceduralAnalysis.t
  -> Location.t
  -> AbductiveDomain.t AccessResult.t list
  -> ExecutionDomain.t list

val report_exec_results :
     _ InterproceduralAnalysis.t
  -> Location.t
  -> ExecutionDomain.t AccessResult.t list
  -> ExecutionDomain.t list
