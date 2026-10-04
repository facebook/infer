(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

[@@@warning "-unused-value-declaration"]

val report :
  _ InterproceduralAnalysis.t -> is_suppressed:bool -> latent:bool -> Diagnostic.t -> unit

val report_if_entry_point : _ InterproceduralAnalysis.t -> Trace.t -> Diagnostic.t -> unit

val report_summary_error :
     _ InterproceduralAnalysis.t
  -> PathContext.t
  -> AccessResult.error * AbductiveDomain.Summary.t
  -> _ ExecutionDomain.base_t option
(** [None] means that the execution can continue but we could not compute the continuation state
    (because this only takes a [AccessResult.error], which doesn't have the ok state) *)

val promote_and_report_latent_issues :
     _ InterproceduralAnalysis.t
  -> AbductiveDomain.Summary.t ExecutionDomain.base_t list
  -> AbductiveDomain.Summary.t ExecutionDomain.base_t list
(** To call on the final pre/post pairs of the procedure. Latent issues found by
    [report_summary_error] are only reported here. An issue type at a given location, reached
    through the same calls, that is latent in several pre/post pairs whose disjunction is manifest
    (see {!PulseArithmetic.is_manifest_disjunction}) is reported as a manifest issue instead, and
    these pre/post pairs become [AbortProgram], unless the issue is suppressed in all of them. *)

val report_result :
     _ InterproceduralAnalysis.t
  -> PathContext.t
  -> Location.t
  -> AbductiveDomain.t AccessResult.t
  -> ExecutionDomain.t list

val report_results :
     _ InterproceduralAnalysis.t
  -> PathContext.t
  -> Location.t
  -> AbductiveDomain.t AccessResult.t list
  -> ExecutionDomain.t list

val report_exec_results :
     _ InterproceduralAnalysis.t
  -> PathContext.t
  -> Location.t
  -> ExecutionDomain.t AccessResult.t list
  -> ExecutionDomain.t list
