(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val analyze_procedure :
  StarvationDomain.summary InterproceduralAnalysis.t -> StarvationDomain.summary option

val reporting : StarvationDomain.summary InterproceduralAnalysis.file_t -> IssueLog.t

module ReportMap : sig
  type t

  val empty : t

  val store_multi_file : t -> unit
  (** generate and store issue logs for all source files involved in this report map; for use in the
      whole-program mode only *)
end

val report_on_pair :
     analyze_ondemand:(Procname.t -> StarvationDomain.summary option)
  -> Tenv.t
  -> ProcAttributes.t
  -> StarvationDomain.CriticalPair.t
  -> ReportMap.t
  -> ReportMap.t

val report_on_parallel_composition :
     should_report_starvation:bool
  -> Tenv.t
  -> ProcAttributes.t
  -> StarvationDomain.CriticalPair.t
  -> StarvationDomain.Lock.t
  -> Procname.t
  -> StarvationDomain.CriticalPair.t
  -> ReportMap.t
  -> ReportMap.t
