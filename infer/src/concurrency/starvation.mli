(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val analyze_procedure : Callbacks.proc_callback_t

val reporting : Callbacks.file_callback_t

module ReportMap : sig
  type t

  val empty : t

  val store_multi_file : t -> unit
  (** generate and store issue logs for all source files involved in this report map; for use in the
      whole-program mode only *)
end

val report_on_pair :
  Tenv.t -> Summary.t -> StarvationDomain.CriticalPair.t -> ReportMap.t -> ReportMap.t

val report_on_parallel_composition :
     should_report_starvation:bool
  -> Tenv.t
  -> Procdesc.t
  -> StarvationDomain.CriticalPair.t
  -> StarvationDomain.Lock.t
  -> Procname.t
  -> StarvationDomain.CriticalPair.t
  -> ReportMap.t
  -> ReportMap.t
