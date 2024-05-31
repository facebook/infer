(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Analysis data for interprocedural analysis. This is the data for one procedure under analysis,
    and callbacks to analyze dependencies of it as well as do bookkeeping regarding the current
    procedure. Basically anything that needs to access the [Summary.t] of the current procedure
    should go here. *)
type 'payload t =
  { proc_desc: Procdesc.t  (** the procedure to analyze *)
  ; tenv: Tenv.t  (** {!IR.Tenv.t} corresponding to the current procedure *)
  ; err_log: Errlog.t
        (** the issue log for the current procedure (internally a mutable data structure) *)
  ; exe_env: Exe_env.t  (** {!Exe_env.t} for the current analysis *)
  ; analyze_dependency: ?specialization:Specialization.t -> Procname.t -> 'payload AnalysisResult.t
        (** On-demand analysis of callees or other dependencies of the analysis of the current
            procedure. Uses [Ondemand.analyze_procedure]. If [specialization] is provided, the
            summary will be improved with a specialized version. *)
  ; add_errlog: Procname.t -> Errlog.t -> unit
        (** {!Summary.OnDisk.add_errlog}: add to the issue log of a foreign procedure (otherwise
            just use [err_log] above) *)
  ; update_stats: ?add_symops:int -> ?failure_kind:Exception.failure_kind -> unit -> unit
        (** update the [Summary.Stats.t] of the summary of the current procedure *) }

val for_procedure : Procdesc.t -> Errlog.t -> 'a t -> 'a t

(** Analysis data for the analysis of a source file. *)
type 'payload file_t =
  { source_file: SourceFile.t  (** the source file under analysis *)
  ; procedures: Procname.t list  (** list of procedures declared in the source file *)
  ; file_exe_env: Exe_env.t  (** {!Exe_env.t} for the current analysis *)
  ; analyze_file_dependency: Procname.t -> 'payload AnalysisResult.t
        (** On-demand analysis of dependencies needed for the file analysis, e.g. the proc names in
            [procedures] *) }

val bind_payload_opt : 'payload1 t -> f:('payload1 -> 'payload2 option) -> 'payload2 t
