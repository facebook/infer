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
  ; tenv: Tenv.t  (** {!Tenv.t} corresponding to the current procedure *)
  ; err_log: Errlog.t
        (** the issue log for the current procedure (internally a mutable data structure) *)
  ; exe_env: Exe_env.t  (** {!Exe_env.t} for the current analysis *)
  ; analyze_dependency: Procname.t -> (Procdesc.t * 'payload) option
        (** On-demand analysis of callees or other dependencies of the analysis of the current
            procedure. Uses [Ondemand.analyze_procedure]. *)
  ; analyze_pdesc_dependency: Procdesc.t -> 'payload option
        (** same as above when we already know the {!Procdesc.t} *)
  ; update_stats: ?add_symops:int -> ?failure_kind:SymOp.failure_kind -> unit -> unit
        (** update the [Summary.Stats.t] of the summary of the current procedure *) }
