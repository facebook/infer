(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Procedure summaries: the results of the capture and all the analysis for a single procedure,
   plus some statistics *)

(** Execution statistics *)
type stats =
  { stats_failure: SymOp.failure_kind option
        (** what type of failure stopped the analysis (if any) *)
  ; symops: int  (** Number of SymOp's throughout the whole analysis of the function *)
  ; mutable nodes_visited_fp: IntSet.t  (** Nodes visited during the footprint phase *)
  ; mutable nodes_visited_re: IntSet.t  (** Nodes visited during the re-execution phase *) }

(** Analysis status of the procedure *)
type status =
  | Pending  (** the summary has been created by the procedure has not been analyzed yet *)
  | Analyzed  (** the analysis of the procedure is finished *)

val equal_status : status -> status -> bool

val string_of_status : status -> string

(** analysis results *)
type payload =
  { annot_map: AnnotReachabilityDomain.astate option
  ; biabduction: BiabductionSummary.t option
  ; buffer_overrun: BufferOverrunDomain.Summary.t option
  ; crashcontext_frame: Stacktree_t.stacktree option
  ; litho: LithoDomain.astate option
  ; quandary: QuandarySummary.t option
  ; racerd: RacerDDomain.summary option
  ; resources: ResourceLeakDomain.summary option
  ; siof: SiofDomain.astate option
  ; typestate: unit TypeState.t option
  ; uninit: UninitDomain.summary option
  ; cost: CostDomain.summary option
  ; starvation: StarvationDomain.summary option }

(** summary of a procedure name *)
type t =
  { payload: payload
  ; sessions: int ref  (** Session number: how many nodes went trough symbolic execution *)
  ; stats: stats
  ; status: status
  ; proc_desc: Procdesc.t }

val dummy : t
(** dummy summary for testing *)

val add : Typ.Procname.t -> t -> unit
(** Add the summary to the table for the given function *)

val has_model : Typ.Procname.t -> bool
(** Check if a summary for a given procedure exists in the models directory *)

val clear_cache : unit -> unit
(** remove all the elements from the cache of summaries *)

val get : Typ.Procname.t -> t option
(** Return the summary option for the procedure name *)

val get_proc_name : t -> Typ.Procname.t
(** Get the procedure name *)

val get_proc_desc : t -> Procdesc.t

val get_attributes : t -> ProcAttributes.t
(** Get the attributes of the procedure. *)

val get_formals : t -> (Mangled.t * Typ.t) list
(** Get the formal parameters of the procedure *)

val get_err_log : t -> Errlog.t

val get_loc : t -> Location.t

val get_signature : t -> string
(** Return the signature of a procedure declaration as a string *)

val get_unsafe : Typ.Procname.t -> t
(** @deprecated Return the summary for the procedure name. Raises an exception when not found. *)

val get_status : t -> status
(** Return the status (active v.s. inactive) of a procedure summary *)

val reset : Procdesc.t -> t
(** Reset a summary rebuilding the dependents and preserving the proc attributes if present. *)

val load_from_file : DB.filename -> t option

val pp_html : SourceFile.t -> Pp.color -> Format.formatter -> t -> unit
(** Print the summary in html format *)

val pp_text : Format.formatter -> t -> unit
(** Print the summary in text format *)

val pdesc_resolve_attributes : Procdesc.t -> ProcAttributes.t
(** Like proc_resolve_attributes but start from a proc_desc. *)

val proc_resolve_attributes : Typ.Procname.t -> ProcAttributes.t option
(** Try to find the attributes for a defined proc.
    First look at specs (to get attributes computed by analysis)
    then look at the attributes table.
    If no attributes can be found, return None.
*)

val proc_is_library : ProcAttributes.t -> bool
(** Check if the procedure is from a library:
    It's not defined, and there is no spec file for it. *)

val store : t -> unit
(** Save summary for the procedure into the spec database *)
