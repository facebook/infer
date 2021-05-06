(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Procedure summaries: the results of the capture and all the analysis for a single procedure,
    plus some statistics *)

module Stats : sig
  (** Execution statistics *)
  type t

  val add_visited : t -> int -> unit

  val is_visited : t -> int -> bool

  val update : ?add_symops:int -> ?failure_kind:SymOp.failure_kind -> t -> t
end

module Status : sig
  (** Analysis status of the procedure *)
  type t

  val is_analyzed : t -> bool
end

(** summary of a procedure name *)
type t =
  { payloads: Payloads.t
  ; mutable sessions: int  (** Session number: how many nodes went through symbolic execution *)
  ; stats: Stats.t
  ; status: Status.t
  ; proc_desc: Procdesc.t
  ; err_log: Errlog.t
        (** Those are issues that are detected for this procedure after per-procedure analysis. In
            addition to that there can be errors detected after file-level analysis (next stage
            after per-procedure analysis). This latter category of errors should NOT be written
            here, use [IssueLog] and its serialization capabilities instead. *)
  ; mutable callee_pnames: Procname.Set.t }
[@@deriving yojson_of]

val get_proc_name : t -> Procname.t
(** Get the procedure name *)

val get_proc_desc : t -> Procdesc.t

val get_err_log : t -> Errlog.t

val get_status : t -> Status.t
(** Return the status (active v.s. inactive) of a procedure summary *)

val pp_html : SourceFile.t -> Format.formatter -> t -> unit
(** Print the summary in html format *)

val pp_text : Format.formatter -> t -> unit
(** Print the summary in text format *)

module OnDisk : sig
  val clear_cache : unit -> unit
  (** Remove all the elements from the cache of summaries *)

  val get : Procname.t -> t option
  (** Return the summary option for the procedure name *)

  val reset : Procdesc.t -> t
  (** Reset a summary rebuilding the dependents and preserving the proc attributes if present. *)

  val proc_resolve_attributes : Procname.t -> ProcAttributes.t option
  (** Try to find the attributes for a defined proc. First look at specs (to get attributes computed
      by analysis) then look at the attributes table. If no attributes can be found, return None. *)

  val store_analyzed : t -> unit
  (** Save summary for the procedure into the spec database *)

  val reset_all : filter:Filtering.procedures_filter -> unit -> unit

  val delete : Procname.t -> unit
  (** Delete the .specs file corresponding to the procname and remove its summary from the Summary
      cache *)

  val iter_specs : f:(t -> unit) -> unit
  (** Iterates over all stored summaries *)

  val iter_report_summaries_from_config :
       f:
         (   Procname.t
          -> Location.t
          -> CostDomain.summary option
          -> ConfigImpactAnalysis.Summary.t option
          -> Errlog.t
          -> unit)
    -> unit
  (** Iterates over all analysis artefacts listed above, for each procedure *)

  val pp_specs_from_config : Format.formatter -> unit
  (** pretty print all stored summaries *)
end
