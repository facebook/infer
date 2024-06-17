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

  val update : ?add_symops:int -> ?failure_kind:Exception.failure_kind -> t -> t
end

(** summary of a procedure name *)
type t =
  { payloads: Payloads.t
  ; mutable sessions: int  (** Session number: how many nodes went through symbolic execution *)
  ; stats: Stats.t
  ; proc_name: Procname.t
  ; err_log: Errlog.t
        (** Those are issues that are detected for this procedure after per-procedure analysis. In
            addition to that there can be errors detected after file-level analysis (next stage
            after per-procedure analysis). This latter category of errors should NOT be written
            here, use [IssueLog] and its serialization capabilities instead. *)
  ; mutable dependencies: Dependencies.t
        (** Dynamically discovered analysis-time dependencies used to compute this summary *)
  ; mutable is_complete_result: bool  (** If the summary has a complete result *) }
[@@deriving yojson_of]

val pp_html : SourceFile.t -> Format.formatter -> t -> unit
(** Print the summary in html format *)

val pp_text : Format.formatter -> t -> unit
(** Print the summary in text format *)

module OnDisk : sig
  val clear_cache : unit -> unit
  (** Remove all the elements from the cache of summaries *)

  val get : lazy_payloads:bool -> AnalysisRequest.t -> Procname.t -> t option
  (** Return the summary option for the procedure name *)

  val reset : Procname.t -> AnalysisRequest.t -> t
  (** Reset a summary rebuilding the dependents and preserving the proc attributes if present. *)

  val store : AnalysisRequest.t -> t -> t
  (** Save summary for the procedure into the spec database and return it. If the operation fails,
      store an empty summary and return that instead. *)

  val add_errlog : Procname.t -> Errlog.t -> unit
  (** save additional errors in the summary of the given proc name; only use this if you need to
      report errors in a proc name that is *not* the current proc name (in which case just append to
      its own errlog), and if you cannot use a file-level analysis to do this either *)

  val delete_all : procedures:Procname.t list -> unit
  (** Similar to [delete], but delete all summaries for a list of [procedures] *)

  val iter_specs : f:(t -> unit) -> unit
  (** Iterates over all stored summaries *)

  val iter_report_summaries_from_config :
       f:
         (   Procname.t
          -> Location.t
          -> CostDomain.summary option
          -> ConfigImpactAnalysis.Summary.t option
          -> Errlog.t
          -> unit )
    -> unit
  (** Iterates over all analysis artefacts listed above, for each procedure *)

  val get_count : unit -> int
  (** Counts the summaries currently stored on disk. *)
end
