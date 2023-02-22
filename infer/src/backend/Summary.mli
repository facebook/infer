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

module Deps : sig
  type partial = Procname.HashSet.t

  type complete =
    { callees: Procname.t list
          (** Summaries of these procedures were used to compute this summary. *)
    ; used_tenv_sources: SourceFile.t list
          (** These source files contain type definitions that were used to compute this summary. *)
    }

  type t =
    | Partial of partial
    | Complete of complete
        (** Dependencies are [partial] and mutable while the summary to which they belong is being
            computed, then made [complete] and immutable once the summary is fully analyzed. *)

  val add_exn : t -> Procname.t -> unit
  (** Adds a procname to a partial dependency set ; raises an error if the given dependencies have
      been marked as complete *)
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
  ; mutable dependencies: Deps.t
        (** Dynamically discovered analysis-time dependencies used to compute this summary *) }
[@@deriving yojson_of]

val pp_html : SourceFile.t -> Format.formatter -> t -> unit
(** Print the summary in html format *)

val pp_text : Format.formatter -> t -> unit
(** Print the summary in text format *)

module OnDisk : sig
  val clear_cache : unit -> unit
  (** Remove all the elements from the cache of summaries *)

  val get : lazy_payloads:bool -> Procname.t -> t option
  (** Return the summary option for the procedure name *)

  val reset : Procname.t -> t
  (** Reset a summary rebuilding the dependents and preserving the proc attributes if present. *)

  val store : t -> unit
  (** Save summary for the procedure into the spec database *)

  val delete : Procname.t -> unit
  (** Delete the .specs file corresponding to the procname and remove its summary from the Summary
      cache *)

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
