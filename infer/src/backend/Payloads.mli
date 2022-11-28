(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include sig
  (* ignore dead modules added by @@deriving fields *)
  [@@@warning "-unused-module"]

  (** Analysis summaries for inter-procedural analyses.

      When retrieving already-computed summaries, each entry is lazily loaded from the analysis
      database to avoid loading and deserializing payloads unnecessarily, which can be overly
      expensive and memory-hungry. *)
  type t =
    { annot_map: AnnotationReachabilityDomain.t option Lazy.t
    ; biabduction: BiabductionSummary.t option Lazy.t
    ; buffer_overrun_analysis: BufferOverrunAnalysisSummary.t option Lazy.t
    ; buffer_overrun_checker: BufferOverrunCheckerSummary.t option Lazy.t
    ; config_impact_analysis: ConfigImpactAnalysis.Summary.t option Lazy.t
    ; cost: CostDomain.summary option Lazy.t
    ; disjunctive_demo: DisjunctiveDemo.domain option Lazy.t
    ; dotnet_resource_leaks: ResourceLeakCSDomain.summary option Lazy.t
    ; lab_resource_leaks: ResourceLeakDomain.summary option Lazy.t
    ; litho_required_props: LithoDomain.summary option Lazy.t
    ; pulse: PulseSummary.t option Lazy.t
    ; purity: PurityDomain.summary option Lazy.t
    ; quandary: QuandarySummary.t option Lazy.t
    ; racerd: RacerDDomain.summary option Lazy.t
    ; siof: SiofDomain.Summary.t option Lazy.t
    ; simple_lineage: SimpleLineage.Summary.t option Lazy.t
    ; simple_shape: SimpleShape.Summary.t option Lazy.t
    ; starvation: StarvationDomain.summary option Lazy.t
    ; nullsafe: NullsafeSummary.t option Lazy.t
    ; uninit: UninitDomain.Summary.t option Lazy.t }
  [@@deriving fields, yojson_of]
end

val pp : Pp.env -> Format.formatter -> t -> unit

val empty : t

module SQLite : sig
  val serialize : t -> Sqlite3.Data.t list

  val lazy_load : Database.analysis_table -> rowid:int64 -> t
  (** the [rowid] corresponds to the procedure whose payloads should be lazily loaded. We pass the
      SQLite rowid directly to save a lookup in the index of procedure names given that this
      function is only called after the procedure has already been located in the table *)

  val eager_load : Sqlite3.stmt -> first_column:int -> t
  (** load all payload columns from the statement starting at [first_column] *)
end
