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
      expensive and memory-hungry.

      This type has to be in sync with PayloadId.t *)
  type t =
    { annot_map: AnnotationReachabilityDomain.t Lazy.t option
    ; biabduction: BiabductionSummary.t Lazy.t option
    ; buffer_overrun_analysis: BufferOverrunAnalysisSummary.t Lazy.t option
    ; buffer_overrun_checker: BufferOverrunCheckerSummary.t Lazy.t option
    ; config_impact_analysis: ConfigImpactAnalysis.Summary.t Lazy.t option
    ; cost: CostDomain.summary Lazy.t option
    ; disjunctive_demo: DisjunctiveDemo.domain Lazy.t option
    ; lab_resource_leaks: ResourceLeakDomain.summary Lazy.t option
    ; litho_required_props: LithoDomain.summary Lazy.t option
    ; pulse: PulseSummary.t Lazy.t option
    ; purity: PurityDomain.summary Lazy.t option
    ; racerd: RacerDDomain.summary Lazy.t option
    ; scope_leakage: ScopeLeakage.Summary.t Lazy.t option
    ; siof: SiofDomain.Summary.t Lazy.t option
    ; lineage: Lineage.Summary.t Lazy.t option
    ; lineage_shape: LineageShape.Summary.t Lazy.t option
    ; starvation: StarvationDomain.summary Lazy.t option }
  [@@deriving fields, yojson_of]
end

val pp : Pp.env -> Format.formatter -> t -> unit

val empty : t

val has_payload : PayloadId.t -> t -> bool

val analysis_request_of_field : _ Field.t -> AnalysisRequest.t

module SQLite : sig
  val serialize : t -> old_pulse_payload:Sqlite3.Data.t option -> Sqlite3.Data.t list
  (** serialize payloads, but gets an old Pulse's payload to merge, which was pre-existing in DB *)

  val lazy_load : Database.analysis_table -> proc_uid:string -> t
  (** load each payload lazily *)

  val eager_load : Sqlite3.stmt -> first_column:int -> t
  (** load all payload columns from the statement starting at [first_column] *)

  val deserialize_payload_opt : Sqlite3.Data.t -> 'a Lazy.t option
  (** use to deserialize the data from one column for a particular payload id; callers are
      responsible for casting the result to the appropriate [payload option Lazy.t] type *)
end
