(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include sig
  (* ignore dead modules added by @@deriving fields *)
  [@@@warning "-60"]

  (** analysis results *)
  type t =
    { annot_map: AnnotationReachabilityDomain.t option
    ; biabduction: BiabductionSummary.t option
    ; buffer_overrun_analysis: BufferOverrunAnalysisSummary.t option
    ; buffer_overrun_checker: BufferOverrunCheckerSummary.t option
    ; config_checks_between_markers: ConfigChecksBetweenMarkers.Summary.t option
    ; config_impact_analysis: ConfigImpactAnalysis.Summary.t option
    ; cost: CostDomain.summary option
    ; lab_resource_leaks: ResourceLeakDomain.summary option
    ; dotnet_resource_leaks: ResourceLeakCSDomain.summary option
    ; litho_required_props: LithoDomain.summary option
    ; pulse: PulseSummary.t option
    ; purity: PurityDomain.summary option
    ; quandary: QuandarySummary.t option
    ; racerd: RacerDDomain.summary option
    ; siof: SiofDomain.Summary.t option
    ; starvation: StarvationDomain.summary option
    ; nullsafe: NullsafeSummary.t option
    ; uninit: UninitDomain.Summary.t option }
  [@@deriving fields, yojson_of]
end

val pp : Pp.env -> Format.formatter -> t -> unit

val empty : t
