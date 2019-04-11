(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  { annot_map: AnnotReachabilityDomain.t option
  ; biabduction: BiabductionSummary.t option
  ; buffer_overrun_analysis: BufferOverrunAnalysisSummary.t option
  ; buffer_overrun_checker: BufferOverrunCheckerSummary.t option
  ; class_loads: ClassLoadsDomain.summary option
  ; cost: CostDomain.summary option
  ; lab_resource_leaks: ResourceLeakDomain.summary option
  ; litho: LithoDomain.t option
  ; pulse: PulseSummary.t option
  ; purity: PurityDomain.summary option
  ; quandary: QuandarySummary.t option
  ; racerd: RacerDDomain.summary option
  ; siof: SiofDomain.Summary.t option
  ; starvation: StarvationDomain.summary option
  ; typestate: TypeState.t option
  ; uninit: UninitDomain.Summary.t option }

let pp pe fmt
    { annot_map
    ; biabduction
    ; buffer_overrun_analysis
    ; buffer_overrun_checker
    ; class_loads
    ; cost
    ; lab_resource_leaks
    ; litho
    ; pulse
    ; purity
    ; quandary
    ; racerd
    ; siof
    ; starvation
    ; typestate
    ; uninit } =
  let pp_opt prefix pp fmt = function
    | Some x ->
        F.fprintf fmt "%s: %a@\n" prefix pp x
    | None ->
        ()
  in
  F.fprintf fmt "%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a@\n"
    (pp_opt "AnnotationReachability" AnnotReachabilityDomain.pp)
    annot_map
    (pp_opt "Biabduction" (BiabductionSummary.pp pe))
    biabduction
    (pp_opt "BufferOverrunAnalysis" BufferOverrunAnalysisSummary.pp)
    buffer_overrun_analysis
    (pp_opt "BufferOverrunChecker" BufferOverrunCheckerSummary.pp)
    buffer_overrun_checker
    (pp_opt "ClassLoads" ClassLoadsDomain.pp_summary)
    class_loads
    (pp_opt "Cost" CostDomain.pp_summary)
    cost (pp_opt "Litho" LithoDomain.pp) litho (pp_opt "Pulse" PulseSummary.pp) pulse
    (pp_opt "Purity" PurityDomain.pp_summary)
    purity
    (pp_opt "Quandary" QuandarySummary.pp)
    quandary
    (pp_opt "RacerD" RacerDDomain.pp_summary)
    racerd
    (pp_opt "Resource Leaks Lab" ResourceLeakDomain.pp)
    lab_resource_leaks
    (pp_opt "Siof" SiofDomain.Summary.pp)
    siof
    (pp_opt "Starvation" StarvationDomain.pp_summary)
    starvation (pp_opt "TypeState" TypeState.pp) typestate
    (pp_opt "Uninitialised" UninitDomain.Summary.pp)
    uninit


let empty =
  { annot_map= None
  ; biabduction= None
  ; buffer_overrun_analysis= None
  ; buffer_overrun_checker= None
  ; class_loads= None
  ; cost= None
  ; lab_resource_leaks= None
  ; litho= None
  ; pulse= None
  ; purity= None
  ; quandary= None
  ; racerd= None
  ; siof= None
  ; starvation= None
  ; typestate= None
  ; uninit= None }
