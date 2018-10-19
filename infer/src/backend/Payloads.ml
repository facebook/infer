(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  { annot_map: AnnotReachabilityDomain.astate option
  ; biabduction: BiabductionSummary.t option
  ; buffer_overrun: BufferOverrunSummary.t option
  ; class_loads: ClassLoadsDomain.summary option
  ; cost: CostDomain.summary option
  ; crashcontext_frame: Stacktree_t.stacktree option
  ; litho: LithoDomain.astate option
  ; purity: PurityDomain.summary option
  ; quandary: QuandarySummary.t option
  ; racerd: RacerDDomain.summary option
  ; resources: ResourceLeakDomain.summary option
  ; siof: SiofDomain.Summary.astate option
  ; starvation: StarvationDomain.summary option
  ; typestate: TypeState.t option
  ; uninit: UninitDomain.Summary.t option }

let pp pe fmt
    { annot_map
    ; biabduction
    ; buffer_overrun
    ; class_loads
    ; cost
    ; crashcontext_frame
    ; litho
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
  F.fprintf fmt "%a%a%a%a%a%a%a%a%a%a%a%a%a%a@\n"
    (pp_opt "Biabduction" (BiabductionSummary.pp pe))
    biabduction (pp_opt "TypeState" TypeState.pp) typestate
    (pp_opt "ClassLoads" ClassLoadsDomain.pp_summary)
    class_loads
    (pp_opt "CrashContext" Crashcontext.pp_stacktree)
    crashcontext_frame
    (pp_opt "Quandary" QuandarySummary.pp)
    quandary
    (pp_opt "Siof" SiofDomain.Summary.pp)
    siof
    (pp_opt "RacerD" RacerDDomain.pp_summary)
    racerd (pp_opt "Litho" LithoDomain.pp) litho
    (pp_opt "BufferOverrun" BufferOverrunSummary.pp)
    buffer_overrun
    (pp_opt "AnnotationReachability" AnnotReachabilityDomain.pp)
    annot_map
    (pp_opt "Uninitialised" UninitDomain.Summary.pp)
    uninit
    (pp_opt "Cost" CostDomain.pp_summary)
    cost
    (pp_opt "Starvation" StarvationDomain.pp_summary)
    starvation
    (pp_opt "Purity" PurityDomain.pp_summary)
    purity


let empty =
  { annot_map= None
  ; biabduction= None
  ; class_loads= None
  ; buffer_overrun= None
  ; crashcontext_frame= None
  ; cost= None
  ; litho= None
  ; purity= None
  ; quandary= None
  ; racerd= None
  ; resources= None
  ; siof= None
  ; starvation= None
  ; typestate= None
  ; uninit= None }
