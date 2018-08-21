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
  ; buffer_overrun: BufferOverrunDomain.Summary.t option
  ; crashcontext_frame: Stacktree_t.stacktree option
  ; litho: LithoDomain.astate option
  ; quandary: QuandarySummary.t option
  ; racerd: RacerDDomain.summary option
  ; resources: ResourceLeakDomain.summary option
  ; siof: SiofDomain.Summary.astate option
  ; typestate: TypeState.t option
  ; uninit: UninitDomain.summary option
  ; cost: CostDomain.summary option
  ; starvation: StarvationDomain.summary option }

let pp pe fmt
    { biabduction
    ; typestate
    ; crashcontext_frame
    ; quandary
    ; siof
    ; racerd
    ; litho
    ; buffer_overrun
    ; annot_map
    ; uninit
    ; cost
    ; starvation } =
  let pp_opt prefix pp fmt = function
    | Some x ->
        F.fprintf fmt "%s: %a@\n" prefix pp x
    | None ->
        ()
  in
  F.fprintf fmt "%a%a%a%a%a%a%a%a%a%a%a%a@\n"
    (pp_opt "Biabduction" (BiabductionSummary.pp pe))
    biabduction (pp_opt "TypeState" TypeState.pp) typestate
    (pp_opt "CrashContext" Crashcontext.pp_stacktree)
    crashcontext_frame
    (pp_opt "Quandary" QuandarySummary.pp)
    quandary
    (pp_opt "Siof" SiofDomain.Summary.pp)
    siof
    (pp_opt "RacerD" RacerDDomain.pp_summary)
    racerd (pp_opt "Litho" LithoDomain.pp) litho
    (pp_opt "BufferOverrun" BufferOverrunDomain.Summary.pp)
    buffer_overrun
    (pp_opt "AnnotationReachability" AnnotReachabilityDomain.pp)
    annot_map
    (pp_opt "Uninitialised" UninitDomain.pp_summary)
    uninit
    (pp_opt "Cost" CostDomain.pp_summary)
    cost
    (pp_opt "Starvation" StarvationDomain.pp_summary)
    starvation


let empty =
  { biabduction= None
  ; typestate= None
  ; annot_map= None
  ; crashcontext_frame= None
  ; quandary= None
  ; resources= None
  ; siof= None
  ; racerd= None
  ; litho= None
  ; buffer_overrun= None
  ; uninit= None
  ; cost= None
  ; starvation= None }
