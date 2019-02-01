(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** analysis results *)
type t =
  { annot_map: AnnotReachabilityDomain.t option
  ; biabduction: BiabductionSummary.t option
  ; buffer_overrun_analysis: BufferOverrunAnalysisSummary.t option
  ; buffer_overrun_checker: BufferOverrunCheckerSummary.t option
  ; class_loads: ClassLoadsDomain.summary option
  ; cost: CostDomain.summary option
  ; lab_resource_leaks: ResourceLeakDomain.summary option
  ; litho: LithoDomain.t option
  ; purity: PurityDomain.summary option
  ; quandary: QuandarySummary.t option
  ; racerd: RacerDDomain.summary option
  ; siof: SiofDomain.Summary.t option
  ; starvation: StarvationDomain.summary option
  ; typestate: TypeState.t option
  ; uninit: UninitDomain.Summary.t option }

val pp : Pp.env -> Format.formatter -> t -> unit

val empty : t
