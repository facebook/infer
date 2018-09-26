(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** analysis results *)
type t =
  { annot_map: AnnotReachabilityDomain.astate option
  ; biabduction: BiabductionSummary.t option
  ; buffer_overrun: BufferOverrunSummary.t option
  ; crashcontext_frame: Stacktree_t.stacktree option
  ; litho: LithoDomain.astate option
  ; quandary: QuandarySummary.t option
  ; racerd: RacerDDomain.summary option
  ; resources: ResourceLeakDomain.summary option
  ; siof: SiofDomain.Summary.astate option
  ; typestate: TypeState.t option
  ; uninit: UninitDomain.summary option
  ; cost: CostDomain.summary option
  ; starvation: StarvationDomain.summary option
  ; purity: PurityDomain.summary option }

val pp : Pp.env -> Format.formatter -> t -> unit

val empty : t
