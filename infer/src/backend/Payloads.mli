(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** analysis results *)
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
  ; typestate: unit TypeState.t option
  ; uninit: UninitDomain.summary option
  ; cost: CostDomain.summary option
  ; starvation: StarvationDomain.summary option }

val pp : Pp.env -> Format.formatter -> t -> unit

val empty : t
