(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

val get_missed_captures :
     get_summary:(Procname.t -> PulseSummary.t option)
  -> SpecializedProcname.t list
  -> TransitiveInfo.MissedCaptures.t
