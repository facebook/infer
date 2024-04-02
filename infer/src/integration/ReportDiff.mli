(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val reportdiff :
     report_current:string option
  -> report_previous:string option
  -> costs_current:string option
  -> costs_previous:string option
  -> config_impact_current:string option
  -> config_impact_previous:string option
  -> unit
