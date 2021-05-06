(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val reportdiff :
     current_report:string option
  -> previous_report:string option
  -> current_costs:string option
  -> previous_costs:string option
  -> current_config_impact:string option
  -> previous_config_impact:string option
  -> unit
