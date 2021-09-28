(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  { introduced: Jsonbug_t.report
  ; fixed: Jsonbug_t.report
  ; preexisting: Jsonbug_t.report
  ; costs_summary: Yojson.Basic.t }

val issues_of_reports :
     current_report:Jsonbug_t.report
  -> previous_report:Jsonbug_t.report
  -> current_costs:Jsoncost_t.report
  -> previous_costs:Jsoncost_t.report
  -> current_config_impact:Jsonconfigimpact_t.report
  -> previous_config_impact:Jsonconfigimpact_t.report
  -> t

val to_files : t -> string -> unit
