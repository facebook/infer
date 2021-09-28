(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type issue_spec =
  { extract_cost_f: Jsoncost_t.item -> Jsoncost_t.info
  ; name: string
  ; complexity_increase_issue: is_on_ui_thread:bool -> IssueType.t
  ; unreachable_issue: IssueType.t
  ; infinite_issue: IssueType.t
  ; expensive_issue: IssueType.t
  ; top_and_unreachable: bool
  ; expensive: bool }

module CostKindMap : sig
  include PrettyPrintable.PPMap with type key = CostKind.t

  val iter2 : 'a t -> 'b t -> f:(key -> 'a -> 'b -> unit) -> unit
end

val enabled_cost_map : issue_spec CostKindMap.t
