(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type issue_spec =
  { extract_cost_f: Jsonbug_t.cost_item -> Jsonbug_t.cost_info
  ; name: string
  ; threshold: int option
  ; complexity_increase_issue: is_on_cold_start:bool -> is_on_ui_thread:bool -> IssueType.t
  ; expensive_issue: is_on_cold_start:bool -> is_on_ui_thread:bool -> IssueType.t
  ; zero_issue: IssueType.t
  ; infinite_issue: IssueType.t
  ; top_and_bottom: bool }

module CostKindMap = struct
  include PrettyPrintable.MakePPMap (CostKind)

  type no_value = |

  let iter2 map1 map2 ~f =
    let (_ : no_value t) =
      merge
        (fun k v1_opt v2_opt ->
          (match (v1_opt, v2_opt) with Some v1, Some v2 -> f k v1 v2 | _ -> ()) ;
          None )
        map1 map2
    in
    ()
end

let enabled_cost_map =
  List.fold CostKind.enabled_cost_kinds ~init:CostKindMap.empty
    ~f:(fun acc CostKind.{kind; top_and_bottom} ->
      let kind_spec =
        { name= Format.asprintf "The %a" CostKind.pp kind
        ; threshold= (if Config.use_cost_threshold then CostKind.to_threshold kind else None)
        ; extract_cost_f= (fun c -> CostKind.to_json_cost_info c kind)
        ; complexity_increase_issue=
            (fun ~is_on_cold_start ~is_on_ui_thread ->
              IssueType.complexity_increase ~kind ~is_on_cold_start ~is_on_ui_thread )
        ; expensive_issue=
            (fun ~is_on_cold_start ~is_on_ui_thread ->
              IssueType.expensive_cost_call ~kind ~is_on_cold_start ~is_on_ui_thread )
        ; zero_issue= IssueType.zero_cost_call ~kind
        ; infinite_issue= IssueType.infinite_cost_call ~kind
        ; top_and_bottom }
      in
      CostKindMap.add kind kind_spec acc )
