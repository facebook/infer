(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module StringSet = IString.Set
module IntSet = IInt.Set

type t = LineAdded of int | LineRemoved of int [@@deriving compare, equal]

let append_removed_line left_line acc =
  Option.value_map left_line ~default:acc ~f:(fun line -> LineRemoved line :: acc)


let append_added_line right_line acc =
  Option.value_map right_line ~default:acc ~f:(fun line -> LineAdded line :: acc)


let split_diffs diffs =
  List.fold
    ~f:(fun (lines_removed, lines_added) diff ->
      match diff with
      | LineRemoved line_number ->
          (IntSet.add line_number lines_removed, lines_added)
      | LineAdded line_number ->
          (lines_removed, IntSet.add line_number lines_added) )
    ~init:(IntSet.empty, IntSet.empty) diffs


let merge_changes removed added =
  let rec aux removed added acc =
    match (removed, added) with
    | [], [] ->
        List.rev acc
    | a :: as', [] ->
        aux as' [] ((Some a, None) :: acc)
    | [], r :: rs' ->
        aux [] rs' ((None, Some r) :: acc)
    | ((ia, _) as a) :: as', ((ir, _) as r) :: rs' ->
        if Int.equal ia ir then aux as' rs' ((Some a, Some r) :: acc)
        else if ia < ir then aux as' added ((Some a, None) :: acc)
        else aux removed rs' ((None, Some r) :: acc)
  in
  aux added removed []


let gen_explicit_diffs ~previous_content ~current_content diffs =
  let lines_removed, lines_added = split_diffs diffs in
  let lines1 = String.split_on_chars ~on:['\n'] previous_content |> Array.of_list
  and lines2 = String.split_on_chars ~on:['\n'] current_content |> Array.of_list in
  let get_line_content lines n : string =
    if n - 1 < Array.length lines then Array.get lines (n - 1) else ""
  in
  let lines_to_string_set lines =
    List.fold_left ~f:(fun acc (_, s) -> StringSet.add s acc) ~init:StringSet.empty lines
  in
  (* Removes changes from left and right when the only difference is the line number to avoid noise in output *)
  let remove_common_changes list1 list2 =
    let set1 = lines_to_string_set list1 in
    let set2 = lines_to_string_set list2 in
    let common = StringSet.inter set1 set2 in
    let keep (_, s) = not (StringSet.mem s common) in
    (List.filter ~f:keep list1, List.filter ~f:keep list2)
  in
  let lines_removed =
    List.map
      ~f:(fun line_number -> (line_number, get_line_content lines1 line_number))
      (IntSet.to_list lines_removed)
  in
  let lines_added =
    List.map
      ~f:(fun line_number -> (line_number, get_line_content lines2 line_number))
      (IntSet.to_list lines_added)
  in
  let lines_removed, lines_added = remove_common_changes lines_removed lines_added in
  let diffs = merge_changes lines_added lines_removed in
  List.map
    ~f:(fun (line_removed, line_added) ->
      match (line_removed, line_added) with
      | Some (line_number, line_content), None ->
          Printf.sprintf "(Line %d) - %s" line_number line_content
      | Some (line_number, line_content_removed), Some (_, line_content_added) ->
          Printf.sprintf "(Line %d) - %s, + %s" line_number line_content_removed line_content_added
      | None, Some (line_number, line_content_added) ->
          Printf.sprintf "(Line %d) + %s" line_number line_content_added
      | None, None ->
          "" )
    diffs


type explicit = string

let pp_explicit = F.pp_print_string

let write_json ~previous_file ~current_file ~out_path diffs =
  let outcome = if List.is_empty diffs then "equal" else "different" in
  let json =
    `Assoc
      [ ("previous", `String previous_file)
      ; ("current", `String current_file)
      ; ("outcome", `String outcome)
      ; ("diff", `List (List.map ~f:(fun diff -> `String diff) diffs)) ]
  in
  Out_channel.with_file out_path ~f:(fun out_channel -> Yojson.Safe.to_channel out_channel json)
