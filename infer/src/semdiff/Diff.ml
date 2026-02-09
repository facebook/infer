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


let append_lines line_start line_end ~f ~init =
  match (line_start, line_end) with
  | Some line_start, Some line_end when line_start <= line_end ->
      let range = List.range ~start:`inclusive ~stop:`inclusive line_start line_end in
      List.fold ~init ~f range
  | _, _ ->
      init


let append_removed_lines line_start line_end acc =
  append_lines line_start line_end ~init:acc ~f:(fun acc line -> LineRemoved line :: acc)


let append_added_lines line_start line_end acc =
  append_lines line_start line_end ~init:acc ~f:(fun acc line -> LineAdded line :: acc)


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


let pp_content fmt ~prefix content =
  String.split_on_chars ~on:['\n'] content
  |> List.iteri ~f:(fun i line ->
         let i = i + 1 in
         F.fprintf fmt "%s%2d %s@." (prefix i) i line )


let pp fmt ~previous_content ~current_content diffs =
  let lines_removed, lines_added = split_diffs diffs in
  pp_content fmt ~prefix:(fun i -> if IntSet.mem i lines_removed then "-" else " ") previous_content ;
  pp_content fmt ~prefix:(fun i -> if IntSet.mem i lines_added then "+" else " ") current_content


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

let dummy_explicit = "something wrong"

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


let write_from_json ~json_path ~out_path =
  let raise_invalid msg = Logging.die UserError "semdiff json report invalid format: %s" msg in
  let read_diff_lines lines =
    List.map lines ~f:(function `String line -> line | _ -> raise_invalid "string expected")
  in
  let write_report outf = function
    | `Assoc
        [ ("previous", `String _previous_file)
        ; ("current", `String _current_file)
        ; ("outcome", `String "equal")
        ; ("diff", `List _lines) ] ->
        ()
    | `Assoc
        [ ("previous", `String previous_file)
        ; ("current", `String current_file)
        ; ("outcome", `String "different")
        ; ("diff", `List lines) ] ->
        let lines = read_diff_lines lines in
        F.fprintf outf "%s, %s, SEMDIFF MISMATCH: [%a]@." previous_file current_file
          (Pp.comma_seq F.pp_print_string) lines
    | _ ->
        raise_invalid {|{"previous": "...", "current": "...", "outcome": "...", "diff": [...]}|}
  in
  Utils.with_file_out ~append:true out_path ~f:(fun outf ->
      let fmt = F.formatter_of_out_channel outf in
      Yojson.Safe.from_file json_path |> write_report fmt )
