(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

module Span = struct
  type block = {first: int; last: int} [@@deriving compare, equal]

  type t = Every | Blocks of block list [@@deriving compare, equal]

  let pp_block f {first; last} = F.fprintf f "%d-%d" first last

  let pp f = function
    | Every ->
        F.pp_print_string f "Every"
    | Blocks b ->
        F.fprintf f "Blocks %a" (Pp.seq ~sep:", " pp_block) b
end

type t = Span.t String.Map.t

let pp f suppressions =
  if Map.is_empty suppressions then F.pp_print_string f "Empty"
  else Map.iteri suppressions ~f:(fun ~key ~data -> F.fprintf f "%s: %a@\n" key Span.pp data)


type fold_state = {current_line: int; block_start: int option; issue_types: String.Set.t; res: t}

let pp_fold_state f {current_line; block_start; issue_types; res} =
  F.fprintf f "current_line: %d@\n" current_line ;
  F.fprintf f "block_start: %a@\n" (Pp.option F.pp_print_int) block_start ;
  F.fprintf f "issue_types: [%a]@\n" (Pp.seq F.pp_print_string) (Set.to_list issue_types) ;
  F.fprintf f "res: %a@\n" pp res


let update_suppressions_every issue_types suppressions =
  Set.fold issue_types ~init:suppressions ~f:(fun m issue_type ->
      Map.set m ~key:issue_type ~data:Span.Every )


let update_suppressions block_start current_line issue_types suppressions =
  match block_start with
  | None ->
      suppressions
  | Some start ->
      Set.fold issue_types ~init:suppressions ~f:(fun m issue_type ->
          match Map.find suppressions issue_type with
          | None ->
              Map.set m ~key:issue_type ~data:(Span.Blocks [{first= start; last= current_line}])
          | Some (Span.Blocks b) ->
              Map.set m ~key:issue_type
                ~data:(Span.Blocks (b @ [{Span.first= start; last= current_line}]))
          | Some Span.Every ->
              (* don't override Every with Blocks *)
              m )


let substring_after_match rx s =
  match Str.bounded_split rx s 2 with
  | [] | [_] ->
      None
  | _ :: parts ->
      Some (String.concat ~sep:"," parts)


let regex_cache = ref String.Map.empty

let get_regex s =
  match Map.find !regex_cache s with
  | None -> (
    try
      let r = Str.regexp s in
      regex_cache := Map.set !regex_cache ~key:s ~data:r ;
      Some r
    with _ ->
      L.user_error "Invalid regex: %s@\n" s ;
      None )
  | v ->
      v


(* return all matching issue_types for a given regex-string *)
let matching_issue_types s =
  match get_regex s with
  | Some rx ->
      IssueType.all_issues ()
      |> List.map ~f:(fun {IssueType.unique_id} -> unique_id)
      |> List.filter ~f:(fun s -> Str.string_match rx s 0)
  | None ->
      []


(* does a given regex-string match any issue_type (but not all of them) *)
let valid_issue_type s =
  let matching = matching_issue_types s in
  List.is_empty matching |> not && List.length matching <> List.length (IssueType.all_issues ())


(* return valid bug types / wildcards from a comma-separated string *)
let extract_valid_issue_types s =
  s |> String.split ~on:',' |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> s |> String.is_empty |> not)
  |> List.filter ~f:(fun s ->
         let valid = valid_issue_type s in
         if not valid then L.user_error "%s not a valid issue_type / wildcard@\n" s ;
         valid )
  |> String.Set.of_list


(* trailing space intentional *)
let ignore_rx = Str.regexp_string "@infer-ignore "

let ignore_all_rx = Str.regexp_string "@infer-ignore-every "

let parse_lines ?file lines =
  let parse_result =
    List.fold lines
      ~init:
        {current_line= 1; block_start= None; issue_types= String.Set.empty; res= String.Map.empty}
      ~f:(fun {current_line; block_start; issue_types; res} line ->
        let next_line = current_line + 1 in
        match (substring_after_match ignore_all_rx line, substring_after_match ignore_rx line) with
        | None, None ->
            { current_line= next_line
            ; block_start= None
            ; issue_types= String.Set.empty
            ; res= update_suppressions block_start current_line issue_types res }
        | Some s, other ->
            if Option.is_some other then
              L.user_error "Both @infer-ignore-every and @infer-ignore found in %s line %d@\n"
                (Option.value ~default:"" file) next_line ;
            { current_line= next_line
            ; block_start= None
            ; issue_types= String.Set.empty
            ; res= update_suppressions_every (extract_valid_issue_types s) res }
        | _, Some s ->
            { current_line= next_line
            ; block_start= (if Option.is_some block_start then block_start else Some current_line)
            ; issue_types= Set.union issue_types (extract_valid_issue_types s)
            ; res } )
  in
  L.debug Report Verbose "Parse state: %a@\n" pp_fold_state parse_result ;
  let res =
    update_suppressions parse_result.block_start parse_result.current_line parse_result.issue_types
      parse_result.res
  in
  L.debug Report Verbose "Parse result: %a@\n" pp res ;
  res


let first_key_match ~suppressions s =
  Map.to_sequence suppressions
  |> Sequence.find ~f:(fun (k, _) ->
         match get_regex k with Some rx -> Str.string_match rx s 0 | _ -> false )
  |> Option.map ~f:snd


let is_suppressed ~suppressions ~issue_type ~line =
  (*
  Two step lookup process;
  1. simple lookup match (this is the non-wildcard case)
  2. loop trough all keys (treated as regexes) and find first match
*)
  match
    match Map.find suppressions issue_type with
    | None ->
        first_key_match ~suppressions issue_type
    | v ->
        v
  with
  | None ->
      false
  | Some Span.Every ->
      true
  | Some (Span.Blocks blocks) ->
      List.exists blocks ~f:(fun {Span.first; last} -> first <= line && line <= last)
