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

type t = Span.t IString.Map.t

let pp f suppressions =
  if IString.Map.is_empty suppressions then F.pp_print_string f "Empty"
  else IString.Map.iter (fun key data -> F.fprintf f "%s: %a@\n" key Span.pp data) suppressions


type fold_state = {current_line: int; block_start: int option; issue_types: IString.Set.t; res: t}

let pp_fold_state f {current_line; block_start; issue_types; res} =
  F.fprintf f "current_line: %d@\n" current_line ;
  F.fprintf f "block_start: %a@\n" (Pp.option F.pp_print_int) block_start ;
  F.fprintf f "issue_types: [%a]@\n" (Pp.seq F.pp_print_string) (IString.Set.elements issue_types) ;
  F.fprintf f "res: %a@\n" pp res


let update_suppressions_every issue_types suppressions =
  IString.Set.fold
    (fun issue_type m -> IString.Map.add issue_type Span.Every m)
    issue_types suppressions


let update_suppressions block_start current_line issue_types suppressions =
  match block_start with
  | None ->
      suppressions
  | Some start ->
      IString.Set.fold
        (fun issue_type m ->
          IString.Map.update issue_type
            (function
              | None ->
                  Some (Span.Blocks [{first= start; last= current_line}])
              | Some (Span.Blocks b) ->
                  Some (Span.Blocks (b @ [{Span.first= start; last= current_line}]))
              | Some Span.Every as some_every ->
                  (* don't override Every with Blocks *)
                  some_every )
            m )
        issue_types suppressions


let substring_after_match rx s =
  match Str.bounded_split rx s 2 with
  | [] | [_] ->
      None
  | _ :: parts ->
      Some (String.concat ~sep:"," parts)


let regex_cache = ref IString.Map.empty

module ParseResult = struct
  type error = UserError of (unit -> string)

  let pp_error fmt (UserError error) = F.pp_print_string fmt (error ())

  type 'a parse_result = 'a * error list

  let pp_parse_result pp fmt ((x, errors) : _ parse_result) =
    if List.is_empty errors then pp fmt x
    else
      F.fprintf fmt "@[RESULT: @[%a@]@\nERRORS: @[%a@]@]" pp x
        (Pp.semicolon_seq ~print_env:Pp.text_break pp_error)
        errors


  let bind (x, errors) f =
    let x', errors' = f x in
    (x', errors' @ errors)


  let ret x = (x, [])

  let map x f = bind x (fun x' -> ret (f x'))

  let ( >>= ) x f = bind x f

  let ( >>| ) x f = map x f

  let ( let* ) x f = bind x f

  let ( let+ ) x f = map x f

  let error f = ((), [UserError f])

  let result_list_fold l ~init ~f =
    let acc, rev_errors =
      List.fold l ~init:(init, []) ~f:(fun (acc, rev_errors) x ->
          let acc, errors = f acc x in
          let rev_errors = List.rev_append errors rev_errors in
          (acc, rev_errors) )
    in
    (acc, List.rev rev_errors)


  let result_list_filter l ~f =
    result_list_fold l ~init:[] ~f:(fun rev_l' x ->
        let+ b = f x in
        if b then x :: rev_l' else rev_l' )
    >>| List.rev
end

include ParseResult

let get_regex s =
  match IString.Map.find_opt s !regex_cache with
  | None -> (
    try
      let r = Str.regexp s in
      regex_cache := IString.Map.add s r !regex_cache ;
      ret (Some r)
    with _ ->
      let+ () = error (fun () -> F.asprintf "Invalid regex: %s@\n" s) in
      None )
  | Some _ as v ->
      ret v


(* return all matching issue_types for a given regex-string *)
let matching_issue_types s =
  let+ regex = get_regex s in
  match regex with
  | Some rx ->
      IssueType.all_issues ()
      |> List.map ~f:(fun {IssueType.unique_id} -> unique_id)
      |> List.filter ~f:(fun s -> Str.string_match rx s 0)
  | None ->
      []


(* does a given regex-string match any issue_type (but not all of them) *)
let valid_issue_type s =
  let+ matching = matching_issue_types s in
  List.is_empty matching |> not && List.length matching <> List.length (IssueType.all_issues ())


(* return valid bug types / wildcards from a comma-separated string *)
let extract_valid_issue_types s =
  s |> String.split ~on:',' |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> s |> String.is_empty |> not)
  |> ret
  >>= result_list_filter ~f:(fun s ->
          let* valid = valid_issue_type s in
          let+ () =
            if not valid then
              error (fun () -> F.asprintf "%s not a valid issue_type / wildcard@\n" s)
            else ret ()
          in
          valid )
  >>| IString.Set.of_list


(* trailing space intentional *)
let ignore_rx = Str.regexp_string "@infer-ignore "

let ignore_all_rx = Str.regexp_string "@infer-ignore-every "

let parse_lines ?file lines =
  let+ parse_result =
    result_list_fold lines
      ~init:
        {current_line= 1; block_start= None; issue_types= IString.Set.empty; res= IString.Map.empty}
      ~f:(fun {current_line; block_start; issue_types; res} line ->
        let next_line = current_line + 1 in
        match (substring_after_match ignore_all_rx line, substring_after_match ignore_rx line) with
        | None, None ->
            ret
              { current_line= next_line
              ; block_start= None
              ; issue_types= IString.Set.empty
              ; res= update_suppressions block_start current_line issue_types res }
        | Some s, other ->
            let* () =
              if Option.is_some other then
                error (fun () ->
                    F.asprintf "Both @infer-ignore-every and @infer-ignore found in %s line %d@\n"
                      (Option.value ~default:"" file) next_line )
              else ret ()
            in
            let+ valid_issue_types = extract_valid_issue_types s in
            { current_line= next_line
            ; block_start= None
            ; issue_types= IString.Set.empty
            ; res= update_suppressions_every valid_issue_types res }
        | _, Some s ->
            let+ valid_issue_types = extract_valid_issue_types s in
            { current_line= next_line
            ; block_start= (if Option.is_some block_start then block_start else Some current_line)
            ; issue_types= IString.Set.union issue_types valid_issue_types
            ; res } )
  in
  L.debug Report Verbose "Parse state: %a@\n" pp_fold_state parse_result ;
  let res =
    update_suppressions parse_result.block_start parse_result.current_line parse_result.issue_types
      parse_result.res
  in
  L.debug Report Verbose "Parse result: %a@\n" pp res ;
  res


let pp_parse_result fmt parse_result = pp_parse_result pp fmt parse_result

let first_key_match ~suppressions s =
  IString.Map.to_seq suppressions
  |> Seq.find (fun (k, _) ->
         match IString.Map.find_opt k !regex_cache with
         | Some rx ->
             Str.string_match rx s 0
         | None ->
             false )
  |> Option.map ~f:snd


let is_suppressed ~suppressions ~issue_type ~line =
  (*
  Two step lookup process;
  1. simple lookup match (this is the non-wildcard case)
  2. loop trough all keys (treated as regexes) and find first match
*)
  match
    match IString.Map.find_opt issue_type suppressions with
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
