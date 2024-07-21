(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

module FileRenamings = struct
  type renaming = {current: string; previous: string}

  module CurrentToPreviousMap = Caml.Map.Make (String)

  type t = string CurrentToPreviousMap.t [@@deriving compare, equal]

  let empty = CurrentToPreviousMap.empty

  let of_renamings ~fold container =
    fold container ~init:empty ~f:(fun acc {current; previous} ->
        CurrentToPreviousMap.add current previous acc )


  (* A json renaming assoc list looks like:
     [{"current": "aaa.java", "previous": "BBB.java"}, ...] *)
  let from_json input : t =
    let j = Yojson.Basic.from_string input in
    let renaming_of_assoc assoc : renaming =
      try
        match assoc with
        | `Assoc l -> (
            let current_opt = List.Assoc.find ~equal:String.equal l "current" in
            let previous_opt = List.Assoc.find ~equal:String.equal l "previous" in
            match (current_opt, previous_opt) with
            | Some (`String current), Some (`String previous) ->
                {current; previous}
            | None, _ ->
                raise (Yojson.Json_error "\"current\" field missing")
            | Some _, None ->
                raise (Yojson.Json_error "\"previous\" field missing")
            | Some _, Some (`String _) ->
                raise (Yojson.Json_error "\"current\" field is not a string")
            | Some _, Some _ ->
                raise (Yojson.Json_error "\"previous\" field is not a string") )
        | _ ->
            raise (Yojson.Json_error "not a record")
      with Yojson.Json_error err ->
        L.(die UserError)
          "Error parsing file renamings: %s@\n\
           Expected JSON object of the following form: '%s', but instead got: '%s'" err
          "{\"current\": \"aaa.java\", \"previous\": \"BBB.java\"}" (Yojson.Basic.to_string assoc)
    in
    match j with
    | `List json_renamings ->
        of_renamings json_renamings ~fold:(IContainer.map ~f:renaming_of_assoc List.fold)
    | _ ->
        L.(die UserError) "Expected JSON list but got '%s'" input


  let from_json_file file : t = from_json (In_channel.read_all file)

  let find_previous (t : t) current =
    try CurrentToPreviousMap.find current t with Caml.Not_found -> current


  module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY = struct
    type nonrec renaming = renaming = {current: string; previous: string}

    let of_list = of_renamings ~fold:List.fold

    let equal = equal

    let pp fmt t =
      let pp_tuple fmt (current, previous) =
        Format.fprintf fmt "{\"current\": \"%s\", \"previous\": \"%s\"}" current previous
      in
      Format.fprintf fmt "[%a]" (Pp.comma_seq pp_tuple) (CurrentToPreviousMap.bindings t)


    let find_previous = find_previous

    let from_json = from_json
  end
end

(** Returns a triple [(l1', dups, l2')] where [dups] is the set of elements of that are in the
    intersection of [l1] and [l2] according to [cmd] and additionally satisfy [pred], and [lN'] is
    [lN] minus [dups]. [dups] contains only one witness for each removed issue, taken from [l1]. *)
let relative_complements ~compare ~pred l1 l2 =
  let rec aux ((out_l1, dups, out_l2) as out) in_l1 in_l2 =
    let is_last_seen_dup v =
      match dups with ld :: _ -> Int.equal (compare ld v) 0 | [] -> false
    in
    match (in_l1, in_l2) with
    | i :: is, f :: fs when Int.equal (compare i f) 0 ->
        (* i = f *)
        if pred i then aux (out_l1, i :: dups, out_l2) is fs
        else aux (i :: out_l1, dups, f :: out_l2) is fs
    | i :: is, _ when is_last_seen_dup i ->
        aux out is in_l2
    | _, f :: fs when is_last_seen_dup f ->
        aux out in_l1 fs
    | i :: is, f :: _ when compare i f < 0 ->
        (* i < f *)
        aux (i :: out_l1, dups, out_l2) is in_l2
    | _ :: _, f :: fs ->
        (* i > f *)
        aux (out_l1, dups, f :: out_l2) in_l1 fs
    | [], _ | _, [] ->
        (List.rev_append in_l1 out_l1, dups, List.rev_append in_l2 out_l2)
  in
  let l1_sorted = List.sort ~compare l1 in
  let l2_sorted = List.sort ~compare l2 in
  aux ([], [], []) l1_sorted l2_sorted


let skip_duplicated_types_on_filenames renamings (diff : Differential.t) : Differential.t =
  let compare (issue1, previous_file1) (issue2, previous_file2) =
    [%compare: Caml.Digest.t option * string * string]
      (issue1.Jsonbug_t.node_key, issue1.Jsonbug_t.bug_type, previous_file1)
      (issue2.Jsonbug_t.node_key, issue2.Jsonbug_t.bug_type, previous_file2)
  in
  let sort_for_print issues =
    let get_fields {Jsonbug_t.file; line; column; bug_type; hash} =
      (file, line, column, bug_type, hash)
    in
    List.sort issues ~compare:(fun issue1 issue2 ->
        [%compare: string * int * int * string * string] (get_fields issue1) (get_fields issue2) )
  in
  let introduced, preexisting, fixed =
    (* All comparisons will be made against filenames *before* renamings.
       This way, all introduced and fixed issues can be sorted independently
       over the same domain. *)
    let introduced_normalized =
      List.map diff.introduced ~f:(fun i ->
          (i, FileRenamings.find_previous renamings i.Jsonbug_t.file) )
    in
    let fixed_normalized = List.map diff.fixed ~f:(fun f -> (f, f.Jsonbug_t.file)) in
    let introduced_normalized', preexisting', fixed_normalized' =
      let has_node_key ({Jsonbug_t.node_key}, _) = Option.is_some node_key in
      relative_complements ~compare ~pred:has_node_key introduced_normalized fixed_normalized
    in
    let list_map_fst = List.map ~f:fst in
    ( list_map_fst introduced_normalized' |> sort_for_print
    , list_map_fst preexisting' @ diff.preexisting |> sort_for_print
    , list_map_fst fixed_normalized' |> sort_for_print )
  in
  {introduced; fixed; preexisting; costs_summary= diff.costs_summary}


type stat = {all: int; mutable filtered_out: int; mutable filtered_out_header: int}

let incr_stat is_interesting_path file stat =
  if not is_interesting_path then (
    stat.filtered_out <- stat.filtered_out + 1 ;
    if String.is_suffix file ~suffix:".h" then
      stat.filtered_out_header <- stat.filtered_out_header + 1 )


let log_to_scuba {all; filtered_out; filtered_out_header} =
  let mk_entry ~label ~value =
    LogEntry.mk_count ~label:(Printf.sprintf "differential_filters.%s" label) ~value
  in
  ScubaLogging.log_many
    [ mk_entry ~label:"all" ~value:all
    ; mk_entry ~label:"filtered_out" ~value:filtered_out
    ; mk_entry ~label:"filtered_out_header" ~value:filtered_out_header ]


(* Strip issues whose paths are not among those we're interested in *)
let interesting_paths_filter (interesting_paths : SourceFile.t list option) =
  match interesting_paths with
  | Some (paths : SourceFile.t list) ->
      let interesting_paths_set =
        paths
        |> List.filter_map ~f:(fun p ->
               if (not (SourceFile.is_invalid p)) && SourceFile.is_under_project_root p then
                 Some (SourceFile.to_string p)
               else None )
        |> String.Set.of_list
      in
      fun ~do_log report ->
        let stat = {all= List.length report; filtered_out= 0; filtered_out_header= 0} in
        let filtered_report =
          List.filter
            ~f:(fun issue ->
              let is_interesting_path = String.Set.mem interesting_paths_set issue.Jsonbug_t.file in
              if do_log then incr_stat is_interesting_path issue.Jsonbug_t.file stat ;
              is_interesting_path )
            report
        in
        if do_log then log_to_scuba stat ;
        filtered_report
  | None ->
      fun ~do_log:_ -> Fn.id


let do_filter (diff : Differential.t) (renamings : FileRenamings.t) ~(skip_duplicated_types : bool)
    ~(interesting_paths : SourceFile.t list option) : Differential.t =
  let paths_filter = interesting_paths_filter interesting_paths in
  let apply_paths_filter_if_needed label issues =
    if List.exists ~f:(PolyVariantEqual.( = ) label) Config.differential_filter_set then
      paths_filter ~do_log:(PolyVariantEqual.( = ) label `Introduced) issues
    else issues
  in
  let diff' =
    if skip_duplicated_types then skip_duplicated_types_on_filenames renamings diff else diff
  in
  { introduced= apply_paths_filter_if_needed `Introduced diff'.introduced
  ; fixed= apply_paths_filter_if_needed `Fixed diff'.fixed
  ; preexisting= apply_paths_filter_if_needed `Preexisting diff'.preexisting
  ; costs_summary= diff'.costs_summary }


module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY = struct
  let relative_complements = relative_complements

  let skip_duplicated_types_on_filenames = skip_duplicated_types_on_filenames

  let interesting_paths_filter interesting_paths report =
    interesting_paths_filter interesting_paths ~do_log:false report
end
