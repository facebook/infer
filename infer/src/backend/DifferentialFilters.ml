(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module FileRenamings = struct
  type renaming = {current: string; previous: string} [@@deriving compare]

  type t = renaming list [@@deriving compare]

  let equal = [%compare.equal : t]

  let empty = []

  let from_renamings rl : t = rl

  (* A json renaming assoc list looks like:
     [{"current": "aaa.java", "previous": "BBB.java"}, ...] *)
  let from_json input : t =
    let j = Yojson.Basic.from_string input in
    let renaming_of_assoc assoc : renaming =
      match assoc with
      | `Assoc [("current", `String current); ("previous", `String previous)]
       -> {current; previous}
      | _
       -> failwithf "Expected JSON object of the following form: '%s', but instead got: '%s'"
            "{\"current\": \"aaa.java\", \"previous\": \"BBB.java\"}"
            (Yojson.Basic.to_string assoc)
    in
    match j with
    | `List json_renamings
     -> List.map ~f:renaming_of_assoc json_renamings
    | _
     -> failwithf "Expected JSON list but got '%s'" input

  let from_json_file file : t = from_json (In_channel.read_all file)

  let find_previous (t: t) current =
    let r = List.find ~f:(fun r -> String.equal current r.current) t in
    Option.map ~f:(fun r -> r.previous) r

  let pp fmt t =
    let pp_tuple fmt {current; previous} =
      Format.fprintf fmt "{\"current\": \"%s\", \"previous\": \"%s\"}" current previous
    in
    Format.fprintf fmt "[%a]" (Pp.comma_seq pp_tuple) t

  module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY = struct
    let from_renamings = from_renamings

    let equal = equal

    let pp = pp
  end
end

(** Returns a triple [(l1', dups, l2')] where [dups] is the set of elements of that are in the
    intersection of [l1] and [l2] according to [cmd] and additionally satisfy [pred], and [lN'] is
    [lN] minus [dups]. [dups] contains only one witness for each removed issue, taken from [l1]. *)
let relative_complements ~cmp ?(pred= fun _ -> true) l1 l2 =
  let rec aux (out_l1, dups, out_l2 as out) in_l1 in_l2 =
    let is_last_seen_dup v = match dups with ld :: _ -> Int.equal (cmp ld v) 0 | [] -> false in
    match (in_l1, in_l2) with
    | i :: is, f :: fs when Int.equal (cmp i f) 0
     -> (* i = f *)
        if pred i then aux (out_l1, i :: dups, out_l2) is fs
        else aux (i :: out_l1, dups, f :: out_l2) is fs
    | i :: is, f :: _ when cmp i f < 0
     -> (* i < f *)
        let out_l1' = if is_last_seen_dup i then out_l1 else i :: out_l1 in
        aux (out_l1', dups, out_l2) is in_l2
    | _ :: _, f :: fs
     -> (* i > f *)
        let out_l2' = if is_last_seen_dup f then out_l2 else f :: out_l2 in
        aux (out_l1, dups, out_l2') in_l1 fs
    | i :: is, [] when is_last_seen_dup i
     -> aux out is in_l2
    | [], f :: fs when is_last_seen_dup f
     -> aux out in_l1 fs
    | _, _
     -> (List.rev_append in_l1 out_l1, dups, List.rev_append in_l2 out_l2)
  in
  let l1_sorted = List.sort ~cmp l1 in
  let l2_sorted = List.sort ~cmp l2 in
  aux ([], [], []) l1_sorted l2_sorted

type issue_file_with_renaming = Jsonbug_t.jsonbug * string option

let skip_duplicated_types_on_filenames renamings (diff: Differential.t) : Differential.t =
  let compare_issue_file_with_renaming (issue1, previous_file1) (issue2, previous_file2) =
    let f1, f2 =
      ( Option.value previous_file1 ~default:issue1.Jsonbug_t.file
      , Option.value previous_file2 ~default:issue2.Jsonbug_t.file )
    in
    String.compare f1 f2
  in
  let cmp (issue1, _ as issue_with_previous_file1) (issue2, _ as issue_with_previous_file2) =
    [%compare : string * issue_file_with_renaming]
      (issue1.Jsonbug_t.bug_type, issue_with_previous_file1)
      (issue2.Jsonbug_t.bug_type, issue_with_previous_file2)
  in
  let introduced, preexisting, fixed =
    (* All comparisons will be made against filenames *before* renamings.
       This way, all introduced and fixed issues can be sorted independently
       over the same domain. *)
    let introduced_normalized =
      List.map diff.introduced ~f:(fun i ->
          (i, FileRenamings.find_previous renamings i.Jsonbug_t.file) )
    in
    let fixed_normalized = List.map diff.fixed ~f:(fun f -> (f, None)) in
    let introduced_normalized', preexisting', fixed_normalized' =
      relative_complements ~cmp introduced_normalized fixed_normalized
    in
    let list_map_fst = List.map ~f:fst in
    ( list_map_fst introduced_normalized'
    , list_map_fst preexisting' @ diff.preexisting
    , list_map_fst fixed_normalized' )
  in
  {introduced; fixed; preexisting}

let java_anon_class_pattern = Str.regexp "\\$[0-9]+"

type procedure_id = string

let compare_procedure_id pid1 pid2 =
  (* THIS COMPARISON FUNCTION IS INTENDED FOR JAVA ONLY *)
  let normalize_procedure_id pid =
    let anon_token = "$ANON" in
    Str.global_replace java_anon_class_pattern anon_token pid
  in
  let pid1_norm = normalize_procedure_id pid1 in
  let pid2_norm = normalize_procedure_id pid2 in
  (* NOTE: The CRC may swallow some extra chars if the anon class has more
   * digits (e.g. ...$9.abcde():int.A1B2 and ...$10.abcde():in.C1FF), and this
   * makes the 2 strings different.
   * Cut the length to the min_length to match the 2 strings *)
  let pid1_norm_trimmed, pid2_norm_trimmed =
    let min_length = min (String.length pid1_norm) (String.length pid2_norm) in
    (String.sub pid1_norm ~pos:0 ~len:min_length, String.sub pid2_norm ~pos:0 ~len:min_length)
  in
  String.compare pid1_norm_trimmed pid2_norm_trimmed

let value_of_qualifier_tag qts tag =
  match List.find ~f:(fun elem -> String.equal elem.Jsonbug_t.tag tag) qts with
  | Some qt
   -> Some qt.Jsonbug_t.value
  | None
   -> None

type file_extension = string [@@deriving compare]

type weak_hash = string * string * string * int * string option [@@deriving compare]

let skip_anonymous_class_renamings (diff: Differential.t) : Differential.t =
  (*
   * THIS HEURISTIC IS INTENDED FOR JAVA ONLY.
   * Two issues are similar (for the purpose of anonymous class renamings detection)
   * when all of the following apply:
   *  1) they are Java files
   *  2) their weak hashes match
   *  3) their anonymous procedure ids match
   *)
  let string_of_procedure_id issue = DB.strip_crc issue.Jsonbug_t.procedure_id in
  let extension fname = snd (Filename.split_extension fname) in
  let cmp (i1: Jsonbug_t.jsonbug) (i2: Jsonbug_t.jsonbug) =
    [%compare : file_extension option * weak_hash * procedure_id]
      ( extension i1.file
      , ( i1.kind
        , i1.bug_type
        , i1.file
        , i1.key
        , value_of_qualifier_tag i1.qualifier_tags "call_procedure" )
      , string_of_procedure_id i1 )
      ( extension i2.file
      , ( i2.kind
        , i2.bug_type
        , i2.file
        , i2.key
        , value_of_qualifier_tag i2.qualifier_tags "call_procedure" )
      , string_of_procedure_id i2 )
  in
  let pred (issue: Jsonbug_t.jsonbug) =
    let is_java_file () =
      match extension issue.file with
      | Some ext
       -> String.equal (String.lowercase ext) "java"
      | None
       -> false
    in
    let has_anonymous_class_token () =
      try
        ignore (Str.search_forward java_anon_class_pattern issue.procedure_id 0) ;
        true
      with Not_found -> false
    in
    is_java_file () && has_anonymous_class_token ()
  in
  let introduced, preexisting, fixed =
    relative_complements ~cmp ~pred diff.introduced diff.fixed
  in
  {introduced; fixed; preexisting= preexisting @ diff.preexisting}

(* Filter out null dereferences reported by infer if file has eradicate
   enabled, to avoid double reporting. *)
let resolve_infer_eradicate_conflict (analyzer: Config.analyzer)
    (filters_of_analyzer: Config.analyzer -> Inferconfig.filters) (diff: Differential.t)
    : Differential.t =
  let should_discard_issue (issue: Jsonbug_t.jsonbug) =
    let file_is_whitelisted () =
      let source_file = SourceFile.UNSAFE.from_string issue.file in
      let filters = filters_of_analyzer Config.Eradicate in
      filters.path_filter source_file
    in
    Config.equal_analyzer analyzer Config.BiAbduction
    && String.equal issue.bug_type (Localise.to_issue_id Localise.null_dereference)
    && file_is_whitelisted ()
  in
  let filter issues = List.filter ~f:(Fn.non should_discard_issue) issues in
  { introduced= filter diff.introduced
  ; fixed= filter diff.fixed
  ; preexisting= filter diff.preexisting }

(* Strip issues whose paths are not among those we're interested in *)
let interesting_paths_filter (interesting_paths: SourceFile.t list option) =
  match interesting_paths with
  | Some (paths: SourceFile.t list)
   -> let interesting_paths_set =
        paths
        |> List.filter_map ~f:(fun p ->
               if not (SourceFile.is_invalid p) && SourceFile.is_under_project_root p then
                 Some (SourceFile.to_string p)
               else None )
        |> String.Set.of_list
      in
      fun report ->
        List.filter
          ~f:(fun issue -> String.Set.mem interesting_paths_set issue.Jsonbug_t.file)
          report
  | None
   -> Fn.id

let do_filter (diff: Differential.t) (renamings: FileRenamings.t) ~(skip_duplicated_types: bool)
    ~(interesting_paths: SourceFile.t list option) : Differential.t =
  let paths_filter = interesting_paths_filter interesting_paths in
  let apply_paths_filter_if_needed label issues =
    if List.exists ~f:(PVariant.( = ) label) Config.differential_filter_set then
      paths_filter issues
    else issues
  in
  let diff' =
    diff
    |> ( if Config.equal_analyzer Config.analyzer Config.BiAbduction then
           skip_anonymous_class_renamings
       else Fn.id )
    |> (if skip_duplicated_types then skip_duplicated_types_on_filenames renamings else Fn.id)
    |>
    if Config.resolve_infer_eradicate_conflict then
      resolve_infer_eradicate_conflict Config.analyzer Inferconfig.create_filters
    else Fn.id
  in
  { introduced= apply_paths_filter_if_needed `Introduced diff'.introduced
  ; fixed= apply_paths_filter_if_needed `Fixed diff'.fixed
  ; preexisting= apply_paths_filter_if_needed `Preexisting diff'.preexisting }

module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY = struct
  let relative_complements = relative_complements

  let skip_duplicated_types_on_filenames = skip_duplicated_types_on_filenames

  let java_anon_class_pattern = java_anon_class_pattern

  let value_of_qualifier_tag = value_of_qualifier_tag

  let skip_anonymous_class_renamings = skip_anonymous_class_renamings

  let resolve_infer_eradicate_conflict = resolve_infer_eradicate_conflict

  let interesting_paths_filter = interesting_paths_filter
end
