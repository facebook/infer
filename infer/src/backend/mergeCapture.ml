(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual
module L = Logging

(** Module to merge the results of capture for different buck targets. *)

(** Flag to control whether the timestamp of symbolic links
    is used to determine whether a captured directory needs to be merged. *)
let check_timestamp_of_symlinks = true

let buck_out () = Filename.concat Config.project_root "buck-out"

let modified_targets = ref String.Set.empty

let record_modified_targets_from_file file =
  match Utils.read_file file with
  | Ok targets ->
      modified_targets := List.fold ~f:String.Set.add ~init:String.Set.empty targets
  | Error error ->
      L.user_error "Failed to read modified targets file '%s': %s@." file error ;
      ()


type stats = {mutable files_linked: int; mutable targets_merged: int}

let empty_stats () = {files_linked= 0; targets_merged= 0}

let link_exists s =
  try
    ignore (Unix.lstat s) ;
    true
  with Unix.Unix_error _ -> false


let create_link ~stats src dst =
  if link_exists dst then Unix.unlink dst ;
  Unix.symlink ~src ~dst ;
  (* Set the accessed and modified time of the original file slightly in the past.  Due to
     the coarse precision of the timestamps, it is possible for the source and destination of a
     link to have the same modification time. When this happens, the files will be considered to
     need re-analysis every time, indefinitely. *)
  let near_past = Unix.gettimeofday () -. 1. in
  Unix.utimes src ~access:near_past ~modif:near_past ;
  stats.files_linked <- stats.files_linked + 1


(** Create symbolic links recursively from the destination to the source.
    Replicate the structure of the source directory in the destination,
    with files replaced by links to the source. *)
let rec slink ~stats ~skiplevels src dst =
  L.(debug MergeCapture Verbose) "slink src:%s dst:%s skiplevels:%d@." src dst skiplevels ;
  if Sys.is_directory src = `Yes then (
    if Sys.file_exists dst <> `Yes then Unix.mkdir dst ~perm:0o700 ;
    let items = Sys.readdir src in
    Array.iter
      ~f:(fun item ->
        slink ~stats ~skiplevels:(skiplevels - 1) (Filename.concat src item)
          (Filename.concat dst item) )
      items )
  else if skiplevels > 0 then ()
  else create_link ~stats src dst


(** Determine if the destination should link to the source.
    To check if it was linked before, check if all the captured source files
    from the source are also in the destination.
    And for each of the files inside (.cfg, .cg, etc), check that the destinations
    of symbolic links were not modified after the links themselves. *)
let should_link ~target ~target_results_dir ~stats infer_out_src infer_out_dst =
  let num_captured_files = ref 0 in
  let symlink_up_to_date file =
    let filename = DB.filename_from_string file in
    let time_orig = DB.file_modified_time ~symlink:false filename in
    let time_link = DB.file_modified_time ~symlink:true filename in
    L.(debug MergeCapture Verbose) "file:%s time_orig:%f time_link:%f@." file time_orig time_link ;
    time_link > time_orig
  in
  let symlinks_up_to_date captured_file =
    if Sys.is_directory captured_file = `Yes then
      let contents = Array.to_list (Sys.readdir captured_file) in
      List.for_all
        ~f:(fun file ->
          let file_path = Filename.concat captured_file file in
          Sys.file_exists file_path = `Yes
          && ((not check_timestamp_of_symlinks) || symlink_up_to_date file_path) )
        contents
    else true
  in
  let check_file captured_file =
    Sys.file_exists captured_file = `Yes && symlinks_up_to_date captured_file
  in
  let was_copied () =
    let captured_src = Filename.concat infer_out_src Config.captured_dir_name in
    let captured_dst = Filename.concat infer_out_dst Config.captured_dir_name in
    if Sys.file_exists captured_src = `Yes && Sys.is_directory captured_src = `Yes then (
      let captured_files = Array.to_list (Sys.readdir captured_src) in
      num_captured_files := List.length captured_files ;
      List.for_all ~f:(fun file -> check_file (Filename.concat captured_dst file)) captured_files )
    else true
  in
  let was_modified () = String.Set.mem !modified_targets target in
  let r = was_modified () || not (was_copied ()) in
  if r then stats.targets_merged <- stats.targets_merged + 1 ;
  L.(debug MergeCapture Verbose)
    "lnk:%s:%d %s@."
    (if r then "T" else "F")
    !num_captured_files target_results_dir ;
  if r then L.(debug MergeCapture Medium) "%s@." target_results_dir ;
  r


(** should_link needs to know whether the source file has changed,
    and  to determine whether the destination has never been copied.
    In both cases, perform the link. *)
let process_merge_file deps_file =
  let infer_out_dst = Config.results_dir in
  let stats = empty_stats () in
  let process_line line =
    match Str.split_delim (Str.regexp (Str.quote "\t")) line with
    | target :: _ :: target_results_dir :: _ ->
        let infer_out_src =
          if Filename.is_relative target_results_dir then
            Filename.dirname (buck_out ()) ^/ target_results_dir
          else target_results_dir
        in
        let skiplevels = 2 in
        (* Don't link toplevel files *)
        if should_link ~target ~target_results_dir ~stats infer_out_src infer_out_dst then
          slink ~stats ~skiplevels infer_out_src infer_out_dst
    | _ ->
        ()
  in
  ( match Utils.read_file deps_file with
  | Ok lines ->
      List.iter ~f:process_line lines
  | Error error ->
      L.internal_error "Couldn't read deps file '%s': %s" deps_file error ) ;
  L.progress "Targets merged: %d@\n" stats.targets_merged ;
  L.progress "Files linked: %d@\n" stats.files_linked


let merge_captured_targets () =
  let time0 = Mtime_clock.counter () in
  L.progress "Merging captured Buck targets...@\n%!" ;
  let infer_deps_file = Config.(results_dir ^/ buck_infer_deps_file_name) in
  MergeResults.merge_buck_flavors_results infer_deps_file ;
  process_merge_file infer_deps_file ;
  L.progress "Merging captured Buck targets took %a@\n%!" Mtime.Span.pp (Mtime_clock.count time0)


(* shadowed for tracing *)
let merge_captured_targets () =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"merge buck targets" ())) ;
  merge_captured_targets () ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))
