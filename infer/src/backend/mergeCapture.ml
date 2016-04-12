(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format

(** Module to merge the results of capture for different buck targets. *)

(** Flag to control whether the timestamp of symbolic links
    is used to determine whether a captured directory needs to be merged. *)
let check_timestamp_of_symlinks = true

let buck_out () =
  match !Config.project_root with
  | Some root ->
      Filename.concat root "buck-out"
  | None ->
      Filename.concat (Filename.dirname !Config.results_dir) "buck-out"

let infer_deps () = Filename.concat !Config.results_dir "infer-deps.txt"

let modified_targets = ref StringSet.empty

let modified_file file = match Utils.read_file file with
  | Some targets ->
      modified_targets :=
        IList.fold_left (fun s target -> StringSet.add target s) StringSet.empty targets
  | None ->
      ()

let debug = 0

type stats =
  {
    mutable files_linked: int;
    mutable targets_merged: int;
  }

let empty_stats () =
  {
    files_linked = 0;
    targets_merged = 0;
  }

let link_exists s =
  try
    let _ = Unix.lstat s in
    true
  with Unix.Unix_error _ -> false

(** Create symbolic links recursively from the destination to the source.
    Replicate the structure of the source directory in the destination,
    with files replaced by links to the source. *)
let rec slink ~stats ~skiplevels src dst =
  if debug >=3
  then L.stderr "slink src:%s dst:%s skiplevels:%d@." src dst skiplevels;
  if Sys.is_directory src
  then
    begin
      if not (Sys.file_exists dst)
      then Unix.mkdir dst 0o700;
      let items = Sys.readdir src in
      Array.iter
        (fun item ->
           slink ~stats ~skiplevels:(skiplevels - 1)
             (Filename.concat src item) (Filename.concat dst item))
        items
    end
  else if skiplevels > 0 then ()
  else
    begin
      if link_exists dst then Unix.unlink dst;
      Unix.symlink src dst;
      (* Set the accessed and modified time of the original file slightly in the past.  Due to
         the coarse precision of the timestamps, it is possible for the source and destination of a
         link to have the same modification time. When this happens, the files will be considered to
         need re-analysis every time, indefinitely. *)
      let near_past = Unix.gettimeofday () -. 1. in
      Unix.utimes src near_past near_past;
      stats.files_linked <- stats.files_linked + 1;
    end

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
    if debug >= 2 then
      L.stderr "file:%s time_orig:%f time_link:%f@."
        file time_orig time_link;
    time_link > time_orig in
  let symlinks_up_to_date captured_file =
    if Sys.is_directory captured_file then
      let contents = Array.to_list (Sys.readdir captured_file) in
      IList.for_all
        (fun file ->
           let file_path = Filename.concat captured_file file in
           Sys.file_exists file_path &&
           (not check_timestamp_of_symlinks || symlink_up_to_date file_path))
        contents
    else true in
  let check_file captured_file =
    Sys.file_exists captured_file &&
    symlinks_up_to_date captured_file in
  let was_copied () =
    let captured_src = Filename.concat infer_out_src Config.captured_dir_name in
    let captured_dst = Filename.concat infer_out_dst Config.captured_dir_name in
    if Sys.file_exists captured_src && Sys.is_directory captured_src
    then
      begin
        let captured_files = Array.to_list (Sys.readdir captured_src) in
        num_captured_files := IList.length captured_files;
        IList.for_all
          (fun file ->
             check_file (Filename.concat captured_dst file))
          captured_files
      end
    else
      true in
  let was_modified () =
    StringSet.mem target !modified_targets in
  let r =
    was_modified () ||
    not (was_copied ()) in
  if r then stats.targets_merged <- stats.targets_merged + 1;
  if debug >= 2
  then L.stderr "lnk:%s:%d %s@." (if r then "T" else "F") !num_captured_files target_results_dir
  else if debug >= 1 && r
  then L.stderr "%s@."target_results_dir;
  r

(** should_link needs to know whether the source file has changed,
    and  to determine whether the destination has never been copied.
    In both cases, perform the link. *)
let process_merge_file deps_file =
  let infer_out_dst = !Config.results_dir in
  let stats = empty_stats () in
  let process_line line =
    match Str.split_delim (Str.regexp (Str.quote "\t")) line with
    | target :: _ :: target_results_dir :: _ ->
        let infer_out_src = Filename.concat (Filename.dirname (buck_out ())) target_results_dir in
        let skiplevels = 2 in (** Don't link toplevel files, definitely not .start *)
        if should_link ~target ~target_results_dir ~stats infer_out_src infer_out_dst
        then slink ~stats ~skiplevels infer_out_src infer_out_dst
    | _ ->
        () in
  Option.may
    (fun lines -> IList.iter process_line lines)
    (read_file deps_file);
  L.stdout "Captured results merged.@.";
  L.stdout "Targets merged: %d@." stats.targets_merged;
  L.stdout "Files linked: %d@." stats.files_linked


let merge_captured_targets () =
  process_merge_file (infer_deps ())
