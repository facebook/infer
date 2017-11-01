(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
open! PVariant
module L = Logging

let results_dir_dir_markers =
  List.map ~f:(Filename.concat Config.results_dir) [Config.captured_dir_name; Config.specs_dir_name]


let is_results_dir ~check_correct_version () =
  let not_found = ref "" in
  let has_all_markers =
    List.for_all results_dir_dir_markers ~f:(fun d ->
        Sys.is_directory d = `Yes
        ||
        (not_found := d ^ "/" ;
        false) )
    && ( not check_correct_version || Sys.is_file ResultsDatabase.database_fullpath = `Yes
       ||
       (not_found := ResultsDatabase.database_fullpath ;
       false) )
  in
  Result.ok_if_true has_all_markers ~error:(Printf.sprintf "'%s' not found" !not_found)


let remove_results_dir () =
  (* Look if file exists, it may not be a directory but that will be caught by the call to [is_results_dir]. If it's an empty directory, leave it alone. This allows users to create a temporary directory for the infer results without infer removing it to recreate it, which could be racy. *)
  if Sys.file_exists Config.results_dir = `Yes && not (Utils.directory_is_empty Config.results_dir)
  then (
    if not Config.force_delete_results_dir then
      Result.iter_error (is_results_dir ~check_correct_version:false ()) ~f:(fun err ->
          L.(die UserError)
            "ERROR: '%s' exists but does not seem to be an infer results directory: %s@\nERROR: Please delete '%s' and try again@."
            Config.results_dir err Config.results_dir ) ;
    Utils.rmtree Config.results_dir )


let create_results_dir () =
  Unix.mkdir_p Config.results_dir ;
  L.setup_log_file () ;
  if Sys.is_file ResultsDatabase.database_fullpath <> `Yes then ResultsDatabase.create_db () ;
  ResultsDatabase.new_database_connection () ;
  List.iter ~f:Unix.mkdir_p results_dir_dir_markers


let assert_results_dir advice =
  Result.iter_error (is_results_dir ~check_correct_version:true ()) ~f:(fun err ->
      L.(die UserError)
        "ERROR: No results directory at '%s': %s@\nERROR: %s@." Config.results_dir err advice ) ;
  L.setup_log_file () ;
  ResultsDatabase.new_database_connection ()


let delete_capture_and_analysis_data () =
  ResultsDatabase.reset_attributes_table () ;
  let dirs_to_delete =
    List.map ~f:(Filename.concat Config.results_dir) Config.([captured_dir_name; specs_dir_name])
  in
  List.iter ~f:Utils.rmtree dirs_to_delete ;
  List.iter ~f:Unix.mkdir_p dirs_to_delete ;
  ()
