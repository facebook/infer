(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PolyVariantEqual
module L = Logging

let get_path entry = ResultsDirEntryName.get_path ~results_dir:Config.results_dir entry

module RunState = struct
  let run_time_string = Time.now () |> Time.to_string

  let state0 =
    let open Runstate_t in
    { run_sequence= []
    ; results_dir_format=
        Printf.sprintf "db_filename: %s\ndb_schema: %s"
          (ResultsDirEntryName.get_path ~results_dir:"infer-out" CaptureDB)
          ResultsDatabase.schema_hum
    ; should_merge_capture= false }


  let state : Runstate_t.t ref = ref state0

  let state_file_path = get_path RunState

  let store () =
    Utils.with_file_out state_file_path ~f:(fun oc ->
        Runstate_j.string_of_t !state |> Out_channel.output_string oc )


  let load_and_validate () =
    let error msg =
      Printf.ksprintf
        (fun err_msg ->
          Error
            (Printf.sprintf
               "'%s' already exists but it is not an empty directory and it does not look like an \
                infer results directory:\n\
               \  %s\n\
                Was it created using an older version of infer?" Config.results_dir err_msg) )
        msg
    in
    if PolyVariantEqual.(Sys.file_exists state_file_path <> `Yes) then
      error "save state not found: '%s' does not exist" state_file_path
    else
      match Atdgen_runtime.Util.Json.from_file Runstate_j.read_t state_file_path with
      | {Runstate_t.results_dir_format} as loaded_state
        when String.equal !state.Runstate_t.results_dir_format results_dir_format ->
          state := loaded_state ;
          Ok ()
      | {Runstate_t.results_dir_format} ->
          error "Incompatible formats: found\n  %s\n\nbut expected this format:\n  %s\n\n"
            results_dir_format !state.Runstate_t.results_dir_format
      | exception e ->
          error "could not read the save state '%s': %s" state_file_path (Exn.to_string e)


  let reset () = state := state0

  let set_merge_capture onoff =
    state := {!state with Runstate_t.should_merge_capture= onoff} ;
    (* store change to the runstate *)
    store ()


  let get_merge_capture () = !state.Runstate_t.should_merge_capture

  let add_run_to_sequence () =
    let run =
      { Runstate_t.infer_version= Version.{Runstate_t.major; minor; patch; commit}
      ; date= run_time_string
      ; command= Config.command }
    in
    state := {!state with Runstate_t.run_sequence= run :: !state.run_sequence} ;
    (* store change to the runstate *)
    store ()
end

let is_results_dir () =
  let capture_db_path = get_path CaptureDB in
  let has_all_markers = Sys.is_file capture_db_path = `Yes in
  Result.ok_if_true has_all_markers ~error:(Printf.sprintf "'%s' not found" capture_db_path)


let non_empty_directory_exists results_dir =
  (* Look if [results_dir] exists and is a non-empty directory. If it's an empty directory, leave it
     alone. This allows users to create a temporary directory for the infer results without infer
     removing it to recreate it, which could be racy. *)
  Sys.is_directory results_dir = `Yes && not (Utils.directory_is_empty results_dir)


let remove_results_dir () =
  if non_empty_directory_exists Config.results_dir then (
    if (not Config.buck) && not Config.force_delete_results_dir then
      Result.iter_error (is_results_dir ()) ~f:(fun err ->
          L.(die UserError)
            "ERROR: '%s' exists but does not seem to be an infer results directory: %s@\n\
             ERROR: Please delete '%s' and try again@." Config.results_dir err Config.results_dir ) ;
    Utils.rmtree Config.results_dir ) ;
  RunState.reset ()


let prepare_logging_and_db () =
  L.setup_log_file () ;
  PerfEvent.init () ;
  if Sys.is_file (get_path CaptureDB) <> `Yes then ResultsDatabase.create_db () ;
  ResultsDatabase.new_database_connection ()


let create_results_dir () =
  if non_empty_directory_exists Config.results_dir then
    RunState.load_and_validate ()
    |> Result.iter_error ~f:(fun error ->
           if Config.force_delete_results_dir then (
             L.user_warning "WARNING: %s@\n" error ;
             L.progress "Deleting results dir because --force-delete-results-dir was passed@." ;
             remove_results_dir () )
           else
             L.die UserError "ERROR: %s@\nPlease remove '%s' and try again" error Config.results_dir ) ;
  Unix.mkdir_p Config.results_dir ;
  Unix.mkdir_p (get_path Temporary) ;
  prepare_logging_and_db () ;
  ()


let assert_results_dir advice =
  Result.iter_error (is_results_dir ()) ~f:(fun err ->
      L.(die UserError)
        "ERROR: No results directory at '%s': %s@\nERROR: %s@." Config.results_dir err advice ) ;
  RunState.load_and_validate ()
  |> Result.iter_error ~f:(fun error ->
         L.die UserError "%s@\nPlease remove '%s' and try again" error Config.results_dir ) ;
  prepare_logging_and_db () ;
  ()


let scrub_for_incremental () =
  DBWriter.reset_capture_tables () ;
  List.iter ~f:Utils.rmtree
    (ResultsDirEntryName.to_delete_before_incremental_capture_and_analysis
       ~results_dir:Config.results_dir) ;
  ()


let scrub_for_caching () =
  let cache_capture =
    Config.genrule_mode || Option.exists Config.buck_mode ~f:BuckMode.is_clang_flavors
  in
  if cache_capture then DBWriter.canonicalize () ;
  (* make sure we are done with the database *)
  ResultsDatabase.db_close () ;
  List.iter ~f:Utils.rmtree
    (ResultsDirEntryName.to_delete_before_caching_capture ~results_dir:Config.results_dir)
