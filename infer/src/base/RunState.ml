(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let run_time_string = Time.now () |> Time.to_string

let state0 =
  let open Runstate_t in
  { run_sequence= []
  ; results_dir_format=
      Printf.sprintf "db_filename: %s\ndb_schema: %s" ResultsDatabase.database_filename
        ResultsDatabase.schema_hum
  ; should_merge_capture= false }


let state : Runstate_t.t ref = ref state0

let add_run_to_sequence () =
  let run =
    { Runstate_t.infer_version= Version.{Runstate_t.major; minor; patch; commit}
    ; date= run_time_string
    ; command= Config.command }
  in
  Runstate_t.(state := {!state with run_sequence= run :: !state.run_sequence})


let state_filename = ".infer_runstate.json"

let state_file_path = Config.results_dir ^/ state_filename

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
              Was it created using an older version of infer?"
             Config.results_dir err_msg) )
      msg
  in
  if Sys.file_exists state_file_path <> `Yes then
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

let set_merge_capture onoff = Runstate_t.(state := {!state with should_merge_capture= onoff})

let get_merge_capture () = !state.Runstate_t.should_merge_capture
