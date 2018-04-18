(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

let run_time_string = Time.now () |> Time.to_string

let state0 =
  let open Runstate_t in
  { run_sequence= []
  ; results_dir_format=
      Printf.sprintf "db_filename: %s\ndb_schema: %s" ResultsDatabase.database_filename
        ResultsDatabase.schema_hum }


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
             "Incompatible results directory '%s':\n\
              %s\n\
              Was '%s' created using an older version of infer?"
             Config.results_dir err_msg Config.results_dir) )
      msg
  in
  if Sys.file_exists state_file_path <> `Yes then error "save state not found"
  else
    try
      let loaded_state = Ag_util.Json.from_file Runstate_j.read_t state_file_path in
      if
        not
          (String.equal !state.Runstate_t.results_dir_format
             loaded_state.Runstate_t.results_dir_format)
      then
        error "Incompatible formats: found\n  %s\n\nbut expected this format:\n  %s\n\n"
          loaded_state.results_dir_format !state.Runstate_t.results_dir_format
      else (
        state := loaded_state ;
        Ok () )
    with _ -> Error "error reading the save state"


let reset () = state := state0
