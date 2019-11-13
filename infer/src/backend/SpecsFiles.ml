(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module CLOpt = CommandLineOption

(** return the list of the .specs files in the results dir and libs, if they're defined *)
let load_specfiles () =
  let specs_files_in_dir dir =
    let is_specs_file fname =
      Sys.is_directory fname <> `Yes && Filename.check_suffix fname Config.specs_files_suffix
    in
    match Sys.readdir dir with
    | exception Sys_error _ ->
        []
    | files ->
        Array.fold files ~init:[] ~f:(fun acc fname ->
            let path = Filename.concat dir fname in
            if is_specs_file path then path :: acc else acc )
  in
  let result_specs_dir = DB.filename_to_string DB.Results_dir.specs_dir in
  specs_files_in_dir result_specs_dir


let print_usage_exit err_s =
  L.user_error "Load Error: %s@\n@." err_s ;
  Config.print_usage_exit ()


let spec_files_from_cmdline () =
  if CLOpt.is_originator then (
    (* Find spec files specified by command-line arguments.  Not run at init time since the specs
       files may be generated between init and report time. *)
    List.iter
      ~f:(fun arg ->
        if (not (Filename.check_suffix arg Config.specs_files_suffix)) && arg <> "." then
          print_usage_exit ("file " ^ arg ^ ": arguments must be .specs files") )
      Config.anon_args ;
    if Config.test_filtering then (Inferconfig.test () ; L.exit 0) ;
    if List.is_empty Config.anon_args then load_specfiles () else List.rev Config.anon_args )
  else load_specfiles ()


(** Create an iterator which loads spec files one at a time *)
let summary_iterator spec_files =
  let sorted_spec_files = List.sort ~compare:String.compare (spec_files ()) in
  let do_spec f fname =
    match Summary.OnDisk.load_from_file (DB.filename_from_string fname) with
    | None ->
        L.(die UserError) "Error: cannot open file %s@." fname
    | Some summary ->
        f summary
  in
  let iterate f = List.iter ~f:(do_spec f) sorted_spec_files in
  iterate


let iter_from_config ~f = summary_iterator spec_files_from_cmdline f

let iter ~f = summary_iterator load_specfiles f

let delete pname =
  let filename = Summary.OnDisk.specs_filename_of_procname pname |> DB.filename_to_string in
  (* Unix_error is raised if the file isn't present so do nothing in this case *)
  (try Unix.unlink filename with Unix.Unix_error _ -> ()) ;
  Ondemand.LocalCache.remove pname ;
  Summary.OnDisk.remove_from_cache pname
