(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module to store a set of issues per procedure *)

open! IStd

let errLogMap = ref Typ.Procname.Map.empty

let exists_issues () = not (Typ.Procname.Map.is_empty !errLogMap)

let get_err_log procname =
  try Typ.Procname.Map.find procname !errLogMap
  with Not_found ->
    let errlog = Errlog.empty () in
    errLogMap := Typ.Procname.Map.add procname errlog !errLogMap ;
    errlog

let lint_issues_serializer : Errlog.t Typ.Procname.Map.t Serialization.serializer =
  Serialization.create_serializer Serialization.Key.lint_issues

(** Save issues to a file *)
let store_issues filename errLogMap =
  Serialization.write_to_file lint_issues_serializer filename ~data:errLogMap

(** Load issues from the given file *)
let load_issues issues_file = Serialization.read_from_file lint_issues_serializer issues_file

(** Load all the lint issues in the given dir and update the issues map *)
let load_issues_to_errlog_map dir =
  let issues_dir = Filename.concat Config.results_dir dir in
  let children_opt =
    try Some (Sys.readdir issues_dir)
    with Sys_error _ -> None
  in
  let load_issues_to_map issues_file =
    let file = DB.filename_from_string (Filename.concat issues_dir issues_file) in
    match load_issues file with
    | Some map
     -> errLogMap
        := Typ.Procname.Map.merge
             (fun _ issues1 issues2 ->
               match (issues1, issues2) with
               | Some issues1, Some issues2
                -> Errlog.update issues1 issues2 ; Some issues1
               | Some issues1, None
                -> Some issues1
               | None, Some issues2
                -> Some issues2
               | None, None
                -> None)
             !errLogMap map
    | None
     -> ()
  in
  match children_opt with Some children -> Array.iter ~f:load_issues_to_map children | None -> ()
