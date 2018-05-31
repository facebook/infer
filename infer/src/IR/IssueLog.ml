(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Module to store a set of issues per procedure *)

open! IStd

let errLogMap = ref Typ.Procname.Map.empty

let get_errlog procname =
  try Typ.Procname.Map.find procname !errLogMap with Caml.Not_found ->
    let errlog = Errlog.empty () in
    errLogMap := Typ.Procname.Map.add procname errlog !errLogMap ;
    errlog


let issues_serializer : Errlog.t Typ.Procname.Map.t Serialization.serializer =
  Serialization.create_serializer Serialization.Key.issues


let iter f = Typ.Procname.Map.iter f !errLogMap

let store directory source_file =
  if not (Typ.Procname.Map.is_empty !errLogMap) then (
    let abbrev_source_file = DB.source_file_encoding source_file in
    let issues_dir = Config.results_dir ^/ directory in
    Utils.create_dir issues_dir ;
    let filename =
      DB.filename_from_string (Filename.concat issues_dir (abbrev_source_file ^ ".issue"))
    in
    Serialization.write_to_file issues_serializer filename ~data:!errLogMap )
  else ()


(** Load issues from the given file *)
let load_issues issues_file = Serialization.read_from_file issues_serializer issues_file

(** Load all the issues in the given dir and update the issues map *)
let load dir =
  let () = errLogMap := Typ.Procname.Map.empty in
  let issues_dir = Filename.concat Config.results_dir dir in
  let children_opt = try Some (Sys.readdir issues_dir) with Sys_error _ -> None in
  let load_issues_to_map issues_file =
    let file = DB.filename_from_string (Filename.concat issues_dir issues_file) in
    match load_issues file with
    | Some map ->
        errLogMap :=
          Typ.Procname.Map.merge
            (fun _ issues1 issues2 ->
              match (issues1, issues2) with
              | Some issues1, Some issues2 ->
                  Errlog.update issues1 issues2 ; Some issues1
              | Some issues1, None ->
                  Some issues1
              | None, Some issues2 ->
                  Some issues2
              | None, None ->
                  None )
            !errLogMap map
    | None ->
        ()
  in
  match children_opt with Some children -> Array.iter ~f:load_issues_to_map children | None -> ()
