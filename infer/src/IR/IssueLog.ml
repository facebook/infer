(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Module to store a set of issues per procedure *)

open! IStd

type t = Errlog.t Procname.Map.t

let empty = Procname.Map.empty

let get_or_add ~proc m =
  match Procname.Map.find_opt proc m with
  | Some errlog ->
      (m, errlog)
  | None ->
      let errlog = Errlog.empty () in
      let m = Procname.Map.add proc errlog m in
      (m, errlog)


let issues_serializer : Errlog.t Procname.Map.t Serialization.serializer =
  Serialization.create_serializer Serialization.Key.issues


let iter ~f m = Procname.Map.iter f m

let store ~entry ~file m =
  if not (Procname.Map.is_empty m) then (
    let abbrev_source_file = DB.source_file_encoding file in
    let issues_dir = ResultsDir.get_path entry in
    Utils.create_dir issues_dir ;
    let filename =
      DB.filename_from_string (Filename.concat issues_dir (abbrev_source_file ^ ".issue"))
    in
    Serialization.write_to_file issues_serializer filename ~data:m )
  else ()


(** Load issues from the given file *)
let load_issues issues_file = Serialization.read_from_file issues_serializer issues_file

(** Load all the issues in the given dir and update the issues map *)
let load entry =
  let issues_dir = ResultsDir.get_path entry in
  let load_issues_to_map init issues_file =
    let file = DB.filename_from_string (Filename.concat issues_dir issues_file) in
    load_issues file
    |> Option.fold ~init ~f:(fun acc map ->
           Procname.Map.merge
             (fun _ issues1 issues2 ->
               match (issues1, issues2) with
               | Some issues1, Some issues2 ->
                   Errlog.update issues1 issues2 ;
                   Some issues1
               | Some issues1, None ->
                   Some issues1
               | None, Some issues2 ->
                   Some issues2
               | None, None ->
                   None )
             acc map )
  in
  match Sys.readdir issues_dir with
  | children ->
      Array.fold children ~init:empty ~f:load_issues_to_map
  | exception Sys_error _ ->
      empty
