(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module ProcUidSet = HashSet.Make (String)

type t = {procedures: ProcUidSet.t; sources: SourceFile.HashSet.t}

let merge_into ~acc t =
  ProcUidSet.union_into t.procedures ~into:acc.procedures ;
  SourceFile.HashSet.union_into t.sources ~into:acc.sources


let merge = function
  | [] ->
      assert false
  | [t] ->
      t
  | t :: ts ->
      List.iter ts ~f:(merge_into ~acc:t) ;
      t


let get_missing_procs_filename () =
  ResultsDirEntryName.get_path ~results_dir:Config.results_dir MissingProcedures


let get_missing_sources_filename () =
  ResultsDirEntryName.get_path ~results_dir:Config.results_dir MissingSourceFiles


let save ts =
  if Config.log_missing_deps then (
    let t = merge ts in
    Utils.with_file_out (get_missing_procs_filename ()) ~f:(fun channel ->
        ProcUidSet.iter t.procedures
        |> Iter.iter (fun p ->
               Out_channel.output_string channel p ;
               Out_channel.newline channel ) ) ;
    Utils.with_file_out (get_missing_sources_filename ()) ~f:(fun channel ->
        SourceFile.HashSet.iter t.sources
        |> Iter.iter (fun s ->
               let[@warning "-partial-match"] (Sqlite3.Data.TEXT serialized_sourcefile) =
                 SourceFile.SQLite.serialize s
               in
               Out_channel.output_string channel serialized_sourcefile ;
               Out_channel.newline channel ) ) )


let load () =
  let procedures = ProcUidSet.create 1 in
  Utils.read_file (get_missing_procs_filename ())
  |> Result.ok_or_failwith
  |> List.iter ~f:(fun proc_uid -> ProcUidSet.add proc_uid procedures) ;
  let sources = SourceFile.HashSet.create 1 in
  Utils.read_file (get_missing_sources_filename ())
  |> Result.ok_or_failwith
  |> List.iter ~f:(fun serialised_sourcefile ->
         let sourcefile = SourceFile.SQLite.deserialize (Sqlite3.Data.TEXT serialised_sourcefile) in
         SourceFile.HashSet.add sourcefile sources ) ;
  {procedures; sources}


let missing_deps = {procedures= ProcUidSet.create 1; sources= SourceFile.HashSet.create 1}

let get () = missing_deps

let record_procname callee =
  if Config.log_missing_deps && not (BuiltinDecl.is_declared callee) then
    ProcUidSet.add (Procname.to_unique_id callee) missing_deps.procedures


let record_sourcefile sourcefile =
  if Config.log_missing_deps then SourceFile.HashSet.add sourcefile missing_deps.sources
