(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

exception Error of string

exception DataTooBig

let error fmt = Format.kasprintf (fun err -> raise (Error err)) fmt

let pp_db_error fmt db =
  F.fprintf fmt "(%s [%d])" (Sqlite3.errmsg db) (Sqlite3.extended_errcode_int db)


let check_result_code db ~log rc =
  match (rc : Sqlite3.Rc.t) with
  | OK | ROW ->
      ()
  | _ as err ->
      error "%s: %s %a" log (Sqlite3.Rc.to_string err) pp_db_error db


let exec db ~log ~stmt =
  (* Call [check_result_code] and catch exceptions to rewrite the error message. This avoids allocating the error string when not needed. *)
  PerfEvent.log (fun logger ->
      PerfEvent.log_begin_event logger ~name:"sql exec" ~arguments:[("stmt", `String log)] () ) ;
  let rc = Sqlite3.exec db stmt in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  try check_result_code db ~log rc with Error err -> error "exec: %s %a" err pp_db_error db


let finalize db ~log stmt =
  try check_result_code db ~log (Sqlite3.finalize stmt) with
  | Error err ->
      error "finalize: %s %a" err pp_db_error db
  | Sqlite3.Error err ->
      error "finalize: %s: %s %a" log err pp_db_error db


let result_fold_rows ?finalize:(do_finalize = true) db ~log stmt ~init ~f =
  let rec aux accum stmt =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        (* the operation returned a result, get it *)
        aux (f accum stmt) stmt
    | DONE ->
        accum
    | err ->
        L.die InternalError "%s: %s %a" log (Sqlite3.Rc.to_string err) pp_db_error db
  in
  if do_finalize then protect ~finally:(fun () -> finalize db ~log stmt) ~f:(fun () -> aux init stmt)
  else aux init stmt


let result_fold_single_column_rows ?finalize db ~log stmt ~init ~f =
  result_fold_rows ?finalize db ~log stmt ~init ~f:(fun accum stmt ->
      f accum (Sqlite3.column stmt 0) )


let zero_or_one_row ~log = function
  | [] ->
      None
  | [x] ->
      Some x
  | _ :: _ :: _ as l ->
      L.die InternalError "%s: zero or one result expected, got %d rows instead" log (List.length l)


let result_option ?finalize db ~log ~read_row stmt =
  IContainer.rev_map_to_list stmt ~f:read_row ~fold:(result_fold_rows ?finalize db ~log)
  |> zero_or_one_row ~log


let result_single_column_option ?finalize db ~log stmt =
  Container.to_list stmt ~fold:(result_fold_single_column_rows ?finalize db ~log)
  |> zero_or_one_row ~log


let result_unit ?finalize db ~log stmt =
  if not (Container.is_empty stmt ~iter:(Container.iter ~fold:(result_fold_rows ?finalize db ~log)))
  then L.die InternalError "%s: the SQLite query should not return any rows" log


let db_close db =
  if not (Sqlite3.db_close db) then
    raise
      (Error
         (F.asprintf "closing: %s %a" (Sqlite3.errcode db |> Sqlite3.Rc.to_string) pp_db_error db)
      )


(* an underapproximation of the maximum path length allowed in sqlite *)
let sqlite_max_path_length = 500

let with_attached_db ~db_file ~db_name ?(immutable = false) ~f db =
  let db_file, remove_after_use =
    if
      (* If [db_file]'s length exceeds Sqlite's limit then use a symlink trampoline.
         Do not bother if [db_file] is a special Sqlite path like [:memory:] *)
      String.length db_file < sqlite_max_path_length || String.is_prefix db_file ~prefix:":"
    then (db_file, false)
    else
      let in_dir = ResultsDirEntryName.get_path ~results_dir:Config.results_dir Temporary in
      let link_name = Filename.temp_file ~in_dir "infer-merge-sqlite-trampoline-out" "" in
      Unix.unlink link_name ;
      Unix.symlink ~target:(Filename.dirname db_file) ~link_name ;
      (Filename.concat link_name (Filename.basename db_file), true)
  in
  let attach_stmt =
    Printf.sprintf "ATTACH '%s%s%s' AS %s"
      (if immutable then "file:" else "")
      (if immutable then Escape.escape_url db_file else db_file)
      (if immutable then "?immutable=1" else "")
      db_name
  in
  L.debug Capture Verbose "Attach: %s@\n" attach_stmt ;
  exec db ~stmt:attach_stmt ~log:(Printf.sprintf "attaching database '%s'" db_file) ;
  let result = f () in
  exec db ~stmt:("DETACH " ^ db_name) ~log:(Printf.sprintf "detaching database '%s'" db_file) ;
  if remove_after_use then Unix.unlink db_file ;
  result


let transaction ?(immediate = false) db ~f =
  exec db ~log:"begin transaction"
    ~stmt:(Printf.sprintf "BEGIN%s TRANSACTION" (if immediate then " IMMEDIATE" else "")) ;
  let result = f () in
  exec db ~log:"commit transaction" ~stmt:"COMMIT" ;
  result


module type Data = sig
  type t

  val serialize : t -> Sqlite3.Data.t

  val deserialize : Sqlite3.Data.t -> t
end

module type T = sig
  type t
end

module MarshalledDataNOTForComparison (D : T) = struct
  type t = D.t

  let deserialize = function[@warning "-partial-match"]
    | Sqlite3.Data.BLOB b ->
        Marshal.from_string b 0


  let serialize x =
    let s = Marshal.to_string x [] in
    if String.length s < Config.sqlite_max_blob_size then Sqlite3.Data.BLOB s else raise DataTooBig
end

module MarshalledNullableDataNOTForComparison (D : T) = struct
  type t = D.t option

  let deserialize = function[@warning "-partial-match"]
    | Sqlite3.Data.BLOB b ->
        Some (Marshal.from_string b 0)
    | Sqlite3.Data.NULL ->
        None


  let serialize = function
    | None ->
        Sqlite3.Data.NULL
    | Some x ->
        let s = Marshal.to_string x [] in
        if String.length s < Config.sqlite_max_blob_size then Sqlite3.Data.BLOB s
        else raise DataTooBig
end
