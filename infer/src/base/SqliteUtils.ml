(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

exception Error of string

let error ~fatal fmt =
  (if fatal then Format.kasprintf (fun err -> raise (Error err)) else L.internal_error) fmt


let check_sqlite_error ?(fatal= false) db ~log rc =
  match (rc : Sqlite3.Rc.t) with
  | OK | ROW ->
      ()
  | _ as err ->
      error ~fatal "%s: %s (%s)" log (Sqlite3.Rc.to_string err) (Sqlite3.errmsg db)


let exec db ~log ~stmt =
  (* Call [check_sqlite_error] with [fatal:true] and catch exceptions to rewrite the error message. This avoids allocating the error string when not needed. *)
  try check_sqlite_error ~fatal:true db ~log (Sqlite3.exec db stmt) with Error err ->
    error ~fatal:true "exec: %s (%s)" err (Sqlite3.errmsg db)


let finalize db ~log stmt =
  try check_sqlite_error ~fatal:true db ~log (Sqlite3.finalize stmt) with
  | Error err ->
      error ~fatal:true "finalize: %s (%s)" err (Sqlite3.errmsg db)
  | Sqlite3.Error err ->
      error ~fatal:true "finalize: %s: %s (%s)" log err (Sqlite3.errmsg db)


let sqlite_result_rev_list_step ?finalize:(do_finalize = true) db ~log stmt =
  let rec aux rev_results =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        (* the operation returned a result, get it *)
        let value = Some (Sqlite3.column stmt 0) in
        aux (value :: rev_results)
    | DONE ->
        rev_results
    | err ->
        L.die InternalError "%s: %s (%s)" log (Sqlite3.Rc.to_string err) (Sqlite3.errmsg db)
  in
  if do_finalize then protect ~finally:(fun () -> finalize db ~log stmt) ~f:(fun () -> aux [])
  else aux []


let sqlite_result_step ?finalize db ~log stmt =
  match sqlite_result_rev_list_step ?finalize db ~log stmt with
  | [] ->
      None
  | [x] ->
      x
  | l ->
      L.die InternalError "%s: zero or one result expected, got %d instead" log (List.length l)


let sqlite_unit_step ?finalize db ~log stmt =
  match sqlite_result_rev_list_step ?finalize db ~log stmt with
  | [] ->
      ()
  | l ->
      L.die InternalError "%s: exactly zero result expected, got %d instead" log (List.length l)


let db_close db =
  if not (Sqlite3.db_close db) then
    raise
      (Error
         (Printf.sprintf "closing: %s (%s)"
            (Sqlite3.errcode db |> Sqlite3.Rc.to_string)
            (Sqlite3.errmsg db)))


module type Data = sig
  type t

  val serialize : t -> Sqlite3.Data.t

  val deserialize : Sqlite3.Data.t -> t
end

module MarshalledData (D : sig
  type t
end) =
struct
  type t = D.t

  let deserialize = function[@warning "-8"] Sqlite3.Data.BLOB b -> Marshal.from_string b 0

  let serialize x = Sqlite3.Data.BLOB (Marshal.to_string x [])
end
