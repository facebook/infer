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


let check_result_code ?(fatal = false) db ~log rc =
  match (rc : Sqlite3.Rc.t) with
  | OK | ROW ->
      ()
  | _ as err ->
      error ~fatal "%s: %s (%s)" log (Sqlite3.Rc.to_string err) (Sqlite3.errmsg db)


let exec db ~log ~stmt =
  (* Call [check_result_code] with [fatal:true] and catch exceptions to rewrite the error message. This avoids allocating the error string when not needed. *)
  PerfEvent.log (fun logger ->
      PerfEvent.log_begin_event logger ~name:"sql exec" ~arguments:[("stmt", `String log)] () ) ;
  let rc = Sqlite3.exec db stmt in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  try check_result_code ~fatal:true db ~log rc with Error err ->
    error ~fatal:true "exec: %s (%s)" err (Sqlite3.errmsg db)


let finalize db ~log stmt =
  try check_result_code ~fatal:true db ~log (Sqlite3.finalize stmt) with
  | Error err ->
      error ~fatal:true "finalize: %s (%s)" err (Sqlite3.errmsg db)
  | Sqlite3.Error err ->
      error ~fatal:true "finalize: %s: %s (%s)" log err (Sqlite3.errmsg db)


let result_fold_rows ?finalize:(do_finalize = true) db ~log stmt ~init ~f =
  let rec aux accum stmt =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        (* the operation returned a result, get it *)
        aux (f accum stmt) stmt
    | DONE ->
        accum
    | err ->
        L.die InternalError "%s: %s (%s)" log (Sqlite3.Rc.to_string err) (Sqlite3.errmsg db)
  in
  if do_finalize then
    protect ~finally:(fun () -> finalize db ~log stmt) ~f:(fun () -> aux init stmt)
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
      L.die InternalError "%s: zero or one result expected, got %d rows instead" log
        (List.length l)


let result_option ?finalize db ~log ~read_row stmt =
  IContainer.rev_map_to_list stmt ~f:read_row ~fold:(result_fold_rows ?finalize db ~log)
  |> zero_or_one_row ~log


let result_single_column_option ?finalize db ~log stmt =
  Container.to_list stmt ~fold:(result_fold_single_column_rows ?finalize db ~log)
  |> zero_or_one_row ~log


let result_unit ?finalize db ~log stmt =
  if
    not (Container.is_empty stmt ~iter:(Container.iter ~fold:(result_fold_rows ?finalize db ~log)))
  then L.die InternalError "%s: the SQLite query should not return any rows" log


let db_close db =
  if not (Sqlite3.db_close db) then
    raise
      (Error
         (Printf.sprintf "closing: %s (%s)"
            (Sqlite3.errcode db |> Sqlite3.Rc.to_string)
            (Sqlite3.errmsg db)))


let with_transaction db ~f =
  exec db ~log:"begin transaction" ~stmt:"BEGIN IMMEDIATE TRANSACTION" ;
  f () ;
  exec db ~log:"commit transaction" ~stmt:"COMMIT"


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

module MarshalledNullableData (D : sig
  type t
end) =
struct
  type t = D.t option

  let deserialize = function[@warning "-8"]
    | Sqlite3.Data.BLOB b ->
        Some (Marshal.from_string b 0)
    | Sqlite3.Data.NULL ->
        None


  let serialize = function
    | None ->
        Sqlite3.Data.NULL
    | Some x ->
        Sqlite3.Data.BLOB (Marshal.to_string x [])
end
