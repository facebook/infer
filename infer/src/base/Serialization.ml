(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module L = Logging
module F = Format

(** Generic serializer *)
type 'a serializer =
  { read_from_string: string -> 'a option
  ; read_from_file: DB.filename -> 'a option
  ; write_to_file: data:'a -> DB.filename -> unit }

module Key = struct
  (** Serialization key, used to distinguish versions of serializers and avoid assert faults *)
  type t = int

  (** Current keys for various serializable objects. The keys are computed using the [generate_keys]
     function below *)
  let tenv, summary, cluster, lint_issues = (425184201, 160179325, 579094948, 852343110)
end

(** version of the binary files, to be incremented for each change *)
let version = 27

(** Retry the function while an exception filtered is thrown,
    or until the timeout in seconds expires. *)
let retry_exception ~timeout ~catch_exn ~f x =
  let init_time = Mtime_clock.counter () in
  let expired () = Mtime.Span.compare timeout (Mtime_clock.count init_time) <= 0 in
  let rec retry () =
    try f x with e when catch_exn e && not (expired ()) -> Utils.yield () ; (retry [@tailcall]) ()
  in
  retry ()


type 'a write_command = Replace of 'a

let create_serializer (key: Key.t) : 'a serializer =
  let read_data ((key': Key.t), (version': int), (value: 'a)) source_msg =
    if key <> key' then (
      L.user_error
        "Wrong key in when loading data from %s -- are you running infer with results coming from \
         a previous version of infer?@\n\
         " source_msg ;
      None )
    else if version <> version' then (
      L.user_error
        "Wrong version in when loading data from %s -- are you running infer with results coming \
         from a previous version of infer?@\n\
         " source_msg ;
      None )
    else Some value
  in
  let read_from_string (str: string) : 'a option =
    read_data (Marshal.from_string str 0) "string"
  in
  (* The reads happen without synchronization.
     The writes are synchronized with a .lock file. *)
  let read_from_file (fname: DB.filename) : 'a option =
    let fname_str = DB.filename_to_string fname in
    match In_channel.create ~binary:true fname_str with
    | exception Sys_error _ ->
        None
    | inc ->
        let read () =
          try
            In_channel.seek inc 0L ;
            read_data (Marshal.from_channel inc) fname_str
          with Sys_error _ -> None
        in
        let catch_exn = function
          | End_of_file ->
              true
          | Failure _ ->
              true (* handle input_value: truncated object *)
          | _ ->
              false
        in
        (* Retry to read for 1 second in case of end of file, *)
        (* which indicates that another process is writing the same file. *)
        let one_second = Mtime.Span.of_uint64_ns (Int64.of_int 1_000_000_000) in
        SymOp.try_finally
          ~f:(fun () -> retry_exception ~timeout:one_second ~catch_exn ~f:read ())
          ~finally:(fun () -> In_channel.close inc)
  in
  let write_to_tmp_file fname data =
    let fname_tmp =
      Filename.temp_file ~in_dir:(Filename.dirname fname) (Filename.basename fname) ".tmp"
    in
    Utils.write_file_with_locking fname_tmp ~f:(fun outc ->
        Marshal.to_channel outc (key, version, data) [] ) ;
    fname_tmp
  in
  (* The .lock file is used to synchronize the writers.
     Once a lock on `file.lock` is obtained, the new data is written into a temporary file
     and rename is used to move it atomically to `file` *)
  let execute_write_command_with_lock (fname: DB.filename) (cmd: 'a write_command) =
    let fname_str = DB.filename_to_string fname in
    let fname_str_lock = fname_str ^ ".lock" in
    Utils.write_file_with_locking fname_str_lock ~delete:true ~f:(fun _outc ->
        let data_to_write : 'a = match cmd with Replace data -> data in
        let fname_str_tmp = write_to_tmp_file fname_str data_to_write in
        (* Rename is atomic: the readers can only see one version of this file,
             possibly stale but not corrupted. *)
        Unix.rename ~src:fname_str_tmp ~dst:fname_str )
  in
  let write_to_file ~(data: 'a) (fname: DB.filename) =
    execute_write_command_with_lock fname (Replace data)
  in
  {read_from_string; read_from_file; write_to_file}


let read_from_string s = s.read_from_string

let read_from_file s = s.read_from_file

let write_to_file s = s.write_to_file

(** Generate new (random) serialization keys, to be run in an ocaml toplevel and used in the [Key]
    module above *)
let generate_keys () =
  Random.self_init () ;
  let max_rand_int = 0x3FFFFFFF (* determined by Rand library *) in
  let gen () = Random.int max_rand_int in
  (gen (), gen (), gen (), gen (), gen ())
