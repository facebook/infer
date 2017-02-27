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
  {
    read_from_string: (string -> 'a option);
    read_from_file: (DB.filename -> 'a option);
    write_to_file: (DB.filename -> 'a -> unit);
  }

module Key = struct

  (** Serialization key, used to distinguish versions of serializers and avoid assert faults *)
  type t = int

  (** current key for tenv, procedure summary, cfg, error trace, call graph *)
  let tenv, summary, cfg, trace, cg,
      analysis_results, cluster, attributes, lint_issues =
    425184201, 160179325, 1062389858, 221487792, 477305409,
    799050016, 579094948, 972393003, 852343110
end

(** version of the binary files, to be incremented for each change *)
let version = 26


(** Retry the function while an exception filtered is thrown,
    or until the timeout in seconds expires. *)
let retry_exception ~timeout ~catch_exn ~f x =
  let init_time = Unix.gettimeofday () in
  let expired () =
    Unix.gettimeofday () -. init_time >= timeout in
  let rec retry () =
    try f x with
    | e when catch_exn e && not (expired ()) ->
        retry () in
  retry ()


let create_serializer (key : Key.t) : 'a serializer =
  let read_data ((key': Key.t), (version': int), (value: 'a)) source_msg =
    if key <> key' then
      begin
        L.err "Wrong key in when loading data from %s@\n" source_msg;
        None
      end
    else if version <> version' then
      begin
        L.err "Wrong version in when loading data from %s@\n" source_msg;
        None
      end
    else Some value in
  let read_from_string (str : string) : 'a option =
    try
      read_data (Marshal.from_string str 0) "string"
    with Sys_error _ -> None in
  let read_from_file (fname_ : DB.filename) : 'a option =
    let fname = DB.filename_to_string fname_ in
    match open_in_bin fname with
    | exception Sys_error _ ->
        None
    | inc ->
        let read () =
          try
            In_channel.seek inc 0L ;
            read_data (Marshal.from_channel inc) fname
          with
          | Sys_error _ -> None in
        let catch_exn = function
          | End_of_file -> true
          | Failure _ -> true (* handle input_value: truncated object *)
          | _ -> false in
        (* Retry to read for 1 second in case of end of file, *)
        (* which indicates that another process is writing the same file. *)
        SymOp.try_finally
          (fun () -> retry_exception ~timeout:1.0 ~catch_exn ~f:read ())
          (fun () -> In_channel.close inc) in
  let write_to_file (fname : DB.filename) (value : 'a) =
    let fname_str = DB.filename_to_string fname in
    (* support nonblocking reads and writes in parallel: *)
    (* write to a tmp file and use rename which is atomic *)
    let fname_tmp = Filename.temp_file
        ~in_dir:(Filename.dirname fname_str) (Filename.basename fname_str) ".tmp" in
    let outc = open_out_bin fname_tmp in
    Marshal.to_channel outc (key, version, value) [];
    Out_channel.close outc;
    Unix.rename ~src:fname_tmp ~dst:fname_str in
  {read_from_string; read_from_file; write_to_file}


let read_from_string s =
  s.read_from_string

let read_from_file s =
  s.read_from_file

let write_to_file s =
  s.write_to_file

(*
(** Generate random keys, to be used in an ocaml toplevel *)
let generate_keys () =
  Random.self_init ();
  let max_rand_int = 0x3FFFFFFF (* determined by Rand library *) in
  let gen () = Random.int max_rand_int in
  gen (), gen (), gen (), gen (), gen (), gen ()
*)
