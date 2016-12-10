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
type 'a serializer = (string -> 'a option) * (DB.filename -> 'a option) * (DB.filename -> 'a -> unit)

(** Serialization key, used to distinguish versions of serializers and avoid assert faults *)
type key = int

(** current key for tenv, procedure summary, cfg, error trace, call graph *)
let tenv_key, summary_key, cfg_key, trace_key, cg_key,
    analysis_results_key, cluster_key, attributes_key, lint_issues_key =
  425184201, 160179325, 1062389858, 221487792, 477305409,
  799050016, 579094948, 972393003, 852343110

(** version of the binary files, to be incremented for each change *)
let version = 26


(** Retry the function while an exception filtered is thrown,
    or until the timeout in seconds expires. *)
let retry_exception timeout catch_exn f x =
  let init_time = Unix.gettimeofday () in
  let expired () =
    Unix.gettimeofday () -. init_time >= timeout in
  let rec retry () =
    try f x with
    | e when catch_exn e && not (expired ()) ->
        retry () in
  retry ()


let create_serializer (key : key) : 'a serializer =
  let match_data ((key': key), (version': int), (value: 'a)) source_msg =
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
  let from_string (str : string) : 'a option =
    try
      match_data (Marshal.from_string str 0) "string"
    with Sys_error _ -> None in
  let from_file (fname_ : DB.filename) : 'a option =
    let fname = DB.filename_to_string fname_ in
    match open_in_bin fname with
    | exception Sys_error _ ->
        None
    | inc ->
        let read () =
          try
            In_channel.seek inc 0L ;
            match_data (Marshal.from_channel inc) fname
          with
          | Sys_error _ -> None in
        let timeout = 1.0 in
        let catch_exn = function
          | End_of_file -> true
          | Failure _ -> true (* handle input_value: truncated object *)
          | _ -> false in
        (* Retry to read for 1 second in case of end of file, *)
        (* which indicates that another process is writing the same file. *)
        SymOp.try_finally
          (fun () -> retry_exception timeout catch_exn read ())
          (fun () -> In_channel.close inc) in
  let to_file (fname : DB.filename) (value : 'a) =
    let fname_str = DB.filename_to_string fname in
    (* support nonblocking reads and writes in parallel: *)
    (* write to a tmp file and use rename which is atomic *)
    let fname_tmp = Filename.temp_file
        ~in_dir:(Filename.dirname fname_str) (Filename.basename fname_str) ".tmp" in
    let outc = open_out_bin fname_tmp in
    Marshal.to_channel outc (key, version, value) [];
    Out_channel.close outc;
    Unix.rename ~src:fname_tmp ~dst:fname_str in
  (from_string, from_file, to_file)


let from_string (serializer : 'a serializer) =
  let (s, _, _) = serializer in s

let from_file (serializer : 'a serializer) =
  let (_, s, _) = serializer in s

let to_file (serializer : 'a serializer) =
  let (_, _, s) = serializer in s

(*
(** Generate random keys, to be used in an ocaml toplevel *)
let generate_keys () =
  Random.self_init ();
  let max_rand_int = 0x3FFFFFFF (* determined by Rand library *) in
  let gen () = Random.int max_rand_int in
  gen (), gen (), gen (), gen (), gen (), gen ()
*)
