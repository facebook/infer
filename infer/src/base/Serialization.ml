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
  let tenv, summary, issues = (425184201, 160179325, 852343110)
end

(** version of the binary files, to be incremented for each change *)
let version = 27

let create_serializer (key: Key.t) : 'a serializer =
  let read_data ((key': Key.t), (version': int), (value: 'a)) source_msg =
    if key <> key' then (
      L.user_error
        "Wrong key in when loading data from %s -- are you running infer with results coming from \
         a previous version of infer?@\n"
        source_msg ;
      None )
    else if version <> version' then (
      L.user_error
        "Wrong version in when loading data from %s -- are you running infer with results coming \
         from a previous version of infer?@\n"
        source_msg ;
      None )
    else Some value
  in
  let read_from_string (str: string) : 'a option =
    read_data (Marshal.from_string str 0) "string"
  in
  let read_from_file (fname: DB.filename) : 'a option =
    (* The serialization is based on atomic file renames,
       so the deserialization cannot read a file while it is being written. *)
    let filename = DB.filename_to_string fname in
    try Utils.with_file_in filename ~f:(fun inc -> read_data (Marshal.from_channel inc) filename)
    with Sys_error _ -> None
  in
  let write_to_file ~(data: 'a) (fname: DB.filename) =
    let filename = DB.filename_to_string fname in
    Utils.with_intermediate_temp_file_out filename ~f:(fun outc ->
        Marshal.to_channel outc (key, version, data) [] )
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
