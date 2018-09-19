(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** Generic serializer *)
type 'a serializer =
  { read_from_string: string -> 'a option
  ; read_from_file: DB.filename -> 'a option
  ; write_to_file: data:'a -> DB.filename -> unit }

module Key = struct
  type t =
    { name: string  (** for logging purposes *)
    ; key: int
          (** Serialization key, used to distinguish versions of serializers and avoid assert faults *)
    }

  (** Current keys for various serializable objects. The keys are computed using the [generate_keys]
     function below *)
  let tenv, summary, issues =
    ( {name= "tenv"; key= 425184201}
    , {name= "summary"; key= 160179325}
    , {name= "issues"; key= 852343110} )
end

(** version of the binary files, to be incremented for each change *)
let version = 27

let create_serializer (key : Key.t) : 'a serializer =
  let read_data ((key' : int), (version' : int), (value : 'a)) source_msg =
    if key.key <> key' then (
      L.user_error
        "Wrong key in when loading data of type %s from %s -- are you running infer with results \
         coming from a previous version of infer?@\n"
        key.name source_msg ;
      None )
    else if version <> version' then (
      L.user_error
        "Wrong version in when loading data of type %s from %s -- are you running infer with \
         results coming from a previous version of infer?@\n"
        key.name source_msg ;
      None )
    else Some value
  in
  let read_from_string (str : string) : 'a option =
    read_data (Marshal.from_string str 0) "string"
  in
  let read_from_file (fname : DB.filename) : 'a option =
    (* The serialization is based on atomic file renames,
       so the deserialization cannot read a file while it is being written. *)
    let filename = DB.filename_to_string fname in
    PerfEvent.(
      log (fun logger -> log_begin_event logger ~name:("reading " ^ key.name) ~categories:["io"] ())) ;
    let result =
      try Utils.with_file_in filename ~f:(fun inc -> read_data (Marshal.from_channel inc) filename)
      with Sys_error _ -> None
    in
    PerfEvent.(log (fun logger -> log_end_event logger ())) ;
    result
  in
  let write_to_file ~(data : 'a) (fname : DB.filename) =
    let filename = DB.filename_to_string fname in
    PerfEvent.(
      log (fun logger -> log_begin_event logger ~name:("writing " ^ key.name) ~categories:["io"] ())) ;
    Utils.with_intermediate_temp_file_out filename ~f:(fun outc ->
        Marshal.to_channel outc (key.key, version, data) [] ) ;
    PerfEvent.(log (fun logger -> log_end_event logger ()))
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
