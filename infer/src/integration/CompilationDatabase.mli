(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t

type compilation_data = {
  dir : string;
  command : string;
  args : string;
}

val empty : unit -> t

val get_size : t -> int

val iter : t -> (SourceFile.t -> compilation_data -> unit) -> unit

val find : t -> SourceFile.t -> compilation_data

val decode_json_file : t ->  [< `Escaped of string | `Raw of string ] -> unit

val from_json_files :  [< `Escaped of string | `Raw of string ] list -> t
