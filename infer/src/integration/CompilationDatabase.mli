(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

type t

type compilation_data = {
  dir : string;
  command : string;
  args : string;
}

val empty : unit -> t

val get_size : t -> int

val iter : t -> (string -> compilation_data -> unit) -> unit

val find : t -> string -> compilation_data

val decode_json_file : t -> string -> unit
