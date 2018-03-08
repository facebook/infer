(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type t

val empty : t

val add_int : t -> key:string -> data:int -> t

val add_float : t -> key:string -> data:float -> t

val add_string : t -> key:string -> data:string -> t

val add_string_opt : t -> key:string -> data:string option -> t

val to_json : t -> string
