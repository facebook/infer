(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

val empty : t

val add_int : t -> key:string -> data:int -> t

val add_float : t -> key:string -> data:float -> t

val add_string : t -> key:string -> data:string -> t

val add_string_opt : t -> key:string -> data:string option -> t

val to_json : t -> string
