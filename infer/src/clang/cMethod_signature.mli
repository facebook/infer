(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

(** Define the signature of a method consisting of its name, its arguments, *)
(** return type, location and whether its an instance method.  *)

type method_signature

val add : method_signature -> unit

val find : Procname.t -> method_signature

val reset_map : unit -> unit

val ms_get_name : method_signature -> Procname.t

val ms_get_args : method_signature -> (string * string * Clang_ast_t.stmt option) list

val ms_get_ret_type : method_signature -> string

val ms_get_attributes : method_signature -> Clang_ast_t.attribute list

val ms_get_loc : method_signature -> Clang_ast_t.source_range

val ms_is_instance : method_signature -> bool

val make_ms : Procname.t -> (string * string * Clang_ast_t.stmt option) list -> string -> Clang_ast_t.attribute list ->
Clang_ast_t.source_range -> bool -> bool -> method_signature

val replace_name_ms : method_signature -> Procname.t -> method_signature

val ms_to_string : method_signature -> string

val ms_is_generated : method_signature -> bool
