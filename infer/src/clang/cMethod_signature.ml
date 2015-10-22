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

type method_signature = {
  _name : Procname.t;
  _args : (string * Clang_ast_t.type_ptr) list;
  _ret_type : Clang_ast_t.type_ptr;
  _attributes : Clang_ast_t.attribute list;
  _loc : Clang_ast_t.source_range;
  _is_instance : bool;
  _is_generated : bool;
  _language : CFrontend_config.lang;
}

let ms_get_name ms =
  ms._name

let ms_get_args ms =
  ms._args

let ms_get_ret_type ms =
  ms._ret_type

let ms_get_attributes ms =
  ms._attributes

let ms_get_loc ms =
  ms._loc

let ms_is_instance ms =
  ms._is_instance

let ms_is_generated ms =
  ms._is_generated

let ms_get_lang ms =
  ms._language

let make_ms procname args ret_type attributes loc is_instance is_generated lang =
  let meth_signature = {
    _name = procname;
    _args = args;
    _ret_type = ret_type;
    _attributes = attributes;
    _loc = loc;
    _is_instance = is_instance;
    _is_generated = is_generated;
    _language = lang;
  } in
  meth_signature

let replace_name_ms ms name =
  { ms with _name = name }

let ms_to_string ms =
  let gen = if ms._is_generated then " (generated)" else "" in
  "Method " ^ (Procname.to_string ms._name) ^ gen ^ " " ^
  IList.to_string
    (fun (s1, s2) -> s1 ^ ", " ^ (Clang_ast_j.string_of_type_ptr s2))
    ms._args
  ^ "->" ^ (Clang_ast_j.string_of_type_ptr ms._ret_type) ^ " " ^
  Clang_ast_j.string_of_source_range ms._loc
