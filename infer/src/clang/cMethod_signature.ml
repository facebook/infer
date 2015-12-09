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
  mutable _name : Procname.t;
  _args : (string * Clang_ast_t.type_ptr) list;
  _ret_type : Clang_ast_t.type_ptr;
  _attributes : Clang_ast_t.attribute list;
  _loc : Clang_ast_t.source_range;
  _is_instance : bool;
  _language : CFrontend_config.lang;
  _pointer_to_parent : Clang_ast_t.pointer option;
  _pointer_to_property_opt : Clang_ast_t.pointer option; (* If set then method is a getter/setter *)
}

let ms_get_name ms =
  ms._name

let ms_set_name ms name =
  ms._name <- name

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

let ms_get_lang ms =
  ms._language

let ms_get_pointer_to_parent ms =
  ms._pointer_to_parent

let ms_get_pointer_to_property_opt ms =
  ms._pointer_to_property_opt

(* A method is a getter if it has a link to a property and *)
(* it has 1 argument (this includes self) *)
let ms_is_getter ms =
  Option.is_some ms._pointer_to_property_opt &&
  IList.length ms._args == 1

(* A method is a setter if it has a link to a property and *)
(* it has 2 argument (this includes self) *)
let ms_is_setter ms =
  Option.is_some ms._pointer_to_property_opt &&
  IList.length ms._args == 2

let make_ms procname args ret_type attributes loc is_instance lang pointer_to_parent
    pointer_to_property_opt =
  let meth_signature = {
    _name = procname;
    _args = args;
    _ret_type = ret_type;
    _attributes = attributes;
    _loc = loc;
    _is_instance = is_instance;
    _language = lang;
    _pointer_to_parent = pointer_to_parent;
    _pointer_to_property_opt = pointer_to_property_opt;
  } in
  meth_signature

let replace_name_ms ms name =
  { ms with _name = name }

let ms_to_string ms =
  "Method " ^ (Procname.to_string ms._name) ^ " " ^
  IList.to_string
    (fun (s1, s2) -> s1 ^ ", " ^ (Clang_ast_j.string_of_type_ptr s2))
    ms._args
  ^ "->" ^ (Clang_ast_j.string_of_type_ptr ms._ret_type) ^ " " ^
  Clang_ast_j.string_of_source_range ms._loc
