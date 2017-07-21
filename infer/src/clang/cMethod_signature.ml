(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Define the signature of a method consisting of its name, its arguments, *)

(** return type, location and whether its an instance method.  *)

type method_signature =
  { mutable name: Typ.Procname.t
  ; access: Clang_ast_t.access_specifier
  ; args: (Mangled.t * Clang_ast_t.qual_type) list
  ; ret_type: Clang_ast_t.qual_type
  ; attributes: Clang_ast_t.attribute list
  ; loc: Clang_ast_t.source_range
  ; is_instance: bool
  ; is_cpp_virtual: bool
  ; is_cpp_nothrow: bool
  ; language: CFrontend_config.clang_lang
  ; pointer_to_parent: Clang_ast_t.pointer option
  ; pointer_to_property_opt: Clang_ast_t.pointer option
  ; (* If set then method is a getter/setter *)
  return_param_typ: Typ.t option }

let ms_get_name {name} = name

let ms_set_name ms name = ms.name <- name

let ms_get_access {access} = access

let ms_get_args {args} = args

let ms_get_ret_type {ret_type} = ret_type

let ms_get_attributes {attributes} = attributes

let ms_get_loc {loc} = loc

let ms_is_instance {is_instance} = is_instance

let ms_is_cpp_virtual {is_cpp_virtual} = is_cpp_virtual

let ms_is_cpp_nothrow {is_cpp_nothrow} = is_cpp_nothrow

let ms_get_lang {language} = language

let ms_get_pointer_to_parent {pointer_to_parent} = pointer_to_parent

let ms_get_pointer_to_property_opt {pointer_to_property_opt} = pointer_to_property_opt

let ms_get_return_param_typ {return_param_typ} = return_param_typ

(* A method is a getter if it has a link to a property and *)
(* it has 1 argument (this includes self) *)
let ms_is_getter {pointer_to_property_opt; args} =
  Option.is_some pointer_to_property_opt && Int.equal (List.length args) 1

(* A method is a setter if it has a link to a property and *)
(* it has 2 argument (this includes self) *)
let ms_is_setter {pointer_to_property_opt; args} =
  Option.is_some pointer_to_property_opt && Int.equal (List.length args) 2

let make_ms name args ret_type attributes loc is_instance ?is_cpp_virtual ?is_cpp_nothrow language
    pointer_to_parent pointer_to_property_opt return_param_typ access =
  let booloption_to_bool = function Some b -> b | None -> false in
  let is_cpp_virtual = booloption_to_bool is_cpp_virtual in
  let is_cpp_nothrow = booloption_to_bool is_cpp_nothrow in
  { name
  ; access
  ; args
  ; ret_type
  ; attributes
  ; loc
  ; is_instance
  ; is_cpp_virtual
  ; is_cpp_nothrow
  ; language
  ; pointer_to_parent
  ; pointer_to_property_opt
  ; return_param_typ }

let replace_name_ms ms name = {ms with name}

let ms_to_string ms =
  "Method " ^ Typ.Procname.to_string ms.name ^ " "
  ^ IList.to_string
      (fun (s1, s2) -> Mangled.to_string s1 ^ ", " ^ CAst_utils.string_of_qual_type s2)
      ms.args
  ^ "->" ^ Clang_ast_extend.type_ptr_to_string ms.ret_type.Clang_ast_t.qt_type_ptr ^ " "
  ^ Clang_ast_j.string_of_source_range ms.loc
