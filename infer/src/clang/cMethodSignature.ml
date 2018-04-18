(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Define the signature of a method consisting of its name, its arguments, return type, location
   and whether its an instance method.  *)

module F = Format

type t =
  { name: Typ.Procname.t
  ; access: Clang_ast_t.access_specifier
  ; args: (Mangled.t * Clang_ast_t.qual_type) list
  ; ret_type: Clang_ast_t.qual_type
  ; attributes: Clang_ast_t.attribute list
  ; loc: Clang_ast_t.source_range
  ; method_kind: ProcAttributes.clang_method_kind
  ; is_cpp_virtual: bool
  ; is_cpp_nothrow: bool
  ; lang: CFrontend_config.clang_lang
  ; pointer_to_parent: Clang_ast_t.pointer option
  ; pointer_to_property_opt: Clang_ast_t.pointer option
  ; (* If set then method is a getter/setter *)
    return_param_typ: Typ.t option }

(* A method is a getter if it has a link to a property and *)
(* it has 1 argument (this includes self) *)
let is_getter {pointer_to_property_opt; args} =
  Option.is_some pointer_to_property_opt && Int.equal (List.length args) 1


(* A method is a setter if it has a link to a property and *)
(* it has 2 argument (this includes self) *)
let is_setter {pointer_to_property_opt; args} =
  Option.is_some pointer_to_property_opt && Int.equal (List.length args) 2


let mk name args ret_type attributes loc method_kind ?is_cpp_virtual ?is_cpp_nothrow lang
    pointer_to_parent pointer_to_property_opt return_param_typ access =
  let is_cpp_virtual = Option.value is_cpp_virtual ~default:false in
  let is_cpp_nothrow = Option.value is_cpp_nothrow ~default:false in
  { name
  ; access
  ; args
  ; ret_type
  ; attributes
  ; loc
  ; method_kind
  ; is_cpp_virtual
  ; is_cpp_nothrow
  ; lang
  ; pointer_to_parent
  ; pointer_to_property_opt
  ; return_param_typ }


let pp fmt ms =
  let pp_arg fmt (mangled, qual_type) =
    F.fprintf fmt "%a, %a" Mangled.pp mangled
      (Pp.to_string ~f:CAst_utils.string_of_qual_type)
      qual_type
  in
  Format.fprintf fmt "Method %a [%a]->%a %a"
    (Pp.to_string ~f:Typ.Procname.to_string)
    ms.name (Pp.comma_seq pp_arg) ms.args
    (Pp.to_string ~f:Clang_ast_extend.type_ptr_to_string)
    ms.ret_type.Clang_ast_t.qt_type_ptr
    (Pp.to_string ~f:Clang_ast_j.string_of_source_range)
    ms.loc
