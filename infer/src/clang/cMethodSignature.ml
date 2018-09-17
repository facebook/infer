(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Define the signature of a method consisting of its name, its arguments, return type, location
   and whether its an instance method.  *)

module F = Format

type param_type = {name: Mangled.t; typ: Typ.t; is_pointer_to_const: bool; annot: Annot.Item.t}

let mk_param_type ?(is_pointer_to_const = false) ?(annot = Annot.Item.empty) name typ =
  {name; typ; is_pointer_to_const; annot}


type t =
  { name: Typ.Procname.t
  ; access: Clang_ast_t.access_specifier
  ; class_param: param_type option
  ; params: param_type list
  ; ret_type: Typ.t * Annot.Item.t
  ; has_added_return_param: bool
  ; attributes: Clang_ast_t.attribute list
  ; loc: Clang_ast_t.source_range
  ; method_kind: ClangMethodKind.t
  ; is_cpp_virtual: bool
  ; is_cpp_nothrow: bool
  ; is_variadic: bool
  ; pointer_to_parent: Clang_ast_t.pointer option
  ; pointer_to_property_opt: Clang_ast_t.pointer option
  ; (* If set then method is a getter/setter *)
    return_param_typ: Typ.t option }

(* A method is a getter if it has a link to a property and *)
(* it has 0 arguments *)
let is_getter {pointer_to_property_opt; params} =
  Option.is_some pointer_to_property_opt && Int.equal (List.length params) 0


(* A method is a setter if it has a link to a property and *)
(* it has 1 argument *)
let is_setter {pointer_to_property_opt; params} =
  Option.is_some pointer_to_property_opt && Int.equal (List.length params) 1


let mk name class_param params ret_type ?(has_added_return_param = false) attributes loc
    method_kind ?(is_cpp_virtual = false) ?(is_cpp_nothrow = false) ?(is_variadic = false)
    pointer_to_parent pointer_to_property_opt return_param_typ access =
  { name
  ; access
  ; class_param
  ; params
  ; ret_type
  ; has_added_return_param
  ; attributes
  ; loc
  ; method_kind
  ; is_cpp_virtual
  ; is_cpp_nothrow
  ; is_variadic
  ; pointer_to_parent
  ; pointer_to_property_opt
  ; return_param_typ }


let pp fmt ms =
  let pp_param fmt {name; typ} = F.fprintf fmt "%a, %a" Mangled.pp name (Typ.pp Pp.text) typ in
  Format.fprintf fmt "Method %a [%a]->%a %a"
    (Pp.to_string ~f:Typ.Procname.to_string)
    ms.name (Pp.comma_seq pp_param) ms.params (Typ.pp Pp.text) (fst ms.ret_type)
    (Pp.to_string ~f:Clang_ast_j.string_of_source_range)
    ms.loc
