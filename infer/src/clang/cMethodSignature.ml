(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Define the signature of a method consisting of its name, its arguments, return type, location
    and whether its an instance method. *)

module F = Format

type param_type =
  { annot: Annot.Item.t
  ; is_no_escape_block_arg: bool
  ; is_pointer_to_const: bool
  ; is_reference: bool
  ; name: Mangled.t
  ; typ: Typ.t }

let mk_param_type ?(is_pointer_to_const = false) ?(is_reference = false) ?(annot = Annot.Item.empty)
    ?(is_no_escape_block_arg = false) name typ =
  {name; typ; is_pointer_to_const; is_reference; annot; is_no_escape_block_arg}


type t =
  { name: Procname.t
  ; access: Clang_ast_t.access_specifier
  ; class_param: param_type option
  ; params: param_type list
  ; ret_type: Typ.t * Annot.Item.t
  ; has_added_return_param: bool
  ; is_ret_type_pod: bool
  ; is_ret_constexpr: bool
  ; attributes: Clang_ast_t.attribute list
  ; loc: Clang_ast_t.source_range
  ; method_kind: ClangMethodKind.t
  ; is_cpp_const_member_fun: bool
  ; is_cpp_virtual: bool
  ; is_cpp_copy_assignment: bool
  ; is_cpp_copy_ctor: bool
  ; is_cpp_move_ctor: bool
  ; is_cpp_deleted: bool
  ; is_cpp_implicit: bool
  ; block_as_arg_attributes: ProcAttributes.block_as_arg_attributes option
  ; is_no_return: bool
  ; is_variadic: bool
  ; pointer_to_parent: Clang_ast_t.pointer option
  ; pointer_to_property_opt: Clang_ast_t.pointer option
  ; (* If set then method is a getter/setter *)
    return_param_typ: Typ.t option }

(* A method is a getter if it has a link to a property and *)
(* it has 0 arguments *)
let is_getter {pointer_to_property_opt; params} =
  Option.is_some pointer_to_property_opt
  && match params with [] -> true | [{name}] -> Mangled.is_return_param name | _ -> false


(* A method is a setter if it has a link to a property and *)
(* it has 1 argument *)
let is_setter {pointer_to_property_opt; params} =
  Option.is_some pointer_to_property_opt && Int.equal (List.length params) 1


let mk name class_param params ret_type ?(has_added_return_param = false) ?(is_ret_type_pod = true)
    ~is_ret_constexpr attributes loc method_kind ?(is_cpp_const_member_fun = false)
    ?(is_cpp_virtual = false) ?(is_cpp_copy_assignment = false) ?(is_cpp_copy_ctor = false)
    ?(is_cpp_move_ctor = false) ?(is_cpp_deleted = false) ?(is_cpp_implicit = false)
    ?(block_as_arg_attributes = None) ?(is_no_return = false) ?(is_variadic = false)
    pointer_to_parent pointer_to_property_opt return_param_typ access =
  { name
  ; access
  ; class_param
  ; params
  ; ret_type
  ; has_added_return_param
  ; is_ret_type_pod
  ; is_ret_constexpr
  ; attributes
  ; loc
  ; method_kind
  ; is_cpp_const_member_fun
  ; is_cpp_virtual
  ; is_cpp_copy_assignment
  ; is_cpp_copy_ctor
  ; is_cpp_move_ctor
  ; is_cpp_deleted
  ; is_cpp_implicit
  ; block_as_arg_attributes
  ; is_no_return
  ; is_variadic
  ; pointer_to_parent
  ; pointer_to_property_opt
  ; return_param_typ }


let pp fmt ms =
  let pp_param fmt {name; typ; is_no_escape_block_arg} =
    F.fprintf fmt "%a, %a (is_no_escape_block=%b)" Mangled.pp name (Typ.pp Pp.text) typ
      is_no_escape_block_arg
  in
  Format.fprintf fmt "Method %a [%a]->%a %a(block_as_arg_attributes=%a)"
    (Pp.of_string ~f:Procname.to_string)
    ms.name (Pp.comma_seq pp_param) ms.params (Typ.pp Pp.text) (fst ms.ret_type)
    (Pp.of_string ~f:Clang_ast_j.string_of_source_range)
    ms.loc
    (Pp.option ProcAttributes.pp_block_as_arg_attributes)
    ms.block_as_arg_attributes
