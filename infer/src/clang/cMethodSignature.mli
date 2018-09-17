(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Define the signature of a method consisting of its name, its arguments, return type, location
   and whether its an instance method. *)

type param_type = {name: Mangled.t; typ: Typ.t; is_pointer_to_const: bool; annot: Annot.Item.t}

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

val is_getter : t -> bool

val is_setter : t -> bool

val mk :
     Typ.Procname.t
  -> param_type option
  -> param_type list
  -> Typ.t * Annot.Item.t
  -> ?has_added_return_param:bool
  -> Clang_ast_t.attribute list
  -> Clang_ast_t.source_range
  -> ClangMethodKind.t
  -> ?is_cpp_virtual:bool
  -> ?is_cpp_nothrow:bool
  -> ?is_variadic:bool
  -> Clang_ast_t.pointer option
  -> Clang_ast_t.pointer option
  -> Typ.t option
  -> Clang_ast_t.access_specifier
  -> t

val pp : Format.formatter -> t -> unit

val mk_param_type :
  ?is_pointer_to_const:bool -> ?annot:Annot.Item.t -> Mangled.t -> Typ.t -> param_type
