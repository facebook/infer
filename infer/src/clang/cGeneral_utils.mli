(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** General utility functions such as functions on lists *)

type var_info = Clang_ast_t.decl_info * Clang_ast_t.qual_type * Clang_ast_t.var_decl_info * bool

val add_no_duplicates_fields : Typ.Struct.field -> Typ.Struct.field list -> Typ.Struct.field list

val append_no_duplicates_fields :
  Typ.Struct.field list -> Typ.Struct.field list -> Typ.Struct.field list

val append_no_duplicates_methods :
  Typ.Procname.t list -> Typ.Procname.t list -> Typ.Procname.t list

val swap_elements_list : 'a list -> 'a list

val list_range : int -> int -> int list

val mk_class_field_name : Typ.Name.t -> string -> Typ.Fieldname.t

val get_var_name_mangled :
     Clang_ast_t.decl_info
  -> Clang_ast_t.named_decl_info
  -> Clang_ast_t.var_decl_info
  -> string * Mangled.t

val mk_sil_global_var :
     CFrontend_config.translation_unit_context
  -> ?mk_name:(string -> Mangled.t -> Mangled.t)
  -> Clang_ast_t.decl_info
  -> Clang_ast_t.named_decl_info
  -> Clang_ast_t.var_decl_info
  -> Clang_ast_t.qual_type
  -> Pvar.t

val mk_sil_var :
     CFrontend_config.translation_unit_context
  -> Clang_ast_t.named_decl_info
  -> var_info option
  -> Typ.Procname.t
  -> Typ.Procname.t
  -> Pvar.t

val is_cpp_translation : CFrontend_config.translation_unit_context -> bool
(** true if the current language is C++ or ObjC++ *)

val is_objc_extension : CFrontend_config.translation_unit_context -> bool
(** true if the current language is ObjC or ObjC++ *)
