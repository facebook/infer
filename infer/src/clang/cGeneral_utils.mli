(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** General utility functions such as functions on lists *)

val add_no_duplicates_fields : Struct.field -> Struct.field list -> Struct.field list

val append_no_duplicates_fields : Struct.field list -> Struct.field list -> Struct.field list

val append_no_duplicates_methods : Procname.t list -> Procname.t list -> Procname.t list

val swap_elements_list : 'a list -> 'a list

val list_range : int -> int -> int list

val get_var_name_mangled :
     Clang_ast_t.decl_info
  -> Clang_ast_t.named_decl_info
  -> Clang_ast_t.var_decl_info
  -> string * Mangled.t

val is_cpp_translation : CFrontend_config.translation_unit_context -> bool
(** true if the current language is C++ or ObjC++ *)

val is_objc_extension : CFrontend_config.translation_unit_context -> bool
(** true if the current language is ObjC or ObjC++ *)

val is_type_pod : Clang_ast_t.qual_type -> bool
