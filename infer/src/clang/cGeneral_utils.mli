(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** General utility functions such as functions on lists *)


type var_info = Clang_ast_t.decl_info * Clang_ast_t.qual_type * Clang_ast_t.var_decl_info * bool

val string_from_list : string list -> string

val append_no_duplicates_fields : (Ident.fieldname * Typ.t * Annot.Item.t) list ->
  (Ident.fieldname * Typ.t * Annot.Item.t) list ->
  (Ident.fieldname * Typ.t * Annot.Item.t) list

val append_no_duplicates_csu :
  Typename.t list -> Typename.t list -> Typename.t list

val append_no_duplicates_methods : Procname.t list -> Procname.t list -> Procname.t list

val sort_fields :
  (Ident.fieldname * Typ.t * Annot.Item.t) list ->
  (Ident.fieldname * Typ.t * Annot.Item.t) list

val sort_fields_tenv : Tenv.t -> unit

val collect_list_tuples : ('a list * 'b list * 'c list * 'd list * 'e list) list ->
  'a list * 'b list * 'c list * 'd list * 'e list ->
  'a list * 'b list * 'c list * 'd list * 'e list

val swap_elements_list : 'a list -> 'a list

val is_static_var : Clang_ast_t.var_decl_info -> bool

val mk_fresh_block_procname : Procname.t -> Procname.t

val get_next_block_pvar : Procname.t -> Pvar.t

val reset_block_counter : unit -> unit

val zip: 'a list -> 'b list -> ('a * 'b) list

val list_range: int -> int -> int list

val replicate: int -> 'a -> 'a list

val mk_procname_from_objc_method : string -> string -> Procname.objc_cpp_method_kind -> Procname.t

val mk_procname_from_function : CFrontend_config.translation_unit_context -> string
  -> (Clang_ast_t.decl_info * Clang_ast_t.function_decl_info) option -> Procname.t

val get_mangled_method_name : Clang_ast_t.function_decl_info ->
  Clang_ast_t.cxx_method_decl_info -> string option

val mk_procname_from_cpp_method :
  string -> string -> ?meth_decl:Clang_ast_t.decl -> string option -> Procname.t

val procname_of_decl : CFrontend_config.translation_unit_context -> Clang_ast_t.decl -> Procname.t

val mk_class_field_name : Clang_ast_t.named_decl_info -> Ident.fieldname

val get_var_name_mangled : Clang_ast_t.named_decl_info -> Clang_ast_t.var_decl_info ->
  (string * Mangled.t)

val mk_sil_global_var : CFrontend_config.translation_unit_context ->
  ?mk_name:(string -> Mangled.t -> Mangled.t) ->
  Clang_ast_t.named_decl_info -> Clang_ast_t.var_decl_info -> Clang_ast_t.qual_type -> Pvar.t

val mk_sil_var : CFrontend_config.translation_unit_context -> Clang_ast_t.named_decl_info ->
  var_info option -> Procname.t -> Procname.t -> Pvar.t

(** true if the current language is C++ or ObjC++ *)
val is_cpp_translation : CFrontend_config.translation_unit_context -> bool

(** true if the current language is ObjC or ObjC++ *)
val is_objc_extension : CFrontend_config.translation_unit_context -> bool
