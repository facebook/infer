(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for utility functions for the whole frontend. Includes functions for printing,  *)
(** for transformations of ast nodes and general utility functions such as  functions on lists *)

module Printing :
sig

  val log_out : ('a, Format.formatter, unit) format -> 'a

  val log_err : ('a, Format.formatter, unit) format -> 'a

  val log_stats : ('a, Format.formatter, unit) format -> 'a

  val print_failure_info : string -> unit

  val print_tenv : Tenv.t -> unit

  val print_tenv_struct_unions : Tenv.t -> unit

  val print_procedures : Cfg.cfg -> unit

  val print_nodes : Cfg.Node.t list -> unit

  val instrs_to_string : Sil.instr list -> string

  val field_to_string : Ident.fieldname * Sil.typ * Sil.item_annotation -> string
end

module Ast_utils :
sig
  val string_of_stmt : Clang_ast_t.stmt -> string

  val get_stmts_from_stmt : Clang_ast_t.stmt -> Clang_ast_t.stmt list

  val string_of_decl : Clang_ast_t.decl -> string

  val string_of_unary_operator_kind : Clang_ast_t.unary_operator_kind -> string

  val name_opt_of_name_info_opt : Clang_ast_t.named_decl_info option -> string option

  val property_name : Clang_ast_t.obj_c_property_impl_decl_info -> Clang_ast_t.named_decl_info

  val property_attribute_compare :
    Clang_ast_t.property_attribute -> Clang_ast_t.property_attribute -> int

  val generated_ivar_name :
    Clang_ast_t.named_decl_info -> Clang_ast_t.named_decl_info

  val property_attribute_eq :
    Clang_ast_t.property_attribute -> Clang_ast_t.property_attribute -> bool

  val get_memory_management_attributes : unit -> Clang_ast_t.property_attribute list

  val is_retain : Clang_ast_t.property_attribute option -> bool

  val is_copy : Clang_ast_t.property_attribute option -> bool

  val is_type_nonnull : Clang_ast_t.type_ptr -> bool

  val is_type_nullable : Clang_ast_t.type_ptr -> bool

  val get_fresh_pointer : unit -> Clang_ast_t.pointer

  val get_invalid_pointer : unit -> Clang_ast_t.pointer

  val type_from_unary_expr_or_type_trait_expr_info :
    Clang_ast_t.unary_expr_or_type_trait_expr_info -> Clang_ast_t.type_ptr option

  val get_decl : Clang_ast_t.pointer -> Clang_ast_t.decl option

  val get_decl_opt : Clang_ast_t.pointer option -> Clang_ast_t.decl option

  val get_stmt : Clang_ast_t.pointer -> Clang_ast_t.stmt option

  val get_stmt_opt : Clang_ast_t.pointer option -> Clang_ast_t.stmt option

  val get_decl_opt_with_decl_ref : Clang_ast_t.decl_ref option -> Clang_ast_t.decl option

  val update_sil_types_map : Clang_ast_t.type_ptr -> Sil.typ -> unit

  val update_enum_map : Clang_ast_t.pointer -> Sil.exp -> unit

  val add_enum_constant : Clang_ast_t.pointer -> Clang_ast_t.pointer option -> unit

  val get_enum_constant_exp : Clang_ast_t.pointer -> Clang_ast_t.pointer option * Sil.exp option

  (** returns sanitized, fully qualified name given name info *)
  val get_qualified_name : Clang_ast_t.named_decl_info -> string

  (** returns sanitized unqualified name given name info *)
  val get_unqualified_name : Clang_ast_t.named_decl_info -> string

  (** returns qualified class name given member name info *)
  val get_class_name_from_member : Clang_ast_t.named_decl_info -> string

  (** looks up clang pointer to type and returns c_type. It requires type_ptr to be `TPtr. *)
  val get_type : Clang_ast_t.type_ptr -> Clang_ast_t.c_type option

  (** looks up clang pointer to type and resolves any sugar around it.
      See get_type for more info and restrictions *)
  val get_desugared_type : Clang_ast_t.type_ptr -> Clang_ast_t.c_type option

  (** returns declaration of the type for certain types and crashes for others
      NOTE: this function needs extending to handle objC types *)
  val get_decl_from_typ_ptr : Clang_ast_t.type_ptr -> Clang_ast_t.decl option

  val string_of_type_ptr : Clang_ast_t.type_ptr -> string

  val make_name_decl : string -> Clang_ast_t.named_decl_info

  val make_qual_name_decl : string list -> string -> Clang_ast_t.named_decl_info

  type type_ptr_to_sil_type =  Tenv.t -> Clang_ast_t.type_ptr -> Sil.typ

  val add_type_from_decl_ref : type_ptr_to_sil_type -> Tenv.t -> Clang_ast_t.decl_ref option ->
    bool -> unit

  val add_type_from_decl_ref_list : type_ptr_to_sil_type -> Tenv.t -> Clang_ast_t.decl_ref list ->
    unit

  val get_function_decl_with_body : Clang_ast_t.pointer -> Clang_ast_t.decl option

  val get_info_from_decl_ref : Clang_ast_t.decl_ref ->
    Clang_ast_t.named_decl_info * Clang_ast_t.pointer * Clang_ast_t.type_ptr

end

module General_utils :
sig

  type var_info = Clang_ast_t.decl_info * Clang_ast_t.type_ptr * Clang_ast_t.var_decl_info * bool

  val string_from_list : string list -> string

  val append_no_duplicates_fields : (Ident.fieldname * Sil.typ * Sil.item_annotation) list ->
    (Ident.fieldname * Sil.typ * Sil.item_annotation) list ->
    (Ident.fieldname * Sil.typ * Sil.item_annotation) list

  val append_no_duplicates_csu :
    Typename.t list -> Typename.t list -> Typename.t list

  val append_no_duplicates_methods : Procname.t list -> Procname.t list -> Procname.t list

  val append_no_duplicated_vars :
    (Mangled.t * Sil.typ) list -> (Mangled.t * Sil.typ) list -> (Mangled.t * Sil.typ) list

  val append_no_duplicateds :
    (Sil.exp * Sil.typ) list -> (Sil.exp * Sil.typ) list -> (Sil.exp * Sil.typ) list

  val sort_fields :
    (Ident.fieldname * Sil.typ * Sil.item_annotation) list ->
    (Ident.fieldname * Sil.typ * Sil.item_annotation) list

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

  val mk_procname_from_objc_method : string -> string -> Procname.objc_method_kind -> Procname.t

  val mk_procname_from_function : string -> (Clang_ast_t.decl_info * Clang_ast_t.function_decl_info)
      option -> Clang_ast_t.type_ptr -> CFrontend_config.lang -> Procname.t

  val mk_procname_from_cpp_method : string -> string -> Clang_ast_t.type_ptr -> Procname.t

  val mk_class_field_name : Clang_ast_t.named_decl_info -> Ident.fieldname

  val get_var_name_string : Clang_ast_t.named_decl_info -> Clang_ast_t.var_decl_info -> string

  val mk_sil_var : Clang_ast_t.named_decl_info -> var_info option -> Procname.t -> Procname.t ->
    Pvar.t

  val is_cpp_translation : CFrontend_config.lang -> bool

end
