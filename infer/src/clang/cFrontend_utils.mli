(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Module for utility functions for the whole frontend. Includes functions for printing,  *)
(** for transformations of ast nodes and general utility functions such as  functions on lists *)
open Clang_ast_t

module Printing :
sig

  val log_out : ('a, Format.formatter, unit) format -> 'a

  val log_err : ('a, Format.formatter, unit) format -> 'a

  val log_stats : ('a, Format.formatter, unit) format -> 'a

  val print_failure_info : string -> unit

  val print_tenv : Sil.tenv -> unit

  val print_tenv_struct_unions : Sil.tenv -> unit

  val print_procedures : Cfg.cfg -> unit

  val print_nodes : Cfg.Node.t list -> unit

  val instrs_to_string : Sil.instr list -> string
end

module Ast_utils :
sig
  val namespace_to_string : string option -> string

  val string_of_stmt : Clang_ast_t.stmt -> string

  val get_stmts_from_stmt : Clang_ast_t.stmt -> Clang_ast_t.stmt list

  val string_of_decl : Clang_ast_t.decl -> string

  val string_of_unary_operator_kind : Clang_ast_t.unary_operator_kind -> string

  val property_name : Clang_ast_t.obj_c_property_impl_decl_info -> string

  val property_attribute_compare : property_attribute -> property_attribute -> int

  val property_attribute_eq : property_attribute -> property_attribute -> bool

  val getter_attribute_opt : property_attribute list -> string option

  val setter_attribute_opt : property_attribute list -> string option

  val get_memory_management_attributes : unit -> Clang_ast_t.property_attribute list

  val is_retain : Clang_ast_t.property_attribute option -> bool

  val is_copy : Clang_ast_t.property_attribute option -> bool

  val get_fresh_pointer : unit -> string

  val type_from_unary_expr_or_type_trait_expr_info :
  Clang_ast_t.unary_expr_or_type_trait_expr_info -> Clang_ast_t.qual_type option

end

module General_utils :
sig
  val string_from_list : string list -> string

  val append_no_duplicates_fields : (Ident.fieldname * Sil.typ * Sil.item_annotation) list ->
  (Ident.fieldname * Sil.typ * Sil.item_annotation) list -> (Ident.fieldname * Sil.typ * Sil.item_annotation) list

  val append_no_duplicates_csu : (Sil.csu * Mangled.t) list -> (Sil.csu * Mangled.t) list -> (Sil.csu * Mangled.t) list

  val append_no_duplicates_methods : Procname.t list -> Procname.t list -> Procname.t list

  val append_no_duplicated_vars : (Mangled.t * Sil.typ) list -> (Mangled.t * Sil.typ) list -> (Mangled.t * Sil.typ) list

  val append_no_duplicated_pvars : (Sil.exp * Sil.typ) list -> (Sil.exp * Sil.typ) list -> (Sil.exp * Sil.typ) list

  val sort_fields : (Ident.fieldname * Sil.typ * Sil.item_annotation) list -> (Ident.fieldname * Sil.typ * Sil.item_annotation) list

  val collect_list_tuples : ('a list * 'b list * 'c list * 'd list * 'e list) list ->
  'a list * 'b list * 'c list * 'd list * 'e list -> 'a list * 'b list * 'c list * 'd list * 'e list

  val swap_elements_list : 'a list -> 'a list

  val is_static_var : Clang_ast_t.var_decl_info -> bool

  val mk_fresh_block_procname : Procname.t -> Procname.t

  val get_next_block_pvar : Procname.t -> Sil.pvar

  val reset_block_counter : unit -> unit

  val mk_function_decl_info_from_block : Clang_ast_t.block_decl_info -> Clang_ast_t.function_decl_info

  val zip: 'a list -> 'b list -> ('a * 'b) list

  val list_range: int -> int -> int list

  val replicate: int -> 'a -> 'a list

end
