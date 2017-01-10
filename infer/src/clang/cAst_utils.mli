(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Functions for transformations of ast nodes *)

val string_of_stmt : Clang_ast_t.stmt -> string

val get_stmts_from_stmt : Clang_ast_t.stmt -> Clang_ast_t.stmt list

val string_of_decl : Clang_ast_t.decl -> string

val string_of_unary_operator_kind : Clang_ast_t.unary_operator_kind -> string

val name_opt_of_name_info_opt : Clang_ast_t.named_decl_info option -> string option

val property_name : Clang_ast_t.obj_c_property_impl_decl_info -> Clang_ast_t.named_decl_info

val compare_property_attribute :
  Clang_ast_t.property_attribute -> Clang_ast_t.property_attribute -> int

val generated_ivar_name :
  Clang_ast_t.named_decl_info -> Clang_ast_t.named_decl_info

val equal_property_attribute :
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

val get_property_of_ivar : Clang_ast_t.pointer -> Clang_ast_t.decl option

val update_sil_types_map : Clang_ast_t.type_ptr -> Typ.t -> unit

val update_enum_map : Clang_ast_t.pointer -> Exp.t -> unit

val add_enum_constant : Clang_ast_t.pointer -> Clang_ast_t.pointer option -> unit

val get_enum_constant_exp : Clang_ast_t.pointer -> Clang_ast_t.pointer option * Exp.t option

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

(** returns declaration of the type for certain types
    (RecordType, ObjCInterfaceType and None for others *)
val get_decl_from_typ_ptr : Clang_ast_t.type_ptr -> Clang_ast_t.decl option

(** returns string representation of type_ptr
    NOTE: this doesn't expand type, it only converts type_ptr to string *)
val string_of_type_ptr : Clang_ast_t.type_ptr -> string

val name_of_typedef_type_info : Clang_ast_t.typedef_type_info -> string

(** returns name of typedef if type_ptr points to Typedef, None otherwise *)
val name_opt_of_typedef_type_ptr : Clang_ast_t.type_ptr -> string option

val string_of_qual_type : Clang_ast_t.qual_type -> string

val make_name_decl : string -> Clang_ast_t.named_decl_info

val make_qual_name_decl : string list -> string -> Clang_ast_t.named_decl_info

type type_ptr_to_sil_type =  Tenv.t -> Clang_ast_t.type_ptr -> Typ.t

val add_type_from_decl_ref : type_ptr_to_sil_type -> Tenv.t -> Clang_ast_t.decl_ref option ->
  bool -> unit

val add_type_from_decl_ref_list : type_ptr_to_sil_type -> Tenv.t -> Clang_ast_t.decl_ref list ->
  unit

val get_function_decl_with_body : Clang_ast_t.pointer -> Clang_ast_t.decl option

val get_info_from_decl_ref : Clang_ast_t.decl_ref ->
  Clang_ast_t.named_decl_info * Clang_ast_t.pointer * Clang_ast_t.type_ptr

val exists_eventually_st : ('a -> Clang_ast_t.stmt -> bool) -> 'a -> Clang_ast_t.stmt -> bool

(** true if a declaration is a global variable *)
val is_syntactically_global_var : Clang_ast_t.decl -> bool

(** true if a declaration is a constexpr variable *)
val is_const_expr_var : Clang_ast_t.decl -> bool

val is_ptr_to_objc_class : Clang_ast_t.c_type option -> string -> bool

val full_name_of_decl_opt : Clang_ast_t.decl option -> string

(** Generates a key for a statement based on its sub-statements and the statement tag. *)
val generate_key_stmt : Clang_ast_t.stmt -> string

(** Generates a key for a declaration based on its name and the declaration tag. *)
val generate_key_decl : Clang_ast_t.decl -> string

(** Given an objc impl or interface decl, returns the objc interface decl of
    the superclass, if any. *)
val get_super_if : Clang_ast_t.decl option -> Clang_ast_t.decl option

val get_impl_decl_info : Clang_ast_t.decl -> Clang_ast_t.obj_c_implementation_decl_info option

(** Given an objc impl decl info, return the super class's list of decls and
    its objc impl decl info. *)
val get_super_impl :
  Clang_ast_t.obj_c_implementation_decl_info ->
  (Clang_ast_t.decl list *
   Clang_ast_t.obj_c_implementation_decl_info)
    option

(** Given an objc impl decl info, return its super class implementation decl *)
val get_super_ObjCImplementationDecl :
  Clang_ast_t.obj_c_implementation_decl_info -> Clang_ast_t.decl option

(** Recursively go up the inheritance hierarchy of a given ObjCInterfaceDecl.
    Returns true if the passed in decl is an objc interface decl that's an
    eventual descendant of one of the classes passed in.
    Ancestors param is a list of strings that represent the class names.
    Will short-circuit on NSObject and NSProxy since those are known to be
    common base classes.
    The list of classes to short-circuit on can be overridden via specifying
    the named `blacklist` argument. *)
val is_objc_if_descendant :
  ?blacklist:string list -> Clang_ast_t.decl option -> string list -> bool

val type_ptr_to_objc_interface : Clang_ast_types.t_ptr -> Clang_ast_t.decl option

(** A class method that returns an instance of the class is a factory method. *)
val is_objc_factory_method : Clang_ast_t.decl -> Clang_ast_t.decl -> bool

val name_of_decl_ref_opt : Clang_ast_t.decl_ref option -> string option
