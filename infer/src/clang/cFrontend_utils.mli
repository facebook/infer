(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for utility functions for the whole frontend. Includes functions for transformations of
    ast nodes and general utility functions such as functions on lists *)

module Ast_utils :
sig
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
end

module General_utils :
sig

  type var_info = Clang_ast_t.decl_info * Clang_ast_t.qual_type * Clang_ast_t.var_decl_info * bool

  val string_from_list : string list -> string

  val append_no_duplicates_fields : (Ident.fieldname * Typ.t * Annot.Item.t) list ->
    (Ident.fieldname * Typ.t * Annot.Item.t) list ->
    (Ident.fieldname * Typ.t * Annot.Item.t) list

  val append_no_duplicates_csu :
    Typename.t list -> Typename.t list -> Typename.t list

  val append_no_duplicates_methods : Procname.t list -> Procname.t list -> Procname.t list

  val append_no_duplicated_vars :
    (Mangled.t * Typ.t) list -> (Mangled.t * Typ.t) list -> (Mangled.t * Typ.t) list

  val append_no_duplicateds :
    (Exp.t * Typ.t) list -> (Exp.t * Typ.t) list -> (Exp.t * Typ.t) list

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

end
