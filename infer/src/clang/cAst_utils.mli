(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for transformations of ast nodes *)

val dummy_source_range : unit -> Clang_ast_t.source_range

val dummy_stmt_info : unit -> Clang_ast_t.stmt_info

val get_fresh_pointer : unit -> Clang_ast_t.pointer

val get_decl : Clang_ast_t.pointer -> Clang_ast_t.decl option

val get_decl_opt : Clang_ast_t.pointer option -> Clang_ast_t.decl option

val get_stmt : Clang_ast_t.pointer -> Clang_ast_t.source_range -> Clang_ast_t.stmt option

val get_stmt_opt :
  Clang_ast_t.pointer option -> Clang_ast_t.source_range -> Clang_ast_t.stmt option

val get_decl_opt_with_decl_ref : Clang_ast_t.decl_ref option -> Clang_ast_t.decl option

val get_property_of_ivar : Clang_ast_t.pointer -> Clang_ast_t.decl option

val update_sil_types_map : Clang_ast_t.type_ptr -> Typ.desc -> unit

val update_enum_map : Clang_ast_t.pointer -> Exp.t -> unit

val add_enum_constant : Clang_ast_t.pointer -> Clang_ast_t.pointer option -> unit

val get_enum_constant_exp : Clang_ast_t.pointer -> Clang_ast_t.pointer option * Exp.t option

val get_qualified_name : ?linters_mode:bool -> Clang_ast_t.named_decl_info -> QualifiedCppName.t
(** returns sanitized, fully qualified name given name info *)

val get_unqualified_name : Clang_ast_t.named_decl_info -> string
(** returns sanitized unqualified name given name info *)

val get_class_name_from_member : Clang_ast_t.named_decl_info -> QualifiedCppName.t
(** returns qualified class name given member name info *)

val get_type : Clang_ast_t.type_ptr -> Clang_ast_t.c_type option
(** looks up clang pointer to type and returns c_type. It requires type_ptr to be `TPtr. *)

val get_desugared_type : Clang_ast_t.type_ptr -> Clang_ast_t.c_type option
(** looks up clang pointer to type and resolves any sugar around it.
    See get_type for more info and restrictions *)

val get_decl_from_typ_ptr : Clang_ast_t.type_ptr -> Clang_ast_t.decl option
(** returns declaration of the type for certain types
    (RecordType, ObjCInterfaceType and None for others *)

val name_of_typedef_type_info : Clang_ast_t.typedef_type_info -> QualifiedCppName.t

val name_opt_of_typedef_qual_type : Clang_ast_t.qual_type -> QualifiedCppName.t option

type qual_type_to_sil_type = Tenv.t -> Clang_ast_t.qual_type -> Typ.t

type procname_from_decl =
     ?tenv:Tenv.t
  -> ?block_return_type:Clang_ast_t.qual_type
  -> ?outer_proc:Typ.Procname.t
  -> Clang_ast_t.decl
  -> Typ.Procname.t

val qual_type_of_decl_ptr : Clang_ast_t.pointer -> Clang_ast_t.qual_type

val add_type_from_decl_ref_opt :
  qual_type_to_sil_type -> Tenv.t -> Clang_ast_t.decl_ref option -> bool -> unit

val add_type_from_decl_ref_list :
  qual_type_to_sil_type -> Tenv.t -> Clang_ast_t.decl_ref list -> unit

val get_function_decl_with_body : Clang_ast_t.pointer -> Clang_ast_t.decl option

val get_info_from_decl_ref :
  Clang_ast_t.decl_ref -> Clang_ast_t.named_decl_info * Clang_ast_t.pointer * Clang_ast_t.qual_type

val exists_eventually_st : ('a -> Clang_ast_t.stmt -> bool) -> 'a -> Clang_ast_t.stmt -> bool

val is_syntactically_global_var : Clang_ast_t.decl -> bool
(** true if a declaration is a global variable *)

val is_static_local_var : Clang_ast_t.decl -> bool
(** true if a declaration is a static local variable *)

val is_const_expr_var : Clang_ast_t.decl -> bool
(** true if a declaration is a constexpr variable *)

val generate_key_stmt : Clang_ast_t.stmt -> string
(** Generates a key for a statement based on its sub-statements and the statement tag. *)

val generate_key_decl : Clang_ast_t.decl -> string
(** Generates a key for a declaration based on its name and the declaration tag. *)

val get_super_if : Clang_ast_t.decl option -> Clang_ast_t.decl option
(** Given an objc impl or interface decl, returns the objc interface decl of
    the superclass, if any. *)

val get_impl_decl_info : Clang_ast_t.decl -> Clang_ast_t.obj_c_implementation_decl_info option

val get_super_ObjCImplementationDecl :
  Clang_ast_t.obj_c_implementation_decl_info -> Clang_ast_t.decl option
(** Given an objc impl decl info, return its super class implementation decl *)

val is_objc_if_descendant :
  ?blacklist:string list -> Clang_ast_t.decl option -> string list -> bool
(** Recursively go up the inheritance hierarchy of a given ObjCInterfaceDecl.
    Returns true if the passed in decl is an objc interface decl that's an
    eventual descendant of one of the classes passed in.
    Ancestors param is a list of strings that represent the class names.
    Will short-circuit on NSObject and NSProxy since those are known to be
    common base classes.
    The list of classes to short-circuit on can be overridden via specifying
    the named `blacklist` argument. *)

val qual_type_to_objc_interface : Clang_ast_t.qual_type -> Clang_ast_t.decl option

val is_objc_factory_method :
  class_decl:Clang_ast_t.decl option -> method_decl:Clang_ast_t.decl option -> bool
(** A class method that returns an instance of the class is a factory method. *)

val name_of_decl_ref_opt : Clang_ast_t.decl_ref option -> string option

val sil_annot_of_type : Clang_ast_t.qual_type -> Annot.Item.t

val type_of_decl : Clang_ast_t.decl -> Clang_ast_t.type_ptr option

val get_record_fields : Clang_ast_t.decl -> Clang_ast_t.decl list

val get_cxx_base_classes : Clang_ast_t.decl -> Clang_ast_t.type_ptr list

val get_cxx_virtual_base_classes : Clang_ast_t.decl -> Clang_ast_t.type_ptr list

val is_std_vector : Clang_ast_t.qual_type -> bool

val has_block_attribute : Clang_ast_t.decl -> bool

val is_implicit_decl : Clang_ast_t.decl -> bool

val get_superclass_curr_class_objc_from_decl : Clang_ast_t.decl -> Clang_ast_t.decl_ref option
