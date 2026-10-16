(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

val get_stmt_exn : Clang_ast_t.pointer -> Clang_ast_t.source_range -> Clang_ast_t.stmt

val get_stmt_opt : Clang_ast_t.pointer option -> Clang_ast_t.source_range -> Clang_ast_t.stmt option

val get_decl_opt_with_decl_ref : Clang_ast_t.decl_ref -> Clang_ast_t.decl option

val get_decl_opt_with_decl_ref_opt : Clang_ast_t.decl_ref option -> Clang_ast_t.decl option

val update_sil_types_map : Clang_ast_t.type_ptr -> Typ.desc -> unit

val update_enum_map_exn : Clang_ast_t.pointer -> Exp.t -> unit

val add_enum_constant : Clang_ast_t.pointer -> Clang_ast_t.pointer option -> unit

val get_enum_constant_exp_exn : Clang_ast_t.pointer -> Clang_ast_t.pointer option * Exp.t option

val get_qualified_name : ?linters_mode:bool -> Clang_ast_t.named_decl_info -> QualifiedCppName.t
(** returns sanitized, fully qualified name given name info *)

val get_unqualified_name : Clang_ast_t.named_decl_info -> string
(** returns sanitized unqualified name given name info *)

val get_class_name_from_member : Clang_ast_t.named_decl_info -> QualifiedCppName.t
(** returns qualified class name given member name info *)

val get_type : Clang_ast_t.type_ptr -> Clang_ast_t.c_type option
(** looks up clang pointer to type and returns c_type. It requires type_ptr to be `TPtr. *)

val get_desugared_type : Clang_ast_t.type_ptr -> Clang_ast_t.c_type option
(** looks up clang pointer to type and resolves any sugar around it. See get_type for more info and
    restrictions *)

val get_decl_from_typ_ptr : Clang_ast_t.type_ptr -> Clang_ast_t.decl option
(** returns declaration of the type for certain types (RecordType, ObjCInterfaceType and None for
    others *)

type qual_type_to_sil_type = Tenv.t -> Clang_ast_t.qual_type -> Typ.t

type procname_from_decl =
     ?tenv:Tenv.t
  -> ?block_return_type:Clang_ast_t.qual_type
  -> ?outer_proc:Procname.t
  -> Clang_ast_t.decl
  -> Procname.t

val qual_type_of_decl_ptr : Clang_ast_t.pointer -> Clang_ast_t.qual_type

val add_type_from_decl_ref_opt :
  qual_type_to_sil_type -> Tenv.t -> Clang_ast_t.decl_ref option -> bool -> unit

val add_type_from_decl_ref_list :
  qual_type_to_sil_type -> Tenv.t -> Clang_ast_t.decl_ref list -> unit

val get_function_decl_with_body : Clang_ast_t.pointer -> Clang_ast_t.decl option

val get_info_from_decl_ref :
  Clang_ast_t.decl_ref -> Clang_ast_t.named_decl_info * Clang_ast_t.pointer * Clang_ast_t.qual_type

val sil_annot_of_type : Clang_ast_t.qual_type -> Annot.Item.t

val type_of_decl : Clang_ast_t.decl -> Clang_ast_t.type_ptr option

val get_record_fields : Clang_ast_t.decl -> Clang_ast_t.decl list

val get_cxx_base_classes : Clang_ast_t.decl -> Clang_ast_t.type_ptr list

val get_cxx_virtual_base_classes : Clang_ast_t.decl -> Clang_ast_t.type_ptr list

val is_no_escape_block_arg : Clang_ast_t.decl -> bool

val is_cpp_implicit_decl : Clang_ast_t.decl -> bool

val get_superclass_curr_class_objc_from_decl : Clang_ast_t.decl -> Clang_ast_t.decl_ref option

val get_captured_mode :
     lci_capture_this:bool
  -> lci_capture_kind:[< `LCK_ByCopy | `LCK_ByRef | `LCK_StarThis | `LCK_This | `LCK_VLAType]
  -> CapturedVar.capture_mode

val create_objc_block_name : Clang_ast_t.decl_info -> Clang_ast_t.block_decl_info -> string * string

(** How a structured binding is translated. Not modelled: the initialization of the decomposed
    object of a global or static local decomposition, and C++26 binding packs, whose bindings the
    clang plugin does not export. *)
type structured_binding =
  | BindingExpr of Clang_ast_t.stmt
      (** decomposition of an array, a vector, a complex number or a struct: the binding is a name
          for this expression, an element or a field of the decomposed object *)
  | BindingTemporary of {init: Clang_ast_t.stmt; qual_type: Clang_ast_t.qual_type}
      (** tuple-like binding to the result of a [get<i>()] that returns by value: the binding is the
          lifetime-extended temporary holding that result, of type [qual_type], and is initialized
          like a variable with [init] *)
  | BindingReference
      (** tuple-like binding when [std::tuple_element<i, E>::type] is a reference, eg for
          [std::tuple<int&>], or when the decomposed object is a reference not bound to a temporary,
          eg [auto& [x, y] = p;]: the binding is a reference to the result of [get<i>()] *)
  | BindingAlias
      (** other tuple-like bindings, when the decomposed object is a copy or a temporary bound to a
          reference: the binding is an alias for the result of [get<i>()], an object that usually
          belongs to the decomposed object *)

val get_structured_binding :
  Clang_ast_t.qual_type -> Clang_ast_t.binding_decl_info -> structured_binding option
(** [get_structured_binding qual_type binding_info] for a binding of type [qual_type]; [None] when
    the AST does not say what the binding refers to *)
