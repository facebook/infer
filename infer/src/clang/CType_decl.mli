(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module CProcname : sig
  val from_decl :
       ?tenv:Tenv.t
    -> ?block_return_type:Clang_ast_t.qual_type
    -> ?outer_proc:Typ.Procname.t
    -> Clang_ast_t.decl
    -> Typ.Procname.t
  (** Given decl, return its procname. This function should be used for all procedures
      present in original AST *)

  val from_decl_for_linters : Clang_ast_t.decl -> Typ.Procname.t
  (** This is used for bug hashing for linters. In ObjC the method names contain the parameter names,
      thus if people add new parameters, any bug about the method will be considered different which means
    reporting on unchanged code. So, in the ObjC method case, we create the method name only based on the
    first part of the name without the parameters *)

  (** WARNING: functions from this module should not be used if full decl is available in AST *)
  module NoAstDecl : sig
    val c_function_of_string : Tenv.t -> string -> Typ.Procname.t

    val cpp_method_of_string : Tenv.t -> Typ.Name.t -> string -> Typ.Procname.t

    val objc_method_of_string_kind :
      Typ.Name.t -> string -> Typ.Procname.ObjC_Cpp.kind -> Typ.Procname.t
  end
end

(** Processes types and record declarations by adding them to the tenv *)

val get_record_typename : ?tenv:Tenv.t -> Clang_ast_t.decl -> Typ.Name.t

val add_types_from_decl_to_tenv : Tenv.t -> Clang_ast_t.decl -> Typ.desc

val add_predefined_types : Tenv.t -> unit
(** Add the predefined types objc_class which is a struct, and Class, which is a pointer to
    objc_class. *)

val qual_type_to_sil_type : Tenv.t -> Clang_ast_t.qual_type -> Typ.t

val class_from_pointer_type : Tenv.t -> Clang_ast_t.qual_type -> Typ.Name.t

val get_type_from_expr_info : Clang_ast_t.expr_info -> Tenv.t -> Typ.t

val method_signature_of_decl :
     Tenv.t
  -> Clang_ast_t.decl
  -> ?block_return_type:Clang_ast_t.qual_type
  -> Typ.Procname.t
  -> CMethodSignature.t

val method_signature_body_of_decl :
     Tenv.t
  -> Clang_ast_t.decl
  -> ?block_return_type:Clang_ast_t.qual_type
  -> Typ.Procname.t
  -> CMethodSignature.t
     * Clang_ast_t.stmt option
     * [> `CXXConstructorInit of Clang_ast_t.cxx_ctor_initializer] list

val should_add_return_param : Typ.typ -> is_objc_method:bool -> bool

val type_of_captured_var :
  Tenv.t -> is_block_inside_objc_class_method:bool -> Clang_ast_t.decl_ref -> Typ.typ option
