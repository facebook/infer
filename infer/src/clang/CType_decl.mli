(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module CProcname : sig
  val from_decl :
       ?tenv:Tenv.t
    -> ?block_return_type:Clang_ast_t.qual_type
    -> ?outer_proc:Procname.t
    -> Clang_ast_t.decl
    -> Procname.t
  (** Given decl, return its procname. This function should be used for all procedures present in
      original AST *)

  (** WARNING: functions from this module should not be used if full decl is available in AST *)
  module NoAstDecl : sig
    val c_function_of_string : Tenv.t -> string -> Procname.t

    val cpp_method_of_string : Tenv.t -> Typ.Name.t -> string -> Procname.t

    val objc_method_of_string_kind :
         Typ.Name.t
      -> string
      -> Procname.ObjC_Cpp.kind
      -> Procname.Parameter.clang_parameter list
      -> Procname.t
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

val get_template_args :
  Tenv.t -> Clang_ast_t.template_instantiation_arg_info list -> Typ.template_arg list

val method_signature_of_decl :
     Tenv.t
  -> Clang_ast_t.decl
  -> ?block_return_type:Clang_ast_t.qual_type
  -> ?block_as_arg_attributes:ProcAttributes.block_as_arg_attributes option
  -> Procname.t
  -> CMethodSignature.t

val method_signature_body_of_decl :
     Tenv.t
  -> Clang_ast_t.decl
  -> ?block_return_type:Clang_ast_t.qual_type
  -> ?block_as_arg_attributes:ProcAttributes.block_as_arg_attributes option
  -> Procname.t
  -> CMethodSignature.t * Clang_ast_t.stmt option * CFrontend_config.instr_type list

val should_add_return_param : Typ.t -> bool

val type_of_captured_var :
  Tenv.t -> is_block_inside_objc_class_method:bool -> Clang_ast_t.decl_ref -> Typ.t option
