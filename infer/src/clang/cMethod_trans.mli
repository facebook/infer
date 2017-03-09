(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Methods for creating a procdesc from a method or function declaration
    and for resolving a method call and finding the right callee *)

(** When the methoc call is MCStatic, means that it is a class method.  When it is MCVirtual, it
    means that it is an instance method and that the method to be called will be determined at
    runtime. If it is MCNoVirtual it means that it is an instance method but that the method to be
    called will be determined at compile time *)
type method_call_type =
  | MCVirtual
  | MCNoVirtual
  | MCStatic [@@deriving compare]

val equal_method_call_type : method_call_type -> method_call_type -> bool

val should_add_return_param : Typ.t -> is_objc_method:bool -> bool

val create_local_procdesc : ?set_objc_accessor_attr:bool ->
  CFrontend_config.translation_unit_context -> Cfg.cfg -> Tenv.t ->
  CMethod_signature.method_signature -> Clang_ast_t.stmt list -> (Pvar.t * Typ.t) list ->
  bool -> bool

val create_external_procdesc : Cfg.cfg -> Typ.Procname.t -> bool -> (Typ.t * Typ.t list) option -> unit

val get_objc_method_data : Clang_ast_t.obj_c_message_expr_info ->
  (string * Clang_ast_t.pointer option * method_call_type)

val get_class_name_method_call_from_receiver_kind : CContext.t ->
  Clang_ast_t.obj_c_message_expr_info -> (Exp.t * Typ.t) list -> Typename.t

val get_class_name_method_call_from_clang : CFrontend_config.translation_unit_context -> Tenv.t ->
  Clang_ast_t.obj_c_message_expr_info -> Typename.t option

val method_signature_of_decl : CFrontend_config.translation_unit_context -> Tenv.t ->
  Clang_ast_t.decl -> CModule_type.block_data option ->
  CMethod_signature.method_signature * Clang_ast_t.stmt option * CModule_type.instr_type list

val method_signature_of_pointer : CFrontend_config.translation_unit_context -> Tenv.t ->
  Clang_ast_t.pointer -> CMethod_signature.method_signature option

val get_method_name_from_clang : Tenv.t -> CMethod_signature.method_signature option ->
  CMethod_signature.method_signature option

val create_procdesc_with_pointer : CContext.t -> Clang_ast_t.pointer -> Typename.t option ->
  string -> Typ.Procname.t

val add_default_method_for_class : CFrontend_config.translation_unit_context -> Typename.t ->
  Clang_ast_t.decl_info -> unit

val get_procname_from_cpp_lambda : CContext.t -> Clang_ast_t.decl -> Typ.Procname.t
