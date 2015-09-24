(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Methods for creating a procdesc from a method or function declaration
    and for resolving a method call and finding the right callee *)

(** When the methoc call is MCStatic, means that it is a class method. *)
(** When it is MCVirtual, it means that it is an instance method and that *)
(** the method to be called will be determined at runtime. If it is MCNoVirtual *)
(** it means that it is an instance method but that the method to be called will *)
(** be determined at compile time *)
type method_call_type =
  | MCVirtual
  | MCNoVirtual
  | MCStatic

val create_local_procdesc : Cfg.cfg -> Sil.tenv -> CMethod_signature.method_signature ->
  Clang_ast_t.stmt list -> (Mangled.t * Sil.typ * bool) list -> bool -> bool

val create_external_procdesc : Cfg.cfg -> Procname.t -> bool -> (Sil.typ * Sil.typ list) option -> unit

val captured_vars_from_block_info : CContext.t -> Clang_ast_t.block_captured_variable list -> (Mangled.t * Sil.typ * bool) list

val get_class_selector_instance : CContext.t -> Clang_ast_t.obj_c_message_expr_info -> (Sil.exp * Sil.typ) list
  -> (string * string * Clang_ast_t.pointer option * method_call_type)

val should_create_procdesc : Cfg.cfg -> Procname.t -> bool -> bool -> bool

val method_signature_of_decl : string option -> Clang_ast_t.decl -> CModule_type.block_data option ->
  CMethod_signature.method_signature * Clang_ast_t.stmt option * Clang_ast_t.decl list

val method_signature_of_pointer : string option -> Clang_ast_t.pointer -> CMethod_signature.method_signature option

val create_procdesc_with_pointer : CContext.t -> Clang_ast_t.pointer -> string option ->
  string -> Clang_ast_t.qual_type -> Procname.t
