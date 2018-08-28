(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Methods for creating a procdesc from a method or function declaration
    and for resolving a method call and finding the right callee *)

(** When the methoc call is MCStatic, means that it is a class method.  When it is MCVirtual, it
    means that it is an instance method and that the method to be called will be determined at
    runtime. If it is MCNoVirtual it means that it is an instance method but that the method to be
    called will be determined at compile time *)
type method_call_type = MCVirtual | MCNoVirtual | MCStatic [@@deriving compare]

val equal_method_call_type : method_call_type -> method_call_type -> bool

val create_local_procdesc :
     ?set_objc_accessor_attr:bool
  -> CFrontend_config.translation_unit_context
  -> Cfg.t
  -> Tenv.t
  -> CMethodSignature.t
  -> Clang_ast_t.stmt list
  -> (Pvar.t * Typ.t) list
  -> bool

val create_external_procdesc :
     CFrontend_config.translation_unit_context
  -> Cfg.t
  -> Typ.Procname.t
  -> ClangMethodKind.t
  -> (Typ.t * Typ.t list) option
  -> unit

val get_objc_method_data :
  Clang_ast_t.obj_c_message_expr_info -> string * Clang_ast_t.pointer option * method_call_type

val get_class_name_method_call_from_receiver_kind :
  CContext.t -> Clang_ast_t.obj_c_message_expr_info -> (Exp.t * Typ.t) list -> Typ.Name.t

val get_class_name_method_call_from_clang :
  Tenv.t -> Clang_ast_t.obj_c_message_expr_info -> Typ.Name.t option

val method_signature_of_pointer : Tenv.t -> Clang_ast_t.pointer -> CMethodSignature.t option

val get_method_name_from_clang : Tenv.t -> CMethodSignature.t option -> Typ.Procname.t option

val create_procdesc_with_pointer :
  CContext.t -> Clang_ast_t.pointer -> Typ.Name.t option -> string -> Typ.Procname.t

val get_procname_from_cpp_lambda : CContext.t -> Clang_ast_t.decl -> Typ.Procname.t

val get_captures_from_cpp_lambda : Clang_ast_t.decl -> Clang_ast_t.lambda_capture_info list
