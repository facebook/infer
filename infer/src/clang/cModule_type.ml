(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type block_data =
  { captured_vars: (Pvar.t * Typ.t * CapturedVar.capture_mode) list
  ; context: CContext.t
  ; block_as_arg_attributes: ProcAttributes.block_as_arg_attributes option
  ; procname: Procname.t
  ; return_type: Clang_ast_t.qual_type }

module type CTranslation = sig
  (** Translates instructions: (statements and expressions) from the ast into sil *)

  val instructions_trans :
       CContext.t
    -> Clang_ast_t.stmt
    -> CFrontend_config.instr_type list
    -> Procdesc.Node.t
    -> is_destructor_wrapper:bool
    -> Procdesc.Node.t list
  (** It receives the context, a list of statements from clang ast, list of custom statments to be
      added before clang statements and the exit node and it returns a list of cfg nodes that
      represent the translation of the stmts into sil. *)
end

module type CFrontend = sig
  val function_decl :
       CFrontend_config.translation_unit_context
    -> Tenv.t
    -> Cfg.t
    -> Clang_ast_t.decl
    -> block_data option
    -> unit

  val translate_one_declaration :
       CFrontend_config.translation_unit_context
    -> Tenv.t
    -> Cfg.t
    -> CFrontend_config.decl_trans_context
    -> Clang_ast_t.decl
    -> unit
end
