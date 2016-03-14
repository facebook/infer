(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Translate one file into a cfg. Create a tenv, cg and cfg file for a source file    *)
(** given its ast in json format. Translate the json file into a cfg by adding all     *)
(** the type and class declarations to the tenv, adding all the functions and methods  *)
(** declarations as procdescs to the cfg, and adding the control flow graph of all the *)
(** code of those functions and methods to the cfg   *)

module type CFrontend_decl = sig
  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> Clang_ast_t.decl ->
    CModule_type.block_data option -> unit

  val translate_one_declaration : Sil.tenv -> Cg.t -> Cfg.cfg ->
    Clang_ast_t.decl -> Clang_ast_t.decl -> unit
end

module CFrontend_decl_funct(T: CModule_type.CTranslation) : CFrontend_decl
