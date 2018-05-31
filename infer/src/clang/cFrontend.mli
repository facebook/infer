(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val do_source_file : CFrontend_config.translation_unit_context -> Clang_ast_t.decl -> unit
(** Translate one file into a cfg. Create a tenv, cg and cfg file for a source file given its ast in
    json format. Translate the json file into a cfg by adding all the type and class declarations to
    the tenv, adding all the functions and methods declarations as procdescs to the cfg, and adding
    the control flow graph of all the code of those functions and methods to the cfg. *)
