(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val program_to_textual_module : Textual.SourceFile.t -> DoliAst.doliProgram -> Textual.Module.t
(** [program_to_textual_module] takes a string representing a filename, and a doliProgram, and
    returns a module which contains declarations for all the doli rules from the program. Moreover,
    it stores the declarations in the global variable [global_doli_matcher] *)

val matcher : Procname.t -> Procname.t option
(** [matcher procname] searches in the declarations in the global variable [global_doli_matcher] for
    a doli-model that matches [procname]. If it finds one, then it returns the internal name of that
    doli-model. *)
