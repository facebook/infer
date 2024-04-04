(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val find_normal_variable_funcall :
  Procdesc.Node.t -> Ident.t -> (Exp.t * Exp.t list * Location.t * CallFlags.t) option
(** Find the function call instruction used to initialize normal variable [id], and return the
    function name and arguments *)

val find_program_variable_assignment :
  Procdesc.Node.t -> Pvar.t -> (Procdesc.Node.t * Ident.t) option
(** Find a program variable assignment in the current node or straightline predecessor. *)
