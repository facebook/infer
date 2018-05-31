(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val remove_locals_ret : Tenv.t -> Procdesc.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** remove locals and return variable from the prop *)

val remove_locals_formals :
  Tenv.t -> Procdesc.t -> Prop.normal Prop.t -> Pvar.t list * Prop.normal Prop.t
(** Deallocate the stack variables in [pvars], and replace them by normal variables.
    Return the list of stack variables whose address was still present after deallocation. *)

val remove_seed_vars : Tenv.t -> 'a Prop.t -> Prop.normal Prop.t
(** remove seed vars from a prop *)
