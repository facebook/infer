(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val remove_ret : Tenv.t -> Procdesc.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** remove the return variable from the prop *)

val remove_locals_ret : Tenv.t -> Procdesc.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** remove locals and return variable from the prop *)

val remove_locals_formals :
  Tenv.t -> Procdesc.t -> Prop.normal Prop.t -> Pvar.t list * Prop.normal Prop.t
(** Deallocate the stack variables in [pvars], and replace them by normal variables.
    Return the list of stack variables whose address was still present after deallocation. *)

val remove_seed_vars : Tenv.t -> 'a Prop.t -> Prop.normal Prop.t
(** remove seed vars from a prop *)
