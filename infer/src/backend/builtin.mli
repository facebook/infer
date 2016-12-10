(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for builtin functions with their symbolic execution handler *)

type args = {
  pdesc : Procdesc.t;
  instr : Sil.instr;
  tenv : Tenv.t;
  prop_ : Prop.normal Prop.t;
  path : Paths.Path.t;
  ret_id : (Ident.t * Typ.t) option;
  args : (Exp.t * Typ.t) list;
  proc_name : Procname.t;
  loc : Location.t;
}

type ret_typ = (Prop.normal Prop.t * Paths.Path.t) list

type t = args -> ret_typ

type registered

val register : Procname.t -> t -> registered
(** Register a builtin [Procname.t] and symbolic execution handler *)

val is_registered : Procname.t -> bool
(** Check if the function is a builtin *)

val get : Procname.t -> t option
(** Get the symbolic execution handler associated to the builtin function name *)

val print_and_exit : unit -> 'a
(** Print the builtin functions and exit *)
