(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for builtin functions with their symbolic execution handler *)

type args = {
  pdesc : Cfg.Procdesc.t;
  instr : Sil.instr;
  tenv : Tenv.t;
  prop_ : Prop.normal Prop.t;
  path : Paths.Path.t;
  ret_ids : Ident.t list;
  args : (Sil.exp * Sil.typ) list;
  proc_name : Procname.t;
  loc : Location.t;
}

type ret_typ = (Prop.normal Prop.t * Paths.Path.t) list

type t = args -> ret_typ

val register : string -> t -> Procname.t
(** Register a builtin function name and symbolic execution handler *)

val register_procname : Procname.t -> t -> unit
(** Register a builtin [Procname.t] and symbolic execution handler *)

val is_registered : Procname.t -> bool
(** Check if the function is a builtin *)

val get : Procname.t -> t
(** Get the symbolic execution handler associated to the builtin function name *)

val print_and_exit : unit -> 'a
(** Print the builtin functions and exit *)
