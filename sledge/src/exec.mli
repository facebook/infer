(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

open Ses

val assume : Sh.t -> Term.t -> Sh.t option
val kill : Sh.t -> Var.t -> Sh.t
val move : Sh.t -> (Var.t * Term.t) iarray -> Sh.t
val load : Sh.t -> reg:Var.t -> ptr:Term.t -> len:Term.t -> Sh.t option
val store : Sh.t -> ptr:Term.t -> exp:Term.t -> len:Term.t -> Sh.t option
val memset : Sh.t -> dst:Term.t -> byt:Term.t -> len:Term.t -> Sh.t option
val memcpy : Sh.t -> dst:Term.t -> src:Term.t -> len:Term.t -> Sh.t option
val memmov : Sh.t -> dst:Term.t -> src:Term.t -> len:Term.t -> Sh.t option
val alloc : Sh.t -> reg:Var.t -> num:Term.t -> len:int -> Sh.t option
val free : Sh.t -> ptr:Term.t -> Sh.t option
val nondet : Sh.t -> Var.t option -> Sh.t
val abort : Sh.t -> Sh.t option

val intrinsic :
     skip_throw:bool
  -> Sh.t
  -> Var.t option
  -> Var.t
  -> Term.t list
  -> Sh.t option option
