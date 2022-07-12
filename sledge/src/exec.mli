(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

open Fol
open Symbolic_heap

val assume : Sh.t -> Formula.t -> Sh.t
val kill : Sh.t -> Var.t -> Sh.t
val move : Sh.t -> (Var.t * Term.t) iarray -> Sh.t
val load : Sh.t -> reg:Var.t -> ptr:Term.t -> len:Term.t -> Sh.t option
val store : Sh.t -> ptr:Term.t -> exp:Term.t -> len:Term.t -> Sh.t option

val atomic_rmw :
  Sh.t -> reg:Var.t -> ptr:Term.t -> exp:Term.t -> len:Term.t -> Sh.t option

val atomic_cmpxchg :
     Sh.t
  -> reg:Var.t
  -> ptr:Term.t
  -> cmp:Term.t
  -> exp:Term.t
  -> len:Term.t
  -> len1:Term.t
  -> Sh.t option

val alloc : Sh.t -> reg:Var.t -> num:Term.t -> len:int -> Sh.t option
val free : Sh.t -> ptr:Term.t -> Sh.t option
val nondet : Sh.t -> Var.t option -> Sh.t

val builtin :
  Sh.t -> Var.t option -> Llair.Builtin.t -> Term.t iarray -> Sh.t option
