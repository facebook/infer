(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

open Fol
open Symbolic_heap

val assume : Xsh.t -> Formula.t -> Xsh.t
val kill : Xsh.t -> Var.t -> Xsh.t
val move : Xsh.t -> (Var.t * Term.t) iarray -> Xsh.t
val load : Xsh.t -> reg:Var.t -> ptr:Term.t -> len:Term.t -> Xsh.t option
val store : Xsh.t -> ptr:Term.t -> exp:Term.t -> len:Term.t -> Xsh.t option

val atomic_rmw :
     Xsh.t
  -> reg:Var.t
  -> ptr:Term.t
  -> exp:Term.t
  -> len:Term.t
  -> Xsh.t option

val atomic_cmpxchg :
     Xsh.t
  -> reg:Var.t
  -> ptr:Term.t
  -> cmp:Term.t
  -> exp:Term.t
  -> len:Term.t
  -> len1:Term.t
  -> Xsh.t option

val alloc : Xsh.t -> reg:Var.t -> num:Term.t -> len:int -> Xsh.t option
val free : Xsh.t -> ptr:Term.t -> Xsh.t option
val nondet : Xsh.t -> Var.t option -> Xsh.t

val builtin :
  Xsh.t -> Var.t option -> Llair.Builtin.t -> Term.t iarray -> Xsh.t option
