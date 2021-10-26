(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Theory Solver *)

type oriented_equality = {var: Trm.t; rep: Trm.t}

type t =
  { wrt: Var.Set.t
  ; no_fresh: bool
  ; fresh: Var.Set.t
  ; solved: oriented_equality list option
  ; pending: (Trm.t * Trm.t) list }

val pp : t pp
val prefer : Trm.t -> Trm.t -> int
val solve_concat : Trm.sized array -> Trm.t -> Trm.t -> t -> t
val solve : Trm.t -> Trm.t -> t -> t
