(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Theory Solver *)

type t =
  { wrt: Var.Set.t
  ; no_fresh: bool
  ; fresh: Var.Set.t
  ; solved: (Trm.t * Trm.t) list option
  ; pending: (Trm.t * Trm.t) list }

val pp : t pp

type kind = InterpApp | NonInterpAtom | InterpAtom | UninterpApp
[@@deriving compare, equal]

val classify : Trm.t -> kind
val is_interpreted : Trm.t -> bool
val is_uninterpreted : Trm.t -> bool
val prefer : Trm.t -> Trm.t -> int
val solve_concat : Trm.t array -> Trm.t -> Trm.t -> t -> t
val solve : Trm.t -> Trm.t -> t -> t
