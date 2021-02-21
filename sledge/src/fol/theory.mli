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

val solvables : Trm.t -> Trm.t iter
(** The maximal noninterpreted terms (according to {!is_noninterpreted})
    occurring in a term. Note that for noninterpreted terms, this is the
    single term itself. *)

val solvable_trms : Trm.t -> Trm.t iter
(** The solvables of immediate subterms. That is, maximal noninterpreted
    strict subterms. *)

val map_solvables : Trm.t -> f:(Trm.t -> Trm.t) -> Trm.t
(** Map over the {!solvables}. *)

type kind = InterpApp | NonInterpAtom | InterpAtom | UninterpApp
[@@deriving compare, equal]

val classify : Trm.t -> kind

val is_noninterpreted : Trm.t -> bool
(** Test if a term is either a variable ({!Trm.Var}) or an uninterpreted
    function symbol application ({!Trm.Apply} or {!Trm.Arith} when
    [{!Trm.Arith.classify} a = Uninterpreted]). That is, is not an
    interpreted function symbol application (including constants and nullary
    applications). *)

val is_interpreted : Trm.t -> bool
val is_uninterpreted : Trm.t -> bool
val prefer : Trm.t -> Trm.t -> int
val solve_concat : Trm.t array -> Trm.t -> Trm.t -> t -> t
val solve : Trm.t -> Trm.t -> t -> t
