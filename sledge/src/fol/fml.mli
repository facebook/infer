(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Formulas *)

type set

type t = private
  (* propositional constants *)
  | Tt
  (* equality *)
  | Eq of Trm.t * Trm.t
  (* arithmetic *)
  | Eq0 of Trm.t  (** [Eq0(x)] iff x = 0 *)
  | Pos of Trm.t  (** [Pos(x)] iff x > 0 *)
  (* propositional connectives *)
  | Not of t
  | And of {pos: set; neg: set}
  | Or of {pos: set; neg: set}
  | Iff of t * t
  | Cond of {cnd: t; pos: t; neg: t}
  (* uninterpreted *)
  | Lit of Predsym.t * Trm.t array
[@@deriving compare, equal, sexp]

module Set : sig
  include Set.S with type elt := t with type t = set

  val t_of_sexp : Sexp.t -> t
end

val ppx : Var.strength -> t pp
val pp : t pp

(** Construct *)

(* propositional constants *)
val tt : t
val ff : t
val bool : bool -> t

(* equality *)
val eq : Trm.t -> Trm.t -> t

(* arithmetic *)
val eq0 : Trm.t -> t
val pos : Trm.t -> t

(* propositional connectives *)
val not_ : t -> t
val and_ : t -> t -> t
val andN : pos:set -> neg:set -> t
val or_ : t -> t -> t
val orN : pos:set -> neg:set -> t
val iff : t -> t -> t
val cond : cnd:t -> pos:t -> neg:t -> t

(* uninterpreted *)
val lit : Predsym.t -> Trm.t array -> t

(** Transform *)

val map_vars : t -> f:(Var.t -> Var.t) -> t
val map_trms : t -> f:(Trm.t -> Trm.t) -> t
val map_and : t -> pos:set -> neg:set -> (t -> t) -> t
val map_or : t -> pos:set -> neg:set -> (t -> t) -> t

(** Traverse *)

val fold_pos_neg : pos:set -> neg:set -> 'a -> f:(t -> 'a -> 'a) -> 'a

val iter_dnf :
  meet1:(t -> 'a -> 'a) -> top:'a -> t -> f:('a -> unit) -> unit

val vars : t -> Var.t iter
val trms : t -> Trm.t iter

(** Query *)

val is_negative : t -> bool
