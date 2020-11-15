(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms *)
module rec Term : sig
  type t [@@deriving compare, equal, sexp]

  (* pretty-printing *)
  val ppx : Var.strength -> t pp
  val pp : t pp

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t

  (** Construct *)

  (* variables *)
  val var : Var.t -> t

  (* arithmetic *)
  val zero : t
  val one : t
  val integer : Z.t -> t
  val rational : Q.t -> t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mulq : Q.t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> int -> t

  (* sequences (of flexible size) *)
  val splat : t -> t
  (** Iterated concatenation of a single byte *)

  val sized : seq:t -> siz:t -> t
  (** Size-tagged sequence *)

  val extract : seq:t -> off:t -> len:t -> t
  (** Extract a slice of a sequence *)

  val concat : t array -> t
  (** Concatenation of sequences *)

  (* uninterpreted *)
  val apply : Funsym.t -> t array -> t

  (* if-then-else *)
  val ite : cnd:Formula.t -> thn:t -> els:t -> t

  (* Trm.t is embedded into Term.t *)
  val of_trm : Trm.t -> t
  val get_trm : t -> Trm.t option

  (** Destruct *)

  val get_z : t -> Z.t option
  (** [get_z a] is [Some z] iff [equal a (integer z)] *)

  val get_q : t -> Q.t option
  (** [get_q a] is [Some q] iff [equal a (rational q)] *)

  (** Access *)

  val split_const : t -> t * Q.t
  (** Splits a term into the sum of its constant and non-constant parts.
      That is, [split_const a] is [(b, c)] such that [a = b + c] and the
      absolute value of [c] is maximal. *)

  (** Query *)

  val fv : t -> Var.Set.t

  (** Traverse *)

  val vars : t -> Var.t iter
  val atoms : t -> t iter

  (** Transform *)

  val map_vars : t -> f:(Var.t -> Var.t) -> t
  val map_trms : t -> f:(Trm.t -> Trm.t) -> t
  val fold_map_vars : t -> 's -> f:(Var.t -> 's -> Var.t * 's) -> t * 's
  val rename : Var.Subst.t -> t -> t
end

(** Formulas *)
and Formula : sig
  type t = Fml.t [@@deriving compare, equal, sexp]

  val inject : t -> Term.t
  val project : Term.t -> t option

  (* pretty-printing *)
  val ppx : Var.strength -> t pp
  val pp : t pp

  (** Construct *)

  (* constants *)
  val tt : t
  val ff : t

  (* equality *)
  val eq : Term.t -> Term.t -> t
  val dq : Term.t -> Term.t -> t

  (* arithmetic *)
  val eq0 : Term.t -> t
  val dq0 : Term.t -> t
  val pos : Term.t -> t
  val gt : Term.t -> Term.t -> t
  val ge : Term.t -> Term.t -> t
  val lt : Term.t -> Term.t -> t
  val le : Term.t -> Term.t -> t

  (* uninterpreted *)
  val lit : Predsym.t -> Term.t array -> t

  (* connectives *)
  val not_ : t -> t
  val and_ : t -> t -> t
  val andN : t list -> t
  val or_ : t -> t -> t
  val orN : t list -> t
  val iff : t -> t -> t
  val xor : t -> t -> t
  val cond : cnd:t -> pos:t -> neg:t -> t

  (** Query *)

  val fv : t -> Var.Set.t

  (** Traverse *)

  val vars : t -> Var.t iter

  (** Transform *)

  val map_terms : f:(Term.t -> Term.t) -> t -> t
  val map_vars : t -> f:(Var.t -> Var.t) -> t
  val fold_map_vars : t -> 's -> f:(Var.t -> 's -> Var.t * 's) -> t * 's
  val rename : Var.Subst.t -> t -> t
end
