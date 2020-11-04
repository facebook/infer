(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms *)

type arith

type t = private
  (* variables *)
  | Var of {id: int; name: string}
  (* arithmetic *)
  | Z of Z.t
  | Q of Q.t
  | Arith of arith
  (* sequences (of flexible size) *)
  | Splat of t
  | Sized of {seq: t; siz: t}
  | Extract of {seq: t; off: t; len: t}
  | Concat of t array
  (* records (with fixed indices) *)
  | Select of {idx: int; rcd: t}
  | Update of {idx: int; rcd: t; elt: t}
  | Record of t array
  | Ancestor of int
  (* uninterpreted *)
  | Apply of Funsym.t * t array
[@@deriving compare, equal, sexp]

module Var : sig
  type trm := t

  include Var_intf.VAR with type t = private trm

  val of_ : trm -> t
  val of_trm : trm -> t option
end

module Arith :
  Arithmetic.S with type var := Var.t with type trm := t with type t = arith

module Map : sig
  include Map.S with type key := t

  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
end

val ppx : Var.strength -> t pp
val pp : t pp
val pp_diff : (t * t) pp

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
val arith : Arith.t -> t

(* sequences (of flexible size) *)
val splat : t -> t
val sized : seq:t -> siz:t -> t
val extract : seq:t -> off:t -> len:t -> t
val concat : t array -> t

(* records (with fixed indices) *)
val select : rcd:t -> idx:int -> t
val update : rcd:t -> idx:int -> elt:t -> t
val record : t array -> t
val ancestor : int -> t

(* uninterpreted *)
val apply : Funsym.t -> t array -> t

(** Transform *)

val map_vars : t -> f:(Var.t -> Var.t) -> t
val map : t -> f:(t -> t) -> t

(** Query *)

val seq_size_exn : t -> t
val seq_size : t -> t option
val fv : t -> Var.Set.t
val height : t -> int

(** Traverse *)

val vars : t -> Var.t iter
val subtrms : t -> t iter
