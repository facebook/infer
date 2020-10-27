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
  | Apply of Ses.Funsym.t * t array
[@@deriving compare, equal, sexp]

module Var : sig
  type trm := t

  include Ses.Var_intf.VAR with type t = private trm

  val of_ : trm -> t
end

module Arith :
  Arithmetic.S with type var := Var.t with type trm := t with type t = arith

val ppx : Var.t Var.strength -> t pp
val _Var : int -> string -> t
val _Z : Z.t -> t
val _Q : Q.t -> t
val _Arith : Arith.t -> t
val _Splat : t -> t
val _Sized : t -> t -> t
val _Extract : t -> t -> t -> t
val _Concat : t array -> t
val _Select : int -> t -> t
val _Update : int -> t -> t -> t
val _Record : t array -> t
val _Ancestor : int -> t
val _Apply : Ses.Funsym.t -> t array -> t
val add : t -> t -> t
val sub : t -> t -> t
val seq_size_exn : t -> t
val seq_size : t -> t option
val vars : t -> Var.t iter
val zero : t
val one : t
val map_vars : t -> f:(Var.t -> Var.t) -> t
