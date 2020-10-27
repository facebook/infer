(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms *)

type arith

type trm = private
  (* variables *)
  | Var of {id: int; name: string}
  (* arithmetic *)
  | Z of Z.t
  | Q of Q.t
  | Arith of arith
  (* sequences (of flexible size) *)
  | Splat of trm
  | Sized of {seq: trm; siz: trm}
  | Extract of {seq: trm; off: trm; len: trm}
  | Concat of trm array
  (* records (with fixed indices) *)
  | Select of {idx: int; rcd: trm}
  | Update of {idx: int; rcd: trm; elt: trm}
  | Record of trm array
  | Ancestor of int
  (* uninterpreted *)
  | Apply of Ses.Funsym.t * trm array
[@@deriving compare, equal, sexp]

module Var : sig
  include Ses.Var_intf.VAR with type t = private trm

  val of_ : trm -> t
end

module Arith :
  Arithmetic.S
    with type var := Var.t
    with type trm := trm
    with type t = arith

val ppx : Var.t Var.strength -> trm pp
val _Var : int -> string -> trm
val _Z : Z.t -> trm
val _Q : Q.t -> trm
val _Arith : Arith.t -> trm
val _Splat : trm -> trm
val _Sized : trm -> trm -> trm
val _Extract : trm -> trm -> trm -> trm
val _Concat : trm array -> trm
val _Select : int -> trm -> trm
val _Update : int -> trm -> trm -> trm
val _Record : trm array -> trm
val _Ancestor : int -> trm
val _Apply : Ses.Funsym.t -> trm array -> trm
val add : trm -> trm -> trm
val sub : trm -> trm -> trm
val seq_size_exn : trm -> trm
val seq_size : trm -> trm option
val vars : trm -> Var.t iter
val zero : trm
val one : trm
